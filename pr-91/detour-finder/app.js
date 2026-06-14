const App = (() => {
  let map;
  let baseRouteLayer;
  let detourRouteLayer;
  let markersLayer;
  let originMarker;
  let destMarker;
  let allPlaces = [];
  let detourResults = [];
  let activeFilter = "all";
  let selectedIdx = -1;

  const COLORS = {
    base: "#4A90D9",
    detour: "#E74C3C",
    green: "#27AE60",
    yellow: "#F39C12",
    orange: "#E67E22",
    red: "#C0392B",
  };

  function formatDuration(sec) {
    const h = Math.floor(sec / 3600);
    const m = Math.round((sec % 3600) / 60);
    if (h > 0) return `${h}h ${m}m`;
    return `${m} min`;
  }

  function detourColor(min) {
    if (min <= 5) return COLORS.green;
    if (min <= 15) return COLORS.yellow;
    if (min <= 30) return COLORS.orange;
    return COLORS.red;
  }

  function initMap() {
    map = L.map("map", { zoomControl: false }).setView([34.05, -118.25], 11);
    L.control.zoom({ position: "topright" }).addTo(map);
    L.tileLayer("https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png", {
      attribution: "&copy; OpenStreetMap contributors",
      maxZoom: 19,
    }).addTo(map);
    baseRouteLayer = L.layerGroup().addTo(map);
    detourRouteLayer = L.layerGroup().addTo(map);
    markersLayer = L.layerGroup().addTo(map);
  }

  function updatePlacesCount() {
    const geoCount = allPlaces.filter(
      (p) => p.lat != null && p.lng != null
    ).length;
    const el = document.getElementById("places-count");
    el.textContent = `${geoCount} places loaded`;
    el.style.display = geoCount > 0 ? "inline" : "none";
  }

  function setStatus(msg, type) {
    const el = document.getElementById("status");
    el.textContent = msg;
    el.className = "status " + (type || "");
    el.style.display = msg ? "block" : "none";
  }

  function setProgress(current, total, label) {
    const bar = document.getElementById("progress-bar");
    const fill = document.getElementById("progress-fill");
    const text = document.getElementById("progress-text");
    if (!total) {
      bar.style.display = "none";
      return;
    }
    bar.style.display = "block";
    const pct = Math.round((current / total) * 100);
    fill.style.width = pct + "%";
    text.textContent = label
      ? `${label} (${current}/${total})`
      : `${current}/${total}`;
  }

  async function handleFileUpload(e) {
    const files = e.target.files;
    if (!files.length) return;

    for (const file of files) {
      const text = await file.text();
      let listName = file.name
        .replace(/\.csv$/i, "")
        .replace(/_/g, " ")
        .replace(/saved places /i, "");
      if (/want.to.go/i.test(file.name)) listName = "Want to go";
      else if (/favorite/i.test(file.name)) listName = "Favorites";
      else if (/saved.places/i.test(file.name)) listName = "Saved";

      const parsed = Places.parseCSV(text, listName);
      const needsGeo = parsed.some((p) => p.lat === null);

      if (needsGeo) {
        setStatus(`Geocoding ${listName}...`);
        await Places.geocodeMissing(parsed, (done, total) => {
          setProgress(done, total, "Geocoding");
        });
        setProgress(0, 0);
      }

      const existingNames = new Set(
        allPlaces.map((p) => `${p.name}|${p.list}`)
      );
      const newPlaces = parsed.filter(
        (p) => !existingNames.has(`${p.name}|${p.list}`)
      );
      allPlaces = allPlaces.concat(newPlaces);
    }

    Places.saveToStorage(allPlaces);
    updatePlacesCount();
    updateFilterButtons();
    setStatus(
      `Loaded ${allPlaces.length} places`,
      "success"
    );
    setTimeout(() => setStatus(""), 3000);
    e.target.value = "";
  }

  function updateFilterButtons() {
    const lists = [...new Set(allPlaces.map((p) => p.list))];
    const container = document.getElementById("filter-buttons");
    container.innerHTML = "";

    const allBtn = document.createElement("button");
    allBtn.textContent = "All";
    allBtn.className =
      "filter-btn" + (activeFilter === "all" ? " active" : "");
    allBtn.onclick = () => {
      activeFilter = "all";
      updateFilterButtons();
      if (detourResults.length) renderResults();
    };
    container.appendChild(allBtn);

    for (const list of lists) {
      const btn = document.createElement("button");
      btn.textContent = list;
      btn.className =
        "filter-btn" + (activeFilter === list ? " active" : "");
      btn.onclick = () => {
        activeFilter = list;
        updateFilterButtons();
        if (detourResults.length) renderResults();
      };
      container.appendChild(btn);
    }
  }

  function filteredResults() {
    if (activeFilter === "all") return detourResults;
    return detourResults.filter((r) => r.place.list === activeFilter);
  }

  function renderResults() {
    const container = document.getElementById("results");
    const visible = filteredResults();

    if (!visible.length) {
      container.innerHTML =
        '<div class="empty-results">No places found along this route</div>';
      return;
    }

    container.innerHTML = visible
      .map((r, i) => {
        const detourStr =
          r.detourMin <= 0
            ? "On route"
            : `+${formatDuration(r.detourSec)}`;
        const color = detourColor(r.detourMin);
        const isSelected = i === selectedIdx;
        return `
        <div class="result-item${isSelected ? " selected" : ""}" data-idx="${i}">
          <div class="result-badge" style="background:${color}">${detourStr}</div>
          <div class="result-info">
            <div class="result-name">${r.place.name}</div>
            <div class="result-meta">
              ${r.place.list !== "Saved" ? `<span class="result-list">${r.place.list}</span>` : ""}
              ${r.place.note ? `<span class="result-note">${r.place.note}</span>` : ""}
              ${r.place.address ? `<span class="result-addr">${r.place.address}</span>` : ""}
            </div>
          </div>
          ${r.place.url ? `<a class="result-link" href="${r.place.url}" target="_blank" title="Open in Google Maps">&#8599;</a>` : ""}
        </div>`;
      })
      .join("");

    container.querySelectorAll(".result-item").forEach((el) => {
      el.addEventListener("click", (e) => {
        if (e.target.closest(".result-link")) return;
        const idx = parseInt(el.dataset.idx);
        selectResult(idx);
      });
    });
  }

  function selectResult(idx) {
    selectedIdx = idx;
    const visible = filteredResults();
    const r = visible[idx];
    if (!r) return;

    detourRouteLayer.clearLayers();
    if (r.viaRoute) {
      L.polyline(r.viaRoute.polyline, {
        color: COLORS.detour,
        weight: 4,
        opacity: 0.8,
        dashArray: "8 6",
      }).addTo(detourRouteLayer);
    }

    map.panTo([r.place.lat, r.place.lng]);
    renderResults();
  }

  function placeMarkers(results) {
    markersLayer.clearLayers();
    for (const r of results) {
      const color = detourColor(r.detourMin);
      const marker = L.circleMarker([r.place.lat, r.place.lng], {
        radius: 8,
        fillColor: color,
        color: "#fff",
        weight: 2,
        fillOpacity: 0.9,
      }).addTo(markersLayer);

      const detourStr =
        r.detourMin <= 0 ? "On route" : `+${formatDuration(r.detourSec)}`;
      marker.bindPopup(
        `<strong>${r.place.name}</strong><br>${detourStr}` +
          (r.place.note ? `<br><em>${r.place.note}</em>` : "") +
          (r.place.url
            ? `<br><a href="${r.place.url}" target="_blank">Google Maps</a>`
            : "")
      );
    }
  }

  async function useMyLocation() {
    if (!navigator.geolocation) {
      setStatus("Geolocation not supported", "error");
      return;
    }
    const input = document.getElementById("origin");
    input.value = "Locating...";
    input.disabled = true;

    navigator.geolocation.getCurrentPosition(
      async (pos) => {
        const { latitude, longitude } = pos.coords;
        input.value = `${latitude.toFixed(5)}, ${longitude.toFixed(5)}`;
        input.disabled = false;
        input.dataset.lat = latitude;
        input.dataset.lng = longitude;

        try {
          const url = `https://nominatim.openstreetmap.org/reverse?format=json&lat=${latitude}&lon=${longitude}`;
          const resp = await fetch(url, {
            headers: { "User-Agent": "DetourFinder/1.0" },
          });
          const data = await resp.json();
          if (data.display_name) {
            const short =
              data.address?.road ||
              data.address?.neighbourhood ||
              data.display_name.split(",").slice(0, 2).join(",");
            input.value = short;
          }
        } catch {
          // keep coordinates as fallback
        }
      },
      () => {
        input.value = "";
        input.disabled = false;
        setStatus("Could not get location", "error");
        setTimeout(() => setStatus(""), 3000);
      },
      { enableHighAccuracy: true, timeout: 10000 }
    );
  }

  async function geocodeInput(inputEl) {
    if (inputEl.dataset.lat && inputEl.dataset.lng) {
      return {
        lat: parseFloat(inputEl.dataset.lat),
        lng: parseFloat(inputEl.dataset.lng),
      };
    }
    const query = inputEl.value.trim();
    if (!query) return null;

    const coordMatch = query.match(
      /^(-?\d+\.?\d*)\s*,\s*(-?\d+\.?\d*)$/
    );
    if (coordMatch) {
      return {
        lat: parseFloat(coordMatch[1]),
        lng: parseFloat(coordMatch[2]),
      };
    }

    const results = await Places.geocodeLocation(query);
    if (results.length === 0) return null;
    return { lat: parseFloat(results[0].lat), lng: parseFloat(results[0].lon) };
  }

  async function calculate() {
    const originInput = document.getElementById("origin");
    const destInput = document.getElementById("destination");
    const corridorSlider = document.getElementById("corridor-slider");
    const corridorKm = parseFloat(corridorSlider.value);
    const calcBtn = document.getElementById("calculate-btn");

    const placesWithCoords = allPlaces.filter(
      (p) => p.lat != null && p.lng != null
    );
    if (!placesWithCoords.length) {
      setStatus("Upload your saved places first", "error");
      setTimeout(() => setStatus(""), 3000);
      return;
    }

    calcBtn.disabled = true;
    calcBtn.textContent = "Working...";

    try {
      setStatus("Geocoding origin...");
      const origin = await geocodeInput(originInput);
      if (!origin) {
        setStatus("Could not find origin -- try adding city/state", "error");
        setTimeout(() => setStatus(""), 5000);
        return;
      }
      setStatus("Geocoding destination...");
      const destination = await geocodeInput(destInput);
      if (!destination) {
        setStatus("Could not find destination -- try adding city/state", "error");
        setTimeout(() => setStatus(""), 5000);
        return;
      }

      delete originInput.dataset.lat;
      delete originInput.dataset.lng;

      setStatus("Calculating base route...");
      const baseRoute = await Routing.getRoute(
        origin.lat,
        origin.lng,
        destination.lat,
        destination.lng
      );
      if (!baseRoute) {
        setStatus("Could not find a driving route between these locations", "error");
        setTimeout(() => setStatus(""), 5000);
        return;
      }

      baseRouteLayer.clearLayers();
      detourRouteLayer.clearLayers();
      markersLayer.clearLayers();

      L.polyline(baseRoute.polyline, {
        color: COLORS.base,
        weight: 5,
        opacity: 0.8,
      }).addTo(baseRouteLayer);

      if (originMarker) map.removeLayer(originMarker);
      if (destMarker) map.removeLayer(destMarker);
      originMarker = L.marker([origin.lat, origin.lng], {
        title: "Origin",
      })
        .addTo(map)
        .bindPopup("Origin");
      destMarker = L.marker([destination.lat, destination.lng], {
        title: "Destination",
      })
        .addTo(map)
        .bindPopup("Destination");

      const bounds = L.latLngBounds(baseRoute.polyline);
      map.fitBounds(bounds, { padding: [40, 40] });

      document.getElementById("base-route-info").innerHTML =
        `Direct: <strong>${formatDuration(baseRoute.duration)}</strong> ` +
        `(${(baseRoute.distance / 1000).toFixed(1)} km)`;

      setStatus("Finding nearby places...");
      const nearby = Corridor.filterByCorridorKm(
        placesWithCoords,
        baseRoute.polyline,
        corridorKm
      );

      if (!nearby.length) {
        setStatus("No saved places near this route", "error");
        document.getElementById("results").innerHTML =
          '<div class="empty-results">No saved places within the corridor. Try increasing the corridor width.</div>';
        document.getElementById("results-section").classList.add("visible");
        setTimeout(() => setStatus(""), 3000);
        return;
      }

      setStatus(`Calculating detours for ${nearby.length} places...`);
      detourResults = await Routing.calculateDetours(
        origin,
        destination,
        placesWithCoords,
        baseRoute,
        corridorKm,
        (done, total) => {
          setProgress(done, total, "Calculating detours");
        }
      );
      setProgress(0, 0);

      selectedIdx = -1;
      placeMarkers(detourResults);
      renderResults();
      setStatus("");
      document.getElementById("results-section").classList.add("visible");
    } catch (err) {
      console.error("Detour calculation failed:", err);
      setStatus(`Error: ${err.message}`, "error");
      setTimeout(() => setStatus(""), 5000);
    } finally {
      calcBtn.disabled = false;
      calcBtn.textContent = "Find Detours";
    }
  }

  function handleCorridorChange(e) {
    document.getElementById("corridor-value").textContent = e.target.value;
  }

  function clearData() {
    if (!confirm("Clear all saved places and cache?")) return;
    Places.clearStorage();
    allPlaces = [];
    detourResults = [];
    baseRouteLayer.clearLayers();
    detourRouteLayer.clearLayers();
    markersLayer.clearLayers();
    if (originMarker) map.removeLayer(originMarker);
    if (destMarker) map.removeLayer(destMarker);
    updatePlacesCount();
    updateFilterButtons();
    document.getElementById("results").innerHTML = "";
    document.getElementById("results-section").classList.remove("visible");
    document.getElementById("base-route-info").innerHTML = "";
    setStatus("Data cleared", "success");
    setTimeout(() => setStatus(""), 2000);
  }

  function init() {
    initMap();

    allPlaces = Places.loadFromStorage();
    if (
      !allPlaces.length &&
      typeof PRELOADED_PLACES !== "undefined" &&
      PRELOADED_PLACES.length
    ) {
      allPlaces = PRELOADED_PLACES;
      Places.saveToStorage(allPlaces);
    }
    updatePlacesCount();
    updateFilterButtons();

    document
      .getElementById("csv-upload")
      .addEventListener("change", handleFileUpload);
    document
      .getElementById("use-location-btn")
      .addEventListener("click", useMyLocation);
    document
      .getElementById("calculate-btn")
      .addEventListener("click", calculate);
    document
      .getElementById("corridor-slider")
      .addEventListener("input", handleCorridorChange);
    document
      .getElementById("clear-data-btn")
      .addEventListener("click", clearData);

    document.getElementById("destination").addEventListener("keydown", (e) => {
      if (e.key === "Enter") calculate();
    });
    document.getElementById("origin").addEventListener("keydown", (e) => {
      if (e.key === "Enter") document.getElementById("destination").focus();
    });
  }

  return { init };
})();

document.addEventListener("DOMContentLoaded", App.init);
