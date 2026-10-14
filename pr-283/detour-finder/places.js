const Places = (() => {
  const STORAGE_KEY = "detour_finder_places";
  const GEOCODE_CACHE_KEY = "detour_finder_geocache";

  function loadFromStorage() {
    try {
      const raw = localStorage.getItem(STORAGE_KEY);
      return raw ? JSON.parse(raw) : [];
    } catch {
      return [];
    }
  }

  function saveToStorage(places) {
    localStorage.setItem(STORAGE_KEY, JSON.stringify(places));
  }

  function loadGeocodeCache() {
    try {
      const raw = localStorage.getItem(GEOCODE_CACHE_KEY);
      return raw ? JSON.parse(raw) : {};
    } catch {
      return {};
    }
  }

  function saveGeocodeCache(cache) {
    localStorage.setItem(GEOCODE_CACHE_KEY, JSON.stringify(cache));
  }

  function parseCSVLine(line) {
    const result = [];
    let current = "";
    let inQuotes = false;
    for (let i = 0; i < line.length; i++) {
      const ch = line[i];
      if (inQuotes) {
        if (ch === '"' && line[i + 1] === '"') {
          current += '"';
          i++;
        } else if (ch === '"') {
          inQuotes = false;
        } else {
          current += ch;
        }
      } else if (ch === '"') {
        inQuotes = true;
      } else if (ch === ",") {
        result.push(current);
        current = "";
      } else {
        current += ch;
      }
    }
    result.push(current);
    return result;
  }

  function detectFormat(headerLine) {
    const lower = headerLine.toLowerCase();
    if (lower.includes("latitude") && lower.includes("longitude")) {
      return "takeout";
    }
    if (lower.includes("title") && lower.includes("url")) {
      return "data_csv";
    }
    return "unknown";
  }

  function parseTakeoutCSV(text, listName) {
    const lines = text.split("\n").filter((l) => l.trim());
    if (lines.length < 2) return [];
    const headers = parseCSVLine(lines[0]);
    const nameIdx = headers.findIndex(
      (h) => h.toLowerCase().trim() === "name"
    );
    const addrIdx = headers.findIndex(
      (h) => h.toLowerCase().trim() === "address"
    );
    const latIdx = headers.findIndex(
      (h) => h.toLowerCase().trim() === "latitude"
    );
    const lngIdx = headers.findIndex(
      (h) => h.toLowerCase().trim() === "longitude"
    );
    const urlIdx = headers.findIndex((h) =>
      h.toLowerCase().trim().includes("url")
    );

    const places = [];
    for (let i = 1; i < lines.length; i++) {
      const cols = parseCSVLine(lines[i]);
      const lat = parseFloat(cols[latIdx]);
      const lng = parseFloat(cols[lngIdx]);
      const name = (cols[nameIdx] || "").trim();
      if (!name || (lat === 0 && lng === 0) || isNaN(lat) || isNaN(lng))
        continue;
      places.push({
        name,
        address: (cols[addrIdx] || "").trim(),
        lat,
        lng,
        note: "",
        list: listName || "Saved",
        url: (cols[urlIdx] || "").trim(),
      });
    }
    return places;
  }

  function parseDataCSV(text, listName) {
    const lines = text.split("\n").filter((l) => l.trim());
    if (lines.length < 2) return [];
    const headers = parseCSVLine(lines[0]);
    const titleIdx = headers.findIndex(
      (h) => h.toLowerCase().trim() === "title"
    );
    const noteIdx = headers.findIndex(
      (h) => h.toLowerCase().trim() === "note"
    );
    const urlIdx = headers.findIndex((h) => h.toLowerCase().trim() === "url");

    const places = [];
    for (let i = 1; i < lines.length; i++) {
      const cols = parseCSVLine(lines[i]);
      const title = (cols[titleIdx] || "").trim();
      if (!title) continue;
      places.push({
        name: title,
        address: "",
        lat: null,
        lng: null,
        note: (cols[noteIdx] || "").trim(),
        list: listName || "Saved",
        url: (cols[urlIdx] || "").trim(),
      });
    }
    return places;
  }

  function parseCSV(text, listName) {
    const firstLine = text.split("\n")[0] || "";
    const format = detectFormat(firstLine);
    if (format === "takeout") return parseTakeoutCSV(text, listName);
    if (format === "data_csv") return parseDataCSV(text, listName);
    return parseTakeoutCSV(text, listName);
  }

  async function geocodePlace(name, address) {
    const cache = loadGeocodeCache();
    const key = `${name}||${address}`.toLowerCase();
    if (cache[key]) return cache[key];

    const controller1 = new AbortController();
    const t1 = setTimeout(() => controller1.abort(), 10000);
    const query = address ? `${name}, ${address}` : name;
    const url = `https://nominatim.openstreetmap.org/search?format=json&limit=1&q=${encodeURIComponent(query)}`;
    try {
      const resp = await fetch(url, {
        headers: { "User-Agent": "DetourFinder/1.0" },
        signal: controller1.signal,
      });
      clearTimeout(t1);
      const data = await resp.json();
      if (data.length > 0) {
        const result = { lat: parseFloat(data[0].lat), lng: parseFloat(data[0].lon) };
        cache[key] = result;
        saveGeocodeCache(cache);
        return result;
      }
    } catch {
      clearTimeout(t1);
    }

    if (address) {
      const controller2 = new AbortController();
      const t2 = setTimeout(() => controller2.abort(), 10000);
      const fallbackUrl = `https://nominatim.openstreetmap.org/search?format=json&limit=1&q=${encodeURIComponent(name)}`;
      try {
        const resp2 = await fetch(fallbackUrl, {
          headers: { "User-Agent": "DetourFinder/1.0" },
          signal: controller2.signal,
        });
        clearTimeout(t2);
        const data2 = await resp2.json();
        if (data2.length > 0) {
          const result = { lat: parseFloat(data2[0].lat), lng: parseFloat(data2[0].lon) };
          cache[key] = result;
          saveGeocodeCache(cache);
          return result;
        }
      } catch {
        clearTimeout(t2);
      }
    }

    return null;
  }

  async function geocodeLocation(query) {
    const controller = new AbortController();
    const t = setTimeout(() => controller.abort(), 10000);
    const url = `https://nominatim.openstreetmap.org/search?format=json&limit=5&q=${encodeURIComponent(query)}`;
    try {
      const resp = await fetch(url, {
        headers: { "User-Agent": "DetourFinder/1.0" },
        signal: controller.signal,
      });
      clearTimeout(t);
      return resp.json();
    } catch {
      clearTimeout(t);
      return [];
    }
  }

  function sleep(ms) {
    return new Promise((r) => setTimeout(r, ms));
  }

  async function geocodeMissing(places, onProgress) {
    const needGeo = places.filter((p) => p.lat === null || p.lng === null);
    if (needGeo.length === 0) return places;

    const cache = loadGeocodeCache();
    let resolved = 0;

    for (const place of needGeo) {
      const key = `${place.name}||${place.address}`.toLowerCase();
      if (cache[key]) {
        place.lat = cache[key].lat;
        place.lng = cache[key].lng;
        resolved++;
        if (onProgress) onProgress(resolved, needGeo.length);
        continue;
      }

      await sleep(1100);
      const result = await geocodePlace(place.name, place.address);
      if (result) {
        place.lat = result.lat;
        place.lng = result.lng;
      }
      resolved++;
      if (onProgress) onProgress(resolved, needGeo.length);
    }

    return places;
  }

  function clearStorage() {
    localStorage.removeItem(STORAGE_KEY);
    localStorage.removeItem(GEOCODE_CACHE_KEY);
  }

  return {
    loadFromStorage,
    saveToStorage,
    parseCSV,
    geocodeLocation,
    geocodeMissing,
    clearStorage,
  };
})();
