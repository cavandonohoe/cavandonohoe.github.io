const Routing = (() => {
  const VALHALLA_BASE = "https://valhalla1.openstreetmap.de";
  const FETCH_TIMEOUT = 15000;

  function fetchWithTimeout(url, opts = {}) {
    const controller = new AbortController();
    const id = setTimeout(() => controller.abort(), FETCH_TIMEOUT);
    return fetch(url, { ...opts, signal: controller.signal }).finally(() =>
      clearTimeout(id)
    );
  }

  function decodeValhallaShape(encoded) {
    const points = [];
    let idx = 0;
    let lat = 0;
    let lng = 0;
    while (idx < encoded.length) {
      let shift = 0;
      let result = 0;
      let byte;
      do {
        byte = encoded.charCodeAt(idx++) - 63;
        result |= (byte & 0x1f) << shift;
        shift += 5;
      } while (byte >= 0x20);
      lat += result & 1 ? ~(result >> 1) : result >> 1;

      shift = 0;
      result = 0;
      do {
        byte = encoded.charCodeAt(idx++) - 63;
        result |= (byte & 0x1f) << shift;
        shift += 5;
      } while (byte >= 0x20);
      lng += result & 1 ? ~(result >> 1) : result >> 1;

      points.push([lat / 1e6, lng / 1e6]);
    }
    return points;
  }

  function buildValhallaRequest(locations) {
    return {
      locations: locations.map((l) => ({ lat: l.lat, lon: l.lng })),
      costing: "auto",
      units: "kilometers",
    };
  }

  async function getRoute(fromLat, fromLng, toLat, toLng) {
    const req = buildValhallaRequest([
      { lat: fromLat, lng: fromLng },
      { lat: toLat, lng: toLng },
    ]);
    const url = `${VALHALLA_BASE}/route?json=${encodeURIComponent(JSON.stringify(req))}`;
    const resp = await fetchWithTimeout(url);
    const data = await resp.json();
    if (data.trip && data.trip.legs && data.trip.legs.length > 0) {
      const summary = data.trip.summary;
      return {
        duration: summary.time,
        distance: summary.length * 1000,
        polyline: decodeValhallaShape(data.trip.legs[0].shape),
      };
    }
    return null;
  }

  async function getRouteVia(fromLat, fromLng, viaLat, viaLng, toLat, toLng) {
    const req = buildValhallaRequest([
      { lat: fromLat, lng: fromLng },
      { lat: viaLat, lng: viaLng },
      { lat: toLat, lng: toLng },
    ]);
    const url = `${VALHALLA_BASE}/route?json=${encodeURIComponent(JSON.stringify(req))}`;
    const resp = await fetchWithTimeout(url);
    const data = await resp.json();
    if (data.trip && data.trip.legs) {
      const summary = data.trip.summary;
      const allPoints = [];
      for (const leg of data.trip.legs) {
        const pts = decodeValhallaShape(leg.shape);
        if (allPoints.length > 0) pts.shift();
        allPoints.push(...pts);
      }
      return {
        duration: summary.time,
        distance: summary.length * 1000,
        polyline: allPoints,
      };
    }
    return null;
  }

  function sleep(ms) {
    return new Promise((r) => setTimeout(r, ms));
  }

  async function calculateDetours(
    origin,
    destination,
    places,
    baseRoute,
    corridorKm,
    onProgress
  ) {
    const nearby = Corridor.filterByCorridorKm(
      places.filter((p) => p.lat != null && p.lng != null),
      baseRoute.polyline,
      corridorKm
    );

    const results = [];
    for (let i = 0; i < nearby.length; i++) {
      const place = nearby[i];
      try {
        const viaRoute = await getRouteVia(
          origin.lat,
          origin.lng,
          place.lat,
          place.lng,
          destination.lat,
          destination.lng
        );
        if (viaRoute) {
          const detourSec = viaRoute.duration - baseRoute.duration;
          results.push({
            place,
            detourSec,
            detourMin: Math.round(detourSec / 60),
            viaRoute,
          });
        }
      } catch {
        // skip places that fail routing
      }
      if (onProgress) onProgress(i + 1, nearby.length);
      if (i < nearby.length - 1) await sleep(100);
    }

    results.sort((a, b) => a.detourSec - b.detourSec);
    return results;
  }

  return { getRoute, getRouteVia, calculateDetours };
})();
