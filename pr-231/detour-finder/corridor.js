const Corridor = (() => {
  const R_EARTH_KM = 6371;

  function toRad(deg) {
    return (deg * Math.PI) / 180;
  }

  function haversineKm(lat1, lon1, lat2, lon2) {
    const dLat = toRad(lat2 - lat1);
    const dLon = toRad(lon2 - lon1);
    const a =
      Math.sin(dLat / 2) ** 2 +
      Math.cos(toRad(lat1)) * Math.cos(toRad(lat2)) * Math.sin(dLon / 2) ** 2;
    return R_EARTH_KM * 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1 - a));
  }

  function distToSegmentKm(px, py, ax, ay, bx, by) {
    const dx = bx - ax;
    const dy = by - ay;
    if (dx === 0 && dy === 0) return haversineKm(px, py, ax, ay);
    let t = ((px - ax) * dx + (py - ay) * dy) / (dx * dx + dy * dy);
    t = Math.max(0, Math.min(1, t));
    return haversineKm(px, py, ax + t * dx, ay + t * dy);
  }

  function distToPolylineKm(lat, lng, polyline) {
    let minDist = Infinity;
    for (let i = 0; i < polyline.length - 1; i++) {
      const [aLat, aLng] = polyline[i];
      const [bLat, bLng] = polyline[i + 1];
      const d = distToSegmentKm(lat, lng, aLat, aLng, bLat, bLng);
      if (d < minDist) minDist = d;
    }
    return minDist;
  }

  function filterByCorridorKm(places, polyline, corridorKm) {
    return places.filter(
      (p) => distToPolylineKm(p.lat, p.lng, polyline) <= corridorKm
    );
  }

  return { haversineKm, distToPolylineKm, filterByCorridorKm };
})();
