import csv
import sys
import xml.etree.ElementTree as ET

KML_NS = {"kml": "http://www.opengis.net/kml/2.2"}


def text_or_empty(el, path):
    found = el.find(path, KML_NS)
    return (found.text or "").strip() if found is not None and found.text else ""


def main(kml_path, csv_path):
    tree = ET.parse(kml_path)
    root = tree.getroot()

    rows = []

    # In My Maps KML, layers typically appear as <Folder><name>Layer</name>...<Placemark>...</Placemark></Folder>
    for folder in root.findall(".//kml:Folder", KML_NS):
        layer_name = text_or_empty(folder, "kml:name")
        for pm in folder.findall(".//kml:Placemark", KML_NS):
            name = text_or_empty(pm, "kml:name")
            description = text_or_empty(pm, "kml:description")
            coord_text = text_or_empty(pm, ".//kml:Point/kml:coordinates")

            lon, lat = "", ""
            if coord_text:
                parts = coord_text.split(",")
                if len(parts) >= 2:
                    lon = parts[0].strip()
                    lat = parts[1].strip()

            if not (name or lat or lon):
                continue

            rows.append(
                {
                    "layer": layer_name,
                    "name": name,
                    "description": description,
                    "latitude": lat,
                    "longitude": lon,
                }
            )

    with open(csv_path, "w", newline="", encoding="utf-8") as f:
        w = csv.DictWriter(
            f,
            fieldnames=["layer", "name", "description", "latitude", "longitude"],
        )
        w.writeheader()
        w.writerows(rows)


if __name__ == "__main__":
    if len(sys.argv) != 3:
        raise SystemExit("Usage: kml_to_csv.py input.kml output.csv")
    main(sys.argv[1], sys.argv[2])
