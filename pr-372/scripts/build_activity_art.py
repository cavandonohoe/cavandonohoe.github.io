#!/usr/bin/env python3
"""Render 30 days of repository commit activity to assets/generated/activity.svg."""
import subprocess
from datetime import datetime, timezone, timedelta

from generated_output import write_svg_if_changed

now = datetime.now(timezone.utc)
dates = []
vals = []
for i in range(29, -1, -1):
    d = (now - timedelta(days=i)).date()
    dates.append(d)
    n = int(subprocess.check_output(
        ["git", "rev-list", "--count", f"--since={d}T00:00:00Z", f"--until={d}T23:59:59Z", "HEAD"],
        text=True,
    ).strip())
    vals.append(n)

m = max(vals + [1])
w, h = 660, 195
bw = 18
gap = 3
bars = []
for i, v in enumerate(vals):
    bh = 120 * v / m
    x = 15 + i * (bw + gap)
    y = 145 - bh
    bars.append(
        f'<rect x="{x}" y="{y:.1f}" width="{bw}" height="{bh:.1f}" rx="3" '
        f'fill="currentColor" opacity="{0.25 + 0.75 * v / m:.2f}"/>'
    )

labels = []
for i in [0, 7, 14, 21, 29]:
    x = 15 + i * (bw + gap) + bw / 2
    anchor = "middle"
    if i == 0:
        x = 15
        anchor = "start"
    elif i == 29:
        x = 15 + 29 * (bw + gap) + bw
        anchor = "end"
    labels.append(
        f'<text x="{x}" y="163" text-anchor="{anchor}" class="date-label">'
        f'{dates[i].strftime("%b %-d")}</text>'
    )

svg = (
    f'<svg xmlns="http://www.w3.org/2000/svg" width="{w}" height="{h}" '
    f'viewBox="0 0 {w} {h}" role="img" aria-label="30 days of GitHub activity">'
    '<style>svg{color:#238636} text{font:14px system-ui;fill:#57606a} '
    '.date-label{font-size:12px;font-weight:500;fill:#6e7781}</style>'
    '<text x="15" y="20">30 days of site-repository activity</text>'
    f'{"".join(bars)}{"".join(labels)}'
    f'<text x="15" y="188">Generated {now.date().isoformat()}</text></svg>\n'
)
write_svg_if_changed("assets/generated/activity.svg", svg)
