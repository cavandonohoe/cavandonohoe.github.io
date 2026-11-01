"""Nine evidence-aware Plotly views. Missing data is never plotted as zero content."""

import html
import json
from pathlib import Path

import plotly.graph_objects as go
import plotly.io as pio

from .analysis import analyze, load_project

COLORS = {"got": "#81b9ed", "acotar": "#edacba"}
NAMES = {"got": "A Game of Thrones", "acotar": "ACOTAR · book one"}


def style(fig, title, subtitle):
    fig.update_layout(
        template="plotly_dark",
        title={"text": title, "font_size": 19},
        paper_bgcolor="#121c29",
        plot_bgcolor="#121c29",
        font={"family": "Arial, sans-serif", "color": "#e6edf5", "size": 12},
        margin={"l": 65, "r": 30, "t": 85, "b": 60},
        height=410,
        legend={"orientation": "h", "y": -0.25},
    )
    fig.add_annotation(
        text=subtitle,
        xref="paper",
        yref="paper",
        x=0,
        y=1.14,
        showarrow=False,
        xanchor="left",
        font_size=11,
        font_color="#a8b8c9",
    )
    return fig


def unavailable(title, message):
    fig = go.Figure()
    fig.add_annotation(
        text=message,
        x=0.5,
        y=0.5,
        xref="paper",
        yref="paper",
        showarrow=False,
        font_size=16,
        align="center",
    )
    fig.update_xaxes(visible=False)
    fig.update_yaxes(visible=False)
    return style(fig, title, "Unavailable is a research result, not a score of zero.")


def make_figures(root):
    sources, events, scenes, chapters = load_project(root)
    results = analyze(root)
    figs = {}
    figs["01_overall"] = unavailable(
        "01 · The verdict is still sealed",
        "No defensible winner yet.<br>Neither complete novel has been annotated.<br>"
        "Smut Index: unavailable for both books.",
    )
    cats = list(json.loads((root / "configs/taxonomy.json").read_text())["categories"])
    fig = go.Figure()
    for work in NAMES:
        subset = [e for e in events if e.work == work]
        fig.add_bar(
            name=NAMES[work],
            x=cats,
            y=[sum(any(x.category == c for x in e.labels) for e in subset) for c in cats],
            marker_color=COLORS[work],
        )
    fig.update_xaxes(tickangle=-35)
    fig.update_yaxes(title="Provisional labels in pilot clusters", dtick=1)
    figs["02_categories"] = style(
        fig,
        "02 · What the guides point toward",
        "Purposive secondary-source pilot; labels overlap and are unscored.",
    )
    for i, work in enumerate(NAMES, 3):
        ch = chapters[chapters.work == work]
        subset = [e for e in events if e.work == work]
        fig = go.Figure()
        # Background shows unreviewed PRIMARY text, regardless of guide availability.
        fig.add_scatter(
            x=ch.chapter,
            y=[0] * len(ch),
            mode="markers",
            name="Primary text unreviewed",
            marker={
                "color": "#596777",
                "symbol": "line-ns",
                "size": 11,
                "line": {"color": "#596777", "width": 2},
            },
            cliponaxis=False,
            hovertemplate="Chapter %{x}<br>Primary content unknown<extra></extra>",
        )
        hits = sorted({e.unit_id for e in subset})
        fig.add_bar(
            x=[int(u[1:]) for u in hits],
            y=[sum(e.unit_id == u for e in subset) for u in hits],
            name="Guide event clusters",
            marker_color=COLORS[work],
            hovertemplate="Chapter %{x}<br>%{y} guide clusters; incomplete<extra></extra>",
        )
        fig.update_xaxes(title="Chapter (0 = prologue)" if work == "got" else "Chapter")
        fig.update_yaxes(title="Selected guide clusters", dtick=1)
        figs[f"0{i}_{work}_chapters"] = style(
            fig,
            f"0{i} · {NAMES[work]}: chapter map",
            "Grey marks mean unknown primary text. Blank bars do not mean no content.",
        )
    figs["05_explicitness"] = unavailable(
        "05 · Explicitness distribution",
        "Severity unassessed.<br>A guide reporting sex cannot establish<br>"
        "how explicitly the original prose describes it.",
    )
    fig = go.Figure()
    consent = ["consensual", "coercive", "violent", "ambiguous", "unknown", "not_applicable"]
    for work in NAMES:
        fig.add_bar(
            name=NAMES[work],
            x=consent,
            y=[sum(e.work == work and e.consent == c for e in events) for c in consent],
            marker_color=COLORS[work],
        )
    fig.update_yaxes(title="Pilot clusters · not population rates", dtick=1)
    figs["06_consent"] = style(
        fig,
        "06 · Consent stays separate",
        "Provisional guide-based context. Minor involvement is recorded separately.",
    )
    fig = go.Figure()
    z = []
    for work in NAMES:
        ids = set(chapters[(chapters.work == work) & chapters.summary_reviewed].unit_id)
        # Padded slots are null; they are not additional chapters.
        z.append(
            [1 if f"C{n:02d}" in ids else 0 for n in (range(73) if work == "got" else range(1, 47))]
            + ([None] * 27 if work == "acotar" else [])
        )
    fig.add_heatmap(
        z=z,
        x=list(range(1, 74)),
        y=list(NAMES.values()),
        zmin=0,
        zmax=1,
        colorscale=[[0, "#394554"], [0.499, "#394554"], [0.5, "#67d6be"], [1, "#67d6be"]],
        showscale=False,
        hoverongaps=False,
        hovertemplate="%{y}<br>Chapter-order slot %{x}<br>Guide reviewed = %{z}<extra></extra>",
    )
    fig.update_xaxes(title="Chapter-order slot · different chapter lengths; not word exposure")
    figs["07_coverage_heatmap"] = style(
        fig,
        "07 · Coverage before intensity",
        "Teal: guide reviewed. Grey: guide not reviewed. All primary-text intensity remains unknown.",
    )
    figs["08_sensitivity"] = unavailable(
        "08 · Weight sensitivity",
        "Starting · Equal · Acts-focused · Romance-focused<br>"
        "All four scores withheld for both novels.<br>Weights cannot repair missing observations.",
    )
    fig = go.Figure()
    for work in NAMES:
        r = results["works"][work]
        fig.add_bar(
            name=NAMES[work],
            x=["Chapter inventory", "Guide chapters reviewed", "Primary chapters reviewed"],
            y=[r["chapter_units"], r["summary_chapters_reviewed"], r["primary_chapters_reviewed"]],
            marker_color=COLORS[work],
        )
    fig.update_yaxes(title="Chapters · includes GOT prologue", dtick=10)
    figs["09_source_coverage"] = style(
        fig,
        "09 · The denominator audit",
        "Zero primary chapters reviewed describes coverage; it is not zero sexual content.",
    )
    return figs


def render_dashboard(root: Path, offline=True, export=False):
    sources, events, scenes, chapters = load_project(root)
    figs = make_figures(root)
    target = root / "reports"
    target.mkdir(exist_ok=True)
    parts = []
    for index, (name, fig) in enumerate(figs.items()):
        parts.append(
            '<section class="chart">'
            + pio.to_html(
                fig,
                full_html=False,
                include_plotlyjs=(True if offline else "cdn") if index == 0 else False,
                div_id=name,
                config={
                    "responsive": True,
                    "displaylogo": False,
                    "toImageButtonOptions": {"format": "svg", "filename": name},
                },
            )
            + "</section>"
        )
        (target / "figures" / f"{name}.json").write_text(fig.to_json() + "\n")
        if export:
            fig.write_image(target / "figures" / f"{name}.svg", width=1100, height=480)
            fig.write_image(target / "figures" / f"{name}.png", width=1100, height=480)
    rows = "".join(
        '<tr data-work="'
        + e.work
        + '"><td>'
        + "</td><td>".join(
            html.escape(str(v))
            for v in [
                NAMES[e.work],
                e.unit_id,
                e.event_id,
                ", ".join(x.category for x in e.labels),
                e.consent,
                e.verification,
                e.note,
            ]
        )
        + "</td></tr>"
        for e in events
    )
    source_rows = "".join(
        '<li><a href="'
        + html.escape(s["url"], quote=True)
        + '">'
        + html.escape(s["source_id"])
        + "</a> — "
        + html.escape(s["coverage"])
        + "</li>"
        for s in sources
    )
    page = (
        """<!doctype html><html lang="en"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1"><title>The Smut Index · Book One</title>
<style>*{box-sizing:border-box}body{margin:0;background:#0b121c;color:#e6edf5;font:16px/1.6 Arial,sans-serif}
main{max-width:1450px;margin:auto;padding:40px 28px}h1{font-size:clamp(40px,6vw,76px);line-height:1.05;margin:15px 0}
.eyebrow{color:#67d6be;letter-spacing:3px;font-size:12px} .lede{font-size:20px;color:#b4c2d2;max-width:900px}
.notice{border-left:4px solid #eab476;background:#241f1a;padding:18px 24px;margin:28px 0}
.cards{display:grid;grid-template-columns:repeat(3,1fr);gap:18px;margin:30px 0}.card,.chart{background:#121c29;border:1px solid #273547;border-radius:12px;overflow:hidden}
.card{padding:20px}.card strong{display:block;font-size:32px}.card span{color:#a8b8c9}.grid{display:grid;grid-template-columns:repeat(2,minmax(0,1fr));gap:22px}.chart:first-child{grid-column:1/-1}
a{color:#81b9ed}table{border-collapse:collapse;width:100%;font-size:13px}th,td{text-align:left;border-bottom:1px solid #273547;padding:10px;vertical-align:top}.scroll{overflow:auto}
select{background:#172437;color:white;padding:8px;border:1px solid #596777;border-radius:6px}footer{color:#a8b8c9;margin-top:35px;font-size:13px}
@media(max-width:850px){.grid,.cards{grid-template-columns:1fr}.chart:first-child{grid-column:auto}main{padding:25px 12px}}
</style></head><body><main><div class="eyebrow">RESEARCH PILOT / BOOK ONE VS BOOK ONE</div>
<h1>The Smut Index</h1><p class="lede">A Game of Thrones (1996) × A Court of Thorns and Roses (2015).<br>
A quantitative question with a very necessary denominator.</p>
<div class="notice"><strong>No winner declared.</strong> Full lawful novel texts and complete annotations are missing.
These charts describe source coverage and a small, deliberately selected guide-based pilot. They do not measure whole-book sexual content.</div>
<div class="cards"><div class="card"><strong>119</strong><span>Chapter units inventoried · GOT includes prologue</span></div>
<div class="card"><strong>10</strong><span>Public chapter summaries reviewed</span></div>
<div class="card"><strong>Unavailable</strong><span>Both whole-book Smut Index scores</span></div></div>
<div class="grid">"""
        + "".join(parts)
        + """</div><h2>Annotation audit trail</h2>
<p>One row per unique event cluster; multiple category labels can share that row. All current severities, word spans and calibrated confidence scores are unknown.</p>
<label>Filter audit table <select id="work-filter"><option value="all">Both novels</option><option value="got">A Game of Thrones</option><option value="acotar">ACOTAR book one</option></select></label>
<div class="scroll"><table><thead><tr><th>Work</th><th>Chapter</th><th>Event ID</th><th>Provisional categories</th><th>Consent context</th><th>Evidence</th><th>Note</th></tr></thead><tbody>"""
        + rows
        + """</tbody></table></div>
<h2>Methods in one paragraph</h2><p>The proposed 0–100 index averages severity-normalized scene components with initial weights of 35% acts, 25% anatomy, 20% intimacy, 10% dialogue/thoughts, and 10% intensity. Components use scene maxima to prevent repeated labels inflating scores. Nonsexual affection is descriptive; coercion is reported separately. This instrument is uncalibrated. No episode runtime, television observation, word count estimate, or later-series material is used.</p>
<h2>Sources</h2><ul>"""
        + source_rows
        + """</ul><footer>Research access: October 31, 2026 · reproducible from committed annotations.<br>
Plotly toolbar exports SVG; the published HTML embeds its Plotly runtime for offline use.</footer>
</main><script>document.getElementById('work-filter').addEventListener('change',function(){document.querySelectorAll('tr[data-work]').forEach(r=>{r.hidden=this.value!=='all'&&r.dataset.work!==this.value})});</script></body></html>"""
    )
    (target / "dashboard.html").write_text(page)
    return target / "dashboard.html"
