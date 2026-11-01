"""Optional Playwright screenshot QA. Browser install is a development-only step."""

import argparse
from pathlib import Path

from playwright.sync_api import sync_playwright

parser = argparse.ArgumentParser()
parser.add_argument("--root", type=Path, default=Path.cwd())
args = parser.parse_args()
root = args.root.resolve()
with sync_playwright() as p:
    browser = p.chromium.launch(headless=True)
    page = browser.new_page(viewport={"width": 1440, "height": 1100}, device_scale_factor=1)
    errors = []
    page.on("pageerror", lambda error: errors.append(str(error)))
    page.goto((root / "reports/dashboard.html").as_uri())
    page.wait_for_function(
        "document.querySelectorAll('.js-plotly-plot').length === 9 && "
        "Array.from(document.querySelectorAll('.js-plotly-plot')).every(p => p.querySelector('.main-svg'))"
    )
    page.screenshot(path=str(root / "reports/figures/dashboard.png"), full_page=True)
    page.select_option("#work-filter", "got")
    assert page.locator('tbody tr[data-work="acotar"]:visible').count() == 0
    assert page.locator('tbody tr[data-work="got"]:visible').count() == 4
    page.set_viewport_size({"width": 390, "height": 844})
    page.reload()
    page.wait_for_function(
        "document.querySelectorAll('.js-plotly-plot').length === 9 && Array.from(document.querySelectorAll('.js-plotly-plot')).every(p => p.querySelector('.main-svg'))"
    )
    page.screenshot(path=str(root / "reports/figures/dashboard-mobile.png"), full_page=True)
    assert not errors, errors
    browser.close()
print("Nine plots rendered; filter and mobile layout checked; no JavaScript page errors")
