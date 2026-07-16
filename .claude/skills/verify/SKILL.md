---
name: verify
description: How to run and verify the Climate Explorer app (docs/index.html) end-to-end in a headless browser.
---

# Verifying the Climate Explorer

The entire app is `docs/index.html` — no build step. It needs outbound network access at runtime (CDN scripts + data from `https://mco-normals.s3.us-east-2.amazonaws.com`).

## Launch

```bash
cd docs && python3 -m http.server 8765   # serve; then open http://localhost:8765/
```

## Drive headlessly (Playwright)

`npx playwright --version` works on this machine and Chromium builds are cached. Install the library in a scratch dir (`npm i playwright`) and drive with a Node script.

Gotchas learned the hard way:
- Wait for polygons with `document.querySelectorAll("path.leaflet-interactive").length > 30` before clicking; then `page.mouse.click(650, 500)` reliably hits a county (Fergus County at default view, 1600x1000 viewport).
- Top-level `let` variables in the page's inline scripts are NOT `window` properties — in `page.waitForFunction`, reference them as bare identifiers (e.g. `download_data`), not `window.download_data`.
- The Projections tab is slow: switching to it loads ~12 COG GeoTIFFs from S3; use timeouts of 2–5 minutes for tab switches and map-type changes.
- Blob-URL `<a download>` clicks fire Playwright `download` events normally (`acceptDownloads: true`).
- Simulate S3 failure with `page.route("**/zonal/**", route => route.abort())`.

## Flows worth driving

1. Historical: click county → chart + "Download these data (CSV)" button under plot; variable change (check degF/inch unit conversion in CSV, not raw K/mm); month vs annual.
2. Projections: tab switch → timeseries CSV (scenarios, 1950–2100); Monthly Trend plot type (12 rows per period×scenario, reference period once); Difference From Normal (values become deltas); scenario button toggles.
3. Failure path: blocked `zonal` fetch → download button must hide, no page errors.
