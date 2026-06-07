// Lighthouse CI config used by .github/workflows/lighthouse.yml.
//
// Two modes are supported, selected by the LHCI_BASE_URL env var:
//
//   1. Local serve (workflow_run after a successful build):
//        LHCI_BASE_URL unset  -> defaults to http://localhost:4567
//      The workflow downloads the `site` artifact, serves it with http-server,
//      and Lighthouse audits the local copy.
//
//   2. Deployed preview (pull_request):
//        LHCI_BASE_URL = https://cavandonohoe.github.io/pr-<n>
//      The workflow waits for the preview to be live, then audits the real URL.
//
// The set of pages audited is identical in both modes.

const baseUrl = process.env.LHCI_BASE_URL || "http://localhost:4567";

const pages = [
  "/index.html",
  "/about.html",
  "/cv.html",
  "/best_picture_nominees.html",
  "/sp500.html"
];

module.exports = {
  ci: {
    collect: {
      url: pages.map((p) => `${baseUrl}${p}`),
      numberOfRuns: 1,
      settings: {
        chromeFlags: "--no-sandbox --disable-dev-shm-usage --headless=new"
      }
    },
    assert: {
      assertions: {
        "categories:performance": ["warn", { "minScore": 0.7 }],
        "categories:accessibility": ["warn", { "minScore": 0.85 }],
        "categories:best-practices": ["warn", { "minScore": 0.85 }],
        "categories:seo": ["warn", { "minScore": 0.85 }]
      }
    },
    upload: {
      target: "temporary-public-storage"
    }
  }
};
