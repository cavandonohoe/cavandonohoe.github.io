module.exports = {
  ci: {
    collect: {
      // Pages are served statically; no startServerCommand needed because
      // the workflow runs http-server in the background.
      url: [
        "http://localhost:4567/index.html",
        "http://localhost:4567/about.html",
        "http://localhost:4567/cv.html",
        "http://localhost:4567/best_picture_nominees.html",
        "http://localhost:4567/sp500.html"
      ],
      numberOfRuns: 1,
      settings: {
        chromeFlags: "--no-sandbox --disable-dev-shm-usage --headless=new"
      }
    },
    assert: {
      // Soft thresholds — warn, don't fail the build.
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
