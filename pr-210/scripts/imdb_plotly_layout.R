# Permanent labels stay readable on narrow screens without horizontal overflow.
imdb_plotly_layout <- function(widget, titles, labels, left_margin, axis_title,
                               compact_axis_title, compact_row_height = 68) {
  htmlwidgets::onRender(widget, "
function(el, x, data) {
  let lastWidth = 0;
  let timer;
  let applying = false;
  const desktopRange = x.layout.xaxis.range.slice();
  const maxValue = Math.max.apply(null, el.data[0].x);
  function update() {
    const width = el.clientWidth;
    if (!width || applying) return;
    const compact = width < 600;
    const height = compact ? data.titles.length * data.rowHeight + 120 : 650;
    const instance = HTMLWidgets.getInstance(el);
    // The R binding caches its original 650px height and reuses it on resize.
    // Keep that cached height in sync, so it cannot squash the mobile rows.
    if (instance) instance.height = height;
    if (width === lastWidth && el.clientHeight === height && el.layout.height === height) return;
    lastWidth = width;
    applying = true;
    el.style.height = height + 'px';
    const annotations = compact ? data.titles.map(function(title, i) {
      return {
        xref: 'paper', x: 0, xanchor: 'left',
        yref: 'y', y: title, yshift: 14, yanchor: 'bottom',
        text: '<b>' + data.safeTitles[i] + '</b><br>' + data.labels[i].replace(/<br[^>]*>/gi, ' · ')
          .replace('exceptional episodes', 'episodes').replace('series votes', 'votes'),
        showarrow: false, align: 'left', font: {size: 12}
      };
    }) : [];
    Plotly.restyle(el, {
      textposition: compact ? 'none' : 'outside',
      width: compact ? 18 / data.rowHeight : 0.8
    }).then(function() {
      return Plotly.relayout(el, {
        width: width, height: height, dragmode: false,
        margin: {l: compact ? 12 : data.leftMargin, r: 20, t: 60, b: 60},
        'yaxis.showticklabels': !compact,
        'xaxis.title.text': compact ? data.compactAxisTitle : data.axisTitle,
        'xaxis.range': compact ? [0, maxValue * 1.08] : desktopRange,
        annotations: annotations
      });
    }).finally(function() {
      applying = false;
      schedule();
    });
  }
  function schedule() {
    clearTimeout(timer);
    timer = setTimeout(update, 50);
  }
  update();
  window.addEventListener('resize', schedule);
  el.on('plotly_afterplot', schedule);
  const observer = new ResizeObserver(schedule);
  observer.observe(el);
}
", data = list(
    titles = as.character(titles), safeTitles = htmltools::htmlEscape(as.character(titles)),
    labels = labels, leftMargin = left_margin, axisTitle = axis_title,
    compactAxisTitle = compact_axis_title, rowHeight = compact_row_height
  ))
}
