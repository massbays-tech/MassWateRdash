# Render the Mermaid diagram in README.md to a PNG for the Upload & Validate
# help popup. Shiny doesn't render Mermaid, so the image is made once here and
# included as a static file. Rerun after editing the diagram in README.md.
# Requires chromote and a Chromium browser, e.g. Edge on Windows:
# Sys.setenv(CHROMOTE_CHROME = "C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe")

library(chromote)

# extract the mermaid code block
md <- readLines("README.md", encoding = "UTF-8")
strt <- grep("^```mermaid", md)
ends <- grep("^```$", md)
mermaid_code <- md[(strt + 1):(min(ends[ends > strt]) - 1)]

# standalone page that renders the diagram with mermaid.js
html <- c(
  '<!doctype html><html><head><meta charset="utf-8">',
  '<style>body { margin: 0; font-family: "Open Sans", "Segoe UI", sans-serif; }</style>',
  '</head><body><pre class="mermaid">',
  htmltools::htmlEscape(mermaid_code),
  '</pre><script type="module">',
  'import mermaid from "https://cdn.jsdelivr.net/npm/mermaid@10.9.1/dist/mermaid.esm.min.mjs";',
  'mermaid.initialize({ startOnLoad: false });',
  'await mermaid.run();',
  'window.mermaidDone = true;',
  '</script></body></html>'
)
tmp <- tempfile(fileext = ".html")
writeLines(html, tmp, useBytes = TRUE)

# screenshot only the diagram at 2x for a sharp image
b <- ChromoteSession$new()
b$Emulation$setDeviceMetricsOverride(
  width = 1400, height = 1000, deviceScaleFactor = 2, mobile = FALSE
)
b$Page$navigate(paste0("file:///", normalizePath(tmp, winslash = "/")))

for (i in 1:30) {
  done <- b$Runtime$evaluate("window.mermaidDone === true")$result$value
  if (isTRUE(done)) break
  Sys.sleep(0.5)
}
stopifnot(isTRUE(done))

box <- b$Runtime$evaluate(
  "JSON.stringify(document.querySelector('.mermaid svg').getBoundingClientRect())",
  returnByValue = TRUE
)$result$value
box <- jsonlite::fromJSON(box)

pad <- 10
shot <- b$Page$captureScreenshot(
  clip = list(
    x = max(box$x - pad, 0), y = max(box$y - pad, 0),
    width = box$width + 2 * pad, height = box$height + 2 * pad, scale = 1
  )
)
b$close()

writeBin(
  jsonlite::base64_dec(shot$data),
  "inst/app/www/help/workflow.png"
)
