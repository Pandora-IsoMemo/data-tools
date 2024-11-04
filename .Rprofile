.First <- function() {
  options(repos = c(getOption("repos"),
                    PANDORA = "https://Pandora-IsoMemo.github.io/drat/",
                    INWTLab = "https://inwtlab.github.io/drat/"))
}

.First()

if (interactive()) {
  library(futile.logger)
  futile.logger::flog.threshold(DEBUG)
}
