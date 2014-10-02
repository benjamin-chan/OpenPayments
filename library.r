getAndUnzip <- function (url) {
  f <- paste0(tempfile(), ".zip")
  download.file(url, f, mode="wb")
  unzip(f, exdir=tempdir())
  filenames <- unzip(f, list=TRUE)
  unlink(f)
  filenames
}
niceTable <- function (dt) {
  require(xtable)
  print(xtable(dt), type="html", include.rownames=FALSE)
}
