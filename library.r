niceTable <- function (dt) {
  require(xtable)
  print(xtable(dt), type="html", include.rownames=FALSE)
}
