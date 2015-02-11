getAndUnzip <- function (url) {
  f <- paste0(tempfile(), ".zip")
  download.file(url, f, mode="wb")
  unzip(f, exdir=tempdir())
  filenames <- unzip(f, list=TRUE)
  unlink(f)
  filenames
}
readFile <- function (f) {
  colNames <- names(fread(f, nrows=0))
  regex <- paste("_Transaction_ID",
                 "Submitting_Applicable_Manufacturer_or_Applicable_GPO_Name",
                 "Covered_Recipient_Type",
                 "Teaching_Hospital_Name",
                 "Physician_Last_Name",
                 "Physician_First_Name",
                 "Recipient_State",
                 "Total_Amount_of_Payment_USDollars",
                 "Dollar_Amount_Invested",
                 sep="|")
  colNames <- colNames[grep(regex, colNames)]
  classes <- list(character=1:length(colNames))
  D <- fread(f,
             select=colNames,
             colClasses=classes,
             na.strings="",
             stringsAsFactors=FALSE)
  D
}
niceTable <- function (dt) {
  require(xtable)
  print(xtable(dt), type="html", include.rownames=FALSE)
}
