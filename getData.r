# Download the data dictionary.
f <- paste0(file.path(getwd(), "OpenPaymentsDataDictionary.pdf"))
if (!file.exists(f)) {
  url <- "http://www.cms.gov/OpenPayments/Downloads/OpenPaymentsDataDictionary.pdf"
  download.file(url, f, mode="wb")
}
# Download the *Datasets containing all records with identifying information on covered recipients* file.
filenames <- getAndUnzip("http://download.cms.gov/openpayments/09302014_ALLDTL.ZIP")
# Show the `README.txt` file.
cat(readLines(paste0(tempdir(), "\\README.txt")), sep="\n")


# General Payments
# **General Payments are defined as payments or other transfers of value not made in connection with a research agreement or research protocol.**
# Read the General Payments file into a data table.
# See *Appendix A: General Payments Detail* in [OpenPaymentsDataDictionary.pdf](http://www.cms.gov/OpenPayments/Downloads/OpenPaymentsDataDictionary.pdf).
f <- filenames$Name[grep("GNRL", filenames$Name)]
f <- paste0(tempdir(), "\\", f)
require(data.table)
colNames <- names(fread(f, nrows=0))
classes <- list(character=1:length(colNames))
D <- fread(f,
           colClasses=classes,
           na.strings="",
           stringsAsFactors=FALSE)
unlink(f)
# Recode dates and numeric columns.
colNames[grep("Date", colNames)]
D <- D[,
       `:=` (Program_Year = as.integer(Program_Year),
             Payment_Publication_Date = as.Date(Payment_Publication_Date, format="%m/%d/%Y"),
             Date_of_Payment = as.Date(Date_of_Payment, format="%m/%d/%Y"),
             Total_Amount_of_Payment_USDollars = as.numeric(Total_Amount_of_Payment_USDollars),
             Number_of_Payments_Included_in_Total_Amount = as.integer(Number_of_Payments_Included_in_Total_Amount))]
DGnrl <- D


# Research Payments
# **Research Payments are defined as payments or other transfers of value made in connection with a research agreement or research protocol.**
# Read the Research Payments file into a data table.
# See *Appendix B: Research Payments Detail* in [OpenPaymentsDataDictionary.pdf](http://www.cms.gov/OpenPayments/Downloads/OpenPaymentsDataDictionary.pdf).
f <- filenames$Name[grep("RSRCH", filenames$Name)]
f <- paste0(tempdir(), "\\", f)
require(data.table)
colNames <- names(fread(f, nrows=0))
classes <- list(character=1:length(colNames))
D <- fread(f,
           colClasses=classes,
           na.strings="",
           stringsAsFactors=FALSE)
unlink(f)
# Recode dates and numeric columns.
colNames[grep("Date", colNames)]
D <- D[,
       `:=` (Program_Year = as.integer(Program_Year),
             Payment_Publication_Date = as.Date(Payment_Publication_Date, format="%m/%d/%Y"),
             Date_of_Payment = as.Date(Date_of_Payment, format="%m/%d/%Y"),
             Total_Amount_of_Payment_USDollars = as.numeric(Total_Amount_of_Payment_USDollars))]
DRsrch <- D


# row bind the data tables
D <- rbind(DGnrl, DRsrch, fill=TRUE)


# Create payment type factor
D <- D[!is.na(General_Transaction_ID), payType := "General"]
D <- D[!is.na(Research_Transaction_ID), payType := "Research"]


# Create a payer column
D <- D[, payer := Submitting_Applicable_Manufacturer_or_Applicable_GPO_Name]
# D <- D[, payer := toupper(Applicable_Manufacturer_or_Applicable_GPO_Making_Payment_Name)]
# Create a recipient column
D <- D[grep("Hospital", Covered_Recipient_Type),
       recipient := Teaching_Hospital_Name]
D <- D[grepl("Physician", Covered_Recipient_Type),
       recipient := paste(Physician_Last_Name, Physician_First_Name, sep=", ")]


# Create payment amount category factor
D <- D[, amtCategory := cut(Total_Amount_of_Payment_USDollars, c(0, 1E0, 1E1, 1E2, 1E3, 1E4, 1E5, 1E6, 1E7), include.lowest=TRUE)]


# Write subset of columns to file
# For use in OpenRefine
write.csv(D[, list(payer, recipient, payType, amount = Total_Amount_of_Payment_USDollars)],
          file=file.path(getwd(), "data.csv"))


# Get zip code shapefiles
# See [Plotting color map with zip codes in R or Python](http://stackoverflow.com/a/1446144)
# filenames <- getAndUnzip("http://www2.census.gov/geo/tiger/GENZ2013/cb_2013_us_zcta510_500k.zip")
# zipShapefile <- paste0(tempdir(), "\\", filenames$Name[grep("shp$", filenames$Name)])
# require(maptools)
# zipMap <- readShapeSpatial(zipShapefile)
# labelpos <- data.frame(do.call(rbind, lapply(zipMap@polygons, function(x) x@labpt)))
# names(labelpos) <- c("x","y")                        
# zipMap@data <- data.frame(zipMap@data, labelpos)
