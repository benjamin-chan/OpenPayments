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


require(data.table)


# General Payments
# **General Payments are defined as payments or other transfers of value not made in connection with a research agreement or research protocol.**
# Read the General Payments file into a data table.
# See *Appendix A: General Payments Detail* in [OpenPaymentsDataDictionary.pdf](http://www.cms.gov/OpenPayments/Downloads/OpenPaymentsDataDictionary.pdf).
f <- filenames$Name[grep("GNRL", filenames$Name)]
f <- paste0(tempdir(), "\\", f)
DGnrl <- readFile(f)


# Research Payments
# **Research Payments are defined as payments or other transfers of value made in connection with a research agreement or research protocol.**
# Read the Research Payments file into a data table.
# See *Appendix B: Research Payments Detail* in [OpenPaymentsDataDictionary.pdf](http://www.cms.gov/OpenPayments/Downloads/OpenPaymentsDataDictionary.pdf).
f <- filenames$Name[grep("RSRCH", filenames$Name)]
f <- paste0(tempdir(), "\\", f)
DRsrch <- readFile(f)


# Ownership and Investment Interest
# **Ownership and Investment Interest Information is defined as information about the value of ownership or investment interest in an applicable manufacturer or applicable group purchasing organization.**
# Read the Physician Ownership Information file into a data table.
# See *Appendix C: Physician Ownership Information* in [OpenPaymentsDataDictionary.pdf](http://www.cms.gov/OpenPayments/Downloads/OpenPaymentsDataDictionary.pdf).
f <- filenames$Name[grep("OWNRSHP", filenames$Name)]
f <- paste0(tempdir(), "\\", f)
DOwnrshp <- readFile(f)


# row bind the data tables
D <- rbind(DGnrl, DRsrch, DOwnrshp, fill=TRUE)


# Create payment type factor
D <- D[!is.na(General_Transaction_ID), payType := "General"]
D <- D[!is.na(Research_Transaction_ID), payType := "Research"]
D <- D[!is.na(Physician_Ownership_Transaction_ID), payType := "Ownership"]


# Create a payer column
D <- D[, payer := Submitting_Applicable_Manufacturer_or_Applicable_GPO_Name]
# D <- D[, payer := toupper(Applicable_Manufacturer_or_Applicable_GPO_Making_Payment_Name)]


# Create a recipient column
D <- D[grep("Hospital", Covered_Recipient_Type),
       recipient := Teaching_Hospital_Name]
D <- D[grepl("Physician", Covered_Recipient_Type),
       recipient := paste(Physician_Last_Name, Physician_First_Name, sep=", ")]
D <- D[payType == "Ownership",
       recipient := paste(Physician_Last_Name, Physician_First_Name, sep=", ")]


# Create an amount column
D <- D[payType %in% c("General", "Research"),
       amount := Total_Amount_of_Payment_USDollars]
D <- D[payType == "Ownership",
       amount := Dollar_Amount_Invested]


# Create payment amount category factor
D <- D[, amtCategory := cut(amount, c(0, 1E0, 1E1, 1E2, 1E3, 1E4, 1E5, 1E6, 1E7), include.lowest=TRUE)]


# Subset columns
D <- D[, list(payType, payer, recipient, Recipient_State, amount, amtCategory)]


# Save the data table object to file
save(D, file=file.path(getwd(), "OpenPayments.RData"))


# Write subset of columns to file
# For use in OpenRefine
# write.csv(D[, list(payer, recipient, payType, amount)],
#           file=file.path(getwd(), "data.csv"))


# Get zip code shapefiles
# See [Plotting color map with zip codes in R or Python](http://stackoverflow.com/a/1446144)
# filenames <- getAndUnzip("http://www2.census.gov/geo/tiger/GENZ2013/cb_2013_us_zcta510_500k.zip")
# zipShapefile <- paste0(tempdir(), "\\", filenames$Name[grep("shp$", filenames$Name)])
# require(maptools)
# zipMap <- readShapeSpatial(zipShapefile)
# labelpos <- data.frame(do.call(rbind, lapply(zipMap@polygons, function(x) x@labpt)))
# names(labelpos) <- c("x","y")                        
# zipMap@data <- data.frame(zipMap@data, labelpos)
