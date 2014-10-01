# Download the data dictionary.
url <- "http://www.cms.gov/OpenPayments/Downloads/OpenPaymentsDataDictionary.pdf"
download.file(url, paste0(getwd(), "\\OpenPaymentsDataDictionary.pdf"), mode="wb")
# Download the *Datasets containing all records with identifying information on covered recipients* file.
url <- "http://download.cms.gov/openpayments/09302014_ALLDTL.ZIP"
f <- paste0(tempfile(), ".zip")
download.file(url, f, mode="wb")
# Unzip the file.
unzip(f, exdir=tempdir())
filenames <- unzip(f, list=TRUE)
unlink(f)
filenames
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


# rbind the data tables
D <- rbind(DGnrl[, payType := "General"], DRsrch[, payType := "Research"], fill=TRUE)
D <- D[, payType := factor(payType)]
