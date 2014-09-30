# CMS Open Payments
Benjamin Chan (chanb@ohsu.edu)  
Tuesday, September 30, 2014  

2014-09-30

R version 3.1.1 (2014-07-10)

**Play around with the CMS [Open Payments](http://www.cms.gov/OpenPayments) data.**

Download the data dictionary.


```r
url <- "http://www.cms.gov/OpenPayments/Downloads/OpenPaymentsDataDictionary.pdf"
download.file(url, paste0(getwd(), "\\OpenPaymentsDataDictionary.pdf"), mode="wb")
```

Download the *Datasets containing all records with identifying information on covered recipients* file.


```r
url <- "http://download.cms.gov/openpayments/09302014_ALLDTL.ZIP"
f <- paste0(tempfile(), ".zip")
download.file(url, f, mode="wb")
```

Unzip the file.


```r
unzip(f, exdir=tempdir())
filenames <- unzip(f, list=TRUE)
filenames
```

```
##                                Name    Length                Date
## 1    OPPR_ALL_DTL_GNRL_09302014.csv 1.439e+09 2014-09-28 00:58:00
## 2 OPPR_ALL_DTL_OWNRSHP_09302014.csv 1.410e+06 2014-09-27 23:55:00
## 3   OPPR_ALL_DTL_RSRCH_09302014.csv 1.345e+07 2014-09-27 23:55:00
## 4  OPPR_SPLMTL_PH_PRFL_09302014.csv 7.630e+07 2014-09-27 23:55:00
## 5                        README.txt 3.202e+03 2014-09-26 14:03:00
```

Show the `README.txt` file.


```r
cat(readLines(paste0(tempdir(), "\\README.txt")), sep="\n")
```

```
## ï»¿Filename: README.txt
## Version: 1.0
## Date: September 2014
## 
## 1.  Important Note on Data Files
## The data contained in the CSV files does not include all data submitted to Open Payments for program year 2013. Consult the Open Payments Data Dictionary document for an explanation of the criteria CMS used to determine which data to publish.  The Open Payments Data Dictionary document can be found on the Open Payments website (http://www.cms.gov/openpayments).  The data dictionary also includes information about the data collection and reporting methodology, data fields included in the files, and any notes or special considerations that users should be aware of.
## 
## This data set includes only data that was successfully associated with specific physicians or teaching hospitals. Data that was not successfully associated with specific physicians or teaching hospitals is given in a separate data set.
## 
## 2. Important Considerations for using the CSV Files
## Microsoft Excel removes leading zeros from data fields in CSV files. Certain fields in these data sets may have leading zeros.  These zeroes will be missing when viewing the information within Microsoft Excel. Additionally, the latest versions of Microsoft Excel cannot display data sets with more than 1,048,576 rows. Some of these CSV files may exceed that limit. To display the data in its entirety may require the use of spreadsheet programs capable of handling very large numbers of records.
## 
## 3. Details about the Datasets Included in this 09302014_ALLDTL.zip File
## These files contain data published for the Open Payments program for the 2013 program year. This compressed (.zip) file contains four (4) comma-separated value (.csv) format files and one (1) README.txt file. Descriptions of each file are provided below.
## 
## File #1 - OPPR_ALL_DTL_GNRL_09302014.csv: 
## This file contains the data set for General Payments for the 2013 program year. General Payments are defined as payments or other transfers of value not made in connection with a research agreement or research protocol.
## 
## File #2 - OPPR_ALL_DTL_RSRCH_09302014.csv:
## This file contains the data set for Research Payments for the 2013 program year. Research Payments are defined as payments or other transfers of value made in connection with a research agreement or research protocol.
## 
## File #3 - OPPR_ALL_DTL_OWNRSHP_09302104.csv:
## This file contains the data set for Ownership and Investment Interest Information for the 2013 program year. Ownership and Investment Interest Information is defined as information about the value of ownership or investment interest in an applicable manufacturer or applicable group purchasing organization.  
## 
## File #4 - OPPR_SPLMTL_PH_PRFL_09302014.csv:
## A supplementary file that displays all of the physicians indicated as recipients of payments, other transfers of value, or ownership and investment interest in records reported in Open Payments. Each record includes the physicianâ€™s demographic information, specialties, and license information, as well as a unique identification number (Physician Profile ID) that can be used to search for a specific physician in the general, research, and physician ownership files.
```

Read the General Payments file into a data frame.
See *Appendix A: General Payments Detail* in *OpenPaymentsDataDictionary.pdf*.


```r
f <- filenames$Name[grep("GNRL", filenames$Name)]
f <- paste0(tempdir(), "\\", f)
require(data.table)
colNames <- names(fread(f, nrows=0))
classes <- list(character=1:length(colNames))
D <- fread(f,
           colClasses=classes,
           na.strings="",
           stringsAsFactors=FALSE)
```

```
## 
Read 0.0% of 2626674 rows
Read 5.3% of 2626674 rows
Read 10.7% of 2626674 rows
Read 16.0% of 2626674 rows
Read 20.9% of 2626674 rows
Read 26.3% of 2626674 rows
Read 31.2% of 2626674 rows
Read 36.2% of 2626674 rows
Read 41.5% of 2626674 rows
Read 46.4% of 2626674 rows
Read 51.4% of 2626674 rows
Read 56.3% of 2626674 rows
Read 61.3% of 2626674 rows
Read 66.2% of 2626674 rows
Read 71.2% of 2626674 rows
Read 76.5% of 2626674 rows
Read 81.9% of 2626674 rows
Read 87.2% of 2626674 rows
Read 92.5% of 2626674 rows
Read 97.8% of 2626674 rows
Read 2626674 rows and 63 (of 63) columns from 1.340 GB file in 00:00:23
```

Recode dates and numeric columns.


```r
colNames[grep("Date", colNames)]
```

```
## [1] "Payment_Publication_Date" "Date_of_Payment"
```

```r
D <- D[,
       `:=` (Program_Year = as.integer(Program_Year),
             Payment_Publication_Date = as.Date(Payment_Publication_Date, format="%m/%d/%Y"),
             Date_of_Payment = as.Date(Date_of_Payment, format="%m/%d/%Y"),
             Total_Amount_of_Payment_USDollars = as.numeric(Total_Amount_of_Payment_USDollars),
             Number_of_Payments_Included_in_Total_Amount = as.integer(Number_of_Payments_Included_in_Total_Amount))]
```

Find the largest payment.


```r
maxPayment <- max(D[, Total_Amount_of_Payment_USDollars])
D[Total_Amount_of_Payment_USDollars == maxPayment,
  list(Total_Amount_of_Payment_USDollars,
       Date_of_Payment,
       Submitting_Applicable_Manufacturer_or_Applicable_GPO_Name,
       Physician_First_Name,
       Physician_Last_Name,
       Teaching_Hospital_Name,
       Recipient_City,
       Recipient_State,
       Recipient_Postal_Code,
       Name_of_Associated_Covered_Drug_or_Biological1,
       Form_of_Payment_or_Transfer_of_Value,
       Nature_of_Payment_or_Transfer_of_Value)]
```

```
##    Total_Amount_of_Payment_USDollars Date_of_Payment
## 1:                           9645117      2013-11-21
##    Submitting_Applicable_Manufacturer_or_Applicable_GPO_Name
## 1:                                           Genentech, Inc.
##    Physician_First_Name Physician_Last_Name
## 1:                   NA                  NA
##                  Teaching_Hospital_Name Recipient_City Recipient_State
## 1: CITY OF HOPE NATIONAL MEDICAL CENTER         DUARTE              CA
##    Recipient_Postal_Code Name_of_Associated_Covered_Drug_or_Biological1
## 1:                    NA                                             NA
##    Form_of_Payment_or_Transfer_of_Value
## 1:              Cash or cash equivalent
##    Nature_of_Payment_or_Transfer_of_Value
## 1:                     Royalty or License
```
