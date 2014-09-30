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
Read 26.6% of 2626674 rows
Read 32.0% of 2626674 rows
Read 37.3% of 2626674 rows
Read 42.6% of 2626674 rows
Read 48.0% of 2626674 rows
Read 53.3% of 2626674 rows
Read 58.6% of 2626674 rows
Read 64.0% of 2626674 rows
Read 69.3% of 2626674 rows
Read 74.6% of 2626674 rows
Read 79.9% of 2626674 rows
Read 85.3% of 2626674 rows
Read 90.6% of 2626674 rows
Read 95.9% of 2626674 rows
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

Find the 10 largest payments.


```r
topPayments <- sort(D[, Total_Amount_of_Payment_USDollars], decreasing=TRUE)[1:10]
D[Total_Amount_of_Payment_USDollars %in% topPayments,
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
       Nature_of_Payment_or_Transfer_of_Value)][order(Total_Amount_of_Payment_USDollars, decreasing=TRUE)]
```

```
##     Total_Amount_of_Payment_USDollars Date_of_Payment
##  1:                           9645117      2013-11-21
##  2:                           8411100      2013-11-21
##  3:                           8044086      2013-08-27
##  4:                           7803300      2013-08-27
##  5:                           7012126      2013-11-21
##  6:                           6595011      2013-08-27
##  7:                           5014780      2013-10-03
##  8:                           4717495      2013-11-21
##  9:                           4454795      2013-11-21
## 10:                           4431356      2013-08-27
##     Submitting_Applicable_Manufacturer_or_Applicable_GPO_Name
##  1:                                           Genentech, Inc.
##  2:                                           Genentech, Inc.
##  3:                                           Genentech, Inc.
##  4:                                           Genentech, Inc.
##  5:                                           Genentech, Inc.
##  6:                                           Genentech, Inc.
##  7:                                        Zimmer Holding Inc
##  8:                                           Genentech, Inc.
##  9:                                           Genentech, Inc.
## 10:                                           Genentech, Inc.
##     Physician_First_Name Physician_Last_Name
##  1:                   NA                  NA
##  2:                   NA                  NA
##  3:                   NA                  NA
##  4:                   NA                  NA
##  5:                   NA                  NA
##  6:                   NA                  NA
##  7:                   NA                  NA
##  8:                   NA                  NA
##  9:                   NA                  NA
## 10:                   NA                  NA
##                   Teaching_Hospital_Name Recipient_City Recipient_State
##  1: CITY OF HOPE NATIONAL MEDICAL CENTER         DUARTE              CA
##  2: CITY OF HOPE NATIONAL MEDICAL CENTER         DUARTE              CA
##  3: CITY OF HOPE NATIONAL MEDICAL CENTER         DUARTE              CA
##  4: CITY OF HOPE NATIONAL MEDICAL CENTER         DUARTE              CA
##  5: CITY OF HOPE NATIONAL MEDICAL CENTER         DUARTE              CA
##  6: CITY OF HOPE NATIONAL MEDICAL CENTER         DUARTE              CA
##  7:     The General Hospital Corporation         BOSTON              MA
##  8: CITY OF HOPE NATIONAL MEDICAL CENTER         DUARTE              CA
##  9: CITY OF HOPE NATIONAL MEDICAL CENTER         DUARTE              CA
## 10: CITY OF HOPE NATIONAL MEDICAL CENTER         DUARTE              CA
##     Recipient_Postal_Code Name_of_Associated_Covered_Drug_or_Biological1
##  1:                    NA                                             NA
##  2:                    NA                                             NA
##  3:                    NA                                        Rituxan
##  4:                    NA                                             NA
##  5:                    NA                                        Avastin
##  6:                    NA                                        Avastin
##  7:                    NA                                             NA
##  8:                    NA                                      Herceptin
##  9:                    NA                                       Lucentis
## 10:                    NA                                      Herceptin
##     Form_of_Payment_or_Transfer_of_Value
##  1:              Cash or cash equivalent
##  2:              Cash or cash equivalent
##  3:              Cash or cash equivalent
##  4:              Cash or cash equivalent
##  5:              Cash or cash equivalent
##  6:              Cash or cash equivalent
##  7:              Cash or cash equivalent
##  8:              Cash or cash equivalent
##  9:              Cash or cash equivalent
## 10:              Cash or cash equivalent
##     Nature_of_Payment_or_Transfer_of_Value
##  1:                     Royalty or License
##  2:                     Royalty or License
##  3:                     Royalty or License
##  4:                     Royalty or License
##  5:                     Royalty or License
##  6:                     Royalty or License
##  7:                     Royalty or License
##  8:                     Royalty or License
##  9:                     Royalty or License
## 10:                     Royalty or License
```

How many unique drugs or biologicals are represented?


```r
setkey(D, Name_of_Associated_Covered_Drug_or_Biological1)
length(unique(D[, Name_of_Associated_Covered_Drug_or_Biological1]))
```

```
## [1] 1461
```

List the drugs or biologicals with more than $1M in total payments.


```r
drugs <- D[,
           list(sumPayments = sum(Total_Amount_of_Payment_USDollars),
                nRecords = .N),
           Name_of_Associated_Covered_Drug_or_Biological1][order(sumPayments, decreasing=TRUE)]
drugs[sumPayments > 1E6]
```

```
##     Name_of_Associated_Covered_Drug_or_Biological1 sumPayments nRecords
##  1:                                             NA   389531889   652336
##  2:                                        Avastin    17358840     4651
##  3:                                       Lucentis    15239466     1486
##  4:                                      Herceptin    14879583      499
##  5:                                        Rituxan     8703093     4372
##  6:                                       BRILINTA     6525866    35879
##  7:                                         Latuda     5782169    37187
##  8:                                         Xolair     4782001     4200
##  9:                                       Invokana     4645431    41931
## 10:                                        Xarelto     4179818    54130
## 11:                                        ELIQUIS     4133245    30872
## 12:                                        Actemra     4111678     3734
## 13:                               ABILIFY MAINTENA     3504790    15820
## 14:                                        Zegerid     3352841      693
## 15:                                 ABILIFY TABLET     3278898    32123
## 16:                                        Victoza     3272041    30315
## 17:                                         Humira     3214749    26210
## 18:                                      NON_BRAND     3028619     8106
## 19:                                      SYMBICORT     2929173    39960
## 20:                                        LINZESS     2657872    32198
## 21:                                         SAMSCA     2650685    13567
## 22:                                        Pradaxa     2559642     6718
## 23:                                        TUDORZA     2486974    29554
## 24:                                       COPAXONE     2461761     7193
## 25:                                          BOTOX     2390912    18708
## 26:                                         SUBSYS     2357665     5840
## 27:                                      TRADJENTA     2211690    33105
## 28:                                 Foot and Ankle     2103548     2894
## 29:                                       BYDUREON     2060543    18386
## 30:                                      UNBRANDED     2000000        1
## 31:                                     Brintellix     1981131     5271
## 32:                                    SEROQUEL XR     1959845    16622
## 33:                                        AZILECT     1883411     5277
## 34:                                       Nuedexta     1667017     8682
## 35:                                        Kadcyla     1643496     1200
## 36:                                        Osphena     1557929    13930
## 37:                                        SUCLEAR     1515360       13
## 38:                                         Prolia     1493773    21579
## 39:                                        CRESTOR     1415752    29302
## 40:                                         Enbrel     1399640     8831
## 41:                                       BUSULFEX     1390666       88
## 42:                                          Oseni     1385026    14510
## 43:                                         AMYVID     1368433      413
## 44:                                         TYVASO     1321520     1187
## 45:                                           None     1303985     1784
## 46:                                        Perjeta     1268189     1463
## 47:                                        Vascepa     1259114     7446
## 48:                                        VELCADE     1257607     3861
## 49:                                  ACTHAR- NEURO     1216209     2124
## 50:                                        AUBAGIO     1204090     3777
## 51:                                         TESTIM     1186310    14513
## 52:                                           BREO     1164162     4585
## 53:                                        SOLIRIS     1144949     4605
## 54:                                        AMITIZA     1128025     3089
## 55:                                     NAMENDA XR     1050975    10775
## 56:                                       RESTASIS     1020619    16616
## 57:                                       DALIRESP     1020059    15868
## 58:                                        XELJANZ     1014173     6376
## 59:                                        VIIBRYD     1008522    19252
##     Name_of_Associated_Covered_Drug_or_Biological1 sumPayments nRecords
```
