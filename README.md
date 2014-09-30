# CMS Open Payments
Benjamin Chan (chanb@ohsu.edu)  

**Play around with the CMS [Open Payments](http://www.cms.gov/OpenPayments) data.**

2014-09-30

R version 3.1.1 (2014-07-10)

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

Define helper functions.


```r
niceTable <- function (dt) {
  require(xtable)
  print(xtable(dt), type="html", include.rownames=FALSE)
}
```

# General Payments

**General Payments are defined as payments or other transfers of value not made in connection with a research agreement or research protocol.**

Read the General Payments file into a data table.
See *Appendix A: General Payments Detail* in [OpenPaymentsDataDictionary.pdf](http://www.cms.gov/OpenPayments/Downloads/OpenPaymentsDataDictionary.pdf).


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
Read 11.0% of 2626674 rows
Read 16.4% of 2626674 rows
Read 21.7% of 2626674 rows
Read 27.4% of 2626674 rows
Read 33.1% of 2626674 rows
Read 38.8% of 2626674 rows
Read 44.5% of 2626674 rows
Read 50.3% of 2626674 rows
Read 55.6% of 2626674 rows
Read 61.3% of 2626674 rows
Read 67.0% of 2626674 rows
Read 72.7% of 2626674 rows
Read 78.4% of 2626674 rows
Read 84.1% of 2626674 rows
Read 90.2% of 2626674 rows
Read 95.9% of 2626674 rows
Read 2626674 rows and 63 (of 63) columns from 1.340 GB file in 00:00:21
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

Find the 10 largest payments.


```r
topPayments <- sort(D[, Total_Amount_of_Payment_USDollars], decreasing=TRUE)[1:10]
top <- D[Total_Amount_of_Payment_USDollars %in% topPayments,
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
niceTable(top)
```

```
## Warning: class of 'x' was discarded
```

<!-- html table generated in R 3.1.1 by xtable 1.7-3 package -->
<!-- Tue Sep 30 15:52:40 2014 -->
<TABLE border=1>
<TR> <TH> Total_Amount_of_Payment_USDollars </TH> <TH> Date_of_Payment </TH> <TH> Submitting_Applicable_Manufacturer_or_Applicable_GPO_Name </TH> <TH> Physician_First_Name </TH> <TH> Physician_Last_Name </TH> <TH> Teaching_Hospital_Name </TH> <TH> Recipient_City </TH> <TH> Recipient_State </TH> <TH> Recipient_Postal_Code </TH> <TH> Name_of_Associated_Covered_Drug_or_Biological1 </TH> <TH> Form_of_Payment_or_Transfer_of_Value </TH> <TH> Nature_of_Payment_or_Transfer_of_Value </TH>  </TR>
  <TR> <TD align="right"> 9645117.00 </TD> <TD align="right"> 16030.00 </TD> <TD> Genentech, Inc. </TD> <TD>  </TD> <TD>  </TD> <TD> CITY OF HOPE NATIONAL MEDICAL CENTER </TD> <TD> DUARTE </TD> <TD> CA </TD> <TD>  </TD> <TD>  </TD> <TD> Cash or cash equivalent </TD> <TD> Royalty or License </TD> </TR>
  <TR> <TD align="right"> 8411100.00 </TD> <TD align="right"> 16030.00 </TD> <TD> Genentech, Inc. </TD> <TD>  </TD> <TD>  </TD> <TD> CITY OF HOPE NATIONAL MEDICAL CENTER </TD> <TD> DUARTE </TD> <TD> CA </TD> <TD>  </TD> <TD>  </TD> <TD> Cash or cash equivalent </TD> <TD> Royalty or License </TD> </TR>
  <TR> <TD align="right"> 8044086.00 </TD> <TD align="right"> 15944.00 </TD> <TD> Genentech, Inc. </TD> <TD>  </TD> <TD>  </TD> <TD> CITY OF HOPE NATIONAL MEDICAL CENTER </TD> <TD> DUARTE </TD> <TD> CA </TD> <TD>  </TD> <TD> Rituxan </TD> <TD> Cash or cash equivalent </TD> <TD> Royalty or License </TD> </TR>
  <TR> <TD align="right"> 7803300.00 </TD> <TD align="right"> 15944.00 </TD> <TD> Genentech, Inc. </TD> <TD>  </TD> <TD>  </TD> <TD> CITY OF HOPE NATIONAL MEDICAL CENTER </TD> <TD> DUARTE </TD> <TD> CA </TD> <TD>  </TD> <TD>  </TD> <TD> Cash or cash equivalent </TD> <TD> Royalty or License </TD> </TR>
  <TR> <TD align="right"> 7012126.00 </TD> <TD align="right"> 16030.00 </TD> <TD> Genentech, Inc. </TD> <TD>  </TD> <TD>  </TD> <TD> CITY OF HOPE NATIONAL MEDICAL CENTER </TD> <TD> DUARTE </TD> <TD> CA </TD> <TD>  </TD> <TD> Avastin </TD> <TD> Cash or cash equivalent </TD> <TD> Royalty or License </TD> </TR>
  <TR> <TD align="right"> 6595011.00 </TD> <TD align="right"> 15944.00 </TD> <TD> Genentech, Inc. </TD> <TD>  </TD> <TD>  </TD> <TD> CITY OF HOPE NATIONAL MEDICAL CENTER </TD> <TD> DUARTE </TD> <TD> CA </TD> <TD>  </TD> <TD> Avastin </TD> <TD> Cash or cash equivalent </TD> <TD> Royalty or License </TD> </TR>
  <TR> <TD align="right"> 5014780.19 </TD> <TD align="right"> 15981.00 </TD> <TD> Zimmer Holding Inc </TD> <TD>  </TD> <TD>  </TD> <TD> The General Hospital Corporation </TD> <TD> BOSTON </TD> <TD> MA </TD> <TD>  </TD> <TD>  </TD> <TD> Cash or cash equivalent </TD> <TD> Royalty or License </TD> </TR>
  <TR> <TD align="right"> 4717495.00 </TD> <TD align="right"> 16030.00 </TD> <TD> Genentech, Inc. </TD> <TD>  </TD> <TD>  </TD> <TD> CITY OF HOPE NATIONAL MEDICAL CENTER </TD> <TD> DUARTE </TD> <TD> CA </TD> <TD>  </TD> <TD> Herceptin </TD> <TD> Cash or cash equivalent </TD> <TD> Royalty or License </TD> </TR>
  <TR> <TD align="right"> 4454795.00 </TD> <TD align="right"> 16030.00 </TD> <TD> Genentech, Inc. </TD> <TD>  </TD> <TD>  </TD> <TD> CITY OF HOPE NATIONAL MEDICAL CENTER </TD> <TD> DUARTE </TD> <TD> CA </TD> <TD>  </TD> <TD> Lucentis </TD> <TD> Cash or cash equivalent </TD> <TD> Royalty or License </TD> </TR>
  <TR> <TD align="right"> 4431356.00 </TD> <TD align="right"> 15944.00 </TD> <TD> Genentech, Inc. </TD> <TD>  </TD> <TD>  </TD> <TD> CITY OF HOPE NATIONAL MEDICAL CENTER </TD> <TD> DUARTE </TD> <TD> CA </TD> <TD>  </TD> <TD> Herceptin </TD> <TD> Cash or cash equivalent </TD> <TD> Royalty or License </TD> </TR>
   </TABLE>

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
niceTable(drugs[sumPayments > 1E6])
```

<!-- html table generated in R 3.1.1 by xtable 1.7-3 package -->
<!-- Tue Sep 30 15:52:41 2014 -->
<TABLE border=1>
<TR> <TH> Name_of_Associated_Covered_Drug_or_Biological1 </TH> <TH> sumPayments </TH> <TH> nRecords </TH>  </TR>
  <TR> <TD>  </TD> <TD align="right"> 389531889.10 </TD> <TD align="right"> 652336 </TD> </TR>
  <TR> <TD> Avastin </TD> <TD align="right"> 17358839.86 </TD> <TD align="right"> 4651 </TD> </TR>
  <TR> <TD> Lucentis </TD> <TD align="right"> 15239466.05 </TD> <TD align="right"> 1486 </TD> </TR>
  <TR> <TD> Herceptin </TD> <TD align="right"> 14879582.80 </TD> <TD align="right"> 499 </TD> </TR>
  <TR> <TD> Rituxan </TD> <TD align="right"> 8703093.13 </TD> <TD align="right"> 4372 </TD> </TR>
  <TR> <TD> BRILINTA </TD> <TD align="right"> 6525865.73 </TD> <TD align="right"> 35879 </TD> </TR>
  <TR> <TD> Latuda </TD> <TD align="right"> 5782168.79 </TD> <TD align="right"> 37187 </TD> </TR>
  <TR> <TD> Xolair </TD> <TD align="right"> 4782001.01 </TD> <TD align="right"> 4200 </TD> </TR>
  <TR> <TD> Invokana </TD> <TD align="right"> 4645430.79 </TD> <TD align="right"> 41931 </TD> </TR>
  <TR> <TD> Xarelto </TD> <TD align="right"> 4179817.93 </TD> <TD align="right"> 54130 </TD> </TR>
  <TR> <TD> ELIQUIS </TD> <TD align="right"> 4133245.33 </TD> <TD align="right"> 30872 </TD> </TR>
  <TR> <TD> Actemra </TD> <TD align="right"> 4111677.95 </TD> <TD align="right"> 3734 </TD> </TR>
  <TR> <TD> ABILIFY MAINTENA </TD> <TD align="right"> 3504789.56 </TD> <TD align="right"> 15820 </TD> </TR>
  <TR> <TD> Zegerid </TD> <TD align="right"> 3352841.31 </TD> <TD align="right"> 693 </TD> </TR>
  <TR> <TD> ABILIFY TABLET </TD> <TD align="right"> 3278898.11 </TD> <TD align="right"> 32123 </TD> </TR>
  <TR> <TD> Victoza </TD> <TD align="right"> 3272041.13 </TD> <TD align="right"> 30315 </TD> </TR>
  <TR> <TD> Humira </TD> <TD align="right"> 3214748.98 </TD> <TD align="right"> 26210 </TD> </TR>
  <TR> <TD> NON_BRAND </TD> <TD align="right"> 3028619.39 </TD> <TD align="right"> 8106 </TD> </TR>
  <TR> <TD> SYMBICORT </TD> <TD align="right"> 2929172.62 </TD> <TD align="right"> 39960 </TD> </TR>
  <TR> <TD> LINZESS </TD> <TD align="right"> 2657872.13 </TD> <TD align="right"> 32198 </TD> </TR>
  <TR> <TD> SAMSCA </TD> <TD align="right"> 2650685.14 </TD> <TD align="right"> 13567 </TD> </TR>
  <TR> <TD> Pradaxa </TD> <TD align="right"> 2559642.03 </TD> <TD align="right"> 6718 </TD> </TR>
  <TR> <TD> TUDORZA </TD> <TD align="right"> 2486973.73 </TD> <TD align="right"> 29554 </TD> </TR>
  <TR> <TD> COPAXONE </TD> <TD align="right"> 2461761.37 </TD> <TD align="right"> 7193 </TD> </TR>
  <TR> <TD> BOTOX </TD> <TD align="right"> 2390911.70 </TD> <TD align="right"> 18708 </TD> </TR>
  <TR> <TD> SUBSYS </TD> <TD align="right"> 2357664.61 </TD> <TD align="right"> 5840 </TD> </TR>
  <TR> <TD> TRADJENTA </TD> <TD align="right"> 2211690.03 </TD> <TD align="right"> 33105 </TD> </TR>
  <TR> <TD> Foot and Ankle </TD> <TD align="right"> 2103548.47 </TD> <TD align="right"> 2894 </TD> </TR>
  <TR> <TD> BYDUREON </TD> <TD align="right"> 2060542.81 </TD> <TD align="right"> 18386 </TD> </TR>
  <TR> <TD> UNBRANDED </TD> <TD align="right"> 2000000.00 </TD> <TD align="right">   1 </TD> </TR>
  <TR> <TD> Brintellix </TD> <TD align="right"> 1981131.26 </TD> <TD align="right"> 5271 </TD> </TR>
  <TR> <TD> SEROQUEL XR </TD> <TD align="right"> 1959844.71 </TD> <TD align="right"> 16622 </TD> </TR>
  <TR> <TD> AZILECT </TD> <TD align="right"> 1883410.74 </TD> <TD align="right"> 5277 </TD> </TR>
  <TR> <TD> Nuedexta </TD> <TD align="right"> 1667016.69 </TD> <TD align="right"> 8682 </TD> </TR>
  <TR> <TD> Kadcyla </TD> <TD align="right"> 1643496.48 </TD> <TD align="right"> 1200 </TD> </TR>
  <TR> <TD> Osphena </TD> <TD align="right"> 1557928.75 </TD> <TD align="right"> 13930 </TD> </TR>
  <TR> <TD> SUCLEAR </TD> <TD align="right"> 1515360.01 </TD> <TD align="right">  13 </TD> </TR>
  <TR> <TD> Prolia </TD> <TD align="right"> 1493772.56 </TD> <TD align="right"> 21579 </TD> </TR>
  <TR> <TD> CRESTOR </TD> <TD align="right"> 1415752.33 </TD> <TD align="right"> 29302 </TD> </TR>
  <TR> <TD> Enbrel </TD> <TD align="right"> 1399639.80 </TD> <TD align="right"> 8831 </TD> </TR>
  <TR> <TD> BUSULFEX </TD> <TD align="right"> 1390666.49 </TD> <TD align="right">  88 </TD> </TR>
  <TR> <TD> Oseni </TD> <TD align="right"> 1385025.54 </TD> <TD align="right"> 14510 </TD> </TR>
  <TR> <TD> AMYVID </TD> <TD align="right"> 1368432.99 </TD> <TD align="right"> 413 </TD> </TR>
  <TR> <TD> TYVASO </TD> <TD align="right"> 1321519.71 </TD> <TD align="right"> 1187 </TD> </TR>
  <TR> <TD> None </TD> <TD align="right"> 1303985.34 </TD> <TD align="right"> 1784 </TD> </TR>
  <TR> <TD> Perjeta </TD> <TD align="right"> 1268189.03 </TD> <TD align="right"> 1463 </TD> </TR>
  <TR> <TD> Vascepa </TD> <TD align="right"> 1259113.64 </TD> <TD align="right"> 7446 </TD> </TR>
  <TR> <TD> VELCADE </TD> <TD align="right"> 1257606.59 </TD> <TD align="right"> 3861 </TD> </TR>
  <TR> <TD> ACTHAR- NEURO </TD> <TD align="right"> 1216208.58 </TD> <TD align="right"> 2124 </TD> </TR>
  <TR> <TD> AUBAGIO </TD> <TD align="right"> 1204089.93 </TD> <TD align="right"> 3777 </TD> </TR>
  <TR> <TD> TESTIM </TD> <TD align="right"> 1186310.12 </TD> <TD align="right"> 14513 </TD> </TR>
  <TR> <TD> BREO </TD> <TD align="right"> 1164162.22 </TD> <TD align="right"> 4585 </TD> </TR>
  <TR> <TD> SOLIRIS </TD> <TD align="right"> 1144948.84 </TD> <TD align="right"> 4605 </TD> </TR>
  <TR> <TD> AMITIZA </TD> <TD align="right"> 1128025.28 </TD> <TD align="right"> 3089 </TD> </TR>
  <TR> <TD> NAMENDA XR </TD> <TD align="right"> 1050974.92 </TD> <TD align="right"> 10775 </TD> </TR>
  <TR> <TD> RESTASIS </TD> <TD align="right"> 1020618.88 </TD> <TD align="right"> 16616 </TD> </TR>
  <TR> <TD> DALIRESP </TD> <TD align="right"> 1020059.32 </TD> <TD align="right"> 15868 </TD> </TR>
  <TR> <TD> XELJANZ </TD> <TD align="right"> 1014172.64 </TD> <TD align="right"> 6376 </TD> </TR>
  <TR> <TD> VIIBRYD </TD> <TD align="right"> 1008521.62 </TD> <TD align="right"> 19252 </TD> </TR>
   </TABLE>

List the teaching hospitals with more than $1M in total payments.


```r
hosps <- D[grep("Teaching Hospital", Covered_Recipient_Type),
           list(sumPayments = sum(Total_Amount_of_Payment_USDollars),
                nRecords = .N),
           list(Teaching_Hospital_Name, Recipient_City, Recipient_State)][order(sumPayments, decreasing=TRUE)]
niceTable(hosps[sumPayments > 1E6])
```

<!-- html table generated in R 3.1.1 by xtable 1.7-3 package -->
<!-- Tue Sep 30 15:52:42 2014 -->
<TABLE border=1>
<TR> <TH> Teaching_Hospital_Name </TH> <TH> Recipient_City </TH> <TH> Recipient_State </TH> <TH> sumPayments </TH> <TH> nRecords </TH>  </TR>
  <TR> <TD> CITY OF HOPE NATIONAL MEDICAL CENTER </TD> <TD> DUARTE </TD> <TD> CA </TD> <TD align="right"> 122586713.41 </TD> <TD align="right">  74 </TD> </TR>
  <TR> <TD> The General Hospital Corporation </TD> <TD> BOSTON </TD> <TD> MA </TD> <TD align="right"> 10328637.47 </TD> <TD align="right">  20 </TD> </TR>
  <TR> <TD> The Cleveland Clinic Foundation </TD> <TD> CLEVELAND </TD> <TD> OH </TD> <TD align="right"> 4264188.56 </TD> <TD align="right">   4 </TD> </TR>
  <TR> <TD> Curators Of The University Of Missouri </TD> <TD> Columbia </TD> <TD> MO </TD> <TD align="right"> 3572049.31 </TD> <TD align="right"> 120 </TD> </TR>
  <TR> <TD> TRUSTEES OF THE UNIVERSITY OF PENNSYLVANIA </TD> <TD> PHILADELPHIA </TD> <TD> PA </TD> <TD align="right"> 3482700.00 </TD> <TD align="right">  12 </TD> </TR>
  <TR> <TD> Cedars-Sinai Medical Center </TD> <TD> Los Angeles </TD> <TD> CA </TD> <TD align="right"> 2659433.90 </TD> <TD align="right"> 436 </TD> </TR>
  <TR> <TD> DENVER HEALTH AND HOSPITAL AUTHORITY </TD> <TD> DENVER </TD> <TD> CO </TD> <TD align="right"> 2642642.60 </TD> <TD align="right"> 209 </TD> </TR>
  <TR> <TD> Denver Health And Hospital Authority </TD> <TD> Denver </TD> <TD> CO </TD> <TD align="right"> 2464203.43 </TD> <TD align="right">  43 </TD> </TR>
  <TR> <TD> Duke University Health System, Inc. </TD> <TD> Durham </TD> <TD> NC </TD> <TD align="right"> 2440513.55 </TD> <TD align="right">  85 </TD> </TR>
  <TR> <TD> St Lukes Roosevelt Hospital Center </TD> <TD> New York </TD> <TD> NY </TD> <TD align="right"> 2181846.30 </TD> <TD align="right"> 116 </TD> </TR>
  <TR> <TD> The General Hospital Corporation </TD> <TD> Boston </TD> <TD> MA </TD> <TD align="right"> 2028861.13 </TD> <TD align="right"> 109 </TD> </TR>
  <TR> <TD> University Of Texas M. D. Anderson Cancer Center </TD> <TD> Houston </TD> <TD> TX </TD> <TD align="right"> 1610643.84 </TD> <TD align="right">  60 </TD> </TR>
  <TR> <TD> Montefiore Medical Center </TD> <TD> Bronx </TD> <TD> NY </TD> <TD align="right"> 1484225.09 </TD> <TD align="right">  63 </TD> </TR>
  <TR> <TD> Nebraska Medical Center </TD> <TD> Omaha </TD> <TD> NE </TD> <TD align="right"> 1360817.66 </TD> <TD align="right">  29 </TD> </TR>
  <TR> <TD> Ucsd Medical Center </TD> <TD> San Diego </TD> <TD> CA </TD> <TD align="right"> 1316122.93 </TD> <TD align="right">  43 </TD> </TR>
  <TR> <TD> Children'S Hospital Medical Center </TD> <TD> Cincinnati </TD> <TD> OH </TD> <TD align="right"> 1256820.26 </TD> <TD align="right">  67 </TD> </TR>
  <TR> <TD> Regents Of The University Of California </TD> <TD> Sacramento </TD> <TD> CA </TD> <TD align="right"> 1119139.81 </TD> <TD align="right">  62 </TD> </TR>
  <TR> <TD> Washington Hospital Center Corporation </TD> <TD> Washington </TD> <TD> DC </TD> <TD align="right"> 1073163.70 </TD> <TD align="right">  48 </TD> </TR>
  <TR> <TD> Adventist Health System-Sunbelt Inc </TD> <TD> Orlando </TD> <TD> FL </TD> <TD align="right"> 1072732.95 </TD> <TD align="right"> 742 </TD> </TR>
  <TR> <TD> University Of California - San Francisco </TD> <TD> San Francisco </TD> <TD> CA </TD> <TD align="right"> 1008084.79 </TD> <TD align="right">  65 </TD> </TR>
   </TABLE>

List the physicians with more than $1M in total payments.


```r
docs <- D[grep("Physician", Covered_Recipient_Type),
          list(sumPayments = sum(Total_Amount_of_Payment_USDollars),
               nRecords = .N),
          list(Physician_Last_Name, Physician_First_Name, Recipient_City, Recipient_State)][order(sumPayments, decreasing=TRUE)]
niceTable(docs[sumPayments > 1E6])
```

<!-- html table generated in R 3.1.1 by xtable 1.7-3 package -->
<!-- Tue Sep 30 15:52:45 2014 -->
<TABLE border=1>
<TR> <TH> Physician_Last_Name </TH> <TH> Physician_First_Name </TH> <TH> Recipient_City </TH> <TH> Recipient_State </TH> <TH> sumPayments </TH> <TH> nRecords </TH>  </TR>
  <TR> <TD> BURKHART </TD> <TD> STEPHEN </TD> <TD> SAN ANTONIO </TD> <TD> TX </TD> <TD align="right"> 7356275.69 </TD> <TD align="right">  52 </TD> </TR>
  <TR> <TD> RANAWAT </TD> <TD> CHITRANJAN </TD> <TD> NEW YORK </TD> <TD> NY </TD> <TD align="right"> 3994021.73 </TD> <TD align="right">  19 </TD> </TR>
  <TR> <TD> THORNHILL </TD> <TD> THOMAS </TD> <TD> BOSTON </TD> <TD> MA </TD> <TD align="right"> 3921409.78 </TD> <TD align="right">  18 </TD> </TR>
  <TR> <TD> SCOTT </TD> <TD> RICHARD </TD> <TD> BOSTON </TD> <TD> MA </TD> <TD align="right"> 3849711.15 </TD> <TD align="right">   3 </TD> </TR>
  <TR> <TD> ELATTRACHE </TD> <TD> NEAL </TD> <TD> LOS ANGELES </TD> <TD> CA </TD> <TD align="right"> 2413280.53 </TD> <TD align="right">  20 </TD> </TR>
  <TR> <TD> CHUTER </TD> <TD> TIMOTHY </TD> <TD> SAN FRANCISCO </TD> <TD> CA </TD> <TD align="right"> 2304899.19 </TD> <TD align="right">   4 </TD> </TR>
  <TR> <TD> Lynn </TD> <TD> Lawrence </TD> <TD> Westerville </TD> <TD> OH </TD> <TD align="right"> 2149690.00 </TD> <TD align="right">   2 </TD> </TR>
  <TR> <TD> HAAS </TD> <TD> STEVEN </TD> <TD> NEW YORK </TD> <TD> NY </TD> <TD align="right"> 1752797.06 </TD> <TD align="right">  57 </TD> </TR>
  <TR> <TD> FORDTRAN </TD> <TD> JOHN </TD> <TD> DALLAS </TD> <TD> TX </TD> <TD align="right"> 1715554.05 </TD> <TD align="right">  15 </TD> </TR>
  <TR> <TD> JONES </TD> <TD> RICHARD </TD> <TD> DALLAS </TD> <TD> TX </TD> <TD align="right"> 1457516.72 </TD> <TD align="right">  27 </TD> </TR>
  <TR> <TD> RIES </TD> <TD> MICHAEL </TD> <TD> CARSON CITY </TD> <TD> NV </TD> <TD align="right"> 1185802.86 </TD> <TD align="right">  94 </TD> </TR>
  <TR> <TD> HAID </TD> <TD> REGIS </TD> <TD> Atlanta </TD> <TD> GA </TD> <TD align="right"> 1171245.75 </TD> <TD align="right">  11 </TD> </TR>
  <TR> <TD> RANAWAT </TD> <TD> AMAR </TD> <TD> NEW YORK </TD> <TD> NY </TD> <TD align="right"> 1154597.24 </TD> <TD align="right">  29 </TD> </TR>
  <TR> <TD> PADGETT </TD> <TD> DOUGLAS </TD> <TD> New York </TD> <TD> NY </TD> <TD align="right"> 1139670.16 </TD> <TD align="right">   7 </TD> </TR>
  <TR> <TD> LAVERNIA </TD> <TD> CARLOS </TD> <TD> MIAMI </TD> <TD> FL </TD> <TD align="right"> 1116854.00 </TD> <TD align="right">   6 </TD> </TR>
  <TR> <TD> JACKSON </TD> <TD> ROGER </TD> <TD> NORTH KANSAS CITY </TD> <TD> MO </TD> <TD align="right"> 1020748.68 </TD> <TD align="right">   2 </TD> </TR>
  <TR> <TD> SANDERS </TD> <TD> ROY </TD> <TD> TEMPLE TERRACE </TD> <TD> FL </TD> <TD align="right"> 1019519.16 </TD> <TD align="right">  49 </TD> </TR>
  <TR> <TD> RUSSELL </TD> <TD> THOMAS </TD> <TD> GERMANTOWN </TD> <TD> TN </TD> <TD align="right"> 1016680.40 </TD> <TD align="right">  24 </TD> </TR>
   </TABLE>

List the payments to OHSU.


```r
ohsu <- D[grep("Oregon Health & Science University", Teaching_Hospital_Name),
          list(sumPayments = sum(Total_Amount_of_Payment_USDollars),
               nRecords = .N),
          list(Teaching_Hospital_Name)]
niceTable(ohsu)
```

<!-- html table generated in R 3.1.1 by xtable 1.7-3 package -->
<!-- Tue Sep 30 15:52:45 2014 -->
<TABLE border=1>
<TR> <TH> Teaching_Hospital_Name </TH> <TH> sumPayments </TH> <TH> nRecords </TH>  </TR>
  <TR> <TD> Oregon Health &amp; Science University </TD> <TD align="right"> 321818.99 </TD> <TD align="right">  67 </TD> </TR>
   </TABLE>


# Research Payments

**Research Payments are defined as payments or other transfers of value made in connection with a research agreement or research protocol.**

Read the Research Payments file into a data table.
See *Appendix B: Research Payments Detail* in [OpenPaymentsDataDictionary.pdf](http://www.cms.gov/OpenPayments/Downloads/OpenPaymentsDataDictionary.pdf).


```r
f <- filenames$Name[grep("RSRCH", filenames$Name)]
f <- paste0(tempdir(), "\\", f)
require(data.table)
colNames <- names(fread(f, nrows=0))
classes <- list(character=1:length(colNames))
D <- fread(f,
           colClasses=classes,
           na.strings="",
           stringsAsFactors=FALSE)
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
             Total_Amount_of_Payment_USDollars = as.numeric(Total_Amount_of_Payment_USDollars))]
```

List the principal investigators with more than $1M in total payments.


```r
pis <- D[,
         list(sumPayments = sum(Total_Amount_of_Payment_USDollars),
              nRecords = .N),
         list(Principal_Investigator_Name1, Teaching_Hospital_Name, Recipient_City, Recipient_State)][order(sumPayments, decreasing=TRUE)]
niceTable(pis[sumPayments > 1E6])
```

<!-- html table generated in R 3.1.1 by xtable 1.7-3 package -->
<!-- Tue Sep 30 15:52:46 2014 -->
<TABLE border=1>
<TR> <TH> Principal_Investigator_Name1 </TH> <TH> Teaching_Hospital_Name </TH> <TH> Recipient_City </TH> <TH> Recipient_State </TH> <TH> sumPayments </TH> <TH> nRecords </TH>  </TR>
  <TR> <TD> Frank Stephen Hodi </TD> <TD> Dana Farber Cancer Institute </TD> <TD> Boston </TD> <TD> MA </TD> <TD align="right"> 6205489.59 </TD> <TD align="right">  11 </TD> </TR>
  <TR> <TD> NAVAL GUSTAD DAVER </TD> <TD> University Of Texas M. D. Anderson Cancer Center </TD> <TD> Houston </TD> <TD> TX </TD> <TD align="right"> 4081920.15 </TD> <TD align="right">   1 </TD> </TR>
  <TR> <TD>  </TD> <TD>  </TD> <TD> ALBUQUERQUE </TD> <TD> NM </TD> <TD align="right"> 2894767.24 </TD> <TD align="right">   5 </TD> </TR>
  <TR> <TD> DAVID SANGHYUN HONG </TD> <TD> University Of Texas M. D. Anderson Cancer Center </TD> <TD> Houston </TD> <TD> TX </TD> <TD align="right"> 1685026.70 </TD> <TD align="right">   2 </TD> </TR>
  <TR> <TD>  </TD> <TD>  </TD> <TD> HOUSTON </TD> <TD> TX </TD> <TD align="right"> 1678185.80 </TD> <TD align="right"> 125 </TD> </TR>
  <TR> <TD>  </TD> <TD> University Of Texas M. D. Anderson Cancer Center </TD> <TD> Houston </TD> <TD> TX </TD> <TD align="right"> 1536702.70 </TD> <TD align="right">  18 </TD> </TR>
  <TR> <TD>  </TD> <TD>  </TD> <TD> BOSTON </TD> <TD> MA </TD> <TD align="right"> 1506153.48 </TD> <TD align="right"> 189 </TD> </TR>
  <TR> <TD> PAUL G RICHARDSON </TD> <TD> Dana Farber Cancer Institute </TD> <TD> Boston </TD> <TD> MA </TD> <TD align="right"> 1476737.07 </TD> <TD align="right">  15 </TD> </TR>
  <TR> <TD>  </TD> <TD>  </TD> <TD> NASHVILLE </TD> <TD> TN </TD> <TD align="right"> 1352530.71 </TD> <TD align="right">  99 </TD> </TR>
  <TR> <TD> RUBEN A MESA </TD> <TD> Mayo Clinic Arizona </TD> <TD> Phoenix </TD> <TD> AZ </TD> <TD align="right"> 1259878.65 </TD> <TD align="right">   3 </TD> </TR>
  <TR> <TD> Rodney Pommier </TD> <TD> Oregon Health &amp; Science University </TD> <TD> PORTLAND </TD> <TD> OR </TD> <TD align="right"> 1064015.49 </TD> <TD align="right">   1 </TD> </TR>
  <TR> <TD>  </TD> <TD>  </TD> <TD> DALLAS </TD> <TD> TX </TD> <TD align="right"> 1020290.19 </TD> <TD align="right"> 100 </TD> </TR>
  <TR> <TD> Matthew S Davids </TD> <TD> Dana Farber Cancer Institute </TD> <TD> Boston </TD> <TD> MA </TD> <TD align="right"> 1004153.16 </TD> <TD align="right">   2 </TD> </TR>
  <TR> <TD> Julie N. Graff </TD> <TD> Oregon Health &amp; Science University </TD> <TD> Portland </TD> <TD> OR </TD> <TD align="right"> 1003851.20 </TD> <TD align="right">   1 </TD> </TR>
  <TR> <TD> Svetomir N. Markovic </TD> <TD> Mayo Clinic-Methodist Hospital </TD> <TD> Rochester </TD> <TD> MN </TD> <TD align="right"> 1003851.20 </TD> <TD align="right">   1 </TD> </TR>
   </TABLE>

List the payments to OHSU.


```r
ohsu <- D[grep("Oregon Health & Science University", Teaching_Hospital_Name),
          list(sumPayments = sum(Total_Amount_of_Payment_USDollars),
               nRecords = .N),
          list(Principal_Investigator_Name1, Teaching_Hospital_Name)][order(sumPayments, decreasing=TRUE)]
niceTable(ohsu)
```

<!-- html table generated in R 3.1.1 by xtable 1.7-3 package -->
<!-- Tue Sep 30 15:52:46 2014 -->
<TABLE border=1>
<TR> <TH> Principal_Investigator_Name1 </TH> <TH> Teaching_Hospital_Name </TH> <TH> sumPayments </TH> <TH> nRecords </TH>  </TR>
  <TR> <TD> Rodney Pommier </TD> <TD> Oregon Health &amp; Science University </TD> <TD align="right"> 1064015.49 </TD> <TD align="right">   1 </TD> </TR>
  <TR> <TD> Julie N. Graff </TD> <TD> Oregon Health &amp; Science University </TD> <TD align="right"> 1003851.20 </TD> <TD align="right">   1 </TD> </TR>
  <TR> <TD>  </TD> <TD> Oregon Health &amp; Science University </TD> <TD align="right"> 629804.38 </TD> <TD align="right">  11 </TD> </TR>
  <TR> <TD> KIM-HIEN THI DAO </TD> <TD> Oregon Health &amp; Science University </TD> <TD align="right"> 428854.50 </TD> <TD align="right">   2 </TD> </TR>
  <TR> <TD> MICHAEL CHARLES HEINRICH </TD> <TD> Oregon Health &amp; Science University </TD> <TD align="right"> 297670.73 </TD> <TD align="right">  16 </TD> </TR>
  <TR> <TD> ANDY I CHEN </TD> <TD> Oregon Health &amp; Science University </TD> <TD align="right"> 65891.81 </TD> <TD align="right">  11 </TD> </TR>
  <TR> <TD> Jeffrey Lawrence Saver </TD> <TD> Oregon Health &amp; Science University </TD> <TD align="right"> 47875.00 </TD> <TD align="right">   3 </TD> </TR>
  <TR> <TD> JOSEPH FRANCIS QUINN </TD> <TD> Oregon Health &amp; Science University </TD> <TD align="right"> 46767.00 </TD> <TD align="right">   4 </TD> </TR>
  <TR> <TD> Abigail Hata </TD> <TD> Oregon Health &amp; Science University </TD> <TD align="right"> 43056.00 </TD> <TD align="right">   2 </TD> </TR>
  <TR> <TD> MARTIN CRAIG SALINSKY </TD> <TD> Oregon Health &amp; Science University </TD> <TD align="right"> 39310.80 </TD> <TD align="right">   2 </TD> </TR>
  <TR> <TD> MICHAEL RECHT </TD> <TD> Oregon Health &amp; Science University </TD> <TD align="right"> 36286.55 </TD> <TD align="right">  10 </TD> </TR>
  <TR> <TD> PAUL G RICHARDSON </TD> <TD> Oregon Health &amp; Science University </TD> <TD align="right"> 30880.00 </TD> <TD align="right">   1 </TD> </TR>
  <TR> <TD> PAUL BARTON DUELL </TD> <TD> Oregon Health &amp; Science University </TD> <TD align="right"> 30152.96 </TD> <TD align="right">  10 </TD> </TR>
  <TR> <TD> CHRISTOPHER WALTER RYAN </TD> <TD> Oregon Health &amp; Science University </TD> <TD align="right"> 28743.72 </TD> <TD align="right">   6 </TD> </TR>
  <TR> <TD> ERIC BARTON SUHLER </TD> <TD> Oregon Health &amp; Science University </TD> <TD align="right"> 27476.33 </TD> <TD align="right">   6 </TD> </TR>
  <TR> <TD> Robert  David Steiner </TD> <TD> Oregon Health &amp; Science University </TD> <TD align="right"> 25705.00 </TD> <TD align="right">   1 </TD> </TR>
  <TR> <TD> MICHAEL JOHN MAURO </TD> <TD> Oregon Health &amp; Science University </TD> <TD align="right"> 22901.65 </TD> <TD align="right">   9 </TD> </TR>
  <TR> <TD> MICHAEL FEKRI AZIZ </TD> <TD> Oregon Health &amp; Science University </TD> <TD align="right"> 21030.00 </TD> <TD align="right">   2 </TD> </TR>
  <TR> <TD> STEPHEN E SPURGEON </TD> <TD> Oregon Health &amp; Science University </TD> <TD align="right"> 17896.04 </TD> <TD align="right">   1 </TD> </TR>
  <TR> <TD> JONATHAN QUENTIN PURNELL </TD> <TD> Oregon Health &amp; Science University </TD> <TD align="right"> 17453.00 </TD> <TD align="right">   4 </TD> </TR>
  <TR> <TD> Martin Schreiber </TD> <TD> Oregon Health &amp; Science University </TD> <TD align="right"> 16500.00 </TD> <TD align="right">   3 </TD> </TR>
  <TR> <TD> BRETT RUSSELL STACEY </TD> <TD> Oregon Health &amp; Science University </TD> <TD align="right"> 14465.41 </TD> <TD align="right">   5 </TD> </TR>
  <TR> <TD> ATULYA ACHYUT DEODHAR </TD> <TD> Oregon Health &amp; Science University </TD> <TD align="right"> 14263.20 </TD> <TD align="right">   4 </TD> </TR>
  <TR> <TD> STEPHEN YUN-CHI CHUI </TD> <TD> Oregon Health &amp; Science University </TD> <TD align="right"> 13695.00 </TD> <TD align="right">   7 </TD> </TR>
  <TR> <TD> TOMASZ M BEER </TD> <TD> Oregon Health &amp; Science University </TD> <TD align="right"> 12992.00 </TD> <TD align="right">   7 </TD> </TR>
  <TR> <TD> Jeffrey Saver </TD> <TD> Oregon Health &amp; Science University </TD> <TD align="right"> 12000.00 </TD> <TD align="right">   1 </TD> </TR>
  <TR> <TD> MICHAEL SHAPIRO </TD> <TD> Oregon Health &amp; Science University </TD> <TD align="right"> 11881.25 </TD> <TD align="right">   2 </TD> </TR>
  <TR> <TD> ANDREW JOSEPH AHMANN </TD> <TD> Oregon Health &amp; Science University </TD> <TD align="right"> 11562.52 </TD> <TD align="right">   2 </TD> </TR>
  <TR> <TD> CARY OWEN HARDING </TD> <TD> Oregon Health &amp; Science University </TD> <TD align="right"> 10123.75 </TD> <TD align="right">   2 </TD> </TR>
  <TR> <TD> RAHEL NARDOS </TD> <TD> Oregon Health &amp; Science University </TD> <TD align="right"> 9312.50 </TD> <TD align="right">   1 </TD> </TR>
  <TR> <TD> ALAN SANDLER </TD> <TD> Oregon Health &amp; Science University </TD> <TD align="right"> 8750.00 </TD> <TD align="right">   1 </TD> </TR>
  <TR> <TD> Wayne Marston Clark </TD> <TD> Oregon Health &amp; Science University </TD> <TD align="right"> 8320.00 </TD> <TD align="right">   1 </TD> </TR>
  <TR> <TD> MICHAEL J. MAURO </TD> <TD> Oregon Health &amp; Science University </TD> <TD align="right"> 8025.00 </TD> <TD align="right">   1 </TD> </TR>
  <TR> <TD> SAURABH GUPTA </TD> <TD> Oregon Health &amp; Science University </TD> <TD align="right"> 7925.00 </TD> <TD align="right">   1 </TD> </TR>
  <TR> <TD> CHARLES ALAN HENRIKSON </TD> <TD> Oregon Health &amp; Science University </TD> <TD align="right"> 6407.25 </TD> <TD align="right">   1 </TD> </TR>
  <TR> <TD> David Martin Koeller </TD> <TD> Oregon Health &amp; Science University </TD> <TD align="right"> 5420.00 </TD> <TD align="right">   6 </TD> </TR>
  <TR> <TD> ALAN BART SANDLER </TD> <TD> Oregon Health &amp; Science University </TD> <TD align="right"> 3802.50 </TD> <TD align="right">   5 </TD> </TR>
  <TR> <TD> Andrew Ahmann </TD> <TD> Oregon Health &amp; Science University </TD> <TD align="right"> 3591.80 </TD> <TD align="right">   1 </TD> </TR>
  <TR> <TD> WAYNE MARSTON CLARK </TD> <TD> Oregon Health &amp; Science University </TD> <TD align="right"> 3520.00 </TD> <TD align="right">   1 </TD> </TR>
  <TR> <TD> KEVIN W.H. YEE </TD> <TD> Oregon Health &amp; Science University </TD> <TD align="right"> 2820.00 </TD> <TD align="right">   2 </TD> </TR>
  <TR> <TD> JOSHI J ALUMKAL </TD> <TD> Oregon Health &amp; Science University </TD> <TD align="right"> 2620.15 </TD> <TD align="right">   1 </TD> </TR>
  <TR> <TD> ERIC SIMPSON </TD> <TD> Oregon Health &amp; Science University </TD> <TD align="right"> 2512.00 </TD> <TD align="right">   3 </TD> </TR>
  <TR> <TD> ANDREW TZONG-YOW CHEN </TD> <TD> Oregon Health &amp; Science University </TD> <TD align="right"> 2510.00 </TD> <TD align="right">   2 </TD> </TR>
  <TR> <TD> EMMA C SCOTT </TD> <TD> Oregon Health &amp; Science University </TD> <TD align="right"> 2290.69 </TD> <TD align="right">  51 </TD> </TR>
  <TR> <TD> JAMES MUDD </TD> <TD> Oregon Health &amp; Science University </TD> <TD align="right"> 2170.00 </TD> <TD align="right">   2 </TD> </TR>
  <TR> <TD> MATTHEW HIRAM TAYLOR </TD> <TD> Oregon Health &amp; Science University </TD> <TD align="right"> 2075.00 </TD> <TD align="right">   2 </TD> </TR>
  <TR> <TD> ROBERT BRUCE STEINER </TD> <TD> Oregon Health &amp; Science University </TD> <TD align="right"> 1650.00 </TD> <TD align="right">   3 </TD> </TR>
  <TR> <TD> ARIEL LOPEZ CHAVEZ </TD> <TD> Oregon Health &amp; Science University </TD> <TD align="right"> 1549.50 </TD> <TD align="right">   3 </TD> </TR>
  <TR> <TD> JACQUELINE VUKY </TD> <TD> Oregon Health &amp; Science University </TD> <TD align="right"> 1320.00 </TD> <TD align="right">   1 </TD> </TR>
  <TR> <TD> KEVIN CHOONG JI YUEN </TD> <TD> Oregon Health &amp; Science University </TD> <TD align="right"> 1200.00 </TD> <TD align="right">   1 </TD> </TR>
  <TR> <TD> BENJAMIN DAVID EHST </TD> <TD> Oregon Health &amp; Science University </TD> <TD align="right"> 872.81 </TD> <TD align="right">   1 </TD> </TR>
  <TR> <TD> David Forrest Kallmes </TD> <TD> Oregon Health &amp; Science University </TD> <TD align="right"> 625.00 </TD> <TD align="right">   1 </TD> </TR>
  <TR> <TD> Christopher Lee Amling </TD> <TD> Oregon Health &amp; Science University </TD> <TD align="right"> 625.00 </TD> <TD align="right">   1 </TD> </TR>
  <TR> <TD> STEPHEN Y CHUI </TD> <TD> Oregon Health &amp; Science University </TD> <TD align="right"> 563.00 </TD> <TD align="right">   1 </TD> </TR>
  <TR> <TD> CHARLES A HENRIKSON </TD> <TD> Oregon Health &amp; Science University </TD> <TD align="right"> 480.00 </TD> <TD align="right">   1 </TD> </TR>
  <TR> <TD> JAMES CHRISTOPHER AUSTIN </TD> <TD> Oregon Health &amp; Science University </TD> <TD align="right"> 468.75 </TD> <TD align="right">   1 </TD> </TR>
  <TR> <TD> ANGELA GOFFREDO FLEISCHMAN </TD> <TD> Oregon Health &amp; Science University </TD> <TD align="right"> 300.00 </TD> <TD align="right">   1 </TD> </TR>
  <TR> <TD> JULIE NICOLE GRAFF </TD> <TD> Oregon Health &amp; Science University </TD> <TD align="right"> 41.85 </TD> <TD align="right">   2 </TD> </TR>
   </TABLE>
