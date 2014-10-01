# CMS Open Payments
Benjamin Chan (chanb@ohsu.edu)  

**Play around with the CMS [Open Payments](http://www.cms.gov/OpenPayments) data.**

2014-10-01

R version 3.1.1 (2014-07-10)


Run the script to read the data.
Hide the code and the output since it's under version control on [GitHub](https://github.com/benjamin-chan/OpenPayments).
Also, source a function library.


```r
source("getData.r")
source("library.r")
```


Map at the state level.


```r
DAgg <- D[,
          list(value = sum(Total_Amount_of_Payment_USDollars) / 1E6,
               region = Recipient_State),
          list(Recipient_State)]
require(choroplethr)
choroplethr(DAgg, 
            lod="state", 
            title="General + Research Payments")
```

![plot of chunk mapStates](./mapOpenPayments_files/figure-html/mapStates.png) 


Map at the zip code level.


```r
DAgg <- D[,
          list(value = sum(Total_Amount_of_Payment_USDollars) / 1E6,
               region = Recipient_Zip_Code),
          list(Recipient_Zip_Code)]
require(choroplethr)
choroplethr(DAgg, 
            lod="zip", 
            title="General + Research Payments")
```

![plot of chunk mapZipcodes](./mapOpenPayments_files/figure-html/mapZipcodes.png) 
