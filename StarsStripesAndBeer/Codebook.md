# Stars, Stripes, and Beer Codebook
Jeff Weltman  
October 12, 2017  

### *For course project purposes, all R chunks were set to ECHO=TRUE. This would typically not be the case for presentation documents.*

## Project Description
W&W Analytics has been commissioned by *Stars, Stripes, and Beer Co.* - hereafter referred to as *SS&B* - to analyze a sample of the craft beer market in order to answer the following research questions:

* How many breweries are present in each state?
* What is the median alcohol content (ABV) for each state? What is the median international bitterness unit (IBU) for each state?
* Which state has the maximum alcoholic (ABV) beer? Which state has the most bitter (IBU) beer?
* Is there any apparent relationship between the bitterness of the beer and its alcoholic content?

## Study design and data processing
The study was designed to be a representative sample of craft breweries, both large and small, across all 50 states of the United States of America. The data were consolidated from disparate sources in to these two data files.

### Observational Study
As this is an observational study, we note that any conclusions drawn from the data can only be inferred as associative within the scope of this sample population. 

This research is therefore not intended to support such conclusions as, for example, a higher ABV *causes* a higher IBU. Instead the findings might indicate that evidence suggests *an association* between ABV and IBU.

### Collection of the raw data
*SS&B* provided two raw data files: **Beers.csv** and **Breweries.csv**. 

These data were collected from the public domain across a variety of sources, including *ratebeer.com* and the craft breweries' websites and social media. There are many incomplete records, due largely to the dependency on self-reported information; the tidying of the data will be addressed below.

## Creating the tidy datafile
Step 1. The raw data sets are read in:


```r
Beers <- "https://raw.githubusercontent.com/jeffweltman/StarsStripesAndBeer/master/Raw/Beers.csv" 
DFBeers <- repmis::source_data(Beers)
```

```
## Downloading data from: https://raw.githubusercontent.com/jeffweltman/StarsStripesAndBeer/master/Raw/Beers.csv
```

```
## SHA-1 hash of the downloaded data file is:
## d3e3e8f8e9cf27e0df038f47ccfcfc2dfccf4217
```

```r
Breweries <- "https://raw.githubusercontent.com/jeffweltman/StarsStripesAndBeer/master/Raw/Breweries.csv"
DFBreweries <- repmis::source_data(Breweries)
```

```
## Downloading data from: https://raw.githubusercontent.com/jeffweltman/StarsStripesAndBeer/master/Raw/Breweries.csv
```

```
## SHA-1 hash of the downloaded data file is:
## 4579c1fc92624c25cb2643d7e61c542972fdc7ab
```


Step 2. Column names are re-assigned to aid in merging and improve readability.


```r
colnames(DFBeers) <- c("BeerName","Beer_ID","ABV","IBU","Brewery_ID","Style","Ounces")
colnames(DFBreweries) <- c("Brewery_ID","BreweryName","City","State")
```


Step 3. Exploratory Data Analysis to look for NAs and outliers.


```r
colSums(is.na(DFBeers))                 # DFBeers has 1,005 observations with IBU of NA 
```

```
##   BeerName    Beer_ID        ABV        IBU Brewery_ID      Style 
##          0          0         62       1005          0          0 
##     Ounces 
##          0
```

```r
colSums(is.na(DFBreweries))             # DFBreweries has no NA
```

```
##  Brewery_ID BreweryName        City       State 
##           0           0           0           0
```

```r
summary(DFBreweries)
```

```
##    Brewery_ID    BreweryName            City              State          
##  Min.   :  1.0   Length:558         Length:558         Length:558        
##  1st Qu.:140.2   Class :character   Class :character   Class :character  
##  Median :279.5   Mode  :character   Mode  :character   Mode  :character  
##  Mean   :279.5                                                           
##  3rd Qu.:418.8                                                           
##  Max.   :558.0
```

```r
summary(DFBeers)
```

```
##    BeerName            Beer_ID            ABV               IBU        
##  Length:2410        Min.   :   1.0   Min.   :0.00100   Min.   :  4.00  
##  Class :character   1st Qu.: 808.2   1st Qu.:0.05000   1st Qu.: 21.00  
##  Mode  :character   Median :1453.5   Median :0.05600   Median : 35.00  
##                     Mean   :1431.1   Mean   :0.05977   Mean   : 42.71  
##                     3rd Qu.:2075.8   3rd Qu.:0.06700   3rd Qu.: 64.00  
##                     Max.   :2692.0   Max.   :0.12800   Max.   :138.00  
##                                      NA's   :62        NA's   :1005    
##    Brewery_ID       Style               Ounces     
##  Min.   :  1.0   Length:2410        Min.   : 8.40  
##  1st Qu.: 94.0   Class :character   1st Qu.:12.00  
##  Median :206.0   Mode  :character   Median :12.00  
##  Mean   :232.7                      Mean   :13.59  
##  3rd Qu.:367.0                      3rd Qu.:16.00  
##  Max.   :558.0                      Max.   :32.00  
## 
```

```r
sd(DFBeers$ABV)    # 0.0126
```

```
## [1] NA
```

```r
sd(DFBeers$IBU)    # 25.954
```

```
## [1] NA
```
We learned that all beers from South Dakota are missing IBU data, so we'll recode those after the merge to retain the rest of that data. Otherwise, removing the rows with those NAs would cause us to lose all data for the state - this would negatively impact the scope of the project (to analyze and present beer and brewery data from **all** states).


Step 4. We merge the data set to allow for subsetting and removal of NAs


```r
BrewsAndBreweries <- merge(x=DFBeers, y=DFBreweries, by="Brewery_ID", all=TRUE)
```
Note that we do leave **all=TRUE** to identify the null values and remove them below.


Step 5. Recoding South Dakota IBU to 0 in order to retain data for that state.


```r
BrewsAndBreweries$IBU <- ifelse(BrewsAndBreweries$State=="SD",0,BrewsAndBreweries$IBU) 
```


Step 6. Identification and removal of null values.


```r
colSums(is.na(BrewsAndBreweries))
```

```
##  Brewery_ID    BeerName     Beer_ID         ABV         IBU       Style 
##           0           0           0          62         998           0 
##      Ounces BreweryName        City       State 
##           0           0           0           0
```

```r
BrewsAndBreweries <- subset(BrewsAndBreweries, !is.na(IBU)) # Remove them
# Confirming that all nulls are removed
colSums(is.na(BrewsAndBreweries))
```

```
##  Brewery_ID    BeerName     Beer_ID         ABV         IBU       Style 
##           0           0           0           0           0           0 
##      Ounces BreweryName        City       State 
##           0           0           0           0
```

Step 7. Writing the tidy datafiles.


```r
TidyBeers <- BrewsAndBreweries[,c(1:7)] # subsetting only the original columns
TidyBreweries <- BrewsAndBreweries[,c(1,8:10)] # subsetting only the original columns
write.csv(TidyBeers,"TidyBeers.csv",row.names=FALSE)
write.csv(TidyBreweries,"TidyBreweries.csv",row.names=FALSE)
write.csv(BrewsAndBreweries,"BrewsAndBreweries.csv",row.names=FALSE) 
```


## Description of the variables in the TidyBeers.csv file
TidyBeers.csv contains 1,412 observations across 7 variables:

* *BreweryName* - The Foreign Key (FK) for linking the beers to their breweries. This is an integer which ranges from 1:547.
* *BeerName* - The advertised name of the beer. This is a string (class character).
* *Beer_ID* - This is a unique identifier for each beer. *Beer_ID* is an integer which ranges from 1:2692.
* *ABV* - This is the measurement of the **A**lcohol **B**y **V**olume of each beer, as advertised in compliance with United States law. This identifies what percentage of the beverage is alcoholic. In this dataset, ABV ranges from 0.027 to 0.125. This is a numeric variable.
* *IBU* - This is the measurement of the **I**nternational **B**itterness **U**nits of each beer. Generally, a beer with a higher IBU will be more bitter than a beer with a lower IBU. In this data set, IBU ranges from 0 to 138. For more information on the methods of measurement, visit http://methods.asbcnet.org/summaries/beer-23.aspx. This is a numeric variable.
* *Style* - This is a form of classification of beers. 17 of the 90 beers are classified in two different styles, e.g. "Milk / Sweet Stout", and many styles are variations upon one another, e.g. "Belgian Dark Ale" and "Belgian Strong Dark Ale." As mentioned above, there are 1,412 total beers in our tidy data set, categorized in to 90 styles. Two beers - OktoberFiesta and Kilt Lifter Scottish-Style Ale had no Style provided and were re-coded as "N/A". This is a string (character) variable.
* *Ounces* - This measures the quantity, in ounces, of each beer in the dataset. It ranges from 8.4 ounces to 32 ounces and is a numeric variable.

## Description of the variables in the TidyBreweries.csv file
TidyBreweries.csv contains 1,412 observations across 4 variables:

* *Brewery_ID* - The Primary Key (PK) for linking the beers to their breweries. This is an integer which ranges from 1:547.
* *BreweryName* - This is the name of the brewery. Each brewery name has a unique *Brewery_ID*. Of the 1,412 rows in this dataset, ther are 374 distinct brewery names. This is a string (character class) variable.
* *City* - This variable contains the name of the city in which the brewery is located. Of the 1,412 rows in this dataset, there are 282 distinct cities. This is a string (character class) variable.
* *State* - This variable contains the abbreviated name of the state in which the brewery is located, e.g. "OR" for Oregon. This variable has 51 values, as the District of Columbia is included. This is a string (character class) variable.

## Description of BrewsAndBreweries.csv, our merged datafile
BrewsAndBreweries.scsv contains 1,412 across 10 variables:

* *BreweryName* - The Foreign Key (FK) for linking the beers to their breweries. This is an integer which ranges from 1 to 547.
* *BeerName* - The advertised name of the beer. This is a string (class character).
* *Beer_ID* - This is a unique identifier for each beer. *Beer_ID* is an integer which ranges from 1 to 2,692.
* *ABV* - This is the measurement of the **A**lcohol **B**y **V**olume of each beer, as advertised in compliance with United States law. This identifies what percentage of the beverage is alcoholic. In this dataset, ABV ranges from 0.027 to 0.125. This is a numeric variable.
* *IBU* - This is the measurement of the **I**nternational **B**itterness **U**nits of each beer. Generally, a beer with a higher IBU will be more bitter than a beer with a lower IBU. In this data set, IBU ranges from 0 to 138. For more information on the methods of measurement, visit http://methods.asbcnet.org/summaries/beer-23.aspx. This is a numeric variable.
* *Style* - This is a form of classification of beers. 17 of the 90 beers are classified in two different styles, e.g. "Milk / Sweet Stout", and many styles are variations upon one another, e.g. "Belgian Dark Ale" and "Belgian Strong Dark Ale." As mentioned above, there are 1,412 total beers in our tidy data set, categorized in to 90 styles. Two beers - OktoberFiesta and Kilt Lifter Scottish-Style Ale had no Style provided and were re-coded as "N/A". This is a string (character) variable.
* *Ounces* - This measures the quantity, in fluid ounces, of each beer in the dataset. It ranges from 8.4 ounces to 32 ounces and is a numeric variable.
* *BreweryName* - This is the name of the brewery. Each brewery name has a unique *Brewery_ID*. Of the 1,412 rows in this dataset, there are 374 distinct brewery names. This is a string (character class) variable.
* *City* - This variable contains the name of the city in which the brewery is located. Of the 1,412 rows in this dataset, there are 282 distinctly-named cities. This is a string (character class) variable.
* *State* - This variable contains the abbreviated name of the state in which the brewery is located, e.g. "OR" for Oregon. This variable has 51 values, as the District of Columbia is included. This is a string (character class) variable.

For access to the data files, R code, and more, visit https://github.com/jeffweltman/StarsStripesAndBeer
