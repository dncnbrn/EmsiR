# Introduction to EmsiR: how to query the Episteme API from within R
Dunncan Brown, Emsi UK  
`r Sys.Date()`  

The Emsi Episteme API gathers together a wide range of labour market intelligence datasets, allowing economists and labour market analysts unrivalled resources to explore past, current and projected future employment across the USA, UK, Australia, Canada, Brazil and France. The `EmsiR` package has been produced to facilitate calls to the Episteme API from the R programming language, allowing R users to more quickly identify, specify and implement data pulls to inform their analysis. In this vignette, I take you through a few simple examples to demonstrate how the package cuts down the process of querying Episteme.

The package is simply a wrapper for a workflow which is primarily built around `tidyverse` packages: especially `httr` but with the support of `dplyr`, `tidyr` and `purrr` to wrangle the data into usable form. Querying Episteme can be tricky as there are quite a few potential parameters which need to be set correctly; this package aims to simplify that process.

## Setting up on Episteme

The Episteme API is a proprietary system available only to Emsi and its customers. For that reason, queries require authentication, and to do that we use `EpistemeLogin`:




```r
library(EmsiR)

EpistemeLogin("YourUsername",
              "YourPasscode")
```

Credentials are stored as environment variables, and will be used every time you query Episteme.

## Listing the datasets available on Episteme

We might want to check out what's available, so first we get a listing of the datasets currently hosted on Episteme:


```r
dataset_list()
```

```
## Source: local data frame [188 x 4]
## Groups: Country, Content [69]
## 
##    Version Country              Content                     Identifier
##      <chr>   <chr>                <chr>                          <chr>
## 1        1      AU InternetVacancyIndex EMSI.AU.InternetVacancyIndex/1
## 2   2014.2      AU             Industry        EMSI.AU.Industry/2014.2
## 3   2016.2      AU             Industry        EMSI.AU.Industry/2016.2
## 4   2014.2      AU           Occupation      EMSI.AU.Occupation/2014.2
## 5   2016.2      AU           Occupation      EMSI.AU.Occupation/2016.2
## 6   2014.2      AU             Staffing        EMSI.AU.Staffing/2014.2
## 7   2016.2      AU             Staffing        EMSI.AU.Staffing/2016.2
## 8   2014.1      BR         Institutions    EMSI.BR.Institutions/2014.1
## 9   2013.3      BR         Institutions    EMSI.BR.Institutions/2013.3
## 10  2014.1      BR           Occupation      EMSI.BR.Occupation/2014.1
## # ... with 178 more rows
```

And we can filter this using `dplyr`:


```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
dataset_list() %>% filter(Country=="UK")
```

```
## Source: local data frame [23 x 4]
## Groups: Country, Content [8]
## 
##    Version Country              Content
##      <chr>   <chr>                <chr>
## 1   2013.1      UK             Staffing
## 2   2014.1      UK             Staffing
## 3   2015.1      UK             Staffing
## 4   2016.1      UK             Staffing
## 5   2013.1      UK             Industry
## 6   2014.1      UK             Industry
## 7   2015.1      UK             Industry
## 8   2016.1      UK             Industry
## 9        2      UK       ClaimantCounts
## 10  2015.1      UK Occupation.AgeGender
## # ... with 13 more rows, and 1 more variables: Identifier <chr>
```

## Exploring an individual dataset

To specify a query, we're going to need to understand what metrics are available, but also what dimensions we need to constrain in order to make sense of the data, and to do that we use `dataset_detail()`:


```r
dataset_detail("UK", "Occupation", "2016.1")
```

```
## $Metrics
## $Metrics$value
##  [1] "Earnings.Median.Annual"       "Earnings.Median.Annual"      
##  [3] "Earnings.Percentile10"        "Earnings.Percentile10.Annual"
##  [5] "Earnings.Percentile25"        "Earnings.Percentile25.Annual"
##  [7] "Earnings.Percentile50"        "Earnings.Percentile50.Annual"
##  [9] "Earnings.Percentile75"        "Earnings.Percentile75.Annual"
## [11] "Earnings.Percentile90"        "Earnings.Percentile90.Annual"
## [13] "Jobs.2003"                    "Jobs.2004"                   
## [15] "Jobs.2005"                    "Jobs.2006"                   
## [17] "Jobs.2007"                    "Jobs.2008"                   
## [19] "Jobs.2009"                    "Jobs.2010"                   
## [21] "Jobs.2011"                    "Jobs.2012"                   
## [23] "Jobs.2013"                    "Jobs.2014"                   
## [25] "Jobs.2015"                    "Jobs.2016"                   
## [27] "Jobs.2017"                    "Jobs.2018"                   
## [29] "Jobs.2019"                    "Jobs.2020"                   
## [31] "Jobs.2021"                    "Jobs.2022"                   
## [33] "Replacements"                
## 
## 
## $Dimensions
## $Dimensions$value
## [1] "ClassOfWorker" "Occupation"    "Area"
```

## First query

We're going to keep this simple - we want every occupation, for two geographic areas England (`ENG`) and Wales (`WAL`), with employment in 2016 and projected to 2022.


```r
metSimple <- metricmaker(data.frame(name=c("Jobs.2016","Jobs.2022"),
                         as=c("Employment in 2016","Employment in 2022")))

dimArea <- dimmaker("Area", data.frame(name=c("England", "Wales"),
                                       code=c("ENG", "WAL")))
```

To get every occupation, instead of providing codes, we simply say `"asIdentity"`:


```r
dimOcc <- dimmaker("Occupation", "asIdentity")
```

Under the hood, the `dimmaker()` function is preparing the names and codes so that they'll be neat and tidy for JSON querying:


```r
jsonlite::toJSON(dimArea)
```

```
## {"dimensionName":["Area"],"map":{"England":[["ENG"]],"Wales":[["WAL"]]}}
```

There's one other dimension we've not covered for our dataset - `ClassOfWorker`. Because there are only four options available (Employees or Proprietors, both combined, or both separately), there is a shorthand function ready for this called `CoW()` which simply requires a code. In this place, we're going to have *all* workers, and so it'll become `CoW("A")` - but we can add that when we make our data pull query.

## Pulling the data

We use `datapull()` to make the query, and it combines three main elements: the details of the dataset (country, content, release), then the constraints as a list (in this case, `Occupation`, `Area` and `ClassOfWorker`), and then the metrics we've specified. Like this:


```r
occs <- datapull("UK", "Occupation", "2016.1", constraints = list(dimArea, dimOcc, CoW("A")), metSimple)
```

```
## No encoding supplied: defaulting to UTF-8.
```

```r
occs
```

```
## # A tibble: 734 × 5
##       Area Occupation ClassOfWorker `Employment in 2016`
##     <fctr>     <fctr>        <fctr>                <dbl>
## 1  England       1115           All            61566.014
## 2  England       1116           All             7063.330
## 3  England       1121           All           213487.406
## 4  England       1122           All           108821.161
## 5  England       1123           All             8641.531
## 6  England       1131           All           224906.330
## 7  England       1132           All           183074.574
## 8  England       1133           All            44981.853
## 9  England       1134           All            20785.579
## 10 England       1135           All           138162.919
## # ... with 724 more rows, and 1 more variables: `Employment in 2022` <dbl>
```

The data arrives as a data.frame with each dimension formed as a factor and each metric as a double, ready for analysis.

## Adding derivative metrics and hybrid dimensions

Much of the analysis performed using the Episteme API involves more specific requirements -- for example, looking at hybrid groups of sectors or industries, or adding in derivative metrics such as Location Quotients or Shift-Share analyses. `EmsiR` can accommodate these requirements.

### Derivative metrics

This time, we're going to have three metrics: employment in 2016, Location Quotients in 2016, and Shift-Share analysis comparing 2016 to 2013. As might be expected, specification becomes a little trickier, but not too much.

The start of the task is the same: specifying the underlying metrics in each case (it's the same: `Jobs.2016`) and then giving each one a label (`as` -- remember that Shift-Shares have several columns, so it's good to be brief). Next, we add a `metrics` column which stipulates if they're `"LQ"` for location quotient or `"SS"` for shift-share, or `NA` otherwise. Finally, if we have a shift-share to perform, we need to give it a base metric -- in this case `Jobs.2003` -- or specify `NA` for the othre metrics.


```r
met2 <- data.frame(name=c("Jobs.2016","Jobs.2016","Jobs.2016"),
                   as=c("Employment 2016","Location Quotient 2016","SS16"),
                   metrics=c(NA,"LQ","SS"),
                   base=c(NA,NA,"Jobs.2003"))
```

Then, we pass the data frame through the `metricmaker()` process to prepare it for a query. This time though, we have to add two other elements of information: we have to tell it the `geoparent` against which to compare the local analysis and also the domain `along` which analysis is being performed.


```r
metComp <- metricmaker(met2, "GB", "Occupation")
```


### Hybrid categories

Sometimes we want to merge several categories -- industries or occupations -- together to gain a different perspective. The `dimmaker()` function can easily be adapted to this. If passed a data frame where the same `name` is used across several different codes, it will nest them together for analysis:


```r
mgrs <- data.frame(name=c("CEOs",
                           rep("Group of managers A",3),
                           rep("Group of managers B",4)),
                   code=c("1115","1116","1121","1122","1123","1131","1132","1133"))
dimOcc2 <- dimmaker("Occupation", mgrs)

jsonlite::toJSON(dimOcc2)
```

```
## {"dimensionName":["Occupation"],"map":{"CEOs":[["1115"]],"Group of managers A":[["1116"],["1121"],["1122"]],"Group of managers B":[["1123"],["1131"],["1132"],["1133"]]}}
```

### Pulling the data

Then, the process is the same:


```r
occs <- datapull("UK", "Occupation", "2016.1", constraints = list(dimArea, dimOcc2, CoW("A")), metComp)
```

```
## No encoding supplied: defaulting to UTF-8.
## No encoding supplied: defaulting to UTF-8.
```

```
## Joining, by = c("Area", "Occupation", "ClassOfWorker")
```

```r
occs
```

```
## # A tibble: 6 × 9
##      Area          Occupation ClassOfWorker `Employment 2016`
##    <fctr>              <fctr>        <fctr>             <dbl>
## 1 England                CEOs           All         61566.014
## 2 England Group of managers A           All        329371.897
## 3 England Group of managers B           All        461604.289
## 4   Wales                CEOs           All          1958.291
## 5   Wales Group of managers A           All         17516.478
## 6   Wales Group of managers B           All         13982.093
## # ... with 5 more variables: `Location Quotient 2016` <dbl>, `SS16 Job
## #   Change` <dbl>, `SS16 Parent Growth Effect` <dbl>, `SS16 Mix
## #   Effect` <dbl>, `SS16 Competitive Effect` <dbl>
```
