TS - Varicella Forecast
================
Benjamin BERNARD
28/10/2021

First we start to load necessary packages and dateset

``` r
library(forecast)
```

    ## Registered S3 method overwritten by 'quantmod':
    ##   method            from
    ##   as.zoo.data.frame zoo

``` r
library(ggplot2)
data=read.csv(file="http://eric.univ-lyon2.fr/~jjacques/Download/DataSet/varicelle.csv")
varicelle<-ts(data$x,start=c(1931,1),end=c(1972,6),freq=12)
plot(varicelle)
```

![](TS---Varicella-Forecasting_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

### Data Manipulation

This is the first step to understand the dataset we have.

1.  **Observation, seasonality, trend**

When looking to the plot, data looks seasonal. Seasonal means that there
is a correlation between the time and the value to analyse. In other
words, same even happen at the same period.

To visualise that we can plot a correlogram :

``` r
tmp=acf(varicelle, type="cor", plot=TRUE)
```

![](TS---Varicella-Forecasting_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
ptmp=pacf(varicelle, type="cor", plot=FALSE)
plot (ptmp)
```

![](TS---Varicella-Forecasting_files/figure-gfm/unnamed-chunk-2-2.png)<!-- -->

Correlogram shows the correlations. There are significant when it goes
above or below the blue dotted line. As the correlations are periodic
here (looks that the ACF is sinusoidal) we can conclude that the data
are correlated on a cyclic pattern and so are seasonal.

``` r
ggseasonplot(varicelle,year.labels= TRUE,year.labels.left=TRUE)
```

![](TS---Varicella-Forecasting_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

The seasonal plot above confirm the seasonality : we can easily see a
peak February and April and a dramatic decreasing with a minimum value
in September.

By computing the values year per year we can see better that on top of
the seasonality we have a decreasing trend : varicella number of cases
is decreasing over the time.

``` r
x=rep(0,41)
for (i in 0:40) x[i+1]<-sum(varicelle[(1+12*i):(12*(i+1))])
plot(x,type='l',xaxt='n',xlab='')
axis(1,at = 0:40,labels = 1931:1971)
```

![](TS---Varicella-Forecasting_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->
