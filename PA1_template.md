---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

### Load Packages


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
library(ggplot2)
```

### Load data


```r
# Reading in CSV data using readr package and read_csv function

path <- file.path("activity", "activity.csv")

activity <- readr::read_csv(file = path)
```

```
## Parsed with column specification:
## cols(
##   steps = col_double(),
##   date = col_date(format = ""),
##   interval = col_double()
## )
```

<p>&nbsp;</p>
#### Examining file 


```r
str(activity)
```

```
## tibble [17,568 x 3] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
##  $ steps   : num [1:17568] NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date[1:17568], format: "2012-10-01" "2012-10-01" ...
##  $ interval: num [1:17568] 0 5 10 15 20 25 30 35 40 45 ...
##  - attr(*, "spec")=
##   .. cols(
##   ..   steps = col_double(),
##   ..   date = col_date(format = ""),
##   ..   interval = col_double()
##   .. )
```

<p>&nbsp;</p>
#### Creating new file grouped by days across the two months and removing NA for this part of assignment.


```r
Totalactivity <- activity %>% 
group_by(date) %>% 
  filter(!is.na(steps)) %>% 
  summarise(totalsteps=sum(steps))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

## What is mean total number of steps taken per day?

<p>&nbsp;</p>
* Histogram showing total number of steps taken each day


```r
Totalactivity %>% 
ggplot(mapping = aes(x = totalsteps)) +
  geom_histogram(fill = "blue")+
  labs(x ="Total numbers of steps in a day", y="Count")+
  ggtitle("History showing total number of steps taken each day")
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->


<p>&nbsp;</p>
* Mean and median total number of steps taken per day across the two month period is calculated below.

```r
Totalactivity %>% 
  summarise(meansteps = mean(totalsteps), mediansteps = median(totalsteps))
```

```
## # A tibble: 1 x 2
##   meansteps mediansteps
##       <dbl>       <dbl>
## 1    10766.       10765
```

The mean total number of steps per day was 10766 and the median total number of steps per day was 10765.

## What is the average daily activity pattern?

<p>&nbsp;</p>
* Time series plot of the 5-minute interval and the average number of steps taken.

```r
activity %>% 
  group_by(interval) %>% 
summarise(avgsteps = mean(steps, na.rm = TRUE)) %>% 
  ggplot(mapping = aes(x=interval, y=avgsteps))+
  geom_line(color="blue", size=0.7)+
  labs(x ="Interval", y="Average number of steps taken")+
  ggtitle("Plot showing Average Daily Activity Pattern")
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

The average daily activity pattern shows that this person recorded a significantly higher average number of steps during a particular point of the day, that is between the 750 and 1000 time interval. This could perhaps be due to some form of exercise during this time slot.

<p>&nbsp;</p>
* The 5-minute interval, on average across all the days in the dataset, containing the maximum number of steps.


```r
activity %>% 
  group_by(interval) %>% 
summarise(avgsteps = mean(steps, na.rm = TRUE)) %>% 
            arrange(desc(avgsteps))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```
## # A tibble: 288 x 2
##    interval avgsteps
##       <dbl>    <dbl>
##  1      835     206.
##  2      840     196.
##  3      850     183.
##  4      845     180.
##  5      830     177.
##  6      820     171.
##  7      855     167.
##  8      815     158.
##  9      825     155.
## 10      900     143.
## # ... with 278 more rows
```
From the above, the 5-minute interval on average across all the days in the dataset, containing the maximum number of steps is the 835 minute interval, consistent with time series plot above.

## Imputing missing values

<p>&nbsp;</p>
* Total number of missing values in the dataset


```r
summary(activity)
```

```
##      steps             date               interval     
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-31   Median :1177.5  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0  
##  NA's   :2304
```


The total number of rows with NAs are found in the `steps` field only and as seen as above consist of  2304 rows.

<p>&nbsp;</p>

* Strategy for filling in all of the missing values in the dataset. 


```r
imputedvalues<- activity %>%
  filter(!is.na(steps)) %>% 
  group_by(interval) %>% 
summarise(avgsteps = mean(steps)) 
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
head(imputedvalues)
```

```
## # A tibble: 6 x 2
##   interval avgsteps
##      <dbl>    <dbl>
## 1        0   1.72  
## 2        5   0.340 
## 3       10   0.132 
## 4       15   0.151 
## 5       20   0.0755
## 6       25   2.09
```
Assuming that the data are missing completely at random, mean imputation is the chosen method for filling in the missing values. The mean of each 5-minute intervals has been calculated above and NAs will be replaced by the corresponding mean.

<p>&nbsp;</p>

* New dataset that is equal to the original dataset but with the missing data filled in.


```r
activitynew <- left_join(x = activity, y = imputedvalues, by = "interval") %>% 
  mutate(steps = ifelse(is.na(steps),avgsteps,steps))
head(activitynew)
```

```
## # A tibble: 6 x 4
##    steps date       interval avgsteps
##    <dbl> <date>        <dbl>    <dbl>
## 1 1.72   2012-10-01        0   1.72  
## 2 0.340  2012-10-01        5   0.340 
## 3 0.132  2012-10-01       10   0.132 
## 4 0.151  2012-10-01       15   0.151 
## 5 0.0755 2012-10-01       20   0.0755
## 6 2.09   2012-10-01       25   2.09
```


<p>&nbsp;</p>
* Histogram showing total number of steps taken each day for new dataset with imputed missing values


```r
activitynew %>% 
  group_by(date) %>% 
  summarise(totalsteps=sum(steps)) %>% 
ggplot(mapping = aes(x = totalsteps)) +
  geom_histogram(fill = "blue")+
  labs(x ="Total numbers of steps in a day", y="Count")+
  ggtitle("History showing total number of steps taken each day")
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->


<p>&nbsp;</p>
* Mean and median total number of steps taken per day across the two month period is calculated below using new dataset with imputed missing values.


```r
activitynew %>% 
  group_by(date) %>% 
  summarise(totalsteps=sum(steps))%>% 
  summarise(meansteps = mean(totalsteps), mediansteps = median(totalsteps)) 
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```
## # A tibble: 1 x 2
##   meansteps mediansteps
##       <dbl>       <dbl>
## 1    10766.      10766.
```

The imputation of missing values using mean intervals does not appear to have much impact on the estimates for mean and median total number of daily steps. The mean total number of steps remained the same and the median number of steps only differ by one. It should be noted that the chosen method above is not very sophisticated and indeed perhaps another sophisticated method would probably have greater impact. Additionally, the method above assumes that the data is missing completely at random and if this assumption is false, biased estimates can result.   

## Are there differences in activity patterns between weekdays and weekends?

<p>&nbsp;</p>
* Creating new variable for days of the week using weekdays() function


```r
activitynew <- activitynew %>% 
mutate(Days = weekdays(date))
```

<p>&nbsp;</p>
* Creating new factor variable in the dataset with two levels 


```r
activitynew <- activitynew %>% 
  mutate(typeofday = case_when(Days %in% c( "Saturday","Sunday")~"weekend",TRUE ~ "weekday"))
```

<p>&nbsp;</p>
* Panel plot containing a time series plot of the
5-minute interval and the average number of steps taken. 


```r
activitynew %>% 
  group_by(typeofday, interval) %>% 
summarise(newavgsteps = mean(steps)) %>% 
    mutate(facet = factor(typeofday, levels = c("weekend", "weekday"))) %>%
   ggplot(mapping = aes(x = interval, y = newavgsteps))+
  geom_line(color="blue", size=0.7)+
  labs(x ="Interval", y="Average number of steps taken")+
  ggtitle("Plot showing Average Daily Activity Pattern by Weekend and Weekday") +
   theme_bw() +
   facet_wrap(~ facet, ncol = 1,) 
```

```
## `summarise()` regrouping output by 'typeofday' (override with `.groups` argument)
```

![](PA1_template_files/figure-html/unnamed-chunk-16-1.png)<!-- -->

Based on the above panel time series plot, this person recorded a higher average number of steps on weekdays compared to weekends during the earlier part of the day, particularly between the 750 and 1000 time interval. However, on weekends, during the later time slots, that is, after the 1000 interval mark, this person on average records a greater number of steps than on weekdays.
