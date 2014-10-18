Peer Assessment 1 - Activity Analysis
========================================================

## Introduction

It is now possible to collect a large amount of data about personal movement 
using activity monitoring devices such as a Fitbit, Nike Fuelband, or 
Jawbone Up. These type of devices are part of the “quantified self” movement – a
group of enthusiasts who take measurements about themselves regularly to improve
their health, to find patterns in their behavior, or because they are tech 
geeks. But these data remain under-utilized both because the raw data are hard 
to obtain and there is a lack of statistical methods and software for processing
and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. 
This device collects data at 5 minute intervals through out the day. The data 
consists of two months of data from an anonymous individual collected during the
months of October and November, 2012 and include the number of steps taken in 5 
minute intervals each day.

## Data

The data for this assignment can be downloaded from the course web
site:

* Dataset: [Activity monitoring data]
(https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip) [52K]

The variables included in this dataset are:

* **steps**: Number of steps taking in a 5-minute interval (missing
    values are coded as `NA`)

* **date**: The date on which the measurement was taken in YYYY-MM-DD
    format

* **interval**: Identifier for the 5-minute interval in which
    measurement was taken




The dataset is stored in a comma-separated-value (CSV) file and there
are a total of 17,568 observations in this
dataset.


## Assignment

This assignment is described in multiple parts. 


### Loading and preprocessing the data

To load the dataset inside the workspace in R, the code below is needed:


```r
#check if the file was already extracted from zip file
if(!file.exists("activity.csv"))
  unzip("activity.zip")

#the dataset is called as "x"
x<-read.csv("activity.csv",header=T)
#to convert "character" date column to Date format
x$date<-as.Date(x$date)
```

In default, the language has to be "en-US", for this, we use the code:

```r
Sys.setlocale("LC_TIME", "en_US.utf8")
```

```
## [1] "en_US.utf8"
```

Note this setlocale is only for Linux OS, for another OS, see the [R-manual]
(https://stat.ethz.ch/R-manual/R-devel/library/base/html/locales.html)


### What is mean total number of steps taken per day?

To get the mean total number of steps taken per day, a loop is needed to sum all
the steps in each day. Look the code below:


```r
#a vector "a" is created to save the sum of each day, note that the sum() 
#function is setted with "na.rm=F". If instead this, we would use "T", all the
#missing values were rewritten as zero, causing a incorrect average result.
a<-as.numeric()
for (i in 1:length(unique(x$date)))
        {
        a<-c(a,sum(x$steps[x$date==unique(x$date)[i]],na.rm=F))
        }

hist(a,breaks=10)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

And the mean and median of total steps number per day is given by:



```r
mean(a,na.rm=T)
```

```
## [1] 10766.19
```

```r
median(a,na.rm=T)
```

```
## [1] 10765
```

```r
#here the missing values are ignored
```


### What is the average daily activity pattern?


```r
#a vector "b" is created to save the average of each day, note that the mean() 
#function is setted with "na.rm=T"
b<-as.numeric()

for (i in 1:length(unique(x$date)))
        {
        b<-c(b,mean(x$steps[x$date==unique(x$date)[i]],na.rm=T))
        }

#to replace NaN with 0
b[is.na(b)]<-0

#a plot is created
plot(b~unique(x$date),type="l",ylab="average number of steps", xlab="date")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

And the 5-minute interval, on average across all the days in the dataset, 
contains the maximum number of steps is:

```r
#to obtain the day with max number of steps
unique(x$date)[which(b==max(b))]
```

```
## [1] "2012-11-23"
```


### Imputing missing values

Note that there are a number of days/intervals where there are missing
values (coded as `NA`). The presence of missing days may introduce
bias into some calculations or summaries of the data.

The total number of missing values in the dataset is given by:

```r
length(x$steps[is.na(x$steps)])
```

```
## [1] 2304
```

And now, we can replace all missing values in the dataset with the average value

```r
#to preserve the original dataset, we copy it to another object, called
#"xfilled"
xfilled<-x

#and all missing values, we replace with the mean value of all 5-min interval
xfilled$steps[is.na(xfilled$steps)]<-mean(x$steps,na.rm=T)

#and now, we can create a new vector "d" which save the total steps number for 
#each day. The "na.rm=T" is unnecessary, because there are not missing values
d<-as.numeric()

for (i in 1:length(unique(xfilled$date)))
        {
        d<-c(d,sum(xfilled$steps[xfilled$date==unique(xfilled$date)[i]]))
        }

#we can create a new histogram with the total steps number in each day, and
#the missing values filled with the mean values.
hist(d,breaks=10)
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png) 


```r
mean(d)
```

```
## [1] 10766.19
```

```r
median(d)
```

```
## [1] 10766.19
```

Observe that the "mean(d)" is equal to "mean(a)" and the "median(d)" is 
different to "median(a)" and equal to "mean(d)".
That's occured because when we add average values to a set of values, the 
average value in the set is the same, but the median tends to approach to mean.


### Are there differences in activity patterns between weekdays and weekends?


```r
#a vector "e" is created to save the day type of each day
e<-as.character()
for (i in 1:length(unique(xfilled$date)))
        {
        if(weekdays(unique(xfilled$date)[i])=="Saturday"|
           weekdays(unique(xfilled$date)[i])=="Sunday")
                e<-c(e,"Weekend")
        else
                e<-c(e,"Weekday")
        }

#and now, the "e" is joined to the dates
e<-cbind(e,as.character(unique(xfilled$date)))

#a vector "f" is created to save the average of each day in "xfilled"
f<-as.numeric()

for (i in 1:length(unique(xfilled$date)))
        {
        f<-c(f,mean(xfilled$steps[xfilled$date==unique(xfilled$date)[i]]))
        }

#"e" is joined to "f"
f<-cbind(f,e)

#and a dataframe is created
df<-data.frame(f)
names(df)<-c("steps","daytype","date")
df$daytype<-as.factor(df$daytype)
df$date<-as.Date(df$date)

#here, two dataframes are created splitting between "Weekday" and "Weekend"
dataweekday<-subset(df,df$daytype==levels(df$daytype)[1])
dataweekday$steps<-as.numeric(dataweekday$steps)
dataweekend<-subset(df,df$daytype==levels(df$daytype)[2])
dataweekend$steps<-as.numeric(dataweekend$steps)

par(mfrow=c(2,1)) 
plot(dataweekday$date,dataweekday$steps,type="l",ylab="average number of steps",
     xlab="weekday")
plot(dataweekend$date,dataweekend$steps,type="l",ylab="average number of steps",
     xlab="weekend")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png) 

```r
#to reset the par(mfrow)
par(mfrow=c(1,1)) 
```

We can verify the average steps number for these two day types

```r
mean(dataweekday$steps)
```

```
## [1] 25.48889
```

```r
mean(dataweekend$steps)
```

```
## [1] 32.9375
```

The 5-minute interval average across all weekends is approximately 30% greater 
than the 5-minute interval average across all weekdays.


