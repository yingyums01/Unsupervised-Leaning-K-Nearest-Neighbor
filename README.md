# Unsupervised-Leaning-K-Nearest-Neighbor
K Nearest Neighbor in R
---
title: "Untitled"
author: "Doris Kuo"
date: "11/22/2019"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r}
library(tidyverse)
#class: for KNN
library(class)
```

## 1.Read file

```{r}
wbcd <- read_csv("https://s3.amazonaws.com/notredame.analytics.data/wisconsinbiopsydata.csv")
glimpse(wbcd)
head(wbcd)
```

## 2.organize data

```{r}
wbcd<-wbcd%>%
  select(-id)%>%
  mutate(diagnosis=factor(diagnosis,levels = c('B','M'),labels=c('Benign','Malignant')))
wbcd%>%
  pull(diagnosis)%>%
  table()%>%
  prop.table()*100
head(wbcd)
#round(prop.table(table(select(wbcd,diagnosis))) * 100, digits = 1)
```

## 3.scale the data and check

```{r}
summary(wbcd[c("radius_mean", "area_mean", "smoothness_mean")])
wbcd_z<-as.data.frame(scale(wbcd[-1]))
summary(wbcd)
summary(wbcd_z)
```

## 4.split the data

```{r}
#caTools: sample.split
#need to put Y in sample.split first
library(caTools)
set.seed(1234)
split_index = sample.split(wbcd$diagnosis,SplitRatio = 0.75)
train_X = subset(wbcd_z,split_index==TRUE)
train_Y = wbcd[split_index==TRUE,1]
test_X = subset(wbcd_z,split_index==FALSE)
test_Y = wbcd[split_index==FALSE,1]
train_Y=pull(train_Y,diagnosis)
test_Y=pull(test_Y,diagnosis)
```

## 5.train the model

```{r}
#label should be vector
Y_pred <-
  knn(
    train = train_X,
    test = test_X,
    cl = as.vector(train_Y),
    k=15
  )
Y_pred
```

## 6.Evaluate: confusion matrix

```{r}
eval_table <- table(test_Y,Y_pred)
eval_table
#diag(): Extract or replace the diagonal of a matrix, or construct a diagonal matrix.
diag(eval_table)
#observations we have
nrow(test_X)
#accuracy
sum(diag(eval_table))/nrow(test_X)
```

## 7.improve the model

```{r}
#(1) change the normalization method: min and max
normalize <- function(x){
  my_normal <- (x-min(x)) / (max(x)-min(x))
  return(my_normal)
}
wbcd_n <- as.data.frame(map(wbcd[-1],normalize))
summary(wbcd_n)
#try again
train_X_n = subset(wbcd_n,split_index==TRUE)
test_X_n = subset(wbcd_n,split_index==FALSE)
Y_predict_n <-
  knn(
    train = train_X_n,
    test= test_X_n,
    cl= train_Y,
    k= 15
  )
Y_predict_n
table(test_Y,Y_predict_n)
```

## 8.Decide appropriate k

```{r}
accuracy <- vector()
n=40
set.seed(1234)
for (i in 1:n){
  Y_predict_i <-
    knn(
      train = train_X_n,
      test= test_X_n,
      cl= train_Y,
      k= i
    )
  eval_table_i <- table(test_Y,Y_predict_i)
  accuracy[i] = sum(diag(eval_table_i))/nrow(test_X)
}
accuracy
```
