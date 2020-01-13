library(tidyverse)
#class: for KNN
library(class)

#1 Read file
wbcd <- read_csv("https://s3.amazonaws.com/notredame.analytics.data/wisconsinbiopsydata.csv")
glimpse(wbcd)
head(wbcd)

#2 organize data
wbcd<-wbcd%>%
  select(-id)%>%
  mutate(diagnosis=factor(diagnosis,levels = c('B','M'),labels=c('Benign','Malignant')))
wbcd%>%
  pull(diagnosis)%>%
  table()%>%
  prop.table()*100
head(wbcd)
#round(prop.table(table(select(wbcd,diagnosis))) * 100, digits = 1)

#3scale the data and check
summary(wbcd[c("radius_mean", "area_mean", "smoothness_mean")])
wbcd_z<-as.data.frame(scale(wbcd[-1]))
sammary(wbcd)
summary(wbcd_z)

#4 split the data
#caTools: sample.split
#need to put Y in sample.split first
library(caTools)
set.seed(1234)
split_index = sample.split(wbcd$diagnosis,SplitRatio = 0.75)
train_X = subset(wbcd_z,split_index==TRUE)
train_Y = as.vector(wbcd[split_index==TRUE,1])
test_X = subset(wbcd_z,split_index==FALSE)
test_Y = as.vector(wbcd[split_index==FALSE,1])
#5 train the model
#label should be vector
Y_pred <-
  knn(
    train = train_X,
    test = test_X,
    cl = train_Y,
    k=21
  )
Y_pred

#6 Evaluate: confusion matrix
eval_table <- table(test_Y,Y_pred)
eval_table

#diag(): Extract or replace the diagonal of a matrix, or construct a diagonal matrix.
diag(eval_table)
#observations we have
nrow(test_X)
#accuracy
sum(diag(eval_table))/nrow(test_X)


#7 improve the model
#(1) change the

## 8 Decide appropriate k

```{r}
accuracy <- vector()
for (i in 1:40){
  Y_predict_i <-
    knn(
      train = train_X_n,
      test= test_X_n,
      cl= train_Y,
      k= i
    )
  eval_table_i <- table(test_Y,Y_pred)
  accuracy[i] = sum(diag(eval_table))/nrow(test_X)
}


```