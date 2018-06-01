Data = read.csv('/Users/vamsi_5/Train_Val.csv', header = TRUE)
attach(Data)
plot(Data)
train_idx <- sample(1:nrow(Data),33000,replace=FALSE)
train <- Data[train_idx,] # select all these rows
test <- Data[-train_idx,]

#Feature engineering.

#Linear model for all the variables
m2<-lm(Sale_Log ~  as.factor(BOROUGH) + as.factor(BUILDING.CLASS.AT.PRESENT)+ as.factor(BUILDING.CLASS.AT.TIME.OF.SALE)+
         as.factor(BUILDING.CLASS.CATEGORY)+ COMMERCIAL.UNITS+ GROSS.SQUARE.FEET+ LAND.SQUARE.FEET+ LOT+
         as.factor(NEIGHBORHOOD)+ RESIDENTIAL.UNITS+ SALE.DATE+as.factor(TAX.CLASS.AT.PRESENT)+
         as.factor(TAX.CLASS.AT.TIME.OF.SALE)+ TOTAL.UNITS+ YEAR.BUILT+ as.factor(ZIP.CODE), data = Data)

summary(m2)

#Used Backward Subset Selection to select the variables which are importan(High R^2) to predict the sale price

m1<-lm(Sale_Log~ GROSS.SQUARE.FEET+  COMMERCIAL.UNITS+as.factor(BUILDING.CLASS.CATEGORY)+
         as.factor(TAX.CLASS.AT.PRESENT)+RESIDENTIAL.UNITS+LAND.SQUARE.FEET+ as.factor(ZIP.CODE)+as.factor(BUILDING.CLASS.AT.TIME.OF.SALE),
       data = train)

summary(m1)


plot(m1)






