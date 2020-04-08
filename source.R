library(tidyverse)
df <- read_csv(file="train.csv")




df$amenities_clean <- lapply( df$amenities , gsub , pattern = "[^,a-zA-Z\\s]" , replacement = "" , perl = TRUE )



df$amenities_clean <- gsub("Fixed grab bars for shower  toilet", "Fixed grab bars for shower & toilet", df$amenities_clean)
df$amenities_clean <- gsub("Wide clearance to shower  toilet", "Wide clearance to shower & toilet", df$amenities_clean)
df$amenities_clean <- gsub("Accessibleheight toilet", "Accessible-height toilet" , df$amenities_clean)
df$amenities_clean <- gsub("Accessibleheight bed", "Accessible-height bed" , df$amenities_clean)



a<- lapply(df$amenities_clean, function(x) unique(trimws(unlist(strsplit(x, ",")))))



lengths<- lapply(a, length)
items<- a[which.max(lengths)]
items_2<- data.frame (items_2 = unlist(items))



library(splitstackshape)
df_new<- concat.split.multiple(df, "amenities_clean", seps=",", "long")



df_new$value <- 1



library(utils)
#create a table with all combinations of id and items for merging
for_merger<- expand.grid(id = df$id, amenities_clean = items_2$items_2)



#cut down df to make things run faster
df_new3 <- df_new[,c(1,30,31)]



library(dplyr)
#merge 2 tables
join <- right_join(df_new3,for_merger, by= c("id","amenities_clean"))



library(dplyr)
#change NA in value column to 0
join<- join %>%
  mutate_all(funs(ifelse(is.na(.), 0, .)))



library(dplyr)
library(tidyr)
join<- join %>% 
  group_by(amenities_clean) %>% 
  mutate(grouped_id = row_number())
count<- join %>% 
  spread(amenities_clean, value) 
count<- count[,-2]

#remove duplicate id with NAs
count2 <-  count[!is.na(count$`Accessible-height bed`), ]



library(dplyr)
df_new4 <- df %>% inner_join(count2, by= "id") %>%
  #change NA in value column to 0
  mutate_all(funs(ifelse(is.na(.), 0, .)))



#check to make sure there is no NA
#sapply(df_new4, function(x) sum(is.na(x)))


#transform log_price to price ($)
df_new4$price<- sapply(df_new4['log_price'], function(x) exp(x))



df_new4$cleaning_fee[df_new4$cleaning_fee == TRUE] <- 1
df_new4$cleaning_fee[df_new4$cleaning_fee == FALSE] <- 0



#find top 15 amenities
AA <- colSums(df_new4[, 31:111] != 0)
tail(sort(AA),15)



#cut down df to make things run faster
df_new5 <- df_new4[,c(1,3,4,8)]
df_new5



#df_new4$room_type <- as.character(as.numeric(df_new4$room_type))
df_new5$room_type  <- as.factor(df_new5$room_type) 
df_new5$bed_type  <- as.factor(df_new5$bed_type) 
df_new5$property_type  <- as.factor(df_new5$property_type) 



library(caret)
dmy <- dummyVars(" ~ .", data = df_new5)


trsf <- data.frame(predict(dmy, newdata = df_new5))
trsf
#joining tables
df_new6 <- df_new4 %>% inner_join(trsf, by= "id")


#Keep only the top 15 amentities
df_new7 <- df_new6[,-c(1:5,8,9,13:19,22,23,26,30:32,34:43,45:52,54,56:58,60:67,69,72:74,76,79:92,94,96:101,103:110)]



#find numeric values and their correlations
numeric_var <- sapply(df_new7, is.numeric)
matrix <- cor(df_new7[,numeric_var])
#find locations of highly correlated paris
a<- which( matrix != 1 & (matrix < -0.7 | matrix > 0.7) , arr.ind = TRUE)
correlation<- matrix[which( matrix != 1 & (matrix < -0.7 | matrix > 0.7) , arr.ind = TRUE)]
cbind(a,correlation)



#remove undesired highly correlated columns: accommodates,bedrooms, Dryer, property_type.Apartment,room_type.Entire.home.apt
df_new7 <- df_new7[,-c(1,11,15,29,64)]



#remove undesired columns for modeling: latitude,longitude, zipcode, description 
df_new7 <- df_new7[,-c(4:6,9)]

#Partitioning the dataset into training and test data
# set.seed(100)
# #Generate a random number that is 80% of the total number of rows in dataset.
# train.index <- sample(row.names(df_new7), 0.8*dim(df_new7)[1])  
# valid.index <- setdiff(row.names(df_new7), train.index)  
# train.df <- df_new7[train.index,]
# valid.df <- df_new7[valid.index,]



# library(caret)
# library(stats)
# # Fit the linear model
# fit_lm <-lm(price~., train.df)
# # Predict testing data on our linear model
# summary(fit_lm)
# #pred<- predict(fit_lm, valid.df)
# #make the prediction
# pred_lm <- predict(fit_lm, newdata = valid.df)
# residuls1 <- valid.df$price[1:10]-pred_lm[1:10]
# d1 <- data.frame("Predicted"= pred_lm[1:10], "Actual"=valid.df$price[1:10], "Residul"= residuls1)
# #randomly samples 10 rows from the dataframe to display
# library(knitr)
# kable(sample_n(d1, 10))
# RMSE(pred_lm,valid.df$price) 
#RMSE(pred_lm,valid.df$price) 



# A typical approach
#par(mfrow=c(2,2))
#plot(fit_lm)



# library(MASS)
# # Set seed for reproducibility
# set.seed(111)
# # Stepwise regression model
# step.model1 <- stepAIC(fit_lm, direction = "both")
# summary(step.model1) #return to the best final model
# library(forecast)
# accuracy(step.model1)


library(dplyr)
#keep only significant columns suggested by AIC
requiredCols<-c('price', 'bathrooms' , 'cleaning_fee' , 'city' , 'number_of_reviews' , 
                'review_scores_rating' , 'beds' , 'Air conditioning' , 'Essentials' , 
                'Familykid friendly' , 'Hair dryer' , 'Hangers' , 'Heating' , 
                'Kitchen' , 'Laptop friendly workspace' , 'Shampoo' , 'Smoke detector' , 
                'Washer' , 'Wireless Internet' , 'property_type.Boat' , 'property_type.Boutique.hotel' , 
                'property_type.Condominium' , 'property_type.Dorm' , 'property_type.Earth.House' , 
                'property_type.Guest.suite' , 'property_type.Hostel' , 'property_type.House' , 
                'property_type.In.law' , 'property_type.Loft' , 'property_type.Other' , 
                'property_type.Serviced.apartment' , 'property_type.Timeshare' , 
                'property_type.Treehouse' , 'property_type.Villa' , 'room_type.Private.room' , 
                'room_type.Shared.room' , 'bed_type.Airbed' , 'bed_type.Couch')
df_new8 <- df_new7[,requiredCols]
#24 cols removed


#Convert a few columns to integer and normalize Price
library(LambertW)
df_new8$price <- Gaussianize(df_new8$price)
df_new8$number_of_reviews <- as.integer(df_new8$number_of_reviews)
df_new8$review_scores_rating <- as.integer(df_new8$review_scores_rating)
df_new8$price <- as.integer(df_new8$price)



# #Partitioning the dataset into training and test data
# set.seed(111)
# #Generate a random number that is 80% of the total number of rows in dataset.
# train.index2 <- sample(row.names(df_new8), 0.8*dim(df_new8)[1])  
# valid.index2 <- setdiff(row.names(df_new8), train.index2)  
# train.df2 <- df_new8[train.index2,]
# valid.df2 <- df_new8[valid.index2,]



# # Fit the linear model
# fit_lm2 <-lm(price~., train.df2)
# # Predict testing data on our linear model
# summary(fit_lm2)
# #pred<- predict(fit_lm, valid.df)
# #make the prediction
# pred_lm2 <- predict(fit_lm2, newdata = valid.df2)
# residuls2 <- valid.df2$price[1:10]-pred_lm2[1:10]
# d2 <- data.frame("Predicted"= pred_lm2[1:10], "Actual"=valid.df2$price[1:10], "Residul"= residuls2)
# #randomly samples 10 rows from the dataframe to display
# library(knitr)
# kable(sample_n(d2, 10))
# RMSE(pred_lm2,valid.df2$price) 


# A typical approach
# par(mfrow=c(2,2))
# plot(fit_lm2)


# #Do another Feature Selection - Backward Selection this time
# library(MASS)
# # Set seed for reproducibility
# set.seed(111)
# # Stepwise regression model
# step.model2 <- step(fit_lm2, direction = "backward")
# summary(step.model2) #return to the best final model
# library(forecast)
# accuracy(step.model2)


library(dplyr)
#keep only significant columns suggested by AIC
requiredCols2<-c('price', 'bathrooms' , 'cleaning_fee' , 'city' , 'number_of_reviews' , 
                 'review_scores_rating' , 'beds' , 'Air conditioning' , 'Essentials' , 
                 'Familykid friendly' , 'Hair dryer' , 'Hangers' , 'Heating' , 
                 'Kitchen' , 'Laptop friendly workspace' , 'Shampoo' , 'Smoke detector' , 
                 'Washer' , 'Wireless Internet' , 'property_type.Boat' , 'property_type.Boutique.hotel' , 
                 'property_type.Condominium' , 'property_type.Dorm' , 'property_type.Earth.House' , 
                 'property_type.Guest.suite' , 'property_type.Hostel' , 'property_type.House' , 
                 'property_type.In.law' , 'property_type.Loft' , 'property_type.Other' , 
                 'property_type.Serviced.apartment' , 'property_type.Timeshare' , 
                 'property_type.Villa' , 'room_type.Private.room' , 
                 'room_type.Shared.room' , 'bed_type.Couch')

df_new9 <- df_new8[,requiredCols2]
#2 cols removed



#normalize
library(LambertW)
df_new9$number_of_reviews <- Gaussianize(df_new9$number_of_reviews)
df_new9$number_of_reviews <- as.integer(df_new9$number_of_reviews)
df_new9$review_scores_rating <- Gaussianize(df_new9$review_scores_rating)
df_new9$review_scores_rating <- as.integer(df_new9$review_scores_rating)
df_new9$price <- Gaussianize(df_new9$price)
df_new9$price <- as.integer(df_new9$price)



#Cut out outliers
df_new9$bathrooms <- as.factor(df_new9$bathrooms)
outliers_review<- boxplot(df_new9$number_of_reviews, plot=FALSE)$out
df_new9 <- df_new9[-which(df_new9$number_of_reviews %in% outliers_review),]
outliers_reviewSc<- boxplot(df_new9$review_scores_rating, plot=FALSE)$out
df_new9 <- df_new9[-which(df_new9$review_scores_rating %in% outliers_reviewSc),]
outliers_bed<- boxplot(df_new9$beds, plot=FALSE)$out
df_new9 <- df_new9[-which(df_new9$beds %in% outliers_bed),]



#Partitioning the dataset into training and test data
# set.seed(111)
# #Generate a random number that is 80% of the total number of rows in dataset.
# train.index3 <- sample(row.names(df_new9), 0.8*dim(df_new9)[1])  
# valid.index3 <- setdiff(row.names(df_new9), train.index3)  
# train.df3 <- df_new9[train.index3,]
# valid.df3 <- df_new9[valid.index3,]
# 
# 
# 
# # Fit the linear model
# fit_lm3 <-lm(price~., train.df3)
# # Predict testing data on our linear model
# summary(fit_lm3)
# #pred<- predict(fit_lm, valid.df)
# #make the prediction
# pred_lm3 <- predict(fit_lm3, newdata = valid.df3)
# residuls3 <- valid.df3$price[1:15]-pred_lm3[1:15]
# d3 <- data.frame("Predicted"= pred_lm3[1:15], "Actual"=valid.df3$price[1:15], "Residul"= residuls3)
# #randomly samples 10 rows from the dataframe to display
# library(knitr)
# kable(sample_n(d3, 15))
# RMSE(pred_lm3,valid.df3$price) 


# # A typical approach
# par(mfrow=c(2,2))
# plot(fit_lm3)
# 
# 
# 
# library(MASS)
# # Set seed for reproducibility
# set.seed(111)
# # Stepwise regression model
# step.model3 <- stepAIC(fit_lm3, direction = "both")
# summary(step.model3) #return to the best final model
# library(forecast)
# accuracy(step.model3)



library(dplyr)
#keep only significant columns
#property_type.Otherwill be also removed on top of what resulted in "step.model$call" as the  column is not deemed significant by p-value
requiredCols3<-c('price', 'bathrooms'  , 'city' , 'number_of_reviews' , 
                 'review_scores_rating' , 'beds' , 'Air conditioning' , 'Essentials' , 
                 'Familykid friendly' , 'Hair dryer' , 'Hangers' , 'Heating' , 
                 'Laptop friendly workspace' , 'Shampoo' , 'Smoke detector' , 
                 'Washer' , 'property_type.Boat' , 'property_type.Boutique.hotel' , 
                 'property_type.Condominium' , 'property_type.Dorm' , 
                 'property_type.Guest.suite' , 'property_type.House' , 
                 'property_type.In.law' , 'property_type.Loft' , 'property_type.Other' , 
                 'property_type.Timeshare' ,  'room_type.Private.room' , 
                 'room_type.Shared.room')
df_new10<-df_new9[,requiredCols3]
# 8 bad-fitted columns removed

#Further reduction of features according to the suggestions from Forward Selection
df_new10$bathrooms<- as.character.factor(df_new10$bathrooms)
df_new10<- df_new10[(df_new10$bathrooms != 0) &(df_new10$bathrooms != 0.5) & (df_new10$bathrooms < 5),]
df_new10<- df_new10[(df_new10$beds != 0),]



#normalize 
library(LambertW)
df_new10$number_of_reviews <- Gaussianize(df_new10$number_of_reviews)
df_new10$number_of_reviews <- as.integer(df_new10$number_of_reviews)
df_new10$price <- Gaussianize(df_new10$price)
df_new10$price  <- as.integer(df_new10$price)
df_new10$review_scores_rating <- Gaussianize(df_new10$review_scores_rating)
df_new10$review_scores_rating <- as.integer(df_new10$review_scores_rating)
names(df_new10)[7]<- c("Air_conditioning")
names(df_new10)[9]<- c("Family_kid_friendly")
names(df_new10)[10]<- c("Hair_dryer")
names(df_new10)[13]<- c("Laptop_friendly_workspace")
names(df_new10)[15]<- c("Smoke_detector")
df_new10$bathrooms <- as.numeric(df_new10$bathrooms)


#write_csv(df_new10, "appData.csv")
