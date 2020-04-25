#Datathon competition 
library(readr)
library(readxl)
library("readr")
library("ggplot2")
library("lattice")
library(caret)
library("corrplot")
library("dplyr")
library("e1071")
library(psych)


pacman:: p_load(caret, party, reshape, ggplot2, dplyr)

dengue_labels_train <- read.csv("C:/Respaldo FR/Ubiqum/Proyectos/Datathon/Data_sets/dengue_labels_train.csv")
View(dengue_labels_train)
dengue_features_train <- read.csv("C:/Respaldo FR/Ubiqum/Proyectos/Datathon/Data_sets/dengue_features_train.csv")
View(dengue_features_train)
dengue_features_test <- read.csv("C:/Respaldo FR/Ubiqum/Proyectos/Datathon/Data_sets/dengue_features_test.csv")
View(dengue_features_test)

head(dengue_labels_train)
head(dengue_features_train)
head(dengue_features_test)

totalcases <-dengue_labels_train$total_cases
totalcases

dengue_features_train_totalcases <- data.frame(dengue_features_train,totalcases)
dengue_features_train_totalcases

#Data ####
str(dengue_features_train_totalcases)
dengue_features_train_totalcases$city <- as.numeric()

dengue_features_train_totalcases_iq<- subset(dengue_features_train_totalcases$city == "sj")
dengue_features_train_totalcases_iq

#Corr ####
#newdengue_features_test <- dummyVars(" ~ .", data = dengue_features_test)

#readydengue_features_test <- data.frame(predict(newdengue_features_test[-c(1,4)], newdata = dengue_features_test))

corMatrix <- cor(dengue_features_train_totalcases[-c(1,4)], use="complete.obs")

corrplot(corMatrix,tl.cex = 0.7, type = "lower")


#checking na rows 
nrow(dengue_features_train_totalcases[!complete.cases(dengue_features_train_totalcases),]) 
#Handling NA  
sapply(dengue_features_train_totalcases, function(x) sum(is.na(x)))
delete.na <- function(df, n=0) {
  df[rowSums(is.na(df)) <= n,]
}
delete.na(dengue_features_train_totalcases)
datos <- na.omit(dengue_features_train_totalcases)
sum(is.na(dengue_features_train_totalcases)) 
mean(is.na(dengue_features_train_totalcases)) 
colMeans(is.na(dengue_features_train_totalcases)) 
c_data <- na.omit(dengue_features_train_totalcases) 
c_data


#Plotting 
#Histogram of week of year 

barplot_of_dengue <- plot_ly( 
  x = c_data$weekofyear, 
  y = c_data$total_cases, 
  name = "Barchart", 
  type = "bar" ) %>% 
  layout(title = "Dengue cases by week of year") 
barplot_of_dengue

# Basic line plot with points 

p <- plot_ly(c_data, x = dengue_features_train_totalcases$year, y = dengue_features_train_totalcases$total_cases, type = 'scatter', mode = 
               'lines') 
p

station_max_temp, reanalysis_avg_temp_k, station_avg_temp, and reanalysis_specific_humidity_g_per_kg

#first linear model 

set.seed(123)

in_training <- createDataPartition(c_data$totalcases, p=0.7, list=F)
training <- c_data[in_training,]
testing <- c_data[-in_training,]

a <- c("lm","rf","knn")

features <- c("totalcases ~  reanalysis_max_air_temp_k + reanalysis_max_air_temp_k",
              "totalcases ~ reanalysis_specific_humidity_g_per_kg",
              "totalcases ~ station_max_temp_c")

cv <- trainControl(method="repeatedcv",number = 10, repeats = 1,)

compare <- c()
for (i in a) {
  for (z in features) {
    fit <- train(formula(z), training, method = i,  trControl = cv)
    pred <- predict(fit, testing)
    metric <- postResample(pred,c_data$totalcases)
    compare <- cbind(metric,compare)
    colnames(compare)[colnames(compare) == "metric"] <- paste(i, "-", colname[which (features == z)])
  }
}



# Fitcontrol settings

#MODEl 1 

model1 <- lm(totalcases ~ ndvi_se + ndvi_sw + precipitation_amt_mm + reanalysis_air_temp_k + reanalysis_avg_temp_k + reanalysis_dew_point_temp_k + 
               reanalysis_max_air_temp_k + reanalysis_min_air_temp_k + 
              reanalysis_relative_humidity_percent  + reanalysis_specific_humidity_g_per_kg + 
               station_avg_temp_c + station_diur_temp_rng_c + 
               station_max_temp_c + station_min_temp_c + station_precip_mm, data = 
               c_data) 
#second linear model 
model2 <- lm(totalcases ~ reanalysis_max_air_temp_k + 
               reanalysis_specific_humidity_g_per_kg + station_diur_temp_rng_c + 
               station_max_temp_c, data = c_data) 
#finale linear model by automatic backward elimination 
library(MASS) 
install.packages("MASS")
step <- stepAIC(model1, direction="backward", trace = "FALSE") 
step$anova 

model3 <- lm(total_cases ~ precipitation_amt_mm + reanalysis_avg_temp_k + 
               reanalysis_dew_point_temp_k + reanalysis_max_air_temp_k + 
               reanalysis_precip_amt_kg_per_m2 + reanalysis_specific_humidity_g_per_kg +
               reanalysis_tdtr_k + station_diur_temp_rng_c + station_max_temp_c, data = c_data) 

anova(model1, model2, model3) 
par(mfrow = c(2,2)) 


final_outcome <- data.frame(prediction, submission_format)
final_outcome 


library(readr)
submission_format_ <- read_csv("C:/Respaldo FR/Datathon/submission_format_.csv")
View(submission_format_)

hist(submission_format_$total_cases)








#finale linear model by automatic backward elimination 

step <- stepAIC(model1, direction="backward", trace = "FALSE") 

step$anova 

model3 <- lm(totalcases ~ precipitation_amt_mm + reanalysis_avg_temp_k + 
               reanalysis_dew_point_temp_k + reanalysis_max_air_temp_k + 
               reanalysis_precip_amt_kg_per_m2 + reanalysis_specific_humidity_g_per_kg +
               reanalysis_tdtr_k + station_diur_temp_rng_c + station_max_temp_c, data = c_data) 

anova(model1, model2, model3) 
par(mfrow = c(2,2)) 
plot(model3)



prediction <- predict(model3, dengue_features_test) 
dengue_features_test$prediction <- prediction 
actuals_preds <- data.frame(cbind(actuals=c_data$total_cases, predicteds=prediction)) 
correlation_accuracy <- cor(actuals_preds) 
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max)) 
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals) 

library(Metrics) 
install.packages("Metrics")

mse(actuals_preds$actuals, actuals_preds$predicteds)


pacman::p_load(pacman, caret, dplyr, rmarkdown, ggplot2, C50)

#Predict on test data 

BP3 <- predict(model3, newdata = testing)
outcome3 <- postResample(testing$totalcases,BP3)
outcome3



final_outcome <- predict(BP3, newdata = submission_format)
final_outcome
table(final_outcome)





dengue_features_train %>%
  select(city,station_max_temp_c,reanalysis_avg_temp_k,station_avg_temp_c) %>%
  filter(city == "iq" | 
           city== "sj")  %>%
  group_by(city) %>%
  summarise()

station_max_temp, reanalysis_avg_temp_k, station_avg_temp, and reanalysis_specific_humidity_g_per_kg.


