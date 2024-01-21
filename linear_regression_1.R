####LOAD PACKAGES####
library(readxl)
library(ggplot2)
library(car)
library(QuantPsyc)
data_model <- read_xlsx("DateSets.xlsx",sheet = "model")
data_validate <- read_xlsx("DateSets.xlsx", sheet = "validate")

####Multiple linear regression with 2 factors####

mlr_model1 <- lm( car_density ~ area + population, data = data_model)
sink("mlr_model1_summary.txt")
summary(mlr_model1)
vif(mlr_model1)
durbinWatsonTest(mlr_model1)
lm.beta(mlr_model1)
anova(mlr_model1)
sink()
plot(mlr_model1)


#### Multiple linear regression with 2 factors ####

mlr_model2 <- lm(data = data_model,car_density ~ area + population + employment)
sink("mlr_model2_summary.txt")
summary(mlr_model2)
vif(mlr_model2)
durbinWatsonTest(mlr_model2)
lm.beta(mlr_model2)
anova(mlr_model2)
sink()
plot(mlr_model2)

#### Validation for the second model ####
predictions <- predict(mlr_model2, newdata = data_validate)
sink("validation_model2.txt")
mse <- mean((predictions - data_validate$car_density)^2)
mae <- mean(abs(predictions - data_validate$car_density))
print(mse)
print(mae)
correlation <- cor(predictions, data_validate$car_density)
print(paste("Correlation between Predicted and Observed Values:", correlation))
sink()

#### ANOVA ####
sink("anova.txt")
anova(mlr_model1,mlr_model2)
sink()
