library(ISLR)
library(MASS)
library(InformationValue)
library(ResourceSelection)
library(rms)


# Reading csv File

input_data<-read.csv("Sepsis.csv")

# View Read File

fix(input_data)

# Print column names

head(input_data)

# Creating Train and Test Data sets

sample_size = floor(0.75*nrow(input_data))
sample_size
set.seed(123)
train_sample<-sample(seq_len(nrow(input_data)),sample_size)
train = input_data[train_sample,]
test = input_data[-train_sample,]
write.csv(train,"train_sepsis.csv")
write.csv(test,"test_sepsis.csv")


# Developing Binary Logistic Model using Stepwise Method

binary_model <- stepAIC(glm(formula = Survival_Status ~ THERAPY+PRAPACHE+Age_Group+BLGCS+ORGANNUM+BLIL6+BLLPLAT+BLLBILI+BLLCREAT+TIMFIRST+BLADL+BLSOFA, family = binomial(link = "logit"), 
                            data = train))

null_model<-glm(formula = Survival_Status~1,data=train,family=binomial(link="logit"))

# Model Summary

1-logLik(binary_model)/logLik(null_model)

summary(binary_model)
confint(binary_model)
exp(coef(binary_model))
res<-residuals(binary_model,standardize = TRUE)

# Residual Plot

plot(res)

# To convert log odds into prediction probability scores

predict_probability <-plogis(predict(binary_model, test))
actual_probability =test$Survival_Status
prediction<-cbind(predict_probability,actual_probability)
write.csv(prediction,"prediction_sepsis.csv")


# Finding Optimum cutoff from predict_probability

optCutOff <- optimalCutoff(actual_probability,predict_probability)[1] 
optCutOff

# The Hosmer-Lemeshow goodness of fit test

stat<-hoslem.test(actual_probability,predict_probability,g=7)
stat

# Check for multicollinearity in the model

vif(binary_model)

# Calculation of Misclassification error

misClassError(test$Survival_Status, predict_probability, threshold = optCutOff)

# Plotting ROC Curve between actual_probability and predict_probability

plotROC(actual_probability, predict_probability)

# Concordence 

Concordance(actual_probability, predict_probability)

# Sensitivity(or True Positive Rate) is the percentage of 1's (actuals) correctly predicted by the model)

sensitivity(actual_probability, predict_probability, threshold =optCutOff)

# Specificity is the percentage of  0's (actuals) correctly predicted by the model

specificity(actual_probability, predict_probability, threshold =optCutOff)