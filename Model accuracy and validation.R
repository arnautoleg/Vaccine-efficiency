
#install.packages("caret")
#install.packages("klaR")
#install.packages("predtools")
#install.packages("magrittr")

# load the libraries
library(caret)
library(klaR)
library(ggplot2)
library(pROC)
library(predtools)
library(magrittr)


# define an 80%/20% train/test split of the dataset
set.seed(3456)

split=0.80

df6$ID_sample <- as.numeric(df6$ID_sample)

df6 <- df6 %>% drop_na(ID_sample)



summary(df6$ID_sample)


trainIndex <- createDataPartition(df6$ID_sample, p=split, list=FALSE, times = 1)
data_train <- df6[trainIndex,]
data_test <- df6[-trainIndex,]


# Fit the model
model <- glm(Outcome ~ Sex + Aging_cut + Rural_Urban + Vaccine_status_1 + Data_ANSP_month,
                data=data_train, family=binomial())
# Summarize the model
summary(model)
# Make predictions
probabilities_1 <- model %>% predict(data_train, type = "response")
probabilities_1

probabilities <- model %>% predict(data_test, type = "response")
probabilities

predicted.classes <- ifelse(probabilities > 0.5, "nonSurvive", "Survive")

predicted.classes
# Model accuracy
print(mean(predicted.classes == data_test$Outcome))


#####################################################

#define object to plot and calculate AUC

rocobj_1 <- roc(data_train$Outcome, probabilities_1)
auc_1 <- round(auc(data_train$Outcome, probabilities_1),4)

rocobj <- roc(data_test$Outcome, probabilities)
auc <- round(auc(data_test$Outcome, probabilities),4)

#create ROC plot

ggroc(rocobj_1, colour = 'steelblue', size = 2) +
  ggtitle(paste0('ROC Curve for train sample ', '(AUC = ', auc_1, ')'))+
  theme_minimal()

ggroc(rocobj, colour = 'steelblue', size = 2) +
  ggtitle(paste0('ROC Curve for test sample ', '(AUC = ', auc, ')'))+
  theme_minimal()

######################################################

ggroc(list(Train = rocobj_1, Test = rocobj), size = 1)+
  theme_minimal()+
  ggtitle(paste0('ROC Curve Train vs Test sample ', '(AUC = ', auc_1, ' vs ', auc, ')'))

