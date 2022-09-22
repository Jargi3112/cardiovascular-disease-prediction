library(reshape2)
library(gginference)
library(RColorBrewer)
library(GGally)
library(lattice)
library(olsrr) 
library(performance) 
library(Ecdat) 
library(leaps) 
library(lmtest) 
library(visdat) 
library(inspectdf) 
library(skimr) 
library(ggcorrplot) 
library(gridExtra)
library(e1071)
library(lattice)
library(caret)
library(ISLR)
library(pROC)
library(glmnet)
library(Metrics)
library(dplyr)
library(psych)
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(gridExtra)
library(corrplot)
library(scales)
library(lmSubsets)

# Formatting
F_cardio <- as.data.frame(read.csv(file.choose( ) , sep=";",header = TRUE,stringsAsFactors = FALSE) )
head(F_cardio)

#view(F_cardio)

describe(F_cardio)
summary(F_cardio) 
glimpse(F_cardio)

#Excluding id

F_cardio <- select(F_cardio, -c(id))
View(F_cardio)

#Correlation Matrix:
cardioExplor <- F_cardio
correlation = cor(cardioExplor[,1:12])
cols<- colorRampPalette(c("red", "blue"))(20)
corrplot(correlation,  method ="number",col=cols,type="upper",
         title = "\n\n Correlation Plot Of Cardio train")


#Correlation plot
ggplot(melt(correlation_matrix), aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low="blue", mid="white", high="red") +
  coord_equal()


# #Changing variable to factor
# cols <- c("cholesterol","gluc", "smoke", "alco", "active", "cardio")
# F_cardio[,cols] <- lapply(F_cardio[,cols], factor)
# str(F_cardio)


#changing the male and female values into 0's and 1's
F_cardio$gender <- factor(F_cardio$gender, levels=c(1,2), labels=c(0,1)) 
head(F_cardio$gender)


#Cleaning data 
#Checking for missing values - NA's
any(is.na(F_cardio))

F_cardio[F_cardio == "?"] <- NA
any(is.na(F_cardio))

#Outliers

#For Systolic Blood Pressure Range
#boxplot(F_cardio$ap_hi ~ F_cardio$cardio, main="Systolic blood pressure  by cardio", ylab = "Systolic blood pressure", xlab = "cardio")

ggplot(F_cardio, aes(x = ap_hi, y= cardio)) + 
  geom_boxplot(fill="gray")+
  labs(title="Systolic blood pressure  by cardio",x="cardio", y = "Systolic blood pressure")+
  theme_classic()


#F_cardio <- F_cardio[!(F_cardio$ap_hi>370),]
#F_cardio <- F_cardio[!(F_cardio$ap_lo>360),]
#Replacing value with median
med_ap_hi <- median(F_cardio$ap_hi)
F_cardio$ap_hi[F_cardio$ap_hi < 90 | F_cardio$ap_hi > 120 ] = med_ap_hi


#Box plot after removing outliers
ggplot(F_cardio, aes(x = ap_hi, y= cardio)) + 
  geom_boxplot(fill="gray")+
  labs(title="Systolic blood pressure  by cardio",x="cardio", y = "Systolic blood pressure")+
  theme_classic()



#----For Diastolic Blood Pressure Range-----
ggplot(F_cardio, aes(x = ap_lo, y= cardio)) + 
  geom_boxplot(fill="gray")+
  labs(title="Diastolic blood pressure  by cardio",x="cardio", y = "Diastolic blood pressure")+
  theme_classic()



#Replacing value with median
med_ap_lo <- median(F_cardio$ap_lo)
F_cardio$ap_lo[F_cardio$ap_lo < 60 | F_cardio$ap_lo > 80 ] = med_ap_lo


#Box plot after removing outliers
ggplot(F_cardio, aes(x = ap_lo, y= cardio)) + 
  geom_boxplot(fill="gray")+
  labs(title="Diastolic blood pressure by cardio",x="cardio", y = "Diastolic blood pressure")+
  theme_classic()



#-----------------------
#Scaling
#F_cardio$age <- gsub("(^\\d{2}).*", "\\1", F_cardio$age)

F_cardio$age <- F_cardio$age/365
F_cardio$age<-round(F_cardio$age,digits = 0)

#plotting age and cardio
a <- ggplot(F_cardio, aes(x = weight))
a + geom_histogram(aes(color = as.factor(cardio)), fill = "white", 
                   position = "dodge") +
  scale_color_manual(values = c("#800000", "#E7B800")) 

#calculating BMI
BMI = function(height,weight){(weight/(height/100)^2)}
F_cardio$BMI = BMI(F_cardio$height,F_cardio$weight)
head(F_cardio$BMI)

view(F_cardio)


#-------------------------------------------------
#F_cardio
copy_cardio <- F_cardio
#----------------------------------
#-----------------------------------------
# chi- square on age and cardio
age <- copy_cardio %>%
  dplyr::group_by(cardio) %>%
  summarise(age = sum(age)) %>%
  as_tibble()


cardio_alpha   = 0.05
cardio_LoSig   = 1- cardio_alpha
cardio_k       = nrow(age)  ## No. of rows
cardio_DF      = cardio_k-1


age$Expected <- 1/cardio_k  ### Lets assume that expected frequencies are equal- 1/6th
age

cardio_Critical_Val <- qchisq(p= cardio_LoSig, cardio_DF, lower.tail=TRUE)

cat("Critical value: ",cardio_Critical_Val)

age_chisq = chisq.test(age$age,
                        p = age$Expected,  ## Values in Probability 
                        correct = FALSE) # not to apply continuity correction

age_chisq

cat("The calculated t-value is:",age_chisq$statistic, "p-value is: ",age_chisq$p.value, " and alpha is:",cardio_alpha)

ifelse(wght_chisq$statistic < cardio_Critical_Val, "Fail to reject null  hypothesis ", " Rejecting null hypothesis")
#-----------------------
#-------------------------------

# chi- square on cholesterol 

cholesterol <- copy_cardio %>%
  dplyr::group_by(cardio) %>%
  summarise(cholesterol = sum(cholesterol)) %>%
  as_tibble()


cardio_alpha   = 0.05
cardio_LoSig   = 1- cardio_alpha
cardio_k       = nrow(cholesterol)  ## No. of rows
cardio_DF      = cardio_k-1


cholesterol$Expected <- 1/cardio_k  ### Lets assume that expected frequencies are equal- 1/6th
cholesterol

cardio_Critical_Val <- qchisq(p= cardio_LoSig, cardio_DF, lower.tail=TRUE)

cat("Critical value: ",cardio_Critical_Val)

cholesterol_chisq = chisq.test(cholesterol$cholesterol,
                               p = cholesterol$Expected,  ## Values in Probability 
                               correct = FALSE) # not to apply continuity correction

cholesterol_chisq

cat("The calculated t-value is:",cholesterol_chisq$statistic, "p-value is: ",cholesterol_chisq$p.value, " and alpha is:",cardio_alpha)

ifelse(cholesterol_chisq$statistic < cardio_Critical_Val, "Fail to reject null  hypothesis ", " Rejecting null hypothesis")

#-----------------------------------
#---------------------------------
# chi- square on Weight and cardio

weight <- copy_cardio %>%
  dplyr::group_by(cardio) %>%
  summarise(weight = sum(weight)) %>%
  as_tibble()


cardio_alpha   = 0.05
cardio_LoSig   = 1- cardio_alpha
cardio_k       = nrow(weight)  ## No. of rows
cardio_DF      = cardio_k-1


weight$Expected <- 1/cardio_k  ### Lets assume that expected frequencies are equal- 1/6th
weight

cardio_Critical_Val <- qchisq(p= cardio_LoSig, cardio_DF, lower.tail=TRUE)

cat("Critical value: ",cardio_Critical_Val)

weight_chisq = chisq.test(weight$weight,
                          p = weight$Expected,  ## Values in Probability 
                          correct = FALSE) # not to apply continuity correction

weight_chisq

cat("The calculated t-value is:",weight_chisq$statistic, "p-value is: ",weight_chisq$p.value, " and alpha is:",cardio_alpha)

ifelse(weight_chisq$statistic < cardio_Critical_Val, "Fail to reject null  hypothesis ", " Rejecting null hypothesis")
#--------------------------
#-----------------------------------------

#Chi- square on BMI and cardio

BMI <- copy_cardio %>%
  dplyr::group_by(cardio) %>%
  summarise(BMI = sum(BMI)) %>%
  as_tibble()


cardio_alpha   = 0.05
cardio_LoSig   = 1- cardio_alpha
cardio_k       = nrow(BMI)  ## No. of rows
cardio_DF      = cardio_k-1


BMI$Expected <- 1/cardio_k  ### Lets assume that expected frequencies are equal- 1/6th
BMI

cardio_Critical_Val <- qchisq(p= cardio_LoSig, cardio_DF, lower.tail=TRUE)

cat("Critical value: ",cardio_Critical_Val)

BMI_chisq = chisq.test(BMI$BMI,
                       p = BMI$Expected,  ## Values in Probability 
                       correct = FALSE) # not to apply continuity correction

BMI_chisq

cat("The calculated t-value is:",BMI_chisq$statistic, "p-value is: ",BMI_chisq$p.value, " and alpha is:",cardio_alpha)

ifelse(BMI_chisq$statistic < cardio_Critical_Val, "Fail to reject null  hypothesis ", " Rejecting null hypothesis")

#-------------------------------------------
#-----------LM model-----------------------
# Imapact of age and weight on cardio

F_cardio$cardio <- as.numeric(F_cardio$cardio)
ls(F_cardio)
model1_age_weight <- lm(cardio ~ age + weight, data = F_cardio)
model1_age_weight
summary(model1_age_weight)
summary(model1_age_weight)$coefficients

# Imapact of age and cholesterol on cardio
model1_age_cholesterol <- lm(cardio ~ age + cholesterol, data = F_cardio)
model1_age_cholesterol
summary(model1_age_cholesterol)
summary(model1_age_cholesterol)$coefficients

#------------------------------
#--------------------------------------
# Imapact of weight and cholesterol on cardio

model1_weight_cholesterol <- lm(cardio ~ weight + cholesterol, data = F_cardio)
model1_weight_cholesterol
summary(model1_weight_cholesterol)
summary(model1_weight_cholesterol)$coefficients

#---------------------
#-----------------------------
# Impact of BMI and Cholesterol on cardio

F_cardio$cardio <- as.numeric(F_cardio$cardio)
model1_BMI_cholesterol <- lm(cardio ~ BMI + cholesterol, data = F_cardio)
model1_BMI_cholesterol
summary(model1_BMI_cholesterol)
summary(model1_BMI_cholesterol)$coefficients

#--------------------------------
#---------------------------------------
#------------GLM------------------------

set.seed(123)
trainIndex <- sample(c(TRUE,FALSE), nrow(F_cardio), replace = TRUE, prob = c(0.7,0.3))
train_data <- F_cardio[trainIndex,]
test_data <- F_cardio[!trainIndex,]

F_cardio$cardio <- as.factor(F_cardio$cardio)

## Impact of age and weight on cardio
model2_age_weight <- glm(cardio ~age + weight, family = "binomial", data = train_data)

#disable scientific notation for model summary
options(scipens=999)
summary(model2_age_weight)
summary(model2_age_weight)$coef

# fitting the data 
train_data$pred <- predict(model2_age_weight, train_data, type = "response")
train_data$pred_label <- as.factor(ifelse(train_data$pred >= 0.5, "1", "0"))

train_data$cardio <- as.factor(train_data$cardio)
train_data$cardio
train_data$pred_label
train_data$pred

# Confusion Matrix on Train Data
cM1 <- confusionMatrix(train_data$cardio, train_data$pred_label)
cM1

#Test dataset
test_data$pred <- predict(model2_age_weight, test_data, type = "response")
test_data$pred_label <- as.factor(ifelse(test_data$pred >= 0.5, "1", "0"))
test_data$cardio <- as.factor(test_data$cardio)



#Confusion Matrix on Test Data
conf1<-confusionMatrix(test_data$cardio,test_data$pred_label, )
conf1

#define object to plot and calculate AUC
rocobj <- roc(as.ordered(test_data$cardio), as.ordered(test_data$pred_label), ordered = TRUE)

auc <- round(auc(as.ordered(test_data$cardio), as.ordered(test_data$pred_label)),4)

ggroc(rocobj, colour = 'steelblue', size = 2) +
  ggtitle(paste0('ROC Curve ', '(AUC = ', auc, ')'))

#-----------------------------------------
#---------------------------------------------------
#Imapact of age and cholesterol on cardio
F_cardio$cholesterol
F_cardio$cardio <- as.factor(F_cardio$cardio)

model2_age_cholesterol <- glm(cardio ~age + cholesterol, family = "binomial", data = train_data)

#disable scientific notation for model summary
options(scipens=999)
summary(model2_age_cholesterol)
summary(model2_age_cholesterol)$coef



# fitting the data 
train_data$pred <- predict(model2_age_cholesterol, train_data, type = "response")
train_data$pred_label <- as.factor(ifelse(train_data$pred >= 0.5, "1", "0"))
train_data$cardio <- as.factor(train_data$cardio)
train_data$cardio
train_data$pred_label
train_data$pred

# Confusion Matrix on Train Data
cM2 <- confusionMatrix(train_data$cardio, train_data$pred_label)
cM2

# test dataset
test_data$pred <- predict(model2_age_cholesterol, test_data, type = "response")
test_data$pred_label <- as.factor(ifelse(test_data$pred >= 0.5, "1", "0"))
test_data$cardio <- as.factor(test_data$cardio)

#Confusion Matrix on Test Data
conf2<-confusionMatrix(test_data$cardio,test_data$pred_label, )
conf2

#define object to plot and calculate AUC
rocobj <- roc(as.ordered(test_data$cardio), as.ordered(test_data$pred_label), ordered = TRUE)

auc <- round(auc(as.ordered(test_data$cardio), as.ordered(test_data$pred_label)),4)

ggroc(rocobj, colour = 'steelblue', size = 2) +
  ggtitle(paste0('ROC Curve ', '(AUC = ', auc, ')'))

#--------------------------------------
#---------------------------------------
#Imapact of weight and cholesterol on cardio

F_cardio$weight
F_cardio$cardio <- as.factor(F_cardio$cardio)

model2_weight_cholesterol <- glm(cardio ~weight + cholesterol, family = "binomial", data = train_data)

#disable scientific notation for model summary
options(scipens=999)
summary(model2_weight_cholesterol)
summary(model2_weight_cholesterol)$coef

# fitting the data 
train_data$pred <- predict(model2_weight_cholesterol, train_data, type = "response")
train_data$pred_label <- as.factor(ifelse(train_data$pred >= 0.5, "1", "0"))

train_data$cardio <- as.factor(train_data$cardio)
train_data$cardio
train_data$pred_label
train_data$pred

# Confusion Matrix on Train Data
cM3 <- confusionMatrix(train_data$cardio, train_data$pred_label)
cM3

# test dataset
test_data$pred <- predict(model2_weight_cholesterol, test_data, type = "response")
test_data$pred_label <- as.factor(ifelse(test_data$pred >= 0.5, "1", "0"))
test_data$cardio <- as.factor(test_data$cardio)

#Confusion Matrix on Test Data
conf3<-confusionMatrix(test_data$cardio,test_data$pred_label, )
conf3



#define object to plot and calculate AUC
rocobj <- roc(as.ordered(test_data$cardio), as.ordered(test_data$pred_label), ordered = TRUE)

auc <- round(auc(as.ordered(test_data$cardio), as.ordered(test_data$pred_label)),4)

ggroc(rocobj, colour = 'steelblue', size = 2) +
  ggtitle(paste0('ROC Curve ', '(AUC = ', auc, ')'))

#-----------------------------------
#-------------------------------------------
#Impact of BMI and cholesterol on cardio
F_cardio$BMI
F_cardio$cardio <- as.factor(F_cardio$cardio)

model2_BMI_cholesterol <- glm(cardio ~BMI + cholesterol, family = "binomial", data = train_data)

#disable scientific notation for model summary
options(scipens=999)
summary(model2_BMI_cholesterol)
summary(model2_BMI_cholesterol)$coef

# fitting the data 
train_data$pred <- predict(model2_BMI_cholesterol, train_data, type = "response")
train_data$pred_label <- as.factor(ifelse(train_data$pred >= 0.5, "1", "0"))

train_data$cardio <- as.factor(train_data$cardio)
train_data$cardio
train_data$pred_label
train_data$pred

# Confusion Matrix on Train Data
cM4 <- confusionMatrix(train_data$cardio, train_data$pred_label)
cM4

#test dataset
test_data$pred <- predict(model2_BMI_cholesterol, test_data, type = "response")
test_data$pred_label <- as.factor(ifelse(test_data$pred >= 0.5, "1", "0"))
test_data$cardio <- as.factor(test_data$cardio)


#Confusion Matrix on Test Data
conf4<-confusionMatrix(test_data$cardio,test_data$pred_label, )
conf4

#define object to plot and calculate AUC
rocobj <- roc(as.ordered(test_data$cardio), as.ordered(test_data$pred_label), ordered = TRUE)

auc <- round(auc(as.ordered(test_data$cardio), as.ordered(test_data$pred_label)),4)

ggroc(rocobj, colour = 'steelblue', size = 2) +
  ggtitle(paste0('ROC Curve ', '(AUC = ', auc, ')'))
