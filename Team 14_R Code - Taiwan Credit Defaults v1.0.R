# Installing and loading the following libraries
packages <- c("readxl","data.table","cowplot","grid","caret","randomForest","e1071",
              "dplyr","pROC","ROCR","ggthemes","ggplot2","RColorBrewer","scales","glmnet")
for (i in packages){
  if(!require(i)){
    install.packages(i)
  }
}
lapply(packages, library, character.only = TRUE)

# Set the working directory and load data.
setwd("/Users/chunhsiangchang/Desktop/predictive modeling/group project/data")
banking_data <- read.csv('Taiwan_credit_default_data_final_v1.0.csv')

# Make a copy of the data for analysis.
data_u<-banking_data

###########################################
# Data Exploration
##########################################

#1. Demographics data - Age, Sex, Education and marital status

theme_set(theme_classic())

# Plotting % of credit card holders by education
x<-data_u[,.(count=.N/nrow(data_u)), by=EDUCATION] [order(-count)]
g1<-ggplot(x, aes(x=reorder(EDUCATION,-count),y=count, fill=EDUCATION))+
  geom_bar(stat="identity",width = 0.4, fill ="turquoise")+
  labs(title="Distribution of Credit Card Holders by Education")+
  theme_classic()+
  theme(axis.text.x = element_text(colour="grey20",size=12,angle=0,hjust=0.5,vjust=0.2),
        axis.text.y = element_text(colour="grey20",size=12,angle=0),
        axis.title.x = element_text(colour="grey20",size=12,hjust=0.5),
        axis.title.y = element_text(colour="grey20",size=12,hjust=0.5))+
  ylab("\n % of credit card holders")+ xlab("\n Education")+ 
  scale_y_continuous(expand = c(0,0),labels=scales::percent)
plot(g1)

# Plotting % of credit card holders by gender
y<-data_u[,.(count=.N/nrow(data_u)), by=SEX] [order(-count)]
g2<-ggplot(y, aes(x=SEX,y=count, fill=SEX))+
  geom_bar(stat="identity",width = 0.4, fill ="#ffa500")+
  labs(title="Distribution of Credit Card Holders by Gender")+
  theme_classic()+
  theme(axis.text.x = element_text(colour="grey20",size=12,angle=0,hjust=0.5,vjust=0.2),
        axis.text.y = element_text(colour="grey20",size=12,angle=0),
        axis.title.x = element_text(colour="grey20",size=12,hjust=0.5),
        axis.title.y = element_text(colour="grey20",size=12,hjust=0.5))+
  ylab("\n % of credit card holders")+ xlab("\n Gender")+ 
  scale_y_continuous(expand = c(0,0),labels=scales::percent)
plot(g2)

# Plotting % of credit card holders by marital status
z<-data_u[,.(count=.N/nrow(data_u)), by=MARRIAGE] [order(-count)]
g3<-ggplot(z, aes(x=reorder(MARRIAGE,-count),count,fill=MARRIAGE))+
  geom_bar(stat="identity",width = 0.4, fill ="#01579B")+
  labs(title="Distribution of Credit Card Holders by Marital Status")+
  theme_classic()+
  theme(axis.text.x = element_text(colour="grey20",size=12,angle=0,hjust=0.5,vjust=0.2),
        axis.text.y = element_text(colour="grey20",size=12,angle=0),
        axis.title.x = element_text(colour="grey20",size=12,hjust=0.5),
        axis.title.y = element_text(colour="grey20",size=12,hjust=0.5))+
  ylab("\n % of credit card holders")+ xlab("\n Marital staus")+ 
  scale_y_continuous(expand = c(0,0),labels=scales::percent)
plot (g3)

# Age distribution by default status
g_age <- ggplot(data_u, aes(data_u$AGE))+ 
  geom_density(aes(fill=factor(data_u$default.payment.next.month)), alpha=0.3) + 
  labs(title="Distribution of Age by Default Payment Status", 
       x="Age",fill="Default Payment Next Month") + 
  scale_y_continuous(expand = c(0,0))+
  scale_fill_manual(values=c("blue","#ADB6B5"))
plot(g_age)

# Gender by default status
a <- data_u %>% 
  group_by(SEX,default.payment.next.month) %>% 
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))

gender_default<-ggplot(a, aes(x=SEX ,y=perc, fill= default.payment.next.month))+
  geom_bar(stat="identity", position = "fill",  width = 0.4)+
  labs(title="Default Status by Gender")+
  theme_classic()+
  theme(axis.text.x = element_text(colour="grey20",size=12,angle=0,hjust=0.5,vjust=0.2),
        axis.text.y = element_text(colour="grey20",size=12,angle=0),
        axis.title.x = element_text(colour="grey20",size=12,hjust=0.5),
        axis.title.y = element_text(colour="grey20",size=12,hjust=0.5))+
  ylab("\n % credit card holders")+ xlab("\n Gender")+ 
  scale_y_continuous(expand = c(0,0),labels=percent)
plot(gender_default)

# Credit limit by default status
c_limit <- ggplot(data_u, aes(data_u$LIMIT_BAL))+ 
  geom_density(aes(fill=factor(data_u$default.payment.next.month)), alpha=0.3) + 
  labs(title="Distribution of Credit Limit by Default Payment Status", 
       x="Credit Limit",fill="Default Payment Next Month") + 
  scale_y_continuous(expand = c(0,0))+
  scale_fill_manual(values=c("blue","#ADB6B5"))
plot(c_limit)

###########################################
# Analysis
##########################################

set.seed(200)

factor_vars = c(2, 3, 4, c(26:32))

for (i in factor_vars) {
  data_u[[i]]<-as.factor(data_u[[i]])
}

intraining <- createDataPartition(y=data_u$default.payment.next.month, p=0.8, list=F)
data_u<-na.omit(data_u)

train.batch <- data_u[intraining,]

test.batch <- data_u[-intraining,]

test.batch$default.payment.next.month<-as.factor(test.batch$default.payment.next.month)
train.batch$default.payment.next.month<-as.factor(train.batch$default.payment.next.month)


model_matrix = data_u[, -c(5:11)]
model_X = model.matrix(default.payment.next.month ~ . + SEX * MARRIAGE + MARRIAGE * Age_buckets
                       + Age_buckets * EDUCATION, data_u)

var_names = names(model_X[1,])
var_count = rep(0, length(var_names))
######################################
### Choosing variables using LASSO ###
######################################

for(i in c(1:20)){
  train = sample(c(1:dim(data_u)[1]), size= (dim(data_u)[1])*0.8) 
  Y_train = data_u$default.payment.next.month[train]
  cvfit.lasso = cv.glmnet(model_X[train,],Y_train,
                          alpha=1, family = "binomial", standardize = TRUE)
  
  lasso_coef = predict(cvfit.lasso, newx = model_X[-train,],
                       s = "lambda.min", type="coefficients")
  var_used_index = lasso_coef[-2]
  var_used_index = ifelse(var_used_index ==0, FALSE, TRUE)
  var_count[var_used_index] = var_count[var_used_index] +1
  print(paste0("done", i))
}

# Plotting the results of logistic regression.
train.batch$default.payment.next.month = ifelse(
  train.batch$default.payment.next.month == 'Defaulters',1, 0)

logit.results = glm(default.payment.next.month~
                    LIMIT_BAL + PAY_AMT1 + PAY_AMT2 + PAY_AMT3 + 
                      PAY_AMT4 + PAY_AMT5 + PAY_AMT6 +
                      PAY_1 + PAY_2 + PAY_3 + PAY_4 + PAY_5 + PAY_6 + 
                      pay_spend_ratio + BILL_AMT2 + AGE+SEX + EDUCATION + 
                      MARRIAGE + SEX*MARRIAGE + SEX*MARRIAGE +
                      MARRIAGE* AGE + EDUCATION * AGE,
                    data=train.batch, family = binomial(link = 'logit'))
summary(logit.results)

hist(var_count)
var_used_index_n = ifelse(var_count >= 19, TRUE, FALSE)
summary(var_used_index_n)
names(data.frame(model_X))[var_used_index_n]

train.batch$default.payment.next.month = ifelse(
  train.batch$default.payment.next.month == 1, "Defaulters", "Non-defaulters")
train.batch$default.payment.next.month = as.factor(train.batch$default.payment.next.month)

############################################
### Building three models for prediction ###
############################################
#train.batch$default.payment.next.month = as.character()

set.seed(200)
rand.model <- randomForest(default.payment.next.month ~ LIMIT_BAL+EDUCATION+MARRIAGE+
                             AGE+PAY_AMT1+PAY_AMT2+PAY_AMT3+PAY_AMT4+PAY_AMT5+
                             PAY_AMT6 + BILL_AMT2 +PAY_1+PAY_2+PAY_3+PAY_4+ PAY_5+
                             PAY_6+ SEX + pay_spend_ratio,
                           data=train.batch, importance=T,ntree= 500, keep.forest=T)


#rand.model$importance[order(rand.model$importance[,3], decreasing = TRUE),]

plot(rand.model)
varImpPlot(rand.model)



 naive = naiveBayes(default.payment.next.month ~ ., data=train.batch, type="raw")


train.batch$default.payment.next.month = ifelse(
  train.batch$default.payment.next.month == 'Defaulters',1, 0)
logit.model <- glm(default.payment.next.month~ PAY_1 +pay_spend_ratio+ PAY_AMT1  + BILL_AMT2+
                     PAY_2 +LIMIT_BAL +  PAY_3+PAY_AMT2+PAY_4 + PAY_5 + PAY_AMT3+ 
                     PAY_AMT5 + PAY_6 + PAY_AMT6 + PAY_AMT4+ AGE 
                   ,data=train.batch, family = binomial(link = "logit"))

train.batch$default.payment.next.month = ifelse(
  train.batch$default.payment.next.month == 1, "Defaulters", "Non-defaulters")
train.batch$default.payment.next.month = as.factor(train.batch$default.payment.next.month)



######For Confusion matrices & ROCs

rand.pred <- predict(rand.model, test.batch, type="class")
rand.prob <- predict(rand.model, test.batch, type='response')
rand.prob1 <- predict(rand.model, test.batch, type='prob')[,1]

thresholdr1 <- 0.32
pred1      <- factor( ifelse(rand.prob1 > thresholdr1,  "Defaulters", "Non-defaulters") )
pred4 <- prediction(rand.prob1,test.batch$default.payment.next.month)
confusionMatrix(pred1, test.batch$default.payment.next.month)


prediction.naive = predict(naive, test.batch[,-25], type = c("raw"))
naive.prob1 <- predict(naive, test.batch, type='raw')
naive.prob1<-data.table((naive.prob1))

thresholdr3 <- 0.9995
pred3      <- factor( ifelse(naive.prob1$`Defaulters` > thresholdr3, "Defaulters", "Non-defaulters") )
confusionMatrix(pred3, test.batch$default.payment.next.month)


logit.prob <- predict(logit.model, test.batch)
logit.prob1 <- predict(logit.model, test.batch, type='response')
head(logit.prob1)
thresholdr2 <- 0.57
pred2      <- factor( ifelse(logit.prob1 > thresholdr2,  "Defaulters", "Non-defaulters") )
pred5 <- prediction(logit.prob,test.batch$default.payment.next.month)
confusionMatrix(pred2, test.batch$default.payment.next.month)


###Confusion matrix Random Forest
confusionMatrix(pred1, test.batch$default.payment.next.month)
###Confusion matrix naive
confusionMatrix(pred3, test.batch$default.payment.next.month)

###Confusion matrix Logit model
confusionMatrix(pred2, test.batch$default.payment.next.month)



rf.perf1 <- performance(pred4,"tpr","fpr")


plot.roc(test.batch$default.payment.next.month,rand.prob1 , col="blue",lwd=2, print.auc=TRUE,print.auc.y = 0.4)
plot.roc(test.batch$default.payment.next.month, prediction.naive[,1], col="red",
         lwd=2, print.auc=TRUE, print.auc.y = 0.3, add=TRUE)
plot.roc(test.batch$default.payment.next.month, logit.prob, col="black",
         lwd=2, print.auc = TRUE, print.auc.y = 0.35, add=TRUE)

####################################################################################
### Finding the cutoff the has good specificity while remaining good sensitivity ###
####################################################################################
nums_var = c(5, 10, 15)
improve_var = NULL
improve_thresh = NULL
improve_sen = NULL
improve_spec = NULL
for( num in nums_var){
  rand.model <- randomForest(default.payment.next.month~. ,data=train.batch[,-c(26:32)], importance=T,
                             ntree=200, mtry = num , keep.forest=T)
  rand.prob1 <- predict(rand.model, test.batch, type='prob')[,1]
  max = 0
  for(i in c(10:95)){
    pred1 = factor( ifelse(rand.prob1 > i/100, "Defaulters", "Non-defaulters") )
    cf_matrix = confusionMatrix(pred1, test.batch$default.payment.next.month)
    if( cf_matrix$byClass[1] > 0.80 && cf_matrix$byClass[2] > 0.45) {
      print("bang")
      improve_var = c(num, improve_var)
      improve_thresh = c(i, improve_thresh)
      improve_sen = c(cf_matrix$byClass[1], improve_sen)
      improve_spec = c(cf_matrix$byClass[2], improve_spec)
    }
  }
  print(paste("done:", num))
}
#45, 85, 80

improve_var      #the number of variables used
improve_thresh   # the cutoffs
improve_sen      #the sensitivity 
improve_spec     #the specificity 

