#STAT 228 FINAL PROJECT

mh.df <- read.csv(file.choose(), header = TRUE)
head(mh.df)
colnames(mh.df)
attach(mh.df)

#Data Preparation and Binarization

threshold <- quantile(mh.df$Depression_Score, 0.75) #Try 75th percentile

mh.df$Risk <- ifelse(mh.df$Depression_Score >= threshold, "High", "Low")
mh.df$Risk <- factor(mh.df$Risk, levels = c("Low", "High"))

mh.clean <- mh.df[, names(mh.df) != "Depression_Score"]

table(mh.clean$Risk)

numeric_col <- sapply(mh.clean, is.numeric)
mh.numeric <- mh.clean[,numeric_col]
numeric_cols <- colnames(mh.numeric)

#VIF Screening 
library(usdm)
vifstep(mh.numeric, th = 10) #No predictors violate multicollinearity

# Identify categorical columns (Factors/Characters)
cat_cols <- names(mh.clean)[sapply(mh.clean, function(x) is.factor(x) || is.character(x))]
cat_cols

final_predictors <- c(numeric_cols, cat_cols)
final.df <- mh.clean[,final_predictors]

#Box M Test 
library(biotools)
numeric_final <- final.df[, sapply(final.df, is.numeric)]
box_m_test <- boxM(numeric_final, final.df$Risk)
box_m_test #LDA is better than QDA because p-value = 0.3294

# Check 'Anxiety_Score' normality for each Risk group
high_risk <- final.df[final.df$Risk == "High", "Anxiety_Score"]
low_risk  <- final.df[final.df$Risk == "Low", "Anxiety_Score"]

par(mfrow=c(1,2)) # Side-by-side plots
qqnorm(high_risk, main="Q-Q: High Risk"); qqline(high_risk, col="red")
qqnorm(low_risk, main="Q-Q: Low Risk"); qqline(low_risk, col="blue")






#Train/Test Data
set.seed(123)

index = sample(1:nrow(final.df), 0.7*nrow(final.df))
train.df = final.df[index,]
test.df = final.df[-index,]

#Model Building 
#Logit Model
logit.model <- glm(Risk~., data = train.df, family = "binomial")
summary(logit.model)
logit.probs = predict(logit.model, newdata = test.df, type = "response")
logit.pred = factor(ifelse(logit.probs >0.25, "High", "Low"))

conf_matrix_logit = table(logit.pred, test.df$Risk)


#LDA and QDA model
library(MASS)
lda.model <- lda(Risk~., data = train.df)
lda.pred = predict(lda.model, newdata = test.df)$class
conf_matrix_lda <- table(lda.pred, test.df$Risk)



qda.model <- qda(Risk~., data = train.df)
qda.pred = predict(qda.model, newdata = test.df)$class
conf_matrix_qda = table(qda.pred, test.df$Risk)










#Performance Metrics
#Accuracy 
logit_acc = sum(diag(conf_matrix_logit))/sum(conf_matrix_logit) #0.5972222
lda_acc = sum(diag(conf_matrix_lda))/sum(conf_matrix_lda) #0.7138889
qda_acc = sum(diag(conf_matrix_qda))/sum(conf_matrix_qda) #0.6333333




#ROC-AUC 
library('pROC')
numeric_pred = as.numeric(as.character(logit.pred))
plot(roc(test.df$Risk, logit.pred))
logit_auc = auc(test.df$Risk, logit.pred)
plot(roc(test.df$Risk, lda.pred))
lda_auc = auc(test.df$Risk, lda.pred)
plot(roc(test.df$Risk, qda.pred))
qda_auc = auc(test.df$Risk, qda.pred)

#Recall
recall_logit = diag(conf_matrix_logit)/rowSums(conf_matrix_logit)
recall_lda = diag(conf_matrix_lda)/rowSums(conf_matrix_lda)
recall_qda = diag(conf_matrix_qda)/rowSums(conf_matrix_qda)




#F1-score
library(caret)
conf_logit = confusionMatrix(logit.pred, test.df$Risk, mode = "everything")
f1_logit = conf_logit$Class["F1"]

conf_lda = confusionMatrix(lda.pred, test.df$Risk, mode = "everything")
f1_lda = conf_lda$Class["F1"]

conf_qda = confusionMatrix(qda.pred, test.df$Risk, mode = "everything")
f1_qda = conf_qda$Class['F1']





