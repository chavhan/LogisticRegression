## Logistic regression for bank dataset 
library(psych)
library(tidyr)
bank %>% drop_na()    ## check for na values 
dim(bank)
names(bank)
head(bank)
attach(bank)
## changing y,housing,load,default vaue into numeric
bank$y <- as.numeric(as.factor(bank$y))
bank$housing <- as.numeric(as.factor(bank$housing))
bank$loan <- as.numeric(as.factor(bank$loan))
bank$default <- as.numeric(as.factor(bank$default))

## changing job,marital,education,contact,poutcome into dummy variable 
bank$job <- dummy(bank$job,sep = ".")
head(bank)
bank$marital <- dummy(bank$marital,sep = ".")
bank$education <- dummy(bank$education,sep = ".")
bank$contact <- dummy(bank$contact,sep = ".")
bank$poutcome <- dummy(bank$poutcome,sep = ".")

## chaning values of month from lower case to camel case 
camel <- function(x){ #function for camel case
  capit <- function(x) paste0(toupper(substring(x, 1, 1)), substring(x, 2, nchar(x)))
  sapply(strsplit(x, "\\."), function(x) paste(capit(x), collapse=""))
}
bank$month <- camel(bank$month)
bank$month <- match(bank$month,month.abb)
write.csv(bank, file="bank_filtered_data.csv")

testData <- bank[1:500,]
View(head(testData))
class(testData$education)
str(testData)
bank_filtered$job.job.unknown <- NULL
bank_filtered$marital.marital.single <- NULL
bank_filtered$education.education.unknown <- NULL
bank_filtered$contact.contact.unknown <- NULL
bank_filtered$poutcome.poutcome.unknown <- NULL

names(bank_filtered)
model <- glm(bank_filtered$y~.,data = bank_filtered, family = 'binomial')
summary(model)
prob <- predict(model,type = "response",bank_filtered)
prob
confusion <- table(prob > 0.5,bank_filtered$y)
accuray <- sum(diag(confusion)/sum(confusion))
accuray
confusion
head(bank_filtered)

## applying pca concepts with pccomp
pc <- prcomp(bank_filtered,center = TRUE,scale. = TRUE)
attributes(pc)
print(pc)
pc$x
testData <- bank_filtered[1:500,]
pairs.panels(pc$x,
             gap=0,
             bg = c("red", "yellow", "blue")[testData$y],
             pch=21)
## applying pca concepts with princomp
pcaObj<-princomp(bank_filtered, cor = TRUE, scores = TRUE, covmat = NULL)
summary(pcaObj)
loadings(pcaObj)
plot(pcaObj)
pca_data <- pcaObj$scores[,1:28]
str(pca_data)
head(pca_data)
pca_data1 <- as.data.frame(pca_data)
head(pca_data1$Comp.1)
bank_filtered_upgrd <- cbind(bank_filtered,pca_data1)
head(bank_filtered_upgrd)
write.csv(bank_filtered_upgrd,'bank_filtered_upgrd.csv')
## applying glm function into pca data 
model.pca <- glm(bank_filtered_upgrd$y~bank_filtered_upgrd$Comp.1+
             bank_filtered_upgrd$Comp.2+
             bank_filtered_upgrd$Comp.3+
             bank_filtered_upgrd$Comp.4+
             bank_filtered_upgrd$Comp.5+
             bank_filtered_upgrd$Comp.6+
             bank_filtered_upgrd$Comp.7+
             bank_filtered_upgrd$Comp.8+
             bank_filtered_upgrd$Comp.9+
             bank_filtered_upgrd$Comp.10+
             bank_filtered_upgrd$Comp.11+
             bank_filtered_upgrd$Comp.12+
             bank_filtered_upgrd$Comp.13+
             bank_filtered_upgrd$Comp.14+
             bank_filtered_upgrd$Comp.15+
             bank_filtered_upgrd$Comp.16+
             bank_filtered_upgrd$Comp.17+
             bank_filtered_upgrd$Comp.18+
             bank_filtered_upgrd$Comp.19+
             bank_filtered_upgrd$Comp.20+
             bank_filtered_upgrd$Comp.21+
             bank_filtered_upgrd$Comp.22+
             bank_filtered_upgrd$Comp.23+
             bank_filtered_upgrd$Comp.24+
             bank_filtered_upgrd$Comp.25+
             bank_filtered_upgrd$Comp.26+
             bank_filtered_upgrd$Comp.27+
             bank_filtered_upgrd$Comp.28,data = bank_filtered_upgrd, family = 'binomial')
summary(model.pca)
prob1 <- predict(model.pca,type = "response",bank_filtered_upgrd)
prob1
confusion1 <- table(prob1 > 0.5,bank_filtered_upgrd$y)
confusion1
accuray <- sum(diag(confusion1)/sum(confusion1))
accuray
