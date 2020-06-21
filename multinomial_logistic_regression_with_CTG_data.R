# Logistic regression model to predict whether the patient is normal (1), suspect (2) or pathologic (3. 
# data source: "https://drive.google.com/file/d/0B5W8CO0Gb2GGNC00QjEtam5vS2M/view"

# by: Mohsin Uddin

# library used
library(nnet)

# reading the data
data = read.csv(file.choose(), header = T)
str(data)
data$NSP = as.factor(data$NSP)

# partition the dataset
set.seed(123)
ind = sample(2, nrow(data), replace = T, prob = c(0.7, 0.3))
train = data[ind == 1,]
test = data[ind == 2,]

# regression model
model = multinom(NSP ~ . -MLTV -Width -Min -Max -Nmax -Tendency
                 -Median -FM -MSTV -Nzeros, data)
summary(model)


# prediction with train data
p1 = predict(model, train)
head(p1)
tab1 = table(Predicted = p1, Actual = train$NSP)
tab1
misclassification_error = 1-sum(diag(tab1))/sum(tab1)
misclassification_error

# prediction with test data
p2 = predict(model, test)
head(p2)
tab2 = table(Predicted = p2, Actual = test$NSP)
tab2
misclassification_error2 = 1-sum(diag(tab2))/sum(tab2) 
misclassification_error2

# model assessment
a = table(train$NSP)
a/sum(a)
tab1/colSums(tab1)
tab2/colSums(tab2)
