# Logistic regression model to predict whether the student got admitted
# data source: "https://drive.google.com/file/d/0B5W8CO0Gb2GGVjRILTdWZkpJU1E/view"

# by: Mohsin Uddin

# reading the data
data = read.csv(file.choose(), header = T)
str(data)
data$admit = as.factor(data$admit)
data$rank = as.factor(data$rank)

# partition the dataset
ind = sample(2, nrow(data), replace = T, prob = c(0.7, 0.3))
train = data[ind == 1,]
test = data[ind == 2,]

# regression model
model = glm(admit ~ gpa + rank, data,
            family = "binomial")
summary(model)

# prediction with train data
p1 = predict(model, train, type = "response")
head(p1)
pred1 = ifelse(p1>0.5, 1, 0)
tab1 = table(Predicted = pred1, Actual = train$admit)
tab1
1-sum(diag(tab1))/sum(tab1) # miscalssification error = 27.56%

# prediction with test data
p2 = predict(model, test, type = "response")
head(p2)
pred2 = ifelse(p2>0.5, 1, 0)
tab2 = table(Predicted = pred2, Actual = test$admit)
tab2
1-sum(diag(tab2))/sum(tab2) # miscalssification error = 27.35%

# goodness-of-fit test
with(model, pchisq(null.deviance-deviance, 
                   df.null-df.residual, lower.tail = F)) # 1.716814e-07
