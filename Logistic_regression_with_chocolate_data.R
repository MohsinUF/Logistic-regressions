# Logistic regression model to predict whether the response variable 
# is chocolate. 
# data source: "https://www.kaggle.com/fivethirtyeight/the-ultimate-halloween-candy-power-ranking/"

# by: Mohsin Uddin

# library used
library(tidyverse)

# reading the data
data = read.csv(file.choose(), header = T)
str(data)
data$chocolate = as.factor(data$chocolate)

# partition the dataset
ind = sample(2, nrow(data), replace = T, prob = c(0.7, 0.3))
train = data[ind == 1,]
test = data[ind == 2,]

# regression model
model = glm(chocolate ~ pricepercent + winpercent, data,
            family = "binomial")
summary(model)

# prediction with train data
p1 = predict(model, train, type = "response")
head(p1)
pred1 = ifelse(p1>0.5, 1, 0)
tab1 = table(Predicted = pred1, Actual = train$chocolate)
tab1
1-sum(diag(tab1))/sum(tab1) # miscalssification error = 14%

# prediction with test data
p2 = predict(model, test, type = "response")
head(p2)
pred2 = ifelse(p2>0.5, 1, 0)
tab2 = table(Predicted = pred2, Actual = test$chocolate)
tab2
1-sum(diag(tab2))/sum(tab2) # miscalssification error = 20.83%

# goodness-of-fit test
with(model, pchisq(null.deviance-deviance, 
                   df.null-df.residual, lower.tail = F)) # 4.138651e-12
