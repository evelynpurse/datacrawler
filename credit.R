# 读入数据，并且初步显示
credit<-read.csv("credit.csv")
str(credit)
# 查看savingaccount和checking account里面的钱
table(credit$checking_balance)
table(credit$savings_balance)
# 查看贷款金额和期限的数值特点
summary(credit$months_loan_duration)
summary(credit$amount)
# 查看违约数量
table(credit$default)
# 创建随机排序的数据集
set.seed(12345)
credit_rand <- credit[order(runif(1000)), ]
# 排序不同，但是数值的基本特征是一样的
summary(credit$amount)
summary(credit_rand$amount)
# 头部的几个数据是不一样的
head(credit$amount)
head(credit_rand$amount)
# 划分前900条为训练集，后100条为测试集
credit_train <- credit_rand[1:900, ]
credit_test  <- credit_rand[901:1000, ]
prop.table(table(credit_train$default))
prop.table(table(credit_test$default))

# 基于数据训练模型
install.packages("C50")
library(C50)
# 把第17行剔除
credit_model <- C5.0(credit_train[-17], credit_train$default)
# 查看基本参数
credit_model
summary(credit_model)


credit_pred <- predict(credit_model, credit_test)
library(gmodels)
CrossTable(credit_test$default, credit_pred,prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('actual default', 'predicted default'))
# 改变模型 令trials=10 查看结果
credit_boost10 <- C5.0(credit_train[-17], credit_train$default,trials = 10)
credit_boost10
summary(credit_boost10)
credit_boost_pred10 <- predict(credit_boost10, credit_test)
CrossTable(credit_test$default, credit_boost_pred10, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('actual default', 'predicted default'))

# 建立代价矩阵来优化模型 错误的成本不一样。给予的权重不一样
error_cost <- matrix(c(0, 1, 4, 0), nrow = 2)
error_cost
credit_cost<-C5.0(credit_train[-17], credit_train$default, costs = error_cost)
credit_cost_pred <- predict(credit_cost, credit_test)
CrossTable(credit_test$default, credit_cost_pred, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,dnn = c('actual default', 'predicted default'))
