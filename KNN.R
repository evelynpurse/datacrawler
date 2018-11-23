wbcd <- read.csv("wisc_bc_data.csv", stringsAsFactors = FALSE)
# 数据导入数据框
str(wbcd)
# 检验数据是否已经读入
wbcd
# 剔除ID列
wbcd<-wbcd[-1]
table(wbcd$diagnosis)
# 将诊断结果转换为因子
wbcd$diagnosis<-factor(wbcd$diagnosis,levels = c("B","M"))
# 诊断结果的描述性统计,查看阴性阳性比例
round(prop.table(table(wbcd$diagnosis))*100,digit=2)
# 观察半径、面积和光滑度
summary(wbcd[c("radius_mean","area_mean","smoothness_mean")])
# 创建normalize函数
normalize<-function(x){
  return(
    (x-min(x))/(max(x)-min(x))
  )
}
# 用labby函数将normalize应用到数据框中
wbcd_n <- as.data.frame(lapply(wbcd[2:31], normalize))
# 查看转换后的汇总统计量
summary(wbcd_n)
# 把wcdb_n数据框拆分为wbcd_train数据框和wbcd_test数据框
wbcd_train <- wbcd_n[1:469, ]
wbcd_test <- wbcd_n[470:569, ]
# 剔除诊断结果，把诊断结果单独放在一边
wbcd_train_label<-wbcd[1:469,1]
wbcd_test_label<-wbcd[470:569,1]


# 安装class包
install.packages("class")
library(class)
# K=21 试验
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,
                      cl = wbcd_train_label, k=21)

# 评估模型
install.packages("gmodels")
library(gmodels)
CrossTable(x = wbcd_test_label, y = wbcd_test_pred,prop.chisq=FALSE)
# prop.chisq=FALSE 除去不需要的卡方

# 提高模型的性能
# 1.利用z-score来优化,结果显示拟合效果更差
wbcd_z <- as.data.frame(scale(wbcd[-1]))
summary(wbcd_z[c("area_mean","radius_mean")])
wbcd_train <- wbcd_z[1:469, ]
wbcd_test <- wbcd_z[470:569, ]
wbcd_train_label <- wbcd[1:469, 1]
wbcd_test_label <- wbcd[470:569, 1]
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,
                        cl = wbcd_train_label, k=21)
CrossTable(x = wbcd_test_label, y = wbcd_test_pred,
             prop.chisq=FALSE)
# 2.改变K值
# K=1 识别错误的概率为4%
wbcd_test_pred1 <- knn(train = wbcd_train, test = wbcd_test,
                      cl = wbcd_train_label, k=1)
CrossTable(x = wbcd_test_label, y = wbcd_test_pred1,prop.chisq=FALSE)
#k=5 识别错误的概率为2%
wbcd_test_pred5 <- knn(train = wbcd_train, test = wbcd_test,
                       cl = wbcd_train_label, k=5)
CrossTable(x = wbcd_test_label, y = wbcd_test_pred5,prop.chisq=FALSE)
#k=11 识别错误的概率3%
wbcd_test_pred11 <- knn(train = wbcd_train, test = wbcd_test,
                       cl = wbcd_train_label, k=11)
CrossTable(x = wbcd_test_label, y = wbcd_test_pred11,prop.chisq=FALSE)
#k=15 3%
wbcd_test_pred15 <- knn(train = wbcd_train, test = wbcd_test,
                       cl = wbcd_train_label, k=15)
CrossTable(x = wbcd_test_label, y = wbcd_test_pred15,prop.chisq=FALSE)
#k=27 4%
wbcd_test_pred27 <- knn(train = wbcd_train, test = wbcd_test,
                       cl = wbcd_train_label, k=27)
CrossTable(x = wbcd_test_label, y = wbcd_test_pred27,prop.chisq=FALSE)
