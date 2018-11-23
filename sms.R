sms_raw <- read.csv("sms_spam.csv", stringsAsFactors = FALSE)
str(sms_raw)
# 把type转化为因子
sms_raw$type <- factor(sms_raw$type)
str(sms_raw$type)
table(sms_raw$type)
# 安装package
install.packages("tm")
library(tm)
# 建立一个包含训练数据中短信的语料库
sms_corpus <- Corpus(VectorSource(sms_raw$text))
print(sms_corpus)
inspect(sms_corpus[1:3])
# 将所有字母转化为小写字母
corpus_clean <- tm_map(sms_corpus, tolower)
# 去除数字
corpus_clean <- tm_map(corpus_clean, removeNumbers)
# 去除停用词
corpus_clean <- tm_map(corpus_clean, removeWords, stopwords())
# 去除标点
corpus_clean <- tm_map(corpus_clean, removePunctuation)
# 去除空格
corpus_clean <- tm_map(corpus_clean, stripWhitespace)
inspect(sms_corpus[1:3])
inspect(corpus_clean[1:4])
# 创建稀疏矩阵
sms_dtm <- DocumentTermMatrix(corpus_clean)


# 分解原始数据库
sms_raw_train <- sms_raw[1:4168, ]
sms_raw_test  <- sms_raw[4170:5558, ]
# 输出文档—单词矩阵
sms_dtm_train <- sms_dtm[1:4168, ]
sms_dtm_test  <- sms_dtm[4170:5558, ]
# 得到语料库
sms_corpus_train <- corpus_clean[1:4168]
sms_corpus_test  <- corpus_clean[4170:5558]
# 查看子集中垃圾短信的比例
prop.table(table(sms_raw_train$type))
prop.table(table(sms_raw_test$type))
install.packages("wordcloud")
library(wordcloud)
# 形成词云
wordcloud(sms_corpus_train, min.freq = 40, random.order = FALSE)


# 获取子集
spam <- subset(sms_raw_train, type == "spam")
ham <- subset(sms_raw_train, type == "ham")
# 分别形成词云
wordcloud(spam$text, max.words = 40, scale = c(3, 0.5))
wordcloud(ham$text, max.words = 40, scale = c(3, 0.5))
# 摘出至少出现五次的词
sms_dict <- findFreqTerms(sms_dtm_train, 5)
sms_train <- DocumentTermMatrix(sms_corpus_train,list(dictionary = sms_dict))
sms_test  <- DocumentTermMatrix(sms_corpus_test,    list(dictionary = sms_dict))
# 建立函数，用来计数，并且显示yes no
convert_counts <- function(x) {
  x <- ifelse(x > 0, 1, 0)
  x <- factor(x, levels = c(0, 1), labels = c("No", "Yes"))
  return(x)
}

# 把训练集和检验集转换一下
sms_train <- apply(sms_train, MARGIN = 2, convert_counts)
sms_test  <- apply(sms_test, MARGIN = 2, convert_counts)

# 加载包以便下一步工作
install.packages("e1071")
library(e1071)
# sms_classifier变量现在包含一个可以用于预测的naiveBayes分类器对象。
sms_classifier <- naiveBayes(sms_train, sms_raw_train$type)

# 第四步，评估模型
# 用函数predict()进行预测，并将这些预测值存储在一个名为sms_test_pred的向量中
sms_test_pred <- predict(sms_classifier, sms_test)
library(gmodels)
CrossTable(sms_test_pred, sms_raw_test$type,
           prop.chisq = FALSE, prop.t = FALSE,
           dnn = c('predicted', 'actual'))
# 改变参数 令laplace=1
sms_classifier2 <- naiveBayes(sms_train, sms_raw_train$type,
                              laplace = 1)
sms_test_pred2 <- predict(sms_classifier2, sms_test)
CrossTable(sms_test_pred2, sms_raw_test$type,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))
