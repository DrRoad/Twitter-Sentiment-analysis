# user defined function for installing and loading multiple packages in R
# checks to see if the packages are installed , install them if they are not ,
# and load them into R session.
packages = function(package){
  new_package = package[!(package %in% installed.packages()[,'Package'])]
  if(length(new_package))
    install.packages(new_package,dependencies = T)
  sapply(package , require , character.only = TRUE)
}

# 0-nuetral , 1-negative , 2-positive
original_dataset = read.csv("training-tweets-cortana.csv" , stringsAsFactors = F)
View(original_dataset)

# using user-defined function to automatially install and load the following packages
pkg = c("dplyr","tidyverse","purrrlyr","text2vec","caret","Matrix","randomForest")
packages(pkg)

set.seed(2340)
trainIndex <- createDataPartition(original_dataset$Sentiments, p = 0.8, 
                                  list = FALSE, 
                                  times = 1)
tweets.train <- original_dataset[trainIndex, ]
tweets.test <- original_dataset[-trainIndex, ]


preprocess.function <- tolower
tokenize.fun <- word_tokenizer

train.it <- itoken(tweets.train$Tweets, 
                   preprocessor = preprocess.function, 
                   tokenizer = tokenize.fun,
                   ids = tweets.train$Id,
                   progressbar = TRUE)



test.it <- itoken(tweets.test$Tweets, 
                  preprocessor = preprocess.function, 
                  tokenizer = tokenize.fun,
                  ids = tweets.test$Id,
                  progressbar = TRUE)


vocabulary = create_vocabulary(train.it)
vectorizer <- vocab_vectorizer(vocabulary)
dtm.training <- create_dtm(train.it, vectorizer)


TF.IDF <- TfIdf$new()

dtm.training.TF.IDF <- fit_transform(dtm.training, TF.IDF)

dtm.test.TF.IDF  <- create_dtm(test.it, vectorizer) %>% 
  transform(TF.IDF)


# classifier = randomForest(x = as(dtm.training.TF.IDF,"matrix"),
#                           y = as.factor(tweets.train$Sentiments),
#                           ntree = 101)

#Run to load the classifier easily.
classifier = readRDS("cortanaclassifier.rds")

y_pred = predict(classifier, newdata = as(dtm.test.TF.IDF,"matrix") )
cm = table(tweets.test[,2],y_pred)

confusionMatrix(cm)


 # saveRDS(classifier , "cortanaclassifier.rds")
 # saveRDS(tweets.train , "cortantrainingtweets.rds")
 # saveRDS(tweets.test , "cortanatestingtweets.rds")
 # saveRDS(y_pred , "cortanaprediction.rds")



#-------------------------Plotting----------------------------------------------------------

pkg = c("reshape2","ggplot2","plotrix")
packages(pkg)
#Training-data  bar-Plotting

polarityTrain = factor(tweets.train$Sentiments , 
                       labels = c("Neutral", "Negative","Positive"))

ggplot(tweets.train, 
       aes(x = polarityTrain,fill = polarityTrain)) +
  geom_bar(width = .75 ) +
  labs(x= "Polarity of Tweets" , 
       y= "Number of Tweets" , 
       title = "Polarity of tweets vs Number of tweets" )+
  scale_fill_manual("legend", 
                    values = c("Neutral" = "#FFFF00", 
                               "Negative" = "#CD2626", 
                               "Positive" = "#00BFFF"))


traincount = count(tweets.train ,tweets.train$Sentiments)
trainvector = as.vector(traincount$n) 
neutralTrain = trainvector[1]
negativeTrain = trainvector[2]
positiveTrain = trainvector[3]

# Pie-Chart Plotting for Training-data

slices <- c(negativeTrain,neutralTrain,positiveTrain)
labels <- c("Negative","Neutral" ,"Positive")
pie3D(slices, labels = labels, 
      col=c("Negative" = "#CD2626","Neutral" = "#FFFF00", "Positive" = "#00BFFF"),
      explode=0.00,
      main="Sentiments of training-data")



#Test-data   bar-Plotting after prediction

polarityTest =factor(y_pred , labels =c("Neutral","Negative","Positive"))

ggplot(tweets.test, aes(x = polarityTest, fill = polarityTest)) +
  geom_bar(width = .75 ) +
  labs(x= "Polarity of Tweets" , 
       y= "Number of Tweets" , 
       title = "Polarity of tweets vs Number of tweets" )+
  scale_fill_manual("legend", 
                    values = c("Neutral" = "#FFFF00", 
                               "Negative" = "#CD2626", 
                               "Positive" = "#00BFFF"))


testframe = as.data.frame(y_pred)
testcount = count(testframe , y_pred)
testvector = as.vector(testcount$n)
neutralTest = testvector[1]
negativeTest = testvector[2]
positiveTest = testvector[3]


# Pie-Chart Plotting for Test-data

slices <- c(negativeTest,neutralTest,positiveTest)
labels <- c("Negative","Neutral" ,"Positive")
pie3D(slices, labels = labels, 
      col=c("Negative" = "#CD2626","Neutral" = "#FFFF00", "Positive" = "#00BFFF"),
      explode=0.00, 
      main="Sentiments of test-data after prediction")


#Calculating Total positive , negative , neutral sentiments for google for further calculation
totalpositive = positiveTrain + positiveTest
totalnegative = negativeTrain + negativeTest
totalneutral  = neutralTrain  + neutralTest

Totalcortanasentiments = c("Positve" = totalpositive , 
                        "Negative" = totalnegative , 
                        "Neutral" = totalneutral)

#saveRDS(Totalcortanasentiments , file = "cortanatotal.rds")




