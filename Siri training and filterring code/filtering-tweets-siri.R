# user defined function for installing and loading multiple packages in R
# checks to see if the packages are installed , install them if they are not ,
# and load them into R session.
packages = function(package){
  new_package = package[!(package %in% installed.packages()[,'Package'])]
  if(length(new_package))
    install.packages(new_package,dependencies = T)
  sapply(package , require , character.only = TRUE)
}

# using user-defined function to automatially install and load the following packages
pkg = c("stringr", "syuzhet","dplyr")
packages(pkg)

# set the R session to the current working directory
#setwd("")
# Load the dataset which is a .csv(comma separated) file 
original_dataset = read.csv("SiriTweets.csv" ,stringsAsFactors = F)
#fetching only the text section of the tweets dataset and converting it to a dataframe
original_dataset = as.data.frame(original_dataset$Tweets)
# removing the NA values as some tweets may have NA values
original_dataset = original_dataset %>% na.omit()
# setting the column name of the dataframe to Tweets for our convenience
colnames(original_dataset) = c("Tweets")

#----------------------filtering tweets---------------------------------------------

# using iconv function to remove latin and ASCII characters in tweets  
tweets.dataframe = iconv(original_dataset$Tweets , "latin1" ,"ASCII" ,sub = "")
# using gsub funtion to remove urls from tweets
tweets.dataframe = gsub(" ?(f|ht)tp(s?)://(.*)[.][a-z]+/[a-z,A-Z,0-9]*{8}", "", tweets.dataframe)
# using str_replace function from stringr package to remove retweet(RT) handle from 
#tweets
tweets.dataframe =  str_replace(tweets.dataframe,"RT @[a-z,A-Z]*: ","")
# using str_replace function from stringr package to remove hashtag(#) handle from 
#tweets
tweets.dataframe =  str_replace(tweets.dataframe,"RT[a-z,A-Z]*","")
tweets.dataframe = str_replace_all(tweets.dataframe,"#[a-z,A-Z]*","")
# using str_replace function from stringr package to remove @username handle from 
#tweets
tweets.dataframe = str_replace_all(tweets.dataframe,"@[a-z,A-Z]*","")
# using gsub funtion to remove punctuations from tweets 
tweets.dataframe = gsub("[[:punct:]]", "", tweets.dataframe)
# using gsub funtion to remove digits from tweets
tweets.dataframe = gsub("[[:digit:]]", "", tweets.dataframe)
# replacing all multiple spaces with single spaces or stripping multiple white-spaces
tweets.dataframe = str_replace_all(tweets.dataframe,"  "," ")
# stripping whitespace in the begining of tweets (if-any)
tweets.dataframe = gsub("^ ", "", tweets.dataframe)
# stripping whitespace at the end of tweets (if-any)
tweets.dataframe = gsub(" $", "", tweets.dataframe)

dataframe = as.data.frame(tweets.dataframe)

#----------------------calculating sentiments-----------------------------------------

# converting tweets.dataframe to a vector to detect the sentiment of each tweet 
#using syuzhet package
tweets.vector = as.vector(tweets.dataframe)
# calculating sentiments of each tweet using get_sentiment() function in 
#syuzhet package
sentiments.value = get_sentiment(tweets.vector)
# assigning particular values to the sentiments-calculated for using them as 
# categorical variables in machine-learning algoritms. 
sentiment.category =  ifelse(sentiments.value < 0, 1, ifelse(sentiments.value > 0, 2, 0))
# adding these sentiment values as a categorical column in the original dataset
dataframe$Sentiments = sentiment.category
# using write.csv function to write and save the modified dataset in the
# system. Now the data is ready to be fed into a machine learning algorithm for
# further analysis
colnames(dataframe) = c("Tweets" , "Sentiments")
write.csv(dataframe, "training-tweets-siri.csv",row.names = F)
