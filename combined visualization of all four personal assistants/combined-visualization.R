googletotal =readRDS("Googletotal.rds")
siritotal = readRDS("Siritotal.rds")
alexatotal = readRDS("alexatotal.rds")
cortanatotal = readRDS("cortanatotal.rds")
#finding total positve, negative and neutral tweets for google
googlepositive = googletotal[1]
googlenegative = googletotal[2]
googleneutral = googletotal[3]

#finding total positve, negative and neutral tweets for siri
siripositive = siritotal[1]
sirinegative = siritotal[2]
sirineutral = siritotal[3]

#finding total positve, negative and neutral tweets for cortana
cortanapositive = cortanatotal[1]
cortananegative = cortanatotal[2]
cortananeutral =  cortanatotal[3]

#finding total positve, negative and neutral tweets for Alexa
alexapositive = alexatotal[1]
alexanegative = alexatotal[2]
alexaneutral =  alexatotal[3]


Company = c("Google" , "Siri" , "Cortana" , "Alexa")
negative = c(googlenegative , sirinegative , cortananegative , alexanegative)
neutral = c(googleneutral , sirineutral , cortananeutral , alexaneutral)
Positive = c(googlepositive , siripositive , cortanapositive , alexapositive)

combined.data.frame = data.frame(Company , negative ,neutral , Positive)

library(reshape2)
library(ggplot2)

#making data compatible for visualization
data.main <- melt(combined.data.frame, id.vars='Company')



ggplot(data.main, aes(Company, value)) + 
  geom_bar(aes(fill = variable),
           width = 0.4,
           position = position_dodge(width = 0.5), 
           stat="identity") +
  geom_text(aes(label = value,y=value),position = position_dodge(0.4), vjust=-0.25,
            size=3 ,  color = "black")+
  labs(x= "Personal Assistants" , 
       y= "Number of Tweets")+
  theme(legend.position="top", 
        legend.title = element_blank(),
        axis.title.x=element_text( face="bold"), 
        axis.title.y=element_text( face="bold"))
  

ggplot(data=data.main, aes(x=Company, y=value, fill=variable)) +
  geom_bar(stat="identity")+
  geom_text(aes(label = value,y=value),position = position_stack(),
            size=3 ,  color = "black")+
  labs(x= "Personal Assistants" , 
       y= "Number of Tweets")+
  theme(legend.title = element_blank(),
        axis.title.x=element_text( face="bold"), 
        axis.title.y=element_text( face="bold"))

  





