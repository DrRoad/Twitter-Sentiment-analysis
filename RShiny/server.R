shinyServer(
  function(input , output ,session){
    packages = function(package){
      new_package = package[!(package %in% installed.packages()[,'Package'])]
      if(length(new_package))
        install.packages(new_package,dependencies = T)
      sapply(package , require , character.only = TRUE)
    }
    pkg = c("reshape2","ggplot2","plotrix","dplyr","caret","Matrix")
    packages(pkg)

    
#=======================Google-Plots===============================================
#========Plotting Bar graph and Pie Chart For Google Training Dataset==============
    
    
    output$`training-plot-google` = renderPlot({
      plotType = input$`taining-dataset-plots-google`
      if(is.null(plotType)){
        return(NULL)
      }else if(plotType == 1){
        google.training.dataset = readRDS("googletrainingtweets.rds")
        polarity.Train.google = factor(google.training.dataset$Sentiments , 
                                       labels = c("Neutral", "Negative","Positive"))
        ggplot(google.training.dataset, 
               aes(x = polarity.Train.google,
                   fill = polarity.Train.google)) +
          geom_bar(width = .75 ) +
          labs(title = "Polarity of Tweets vs Number of Tweets of Training-Dataset",
               subtitle = "Tweets from 02-06-2018 to 28-06-2018 have been Analysed here.",
              x= "Polarity of Tweets" , 
              y= "Number of Tweets"  
               )+
          theme(legend.title = element_text(face = "bold"),
                axis.title.x=element_text( face="bold"), 
                axis.title.y=element_text( face="bold"),
                plot.title = element_text(face = "bold")
                
                )+
          theme_gray(base_size = 14)+
          scale_fill_manual("Sentiments", 
                            values = c("Neutral" = "#FFFF00", 
                                       "Negative" = "#CD2626", 
                                       "Positive" = "#00BFFF"))
      }
      else if(plotType == 2){
        
        google.training.dataset = readRDS("googletrainingtweets.rds")
        traincount = count(google.training.dataset ,
                                 google.training.dataset$Sentiments)
        trainvector = as.vector(traincount$n) 
        neutralTrain = trainvector[1]
        negativeTrain = trainvector[2]
        positiveTrain = trainvector[3]
          slices = c(negativeTrain,neutralTrain,positiveTrain)
          labels = c("Negative","Neutral" ,"Positive")
          pct =round((slices/sum(slices))*100)
          labels = paste(labels , pct)
          labels = paste(labels , "%" , sep = "")
        pie3D(slices, labels = labels, 
                col=c("Negative" = "#CD2626","Neutral" = "#FFFF00", "Positive" = "#00BFFF"),
                explode=0.1,
                main="Sentiments of training-data")
      }
        })


#========Plotting Bar graph and Pie Chart For Google prediction Dataset==============
    
    
    output$`prediction-plot-google` = renderPlot({
      plotType = input$`test-dataset-plots-google`
      if(is.null(plotType)){
        return(NULL)
      }else if(plotType == 1){
        google.test.dataset = readRDS("googletestingtweets.rds")
        google.predictor = readRDS("googleprediction.rds")
        polarityTest =factor(google.predictor , labels =c("Neutral","Negative","Positive"))
        ggplot(google.test.dataset, aes(x = polarityTest, fill = polarityTest)) +
          geom_bar(width = .75 ) +
          labs(title = "Polarity of Tweets vs Number of Tweets of Prediction-Dataset",
               subtitle = "Tweets from 02-06-2018 to 28-06-2018 have been Analysed here.",
               x= "Polarity of Tweets" , 
               y= "Number of Tweets"  
          )+
          theme(legend.title = element_text(face = "bold"),
                axis.title.x=element_text( face="bold"), 
                axis.title.y=element_text( face="bold"),
                plot.title = element_text(face = "bold")
                
          )+
          theme_gray(base_size = 14)+
          scale_fill_manual("Sentiments", 
                            values = c("Neutral" = "#FFFF00", 
                                       "Negative" = "#CD2626", 
                                       "Positive" = "#00BFFF"))
        
      }
      else if(plotType == 2){
        google.predictor = readRDS("googleprediction.rds")
        testframe = as.data.frame(google.predictor)
        testcount = count(testframe , google.predictor)
        testvector = as.vector(testcount$n)
        neutralTest = testvector[1]
        negativeTest = testvector[2]
        positiveTest = testvector[3]
        
        slices <- c(negativeTest,neutralTest,positiveTest)
        labels <- c("Negative","Neutral" ,"Positive")
        pct =round((slices/sum(slices))*100)
        labels = paste(labels , pct)
        labels = paste(labels , "%" , sep = "")
        pie3D(slices, labels = labels, 
              col=c("Negative" = "#CD2626","Neutral" = "#FFFF00", "Positive" = "#00BFFF"),
              explode=0.1, 
              main="Sentiments of test-dataset after prediction")
        
        
      }
    })
    
    
#==========Show Training and Prediction DataTables For Google==========================
    
output$`showdata-google` <- DT::renderDataTable({
  selectionType = input$`show-dataset-google`
  if(is.null(selectionType)){
    return()
  }else if(selectionType == 1){
     google.training.dataset = readRDS("googletrainingtweets.rds")
     training_sample <- google.training.dataset %>%
        sample_n(input$`observations-google`)
      DT::datatable(data = training_sample, 
                    options = list(pageLength = 10), 
                    rownames = FALSE)
  }else if(selectionType == 2){
    google.test.dataset = readRDS("googletestingtweets.rds")
    testing_sample <- google.test.dataset %>%
      sample_n(input$`observations-google`)
    DT::datatable(data = testing_sample, 
                  options = list(pageLength = 10), 
                  rownames = FALSE)
    }
    })
    

#=========Show Summary and Confusion-Matrix For Google-Classifier ==================
    
    
    output$`showsummary-google` <- renderPrint({
      selectionType = input$`show-summary-google`
      if(is.null(selectionType)){
        return()
      }else if(selectionType == 1){
        google.classifier = readRDS("googleclassifier.rds")  
        summary(google.classifier)
      }else if(selectionType == 2){
        google.predictor = readRDS("googleprediction.rds")
        google.test.dataset = readRDS("googletestingtweets.rds")
        cm = table(google.test.dataset[,2],google.predictor)
        confusionMatrix(cm)
        }
    })
    

#=======================Alexa-Plots==================================================
#========Plotting Bar graph and Pie Chart For Alexa Training Dataset==============
    
    
    output$`training-plot-alexa` = renderPlot({
      plotType = input$`taining-dataset-plots-alexa`
      if(is.null(plotType)){
        return(NULL)
      }else if(plotType == 1){
        alexa.training.dataset = readRDS("alexatrainingtweets.rds")
        polarity.Train = factor(alexa.training.dataset$Sentiments , 
                                       labels = c("Neutral", "Negative","Positive"))
        ggplot(alexa.training.dataset, 
               aes(x = polarity.Train,
                   fill = polarity.Train)) +
          geom_bar(width = .75 ) +
          labs(title = "Polarity of Tweets vs Number of Tweets of Training-Dataset",
               subtitle = "Tweets from 02-06-2018 to 28-06-2018 have been Analysed here.",
               x= "Polarity of Tweets" , 
               y= "Number of Tweets"  
          )+
          theme(legend.title = element_text(face = "bold"),
                axis.title.x=element_text( face="bold"), 
                axis.title.y=element_text( face="bold"),
                plot.title = element_text(face = "bold")
                
          )+
          theme_gray(base_size = 14)+
          scale_fill_manual("Sentiments", 
                            values = c("Neutral" = "#FFFF00", 
                                       "Negative" = "#CD2626", 
                                       "Positive" = "#00BFFF"))
        
        
      }
      else if(plotType == 2){
        
        alexa.training.dataset = readRDS("alexatrainingtweets.rds")
        traincount = count(alexa.training.dataset ,
                           alexa.training.dataset$Sentiments)
        trainvector = as.vector(traincount$n) 
        neutralTrain = trainvector[1]
        negativeTrain = trainvector[2]
        positiveTrain = trainvector[3]
        slices <- c(negativeTrain,neutralTrain,positiveTrain)
        labels <- c("Negative","Neutral" ,"Positive")
        pct =round((slices/sum(slices))*100)
        labels = paste(labels , pct)
        labels = paste(labels , "%" , sep = "")
        pie3D(slices, labels = labels, 
                   col=c("Negative" = "#CD2626","Neutral" = "#FFFF00", "Positive" = "#00BFFF"),
                   explode=0.1,
                   main="Sentiments of training-data")
      }
    })
    
    
#========Plotting Bar graph and Pie Chart For Alexa prediction Dataset==============
    
    
    
    output$`prediction-plot-alexa` = renderPlot({
      plotType = input$`test-dataset-plots-alexa`
      if(is.null(plotType)){
        return(NULL)
      }else if(plotType == 1){
        alexa.test.dataset = readRDS("alexatestingtweets.rds")
        alexa.predictor = readRDS("alexaprediction.rds")
        polarityTest =factor(alexa.predictor , labels =c("Neutral","Negative","Positive"))
        ggplot(alexa.test.dataset, aes(x = polarityTest, fill = polarityTest)) +
          geom_bar(width = .75 ) +
          labs(title = "Polarity of Tweets vs Number of Tweets of Prediction-Dataset",
               subtitle = "Tweets from 02-06-2018 to 28-06-2018 have been Analysed here.",
               x= "Polarity of Tweets" , 
               y= "Number of Tweets"  
          )+
          theme(legend.title =element_text( face="bold"),
                axis.title.x=element_text( face="bold"), 
                axis.title.y=element_text( face="bold"),
                plot.title = element_text(face = "bold")
                
          )+
          theme_gray(base_size = 14)+
          scale_fill_manual("Sentiments", 
                            values = c("Neutral" = "#FFFF00", 
                                       "Negative" = "#CD2626", 
                                       "Positive" = "#00BFFF"))
        
      }
      else if(plotType == 2){
        alexa.predictor = readRDS("alexaprediction.rds")
        testframe = as.data.frame(alexa.predictor)
        testcount = count(testframe , alexa.predictor)
        testvector = as.vector(testcount$n)
        neutralTest = testvector[1]
        negativeTest = testvector[2]
        positiveTest = testvector[3]
        
        slices <- c(negativeTest,neutralTest,positiveTest)
        labels <- c("Negative","Neutral" ,"Positive")
        pct =round((slices/sum(slices))*100)
        labels = paste(labels , pct)
        labels = paste(labels , "%" , sep = "")
        pie3D(slices, labels = labels, 
              col=c("Negative" = "#CD2626","Neutral" = "#FFFF00", "Positive" = "#00BFFF"),
              explode=0.1, 
              main="Sentiments of test-data after prediction")
        
        
      }
    })
    
    
#==========Show Training and Prediction DataTables For Alexa==========================
    
    output$`showdata-alexa` <- DT::renderDataTable({
      selectionType = input$`show-dataset-alexa`
      if(is.null(selectionType)){
        return()
      }else if(selectionType == 1){
        alexa.training.dataset = readRDS("alexatrainingtweets.rds")
        training_sample <- alexa.training.dataset %>%
          sample_n(input$`observations-alexa`)
        DT::datatable(data = training_sample, 
                      options = list(pageLength = 10), 
                      rownames = FALSE)
      }else if(selectionType == 2){
        alexa.test.dataset = readRDS("alexatestingtweets.rds")
        testing_sample <- alexa.test.dataset %>%
          sample_n(input$`observations-alexa`)
        DT::datatable(data = testing_sample, 
                      options = list(pageLength = 10), 
                      rownames = FALSE)
      }
    })
    
    
#=========Show Summary and Confusion-Matrix For Alexa-Classifier ==================
    
    
    
    output$`showsummary-alexa` <- renderPrint({
      selectionType = input$`show-summary-alexa`
      if(is.null(selectionType)){
        return()
      }else if(selectionType == 1){
        alexa.classifier = readRDS("alexaclassifier.rds")  
        summary(alexa.classifier)
      }else if(selectionType == 2){
        alexa.predictor = readRDS("alexaprediction.rds")
        alexa.test.dataset = readRDS("alexatestingtweets.rds")
        cm = table(alexa.test.dataset[,2],alexa.predictor)
        confusionMatrix(cm)
      }
    })
    
#=======================Siri-Plots==================================================
#========Plotting Bar graph and Pie Chart For Siri Training Dataset==============
    
    
        output$`training-plot-siri` = renderPlot({
      plotType = input$`taining-dataset-plots-siri`
      if(is.null(plotType)){
        return(NULL)
      }else if(plotType == 1){
        siri.training.dataset = readRDS("siritrainingtweets.rds")
        polarity.Train = factor(siri.training.dataset$Sentiments , 
                                labels = c("Neutral", "Negative","Positive"))
        ggplot(siri.training.dataset, 
               aes(x = polarity.Train,
                   fill = polarity.Train)) +
          geom_bar(width = .75 ) +
          labs(title = "Polarity of Tweets vs Number of Tweets of Training-Dataset",
               subtitle = "Tweets from 02-06-2018 to 28-06-2018 have been Analysed here.",
               x= "Polarity of Tweets" , 
               y= "Number of Tweets"  
          )+
          theme(legend.title =element_text( face="bold"),
                axis.title.x=element_text( face="bold"), 
                axis.title.y=element_text( face="bold"),
                plot.title = element_text(face = "bold")
                
          )+
          theme_gray(base_size = 14)+
          scale_fill_manual("Sentiments", 
                            values = c("Neutral" = "#FFFF00", 
                                       "Negative" = "#CD2626", 
                                       "Positive" = "#00BFFF"))
        
      }
      else if(plotType == 2){
        
        siri.training.dataset = readRDS("siritrainingtweets.rds")
        traincount = count(siri.training.dataset ,
                           siri.training.dataset$Sentiments)
        trainvector = as.vector(traincount$n) 
        neutralTrain = trainvector[1]
        negativeTrain = trainvector[2]
        positiveTrain = trainvector[3]
        slices <- c(negativeTrain,neutralTrain,positiveTrain)
        labels <- c("Negative","Neutral" ,"Positive")
        pct =round((slices/sum(slices))*100)
        labels = paste(labels , pct)
        labels = paste(labels , "%" , sep = "")
        pie3D(slices, labels = labels, 
                   col=c("Negative" = "#CD2626","Neutral" = "#FFFF00", "Positive" = "#00BFFF"),
                   explode=0.1,
                   main="Sentiments of training-data")
      }
    })
    
    
#========Plotting Bar graph and Pie Chart For Siri prediction Dataset==============
    
    
    
    output$`prediction-plot-siri` = renderPlot({
      plotType = input$`test-dataset-plots-siri`
      if(is.null(plotType)){
        return(NULL)
      }else if(plotType == 1){
        siri.test.dataset = readRDS("siritestingtweets.rds")
        siri.predictor = readRDS("siriprediction.rds")
        polarityTest =factor(siri.predictor , labels =c("Neutral","Negative","Positive"))
        ggplot(siri.test.dataset, aes(x = polarityTest, fill = polarityTest)) +
          geom_bar(width = .75 ) +
          labs(title = "Polarity of Tweets vs Number of Tweets of Prediction-Dataset",
               subtitle = "Tweets from 02-06-2018 to 28-06-2018 have been Analysed here.",
               x= "Polarity of Tweets" , 
               y= "Number of Tweets"  
          )+
          theme(legend.title =element_text( face="bold"),
                axis.title.x=element_text( face="bold"), 
                axis.title.y=element_text( face="bold"),
                plot.title = element_text(face = "bold")
                
          )+
          theme_gray(base_size = 14)+
          scale_fill_manual("Sentiments", 
                            values = c("Neutral" = "#FFFF00", 
                                       "Negative" = "#CD2626", 
                                       "Positive" = "#00BFFF"))
        
        
      }
      else if(plotType == 2){
        siri.predictor = readRDS("siriprediction.rds")
        testframe = as.data.frame(siri.predictor)
        testcount = count(testframe , siri.predictor)
        testvector = as.vector(testcount$n)
        neutralTest = testvector[1]
        negativeTest = testvector[2]
        positiveTest = testvector[3]
        
        slices <- c(negativeTest,neutralTest,positiveTest)
        labels <- c("Negative","Neutral" ,"Positive")
        pct =round((slices/sum(slices))*100)
        labels = paste(labels , pct)
        labels = paste(labels , "%" , sep = "")
        pie3D(slices, labels = labels, 
              col=c("Negative" = "#CD2626","Neutral" = "#FFFF00", "Positive" = "#00BFFF"),
              explode=0.1, 
              main="Sentiments of test-data after prediction")
        
        
      }
    })
    
    
#==========Show Training and Prediction DataTables For Siri==========================
    
    output$`showdata-siri` <- DT::renderDataTable({
      selectionType = input$`show-dataset-siri`
      if(is.null(selectionType)){
        return()
      }else if(selectionType == 1){
        siri.training.dataset = readRDS("siritrainingtweets.rds")
        training_sample <- siri.training.dataset %>%
          sample_n(input$`observations-siri`)
        DT::datatable(data = training_sample, 
                      options = list(pageLength = 10), 
                      rownames = FALSE)
      }else if(selectionType == 2){
        siri.test.dataset = readRDS("siritestingtweets.rds")
        testing_sample <- siri.test.dataset %>%
          sample_n(input$`observations-siri`)
        DT::datatable(data = testing_sample, 
                      options = list(pageLength = 10), 
                      rownames = FALSE)
      }
    })
    
    
#=========Show Summary and Confusion-Matrix For Siri-Classifier ==================
    
    
    output$`showsummary-siri` <- renderPrint({
      selectionType = input$`show-summary-siri`
      if(is.null(selectionType)){
        return()
      }else if(selectionType == 1){
        siri.classifier = readRDS("siriclassifier.rds")  
        summary(siri.classifier)
      }else if(selectionType == 2){
        siri.predictor = readRDS("siriprediction.rds")
        siri.test.dataset = readRDS("siritestingtweets.rds")
        cm = table(siri.test.dataset[,2],siri.predictor)
        confusionMatrix(cm)
      }
    })
    
    
    
        
    
#=======================Cortana-Plots===============================================
#========Plotting Bar graph and Pie Chart For Cortana Training Dataset==============
    
    
    output$`training-plot-cortana` = renderPlot({
      plotType = input$`taining-dataset-plots-cortana`
      if(is.null(plotType)){
        return(NULL)
      }else if(plotType == 1){
        cortana.training.dataset = readRDS("cortanatrainingtweets.rds")
        polarity.Train = factor(cortana.training.dataset$Sentiments , 
                                labels = c("Neutral", "Negative","Positive"))
        ggplot(cortana.training.dataset, 
               aes(x = polarity.Train,
                   fill = polarity.Train)) +
          geom_bar(width = .75 ) +
          labs(title = "Polarity of Tweets vs Number of Tweets of Training-Dataset",
               subtitle = "Tweets from 02-06-2018 to 28-06-2018 have been Analysed here.",
               x= "Polarity of Tweets" , 
               y= "Number of Tweets"  
          )+
          theme(legend.title =element_text( face="bold"),
                axis.title.x=element_text( face="bold"), 
                axis.title.y=element_text( face="bold"),
                plot.title = element_text(face = "bold")
                
          )+
          theme_gray(base_size = 14)+
          scale_fill_manual("Sentiments", 
                            values = c("Neutral" = "#FFFF00", 
                                       "Negative" = "#CD2626", 
                                       "Positive" = "#00BFFF"))
        
      }
      else if(plotType == 2){
        
        cortana.training.dataset = readRDS("cortanatrainingtweets.rds")
        traincount = count(cortana.training.dataset ,
                           cortana.training.dataset$Sentiments)
        trainvector = as.vector(traincount$n) 
        neutralTrain = trainvector[1]
        negativeTrain = trainvector[2]
        positiveTrain = trainvector[3]
        slices <- c(negativeTrain,neutralTrain,positiveTrain)
        labels <- c("Negative","Neutral" ,"Positive")
        pct =round((slices/sum(slices))*100)
        labels = paste(labels , pct)
        labels = paste(labels , "%" , sep = "")
        pie3D(slices, labels = labels, 
                   col=c("Negative" = "#CD2626","Neutral" = "#FFFF00", "Positive" = "#00BFFF"),
                   explode=0.1,
                   main="Sentiments of training-data")
      }
    })
    

#========Plotting Bar graph and Pie Chart For Cortana prediction Dataset==============
    
    
    
    output$`prediction-plot-cortana` = renderPlot({
      plotType = input$`test-dataset-plots-cortana`
      if(is.null(plotType)){
        return(NULL)
      }else if(plotType == 1){
        cortana.test.dataset = readRDS("cortanatestingtweets.rds")
        cortana.predictor = readRDS("cortanaprediction.rds")
        polarityTest =factor(cortana.predictor , labels =c("Neutral","Negative","Positive"))
        ggplot(cortana.test.dataset, aes(x = polarityTest, fill = polarityTest)) +
          geom_bar(width = .75 ) +
          labs(x= "Polarity of Tweets" , 
               y= "Number of Tweets" ,
               title = "Polarity of Tweets vs Number of Tweets of Prediction-Dataset",
               subtitle = "Tweets from 02-06-2018 to 28-06-2018 have been Analysed here."
          )+
          theme(plot.title = element_text(face = "bold"),
                plot.subtitle = element_text(face = "bold"),
                axis.title.x=element_text( face="bold"), 
                axis.title.y=element_text( face="bold"),
                legend.title =element_text( face="bold")
              )+
          theme_gray(base_size = 14)+
          scale_fill_manual("Sentiments", 
                            values = c("Neutral" = "#FFFF00", 
                                       "Negative" = "#CD2626", 
                                       "Positive" = "#00BFFF"))
        
      }
      else if(plotType == 2){
        cortana.predictor = readRDS("cortanaprediction.rds")
        testframe = as.data.frame(cortana.predictor)
        testcount = count(testframe , cortana.predictor)
        testvector = as.vector(testcount$n)
        neutralTest = testvector[1]
        negativeTest = testvector[2]
        positiveTest = testvector[3]
        
        slices <- c(negativeTest,neutralTest,positiveTest)
        labels <- c("Negative","Neutral" ,"Positive")
        pct =round((slices/sum(slices))*100)
        labels = paste(labels , pct)
        labels = paste(labels , "%" , sep = "")
        pie3D(slices, labels = labels, 
              col=c("Negative" = "#CD2626","Neutral" = "#FFFF00", "Positive" = "#00BFFF"),
              explode=0.1, 
              main="Sentiments of test-data after prediction")
        
        
      }
    })
    

#==========Show Training and Prediction DataTables For Cortana==========================
    
    
    output$`showdata-cortana` <- DT::renderDataTable({
      selectionType = input$`show-dataset-cortana`
      if(is.null(selectionType)){
        return()
      }else if(selectionType == 1){
        cortana.training.dataset = readRDS("cortanatrainingtweets.rds")
        training_sample <- cortana.training.dataset %>%
          sample_n(input$`observations-cortana`)
        DT::datatable(data = training_sample, 
                      options = list(pageLength = 10), 
                      rownames = FALSE)
      }else if(selectionType == 2){
        cortana.test.dataset = readRDS("cortanatestingtweets.rds")
        testing_sample <- cortana.test.dataset %>%
          sample_n(input$`observations-cortana`)
        DT::datatable(data = testing_sample, 
                      options = list(pageLength = 10), 
                      rownames = FALSE)
      }
    })
    
    
#=========Show Summary and Confusion-Matrix For Cortana-Classifier ==================
    
    
    
    output$`showsummary-cortana` <- renderPrint({
      selectionType = input$`show-summary-cortana`
      if(is.null(selectionType)){
        return()
      }else if(selectionType == 1){
        cortana.classifier = readRDS("cortanaclassifier.rds")  
        summary(cortana.classifier)
      }else if(selectionType == 2){
        cortana.predictor = readRDS("cortanaprediction.rds")
        cortana.test.dataset = readRDS("cortanatestingtweets.rds")
        cm = table(cortana.test.dataset[,2],cortana.predictor)
        confusionMatrix(cm)
      }
    })
    
    
#=====All Together Analysis Of All Four personal Assistants Together=================    
    
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
    data.main <- melt(combined.data.frame, id.vars='Company')
    
    output$`grouped-plot` = renderPlot({
      plotType = input$`show-plots-together`
      if(is.null(plotType)){
        return(NULL)
      }else if(plotType == 1){
        ggplot(data.main, aes(Company, value)) + 
          geom_bar(aes(fill = variable),
                   width = 0.4,
                   position = position_dodge(width = 0.5), 
                   stat="identity") +
          geom_text(aes(label = value,y=value),position = position_dodge(0.4), vjust=-0.25,
                    size=3 ,  color = "black")+
          labs(x= "Personal Assistants" , 
               y= "Number of Tweets",
               title = "Personal Assitants vs Number of Tweets",
               subtitle = "Here all four personal assistants plots have been shown together."
               )+
          theme_gray(base_size = 14)+
          theme(legend.position="top", 
                legend.title = element_blank(),
                plot.title = element_text(face = "bold"),
                plot.subtitle = element_text(face = "bold"),
                axis.title.x=element_text( face="bold"), 
                axis.title.y=element_text( face="bold"))
      }else if(plotType == 2){
        
        ggplot(data=data.main, aes(x=Company, y=value, fill=variable)) +
          geom_bar(stat="identity")+
          geom_text(aes(label = value,y=value),position = position_stack(),
                    size=3 ,  color = "black")+
          labs(x= "Personal Assistants" , 
               y= "Number of Tweets",
               title = "Personal Assitants vs Number of Tweets",
               subtitle = "Here all four personal assistants plots have been shown together."
               )+
          theme_gray(base_size = 14)+
          theme(legend.title = element_blank(),
                plot.title = element_text(face = "bold"),
                plot.subtitle = element_text(face = "bold"),
                axis.title.x=element_text( face="bold"), 
                axis.title.y=element_text( face="bold"))
      }
    })
    
    
    
output$DESCRIPTION = renderUI({
 HTML("<div id = 'first'>
        <h4><b><u>Application Perspective</u></b></h4>
        <div id = 'second'>
        <p align='justify'>In this project the mood of tweets of the top four personal
        assistants are analysed.Sentiments for the tweets that came in the year <b>2018</b>
        from  <b>02-06-2018 to 28-06-2018 </b> are analysed.</p>
        <ul>
        <b><li>Google Assistant (Google)</li>
        <li>Cortana (Microsoft)</li>
        <li>Alexa (Amazon)</li>
        <li>Siri (Apple)</li></b>
        </ul>
        <br>
        <p align='justify'>Here in this Project we have used the already extracted dataset from the
        server codes of the individual Assistants that we have developed from scratch.The extracted Datasets are used to
        improve the Application response time and to load the application faster.
        It also prevents overwhelming the requests sent to the server by the client.</p>
        </div>
        <br>
        <div id = 'third'>
        <h4><b><u>Application Description</u></b></h4>
        <p align = 'justify'>This application displays the <b>bar-plots</b> 
        as well as <b>pie-charts</b> of the training sample and prediction 
        sample of all four personal assistants.</p>
        <br>
        <p align = 'justify'>
        This application also displays the training and prediction dataset based
        user selection as a <b>Data-Table</b>.The user can also input the number of 
        observations to be displayed on the <b>Data-Table</b>.If the user dosen't 
        provide any input then <b>ten-observations</b> are displayed by default.
        </p>
        <br>
        <p align = 'justify'>
        The user can also see the summary of the <b>training algorithm(Random Forest 
        Algorithm)</b> that have been used for <b>Text-Classification</b>.<br>
        The user can also see <b>Confusion-Matrix</b> of the <b>prediction-results</b>
        to see the accuracy of the <b>training algorithm</b> and 
        predicted-values for sentiments.
        </p>
        <br>
        <p align = 'justify'>
        Overall Analysis of the sentiments of all four personal assistants have been 
        done together.Here we can analyse the ratio of <b>(positive:negative:neutral)</b> 
        sentiments of all the personal assistants.Then we can compare the ratios of 
        the individual assistants.And finally come to a conclusion which of the 
        following personal assistants are more liked by the people.<br>
        </p>
        </div>
        </div>")

  
})    
    
    
    
  }
  
  
  
)

