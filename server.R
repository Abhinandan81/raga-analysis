library(shiny)
library(rsconnect)
library(tools) #for checking file extension
library(tuneR)
library(seewave)
library(dplyr)
library(tidyr)
library(DBI)
library(RMySQL)
library(ggplot2)
library(reshape2)
library(psych)


#----------------------------------------------------------------------------#
#**********       START: Database Initilization                 *************#
#----------------------------------------------------------------------------#

conn <- dbConnect(
  drv = RMySQL::MySQL(),
  dbname = "ragas",
  host = "localhost",
  user = "root",
  password = "")

#feature table names
mfcc_table_name <- "mfcc_data"
zcr_table_name <- "zcr_data"

#-----------      END: Database Initilization                       ----------#

shinyServer(function(input, output){

  #----------------------------------------------------------------------------#
  #********* START: reactive function to detect the file upload change ********#
  #----------------------------------------------------------------------------#
  getMonoWaveFromRagaFile <- reactive({
    
    #storing the uploaded file
    ragaFile <- input$raga_file

    #If recived file is empty return NULL
    if (is.null(ragaFile)){
      return(NULL)
    }else{
      
      #validating the file type - application will throw an error if fle type is other than .wav or .mp3
      validate(
        need(file_ext(ragaFile$name) %in% c(
          'wav',
          'mp3'
        ), "Please provide valid .wav or .mp3 file."))
      
      if(file_ext(ragaFile$name) == "mp3"){
        #changing file name in the temporary direcctory
        file.rename(ragaFile$datapath, paste(ragaFile$datapath, ".mp3", sep=""))
        
        loadedFile <- readMP3(paste(ragaFile$datapath, ".mp3", sep=""))  ## MP3 file in working directory
        
        tdir <- tempdir()
        tfile <- file.path(tdir, "wavFile.wav")
        writeWave(loadedFile, filename = tfile)
        
        newWobj <- readWave(tfile)
        file.remove(tfile)
      
        #converting sterio channels to mono
        monoWave <- mono(newWobj, "left")
        
        return(monoWave)
      }else{
        #changing file name in the temporary direcctory
        file.rename(ragaFile$datapath, paste(ragaFile$datapath, ".wav", sep=""))
        
        newWobj <- readWave(paste(ragaFile$datapath, ".wav", sep=""))
        
        #converting sterio channels to mono
        monoWave <- mono(newWobj, "left")

        return(monoWave)
      }
    }
  })
  #------- END: reactive function to detect the file upload change      -------#
  
  
  
  
  #----------------------------------------------------------------------------#
  #********* START: Process monowave, extract and store features      *********#
  #----------------------------------------------------------------------------#
  
  output$mfcc_plot <- renderPlot({
    
    monoWave <- getMonoWaveFromRagaFile()

               if(is.null(monoWave)){
                 return(NULL)
               }else{
              
                 #getting the max id
                 raga_counter <- dbGetQuery(conn, "SELECT MAX(id) as counter FROM mfcc_data")
                 
                 if(is.na(raga_counter$counter)){
                   raga_counter <- 1
                 }else{
                   raga_counter <- (raga_counter$counter + 1)
                 }
                 
                 #--------  START : MFCC feature extraction  --------------------#
                 
                 #fetching the existing mfcc features data
                 existing_mfcc_feature_data <- fetchFetureData(mfcc_table_name)
                 
                 #collecting MFCC features of the newly uploaded file
                 new_melfc_data_frame <- mfccFeatureProcessing(monoWave)
                 
                 new_melfc_data_frame["raga_file_name"] <- isolate(input$raga_file$name)
                 minimized_melfc_data_frame <- new_melfc_data_frame %>% group_by(raga_file_name) %>% 
                   summarise(coef_01 =  mean(coef_01, na.rm = TRUE),
                             coef_02 =  mean(coef_02, na.rm = TRUE),
                             coef_03 =  mean(coef_03, na.rm = TRUE),
                             coef_04 =  mean(coef_04, na.rm = TRUE),
                             coef_05 =  mean(coef_05, na.rm = TRUE),
                             coef_06 =  mean(coef_06, na.rm = TRUE),
                             coef_07 =  mean(coef_07, na.rm = TRUE),
                             coef_08 =  mean(coef_08, na.rm = TRUE),
                             coef_09 =  mean(coef_09, na.rm = TRUE),
                             coef_10 =  mean(coef_10, na.rm = TRUE),
                             coef_11 =  mean(coef_11, na.rm = TRUE),
                             coef_12 =  mean(coef_12, na.rm = TRUE))
                 
                 #combining existing_mfcc_feature_data and new_melfc_data_frame
                 combined_mfcc_features <- bind_rows(existing_mfcc_feature_data, minimized_melfc_data_frame)
                 
                 if(raga_counter <= 15){
                   #adding id column to the dataframe, so that data frame can be stored directly to the database
                   minimized_melfc_data_frame["id"] <- raga_counter
                   
                   saveData(minimized_melfc_data_frame, mfcc_table_name)
                 }
  
                 #--------  END : MFCC feature extraction  -----------------------#
                 
                 dat = data.frame(combined_mfcc_features)
                 
                 formated_data_frame <- na.omit(dat)
                 
                 back_df <- formated_data_frame
                 
                 formated_data_frame <-  select(formated_data_frame, -raga_file_name)
                 
                 km1 = kmeans(formated_data_frame, input$number_of_clusters, nstart=2)
                 
         
                 
                 #back_df$cluster <- as.factor(km1$cluster)
                 
                 #factor
                 tmp_f = fa(formated_data_frame, 2, rotate = "none")
                 
                 print("tmp_f$scores")
                 print(tmp_f$scores)
                 
                 print("==================================")
                 print("formated_data_frame")
                 print(formated_data_frame)
                 
                 print("back_df")
                 print(back_df$raga_file_name)
                 
                 #collect data
                 tmp_d = data.frame(matrix(ncol=0, nrow=nrow(formated_data_frame)))
                 tmp_d$cluster = as.factor(km1$cluster)
                 tmp_d$fact_1 = as.numeric(tmp_f$scores[, 1])
                 tmp_d$fact_2 = as.numeric(tmp_f$scores[, 2])
                 tmp_d$label = back_df$raga_file_name
                 
                 ggplot(tmp_d, aes(fact_1, fact_2, color = cluster)) + geom_point() + geom_text(aes(label = label), size = 3, vjust = 1, color = "black")
                 
                 #ggplot(melt(back_df, id.vars = "raga_file_name"), aes(value, variable, colour = raga_file_name)) + 
                   #geom_point() + geom_text(aes(label = raga_file_name), size = 1, vjust = 1, color = "white")
                 
               #  ggplot(melt(back_df, id.vars = "raga_file_name"), aes(value, variable, colour = raga_file_name)) + 
                  # geom_point()
                 
                # ggplot(tmp_d, aes(fact_1, fact_2, color = cluster)) + geom_point() + 
                 #  geom_text(aes(label = raga_file_name), size = 3, vjust = 1, color = "black")
                 
                 #ggplot(formated_data_frame, aes(coef_01, coef_02, coef_03, 
                 #coef_04, coef_05, coef_06, coef_07, coef_08, coef_09, coef_10, 
                 #coef_11, coef_12, color = km1$cluster)) + geom_point()
                 
                 # Plot results
                 #plot(formated_data_frame, col =(km1$cluster) , main="K-Means result with 2 clusters", pch=20, cex=2)
                
               }
  })
  #----------- END: Process monowave, extract and store features --------------#
  
  
  
  
  
  #----------------------------------------------------------------------------#
  #********* START: Process monowave, extract and store features      *********#
  #----------------------------------------------------------------------------#
  
  output$zcr_plot <- renderPlot({
    monoWave <- getMonoWaveFromRagaFile()
    
    if(is.null(monoWave)){
      return(NULL)
    }else{
      
      #getting the max id
      raga_counter <- dbGetQuery(conn, "SELECT MAX(id) as counter FROM zcr_data")
      
      if(is.na(raga_counter$counter)){
        raga_counter <- 1
      }else{
        raga_counter <- (raga_counter$counter + 1)
      }
      
      #--------  START : Zero crossing rate feature extraction  -------#
      
      #fetching the existing mfcc features data
      existing_zcr_feature_data <- fetchFetureData(zcr_table_name)
      
      #collecting MFCC features of the newly uploaded file
      new_zcr_data_frame <- zcrFeatureProcessing(monoWave)
      
      #combining existing_mfcc_feature_data and new_melfc_data_frame
      combined_zcr_features <- bind_rows(existing_zcr_feature_data, new_zcr_data_frame)
      
      if(raga_counter < 5){
        #adding id column to the dataframe, so that data frame can be stored directly to the database
        new_zcr_data_frame["id"] <- raga_counter
        new_zcr_data_frame["raga_file_name"] <- isolate(input$raga_file$name)
        saveData(new_zcr_data_frame, zcr_table_name)
      }
      
      #--------  END : Zero crossing rate feature extraction  -------#
      
      dat = data.frame(combined_zcr_features$time, combined_zcr_features$zcr)
      
      formated_data_frame <- na.omit(dat)
      km1 = kmeans(formated_data_frame, 5, nstart=100)
      
      # Plot results
      plot(formated_data_frame, col =(km1$cluster +1) , main="K-Means result with 2 clusters", pch=20, cex=2)
      
    }
  })
  #----------- END: Process monowave, extract and store features --------------#
  
  
  
  
  
  #----------------------------------------------------------------------------#
  #********* START: Function to store the MFCC feature data           *********#
  #----------------------------------------------------------------------------#
  
    mfccFeatureProcessing <- function(raga_mono_wave){
    
    melfc_data <- melfcc(raga_mono_wave, sr = raga_mono_wave@samp.rate, wintime = 0.025,
                         hoptime = 0.01, numcep = 12, lifterexp = 0.6, htklifter = FALSE,
                         sumpower = TRUE, preemph = 0.97, dither = FALSE, usecmp = FALSE,
                         modelorder = NULL, spec_out = FALSE, frames_in_rows = TRUE)
    
    #defining column names for the melfc_data
    melfc_data_colname <- c("coef_01", "coef_02", "coef_03", "coef_04", "coef_05", "coef_06", "coef_07", "coef_08", "coef_09", "coef_10", "coef_11", "coef_12")
    
    #assigning the column names to the melfc_data
    colnames(melfc_data) <-  melfc_data_colname                
    
    #convert melfc_data vetor to the melfc_data_frame
    melfc_data_frame <- data.frame(melfc_data)
    
    trimmed_melfc_data_frame <- melfc_data_frame[1:100, ]
    
    return(melfc_data_frame)
  }
  #----------- END: Function to store the MFCC feature data -------------------#
  
    
    
  
  
  #----------------------------------------------------------------------------#
  #*********  START: Extract and store the ZCR feature data         ***********#
  #----------------------------------------------------------------------------#
  
    zcrFeatureProcessing <- function(raga_mono_wave){
    
    #extracting the Zero crossing rate feature
    zcr_data <- zcr(raga_mono_wave, wl = 512, ovlp = 0, plot = FALSE)
    
    #converting the zcr_data matrix to the data frame(data frame is having two columns: time and zcr)
    zcr_data_frame <- as.data.frame(zcr_data)
    
    trimmed_zcr_data_frame <- zcr_data_frame[1:100, ]
    
    return(trimmed_zcr_data_frame)
    }
  #********* END: Extract and store the ZCR feature data          *************#
  
    
    
  
  #----------------------------------------------------------------------------#
  #*********  START: Storing extracted features to the database       *********#
  #----------------------------------------------------------------------------#
  
  saveData <- function(data, table_name) {
        dbWriteTable(conn, value = data, name = table_name, row.names = FALSE, append = TRUE)
  }
  #********* END: Storing extracted features to the database          ********#
  
  
  
  
  #----------------------------------------------------------------------------#
  #*********  START: Feature extraction from the database       *********#
  #----------------------------------------------------------------------------#
  
  fetchFetureData <- function(table_name){
    
    #fetching all the data for the given feature
    features_data <- dbGetQuery(conn, paste0("SELECT * FROM ", table_name, ";"))

    #removing id field from the data frame
    trimmed_features_data <- select(features_data, -id)
    
    return(trimmed_features_data)
  }
  #********* END: Feature extraction from the database                ********#
})