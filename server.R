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
  password = ""
)

#feature table names
mfcc_table_name <- "mfcc_data"
zcr_table_name <- "zcr_data"
features_data_table <- "features_data"

#-----------      END: Database Initilization                       ----------#

shinyServer(function(input, output) {
  #----------------------------------------------------------------------------#
  #********* START: reactive function to detect the file upload change ********#
  #----------------------------------------------------------------------------#
  getMonoWaveFromRagaFile <- reactive({
    #storing the uploaded file
    ragaFile <- input$raga_file
    
    #If recived file is empty return NULL
    if (is.null(ragaFile)) {
      return(NULL)
    } else{
      #validating the file type - application will throw an error if fle type is other than .wav or .mp3
      validate(need(
        file_ext(ragaFile$name) %in% c('wav',
                                       'mp3'),
        "Please provide valid .wav or .mp3 file."
      ))
      
      if (file_ext(ragaFile$name) == "mp3") {
        #changing file name in the temporary direcctory
        file.rename(ragaFile$datapath,
                    paste(ragaFile$datapath, ".mp3", sep = ""))
        
        loadedFile <-
          readMP3(paste(ragaFile$datapath, ".mp3", sep = ""))  ## MP3 file in working directory
        
        tdir <- tempdir()
        tfile <- file.path(tdir, "wavFile.wav")
        writeWave(loadedFile, filename = tfile)
        
        newWobj <- readWave(tfile)
        file.remove(tfile)
        
        #converting sterio channels to mono
        monoWave <- mono(newWobj, "left")
        
        return(monoWave)
      } else{
        #changing file name in the temporary direcctory
        file.rename(ragaFile$datapath,
                    paste(ragaFile$datapath, ".wav", sep = ""))
        
        newWobj <-
          readWave(paste(ragaFile$datapath, ".wav", sep = ""))
        
        #converting sterio channels to mono
        monoWave <- mono(newWobj, "left")
        
        return(monoWave)
      }
    }
  })
  #------- END: reactive function to detect the file upload change      -------#
  
  #----------------------------------------------------------------------------#
  #*********              START: K-means rendering                    *********#
  #----------------------------------------------------------------------------#
  output$kmeansPlot <- renderPlot({
    monoWave <- getMonoWaveFromRagaFile()
    
    if (is.null(monoWave)) {
      return(NULL)
    } else{
      feature_data_frame <- featureExtractionAndTransformation(monoWave)
      
      
      #dicarding observations with the NA
      binded_data <- na.omit(feature_data_frame)
      
      #collecting raga file names
      raga_file_names <- select(binded_data, raga_file_name)
      
      #removing raga_file_name(text) from data frame before passing it to the kmeans algorithm
      binded_data <- select(binded_data,-raga_file_name)
      
      #kmeans function call
      km1 = kmeans(binded_data, input$number_of_clusters, nstart = 2)
      
      #factoring the feature data frame for plotting purpose
      tmp_f = fa(binded_data, 2, rotate = "none")
      
      #collect data for graph plotting
      tmp_d = data.frame(matrix(ncol = 0, nrow = nrow(binded_data)))
      tmp_d$cluster = as.factor(km1$cluster)
      tmp_d$fact_1 = as.numeric(tmp_f$scores[, 1])
      tmp_d$fact_2 = as.numeric(tmp_f$scores[, 2])
      tmp_d$label = raga_file_names$raga_file_name
      
      #plotting the clustring graph
      ggplot(tmp_d, aes(fact_1, fact_2, color = cluster)) + geom_point() + geom_text(
        aes(label = label),
        size = 3,
        vjust = 0,
        hjust = 1,
        color = "black"
      )
    }
    
  })
  #----------------------------------------------------------------------------#
  #*********                END: K-means rendering                    *********#
  #----------------------------------------------------------------------------#
  
  
  
  
  #----------------------------------------------------------------------------#
  #*********  START: Process monowave, extract and store features     *********#
  #----------------------------------------------------------------------------#
  featureExtractionAndTransformation <- function(monoWave) {
    if (is.null(monoWave)) {
      return(NULL)
    } else{
      #getting the max id
      raga_counter <-
        dbGetQuery(conn, "SELECT MAX(id) as counter FROM features_data")
      
      if (is.na(raga_counter$counter)) {
        raga_counter <- 1
      } else{
        raga_counter <- (raga_counter$counter + 1)
      }
      
      #--------  START : MFCC feature extraction  --------------------#
      
      #fetching the existing mfcc features data
      existing_feature_data <-
        fetchFetureData(features_data_table)
      
      #collecting MFCC features of the newly uploaded file
      new_melfc_data_frame <-
        mfccFeatureProcessing(monoWave)
      
      new_melfc_data_frame["raga_file_name"] <-
        isolate(input$raga_file$name)
      
      minimized_melfc_data_frame <-
        new_melfc_data_frame %>% group_by(raga_file_name) %>%
        summarise(
          coef_01 =  mean(coef_01, na.rm = TRUE),
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
          coef_12 =  mean(coef_12, na.rm = TRUE)
        )
      
      #--------  END : MFCC feature extraction  -----------------------#
      
      #--------  START : Zero crossing rate feature extraction  -------#
      
      #collecting MFCC features of the newly uploaded file
      new_zcr_data_frame <-
        zcrFeatureProcessing(monoWave)
      
      new_zcr_data_frame["raga_file_name"] <-
        isolate(input$raga_file$name)
      
      minimized_zcr_data_frame <-
        new_zcr_data_frame %>% group_by(raga_file_name) %>%
        summarise(zcr =  mean(zcr, na.rm = TRUE))
      
      #--------  END : Zero crossing rate feature extraction  -------#
      
      
      #initilizing the feature_data_frame
      current_feature_data_frame <- data.frame()
      
      if (!is.null(minimized_melfc_data_frame) &&
          !is.null(minimized_zcr_data_frame)) {
        #removing the raga_file_name column as it will be redudant after binding columns
        minimized_zcr_data_frame <-
          select(minimized_zcr_data_frame,-raga_file_name)
        
        #binding mfcc and zcr data frames by columns
        current_feature_data_frame = cbind(minimized_melfc_data_frame,
                                           minimized_zcr_data_frame)
      } else{
        return(NULL)
      }
      
      #combining existing_mfcc_feature_data and new_melfc_data_frame
      combined_features_data <-
        bind_rows(existing_feature_data, current_feature_data_frame)
      
      if (raga_counter <= 15) {
        #adding id column to the dataframe, so that data frame can be stored directly to the database
        current_feature_data_frame["id"] <- raga_counter
        
        saveData(current_feature_data_frame, features_data_table)
      }
      
      return(combined_features_data)
    }
  }
  #----------- END: Process monowave, extract and store features --------------#
  
  
  #----------------------------------------------------------------------------#
  #********* START: Function to store the MFCC feature data           *********#
  #----------------------------------------------------------------------------#
  
  mfccFeatureProcessing <- function(raga_mono_wave) {
    melfc_data <-
      melfcc(
        raga_mono_wave,
        sr = raga_mono_wave@samp.rate,
        wintime = 0.025,
        hoptime = 0.01,
        numcep = 12,
        lifterexp = 0.6,
        htklifter = FALSE,
        sumpower = TRUE,
        preemph = 0.97,
        dither = FALSE,
        usecmp = FALSE,
        modelorder = NULL,
        spec_out = FALSE,
        frames_in_rows = TRUE
      )
    
    #defining column names for the melfc_data
    melfc_data_colname <-
      c(
        "coef_01",
        "coef_02",
        "coef_03",
        "coef_04",
        "coef_05",
        "coef_06",
        "coef_07",
        "coef_08",
        "coef_09",
        "coef_10",
        "coef_11",
        "coef_12"
      )
    
    #assigning the column names to the melfc_data
    colnames(melfc_data) <-  melfc_data_colname
    
    #convert melfc_data vetor to the melfc_data_frame
    melfc_data_frame <- data.frame(melfc_data)
    
    return(melfc_data_frame)
  }
  #----------- END: Function to store the MFCC feature data -------------------#
  
  
  
  
  
  #----------------------------------------------------------------------------#
  #*********  START: Extract and store the ZCR feature data         ***********#
  #----------------------------------------------------------------------------#
  
  zcrFeatureProcessing <- function(raga_mono_wave) {
    #extracting the Zero crossing rate feature
    zcr_data <-
      zcr(raga_mono_wave,
          wl = 512,
          ovlp = 0,
          plot = FALSE)
    
    #converting the zcr_data matrix to the data frame(data frame is having two columns: time and zcr)
    zcr_data_frame <- as.data.frame(zcr_data)
    
    return(zcr_data_frame)
  }
  #********* END: Extract and store the ZCR feature data          *************#
  
  
  
  
  #----------------------------------------------------------------------------#
  #*********  START: Storing extracted features to the database       *********#
  #----------------------------------------------------------------------------#
  
  saveData <- function(data, table_name) {
    dbWriteTable(
      conn,
      value = data,
      name = table_name,
      row.names = FALSE,
      append = TRUE
    )
  }
  #********* END: Storing extracted features to the database          ********#
  
  
  
  
  #----------------------------------------------------------------------------#
  #*********        START: Feature extraction from the database       *********#
  #----------------------------------------------------------------------------#
  
  fetchFetureData <- function(table_name) {
    #fetching all the data for the given feature
    features_data <-
      dbGetQuery(conn, paste0("SELECT * FROM ", table_name, ";"))
    
    #removing id field from the data frame
    trimmed_features_data <- select(features_data,-id)
    
    return(trimmed_features_data)
  }
  #********* END: Feature extraction from the database                ********#
})