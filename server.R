library(shiny)
library(rsconnect)
library(tools) #for checking file extension
library(tuneR)
library(seewave)
library(dplyr)
library(tidyr)
library(DBI)
library(RMySQL)

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
  getRagaFile <- reactive({
    
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
      
        return(newWobj)
        
      }else{
        #changing file name in the temporary direcctory
        file.rename(ragaFile$datapath, paste(ragaFile$datapath, ".wav", sep=""))
        
        newWobj <- readWave(paste(ragaFile$datapath, ".wav", sep=""))

        return(newWobj)
        
      }
    }
  })
  
  #------- END: reactive function to detect the file upload change      -------#
  
  
  #----------------------------------------------------------------------------#
  #********* START: Process monowave, extract and store features      *********#
  #----------------------------------------------------------------------------#
  
  feature_extraction <- observeEvent(input$raga_file, {
               wavFile <- getRagaFile()
               
               if(is.null(wavFile)){
                 return(NULL)
               }else{
                 
                 #converting sterio channels to mono
                 monoWave <- mono(wavFile, "left")
            
                 #getting the max id
                 raga_counter <- dbGetQuery(conn, "SELECT MAX(id) as counter FROM mfcc_data")
                 
                 if(is.na(raga_counter$counter)){
                   raga_counter <- 1
                 }else{
                   raga_counter <- (raga_counter$counter + 1)
                 }
                 
                 #--------  START : MFCC feature extraction  --------------------#
                 
                 trimmed_melfc_data_frame <- mfcc_feature_processing(monoWave, raga_counter)
                 
                 if(raga_counter < 15){
                   saveData(trimmed_melfc_data_frame, mfcc_table_name)
                 }
                 
                 #--------  END : MFCC feature extraction  -----------------------#
                 
                 #--------  START : Zero crossing rate feature extraction  -------#
          
                 trimmed_zcr_data_frame <- zcr_feature_processing(monoWave, raga_counter)
                 
                 print("trimmed_zcr_data_frame")
                 
                 print(glimpse(trimmed_zcr_data_frame))
                 
                 if(raga_counter < 15){
                   saveData(trimmed_zcr_data_frame, zcr_table_name)
                 }
                 
                 #--------  END : Zero crossing rate feature extraction  -------#
               }
  })
  
  #----------- END: Process monowave, extract and store features --------------#
  
  
  #----------------------------------------------------------------------------#
  #********* START: Function to store the MFCC feature data           *********#
  #----------------------------------------------------------------------------#
  
  mfcc_feature_processing <- function(raga_mono_wave, counter){
    
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
    
    trimmed_melfc_data_frame["id"] <- counter
    
    return(trimmed_melfc_data_frame)
  }
  
  #----------- END: Function to store the MFCC feature data -------------------#
  
  
  
  #----------------------------------------------------------------------------#
  #*********  START: Extract and store the ZCR feature data         ***********#
  #----------------------------------------------------------------------------#
  
    zcr_feature_processing <- function(raga_mono_wave, counter){
    
    #extracting the Zero crossing rate feature
    zcr_data <- zcr(raga_mono_wave, wl = 512, ovlp = 0, plot = FALSE)
    
    #converting the zcr_data matrix to the data frame(data frame is having two columns: time and zcr)
    zcr_data_frame <- as.data.frame(zcr_data)
    
    trimmed_zcr_data_frame <- zcr_data_frame[1:100, ]
    
    trimmed_zcr_data_frame["id"] <- counter
    
    return(trimmed_zcr_data_frame)
    
  }
  
  
  #********* END: Extract and store the ZCR feature data *************#
  
  
  #----------------------------------------------------------------------------#
  #*********  START: Storing extracted features to the database       *********#
  #----------------------------------------------------------------------------#
  
  saveData <- function(data, table_name) {
        dbWriteTable(conn, value = data, name = table_name, row.names = FALSE, append = TRUE)
  }
  
  #********* END: Storing extracted features to the database ***********#
  
  
  
})