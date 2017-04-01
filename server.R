library(shiny)
library(rsconnect)
library(tools) #for checking file extension
library(tuneR)
library(seewave)
library(dplyr)
library(tidyr)
library(DBI)
library(RMySQL)



shinyServer(function(input, output){
  
  #------- START: reactive function to detect the file upload change -------#
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
  
  #------- END: reactive function to detect the file upload change -------#
  
  output$fileDetails <- renderPlot({
               wavFile <- getRagaFile()
               
               if(is.null(wavFile)){
                 return(NULL)
               }else{
                 
                 #converting sterio channels to mono
                 monoWave <- mono(wavFile, "left")
              
                 #--------  START : MFCC feature extraction  -------#
                 melfc_data <- melfcc(monoWave, sr = monoWave@samp.rate, wintime = 0.025,
                                      hoptime = 0.01, numcep = 12, lifterexp = 0.6, htklifter = FALSE,
                                      sumpower = TRUE, preemph = 0.97, dither = FALSE, usecmp = FALSE,
                                      modelorder = NULL, spec_out = FALSE, frames_in_rows = TRUE)
                 
                 #defining column names for the melfc_data
                 melfc_data_colname <- c("coef_01", "coef_02", "coef_03", "coef_04", "coef_05", "coef_06", "coef_07", "coef_08", "coef_09", "coef_10", "coef_11", "coef_12")

                 #assigning the column names to the melfc_data
                 colnames(melfc_data) <-  melfc_data_colname                
                 
                 #convert melfc_data vetor to the melfc_data_frame
                 melfc_data_frame <- data.frame(melfc_data)
                 
                 print("-*-*-* melfc_data_frame-**-*-* ")
                 print(str(melfc_data_frame))
                 #--------  END : MFCC feature extraction  -------#
                 
                 #--------  START : Zero crossing rate feature extraction  -------#
                 
                 #extracting the Zero crossing rate feature
                 zcr_data <- zcr(monoWave, wl = 512, ovlp = 0, plot = FALSE)
                 
                 #converting the zcr_data matrix to the data frame(data frame is having two columns: time and zcr)
                 zcr_data_frame <- as.data.frame(zcr_data)
                 
                 print("-*-*-* zcr_data frame-**-*-* ")
                 print(str(zcr_data_frame))
                 #--------  END : Zero crossing rate feature extraction  -------#
                 
#                 return(zcr_data)
                 
                 dat = data.frame(melfc_data_frame$coef_01, melfc_data_frame$coef_02)
                 
                 km1 = kmeans(dat, 2, nstart=100)
                 
                 # Plot results
                 plot(dat, col =(km1$cluster +1) , main="K-Means result with 2 clusters", pch=20, cex=2)
               }
              
  })
})