#Load Libraries
library(tuneR)
library(h2o)
library(readr)
library(stringr)

midi2abc = function(pathIn, pathOut, name){
  
  # renaming files
  setwd(pathIn)
  system("ls | cat -n | while read n f; do mv \"$f\" \"$n.mid\"; done")
  
  # Detecting files
  files = list.files(pathIn)
  
  # trasform .mid 2 .txt
  for (file in files){
    inFile = file.path(pathIn,file)
    outFile = file.path(pathIn,gsub(".mid",".txt",file))
    system(paste0("midi2abc -f ", inFile," -o ",outFile))
  }
  
  files = list.files(pathIn, pattern = ".txt")
  
  # extract data text from .txt files
  finalStr = ""
  for (file in files){
    str = read_file(file.path(pathIn,file))
    strsplited = strsplit(str, "\n")
    
    for (i in 1:length(strsplited[[1]])){
      s = strsplited[[1]][i]
      if (!grepl(":",s)){
        if (!grepl("%",s)){
          finalStr = paste(finalStr,s)
        }
      }
    }
    
    # remove .txt file
    system(paste0("rm ",file.path(pathIn,file)))
  }
  
  # remove strange characters
  finalStr = str_replace_all(finalStr,"\\|","") 
  finalStr = str_replace_all(finalStr,'\\]',"") 
  finalStr = str_replace_all(finalStr,"\\[","") 
  finalStr = str_replace_all(finalStr,"x8","") 
  finalStr = str_replace_all(finalStr,"x6","") 
  finalStr = str_replace_all(finalStr,"x4","") 
  finalStr = gsub("([\\])","", finalStr)
  finalStr = str_replace_all(finalStr,"  "," ") 
  
  new_file = file.path(pathOut,name)
  saveRDS(finalStr,new_file)
}

acb2midi = function(predict, pathOut, name){
  
  # TODO
  return()
  
  path = file.path(pathOut,name)
  filein = gsub(".mid",".abc",path)
  sink(filein)
  cat("X: 1\n")
  cat("T:Speed the Plough\n")
  cat("M: 4/4\n")
  cat("C: trad.\n")
  cat("Q:1/4=150\n")
  cat("% Last note suggests minor mode tune\n")
  cat("K:B % 5 sharps\n")
  cat("V:1\n")
  cat("% Pulse Synth 5 1\n")
  cat(predict,"\n")
  sink()
  
  system(paste0("midi2abc -f ", filein," -o ",path))
}

mid2RDS = function(path){
  
  # Detecting files
  files = list.files(path)
  final_mid = c()
  
  for (file in files){
    
    print(file)
    mid = readMidi(paste0(path,file))
    mid = tuneR::getMidiNotes(mid)
    mid_clean = mid[,c(1,2,5)]
    
    for (i in 1:nrow(mid_clean)){
      mid_clean[i,1] = mid_clean[i,1]%%3072
    }
    
    mid_clean_next = rbind(mid_clean[2:nrow(mid_clean),],mid_clean[1,])
    
    new_mid = cbind(mid_clean,mid_clean_next)
    final_mid = rbind(final_mid,new_mid)
    names(final_mid) = c("time","length","note","timeTarget","lengthTarget","noteTarget")
  }
  
  new_file = paste0(getwd(),"/Data/RDS/allMelodies.rds")
  saveRDS(final_mid,new_file)
}

loadRDS = function(path){
  return(readRDS(path))
}

saveNNModel = function(model, name){
  model_path <- h2o.saveModel(object=model, path=paste0(getwd(),"/Bin/NeuralNetworks/Models/",name), force=TRUE)
}

loadNNModel = function(path){
  return(h2o.loadModel(path))
}

dataFrame2MidText = function(data,name="melody.txt"){
  path = paste0(getwd(),"/Data/Melodies/")
  sink(paste0(path,name))
  cat("MFile 1 2 96\n")
  cat("MTrk\n")
  cat("0 Tempo 400000\n")
  cat("0 TimeSig 4/4 24 8\n")
  cat("0 Meta TrkEnd\n")
  cat("TrkEnd\n")
  cat("MTrk\n")
  cat("0 Meta TrkName \"Pulse Synth 5 1\"\n")
  
  data$event = "On"
  for (i in 1:nrow(data)){
    row = data[i,]
    row$event = "Off"
    row$time = row$time + row$length
    data = rbind(data,row)
  }
  
  data = data[with(data, order(time)), ]
  for (i in 1:nrow(data)){
    cat(data$time[i], data$event[i], "ch=2", paste0("n=", data$note[i]), paste0("v=100","\n"))
  }
  
  cat(data$time[i],"Meta TrkEnd\n")
  cat("TrkEnd")
  sink()
}