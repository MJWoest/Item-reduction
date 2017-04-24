R.to.Limesurvey.path <- function(object, VariableLength = 3){
  
  path <- rep(NA, length(rl))
  ft <- fitted(object) 
  lb <- tapply(ft[[2]], ft[[1]], function(y) 
    names(sort(table(y), decreasing = TRUE))[1]) 
  rl <- partykit:::.list.rules.party(object) 
  
  for(i in 1:length(rl)){
    
    nd <- unlist(strsplit(rl[i], "&"))
    nd <- gsub("\"", "", nd)
    
    for(j in 1:length(nd)){
      
      if (j == 1){
        to <- substr(nd[j+1], 1,4)
        ndt <- paste("if", nd[j],"go to",to)
      }
      
      if (j+1 > length(nd)| j==1){next}
      else{
        to <- substr(nd[j+1], 1,VariableLength+1)
        nd[j] <- paste(nd[j-1], "&", nd[j])
        ndt <- paste("if", nd[j],"go to",to)  
      }
    }
    
    if (ndt %in% path){next}
    else {
      k <- min(which(path %in% NA))
      path[k] <- ndt
    }
  }
  
  path <- path[!is.na(path)]
  return(path)
}

R.to.Limesurvey.score <- function(object,...){
  
  score <- rep(NA, length(rl))
  ft <- fitted(object) 
  lb <- tapply(ft[[2]], ft[[1]], function(y) 
    names(sort(table(y), decreasing = TRUE))[1]) 
  rl <- partykit:::.list.rules.party(object) 
  
  for(i in 1:length(rl)){
    
    nd <- unlist(strsplit(rl[i], "&"))
    nd <- gsub("\"", "", nd)
    
    for(j in 1:length(nd)){
      
      if (j == 1){
        ndt <- paste(nd[j]," Score == &",lb[i])
      }
      else{
        nd[j] <- paste(nd[j-1], "&", nd[j])
        ndt <- paste(nd[length(nd)],"Score == &",lb[i])
      }
    }
    
    if (ndt %in% score){next}
    else {
      k <- min(which(score %in% NA))
      score[k] <- ndt
    }
  }
  
  score <- score[!is.na(score)]
  return(score)
}
write.limesurvey <- function(object, VariableLength = 3, VariableName = "PQ"){
  if(!require("readxl")){
    install.packages("readxl")
  }
  
  if(!is.loaded("readxl")){
    library("readxl")
  }
  
  if(!require("partykit")){
    install.packages("partykit")
  }
  
  if(!is.loaded("partykit")){
    library("partykit")
  }
  
  message("import excel file of Limesurvey questionnaire")
  dt.excel <- read_excel(choose.files())
  
  rl <- partykit:::.list.rules.party(object)
  path <- R.to.Limesurvey.path(object)
  
  u <- model.frame.rpart(object)
  u <- as.character(u$formula)
  u <- u[3]
  nds <- rep(NA, ceiling(nchar(u)/(VariableLength+3)))
  
  for(i in 1:ceiling(nchar(u)/(VariableLength+3))){
    nds[i] <- substr(u, VariableLength-(VariableLength-1)+((VariableLength+3)*(i-1)),VariableLength+((VariableLength+3)*(i-1)))
    nds[i] <- gsub("[A-z]+", VariableName, nds[i])
  }
  
  rn <- substr(rl[1], 1, VariableLength)
  n <- VariableLength
  rn <- gsub("[A-z]+", VariableName, rn)
  
  for(i in 1:length(path)){
    to <- substr(path[i],1,VariableLength)
    to <- gsub("[A-z]+" , VariableName, to)
    place <- match(to, dt.excel$name)
    
    nd <- unlist(strsplit(path[i], "&"))
    
    for(j in 1:length(nd)){
      if(j == 1){ 
        to <- substr(nd[j], VariableLength+5,2*VariableLength+4)
      }
      
      if(!j == 1){
        to <- substr(nd[2], 3, VariableLength+2)
      }
      
      nd[j] <- gsub(".*\\(", "", nd[j])
      nd[j] <- gsub("\\).*", "", nd[j])
      answers <- unlist(strsplit(nd[j], ", "))
      
      for(k in 1:length(answers)){
        ans <- paste("\"", "A", as.numeric(answers[k])+1, "\"" ,sep = "")
        to <- gsub("[A-z]+", VariableName, to) 
        cond <- paste(to, "_SQ001.NAOK == ", ans, sep = "")
        
        if(k == 1){
          k.arg <- cond
        }
        
        if(!k == 1){
          k.arg <- paste(k.arg, "or", cond)
        }
        #scenarion maken
        if(k == length(answers)){
          k.arg <- paste("(", k.arg, ")", sep = "")
          
          if(j == 1){
            j.arg <- paste(k.arg)
          }
          
          if(!j == 1){
            j.arg <- paste(j.arg, "and", k.arg)
          }
        }
      }
      
      
      if(j == length(nd)){
        j.arg <- paste("(", j.arg, ")", sep = "")
      }
    }
    if(dt.excel$relevance[place] == 1){
      dt.excel$relevance[place] <- j.arg
    }
    if(!dt.excel$relevance[place] == 1){
      dt.excel$relevance[place] <- paste(dt.excel$relevance[place], "or", j.arg)
    }
  }
  for(j in 1:length(nds)){
    place <- match(nds[j], dt.excel$name)
    
    for(i in 1:length(nds)){
      if(i == 1 & exists("count")){
        rm(count)
      }
      
      if(nds[i] == dt.excel$name[place]){
        next
      }
      
      if(!exists("count")){
        count <- paste(nds[i], "_SQ001.NAOK", sep = "")
      }
      
      else{
        count <- paste(count, ", ", nds[i], "_SQ001.NAOK", sep = "")
      }
    }
    dt.excel$relevance[place] <- paste(dt.excel$relevance[place], " and ", "count(",  "(", count, ")", " < 3)")
    
  }
  suppressWarnings("readxl")
  start <- paste("Start questionnaire with", rn)
  message(start)
  return(dt.excel)
} 
write.score <- function(object, VariableLength = 3, VariableName = "PQ", ScoreName = "PAIN"){
  if(!require("partykit")){
    install.packages("partykit")
  }
  
  if(!is.loaded("partykit")){
    library("partykit")
  }
  
  score <- R.to.Limesurvey.score(object)
  
  for(i in 1:length(score)){
    
    nd <- unlist(strsplit(score[i], "&"))
    
    for(j in 1:(length(nd)-1)){
      if(j == 1){ 
        to <- substr(nd[j], 1,VariableLength)
      }
      
      if(!j == 1){
        to <- substr(nd[j], VariableLength, VariableLength+2)
      }
      
      nd[j] <- gsub(".*\\(", "", nd[j])
      nd[j] <- gsub("\\).*", "", nd[j])
      answers <- unlist(strsplit(nd[j], ", "))
      
      for(k in 1:length(answers)){
        ans <- paste("\"", "A", as.numeric(answers[k])+1, "\"" ,sep = "")
        to <- gsub("[A-z]+", VariableName, to) 
        cond <- paste(to, "_SQ001.NAOK == ", ans, sep = "")
        
        if(k == 1){
          k.arg <- cond
        }
        
        if(!k == 1){
          k.arg <- paste(k.arg, "or", cond)
        }
        
        #scenarion maken
        if(k == length(answers)){
          k.arg <- paste("(", k.arg, ")", sep = "")
          
          if(j == 1){
            j.arg <- paste(k.arg)
          }
          
          if(!j == 1){
            j.arg <- paste(j.arg, "and", k.arg)
          }
        }
      }
      
      if(j == (length(nd))){
        j.arg <- paste("(", j.arg, ")", sep = "")
      }
    }
    scr <- nd[length(nd)]
    
    score[i] <- paste("if(", j.arg, ", ", ScoreName, "SCORE", "=", scr, ", ",   sep = "")
  }
  
  score[length(score)] <- paste(score[length(score)], "\"error\"", sep = "")
  for(i in 1:length(score)){
    score[length(score)] <- paste(score[length(score)], ")", sep = "")
  }
  score[1] <- paste("{", score[1], sep = "")
  score[length(score)] <- paste(score[length(score)], "}", sep = "")
  
  message("Export output to excel and paste into an equation within Limesurvey")
  return(score) 
}

R.to.Limesurvey.path(current.chaid.run, VariableLength = 3)
R.to.Limesurvey.score(current.chaid.run)
excel <- write.limesurvey(current.chaid.run, VariableLength = 3, VariableName = "PQ")
score <- write.score(current.chaid.run, VariableLength = 3, VariableName = "PQ", ScoreName = "PAIN")

library("xlsx")
write.xlsx(excel, "F:/Plastische/CAT/R/Project Limesurvey/dt excel.xlsx")
write.xlsx(score, "F:/Plastische/CAT/R/Project Limesurvey/score in LS.xlsx")
 

