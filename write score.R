write.score <- function(object, VariableLength = 3, VariableName = VariableName, ScoreName = ScoreName){
  
  #load conditions
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
    
    score[i] <- paste("if(", j.arg, ", ", scr, ", ",   sep = "")
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