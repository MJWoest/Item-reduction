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