library("readxl")
dt.excel <- read_excel(choose.files())
   

dt.excel <- write.limesurvey(current.chaid.run.functie, VariableName = "FQ")
score <- write.score(current.chaid.run.functie, VariableName = "FQ", ScoreName = "DISABILITY")
         
library("xlsx")
write.xlsx(dt.excel, "F:/Plastische/Decision trees/4. R scripts/Project Limesurvey/dt excel.xlsx")
write.xlsx(score, "F:/Plastische/Decision trees/4. R scripts/Project Limesurvey/score in LS disability.xlsx")          

