##Read and follow these instructions, these functions have been made to convert a specific output.

##Run all other .R files that can be found in: https://github.com/MJWoest/Item-reduction/tree/master/R/3.%20LimeSurvey%20writing

##Import the .xlsx file of the LimeSurvey questionnaire

library("readxl")
dt.excel <- read_excel(choose.files())

##If all other functions are in the Environment, the following steps should work

dt.excel <- write.limesurvey(current.chaid.run.functie, VariableName = "FQ")
score <- write.score(current.chaid.run.functie, VariableName = "FQ", ScoreName = "DISABILITY")
         
library("xlsx")
write.xlsx(dt.excel, "F:/Plastische/Decision trees/4. R scripts/Project Limesurvey/dt excel.xlsx")
write.xlsx(score, "F:/Plastische/Decision trees/4. R scripts/Project Limesurvey/score in LS disability.xlsx")   
