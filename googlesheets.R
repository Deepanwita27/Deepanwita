install.packages("googlesheets")
install.packages("xlsx")
install.packages("readxl")
library(googlesheets)
A = gs_title("e3sheet")
B = gs_read(A)
gs_deauth()

library(readxl)
A = read_xlsx("/Users/bajajvbh/Desktop/Vaibhav/50_Startups.xlsx",sheet = "new_sheet1",col_names = FALSE)

colnames(A) = c("RND","MKT","ADM","STATE","PROFIT")
B = A[1:40,1:3]
gs_upload(file.choose())

A = read_xlsx("/Users/bajajvbh/Downloads/EMP.xlsx")
library(sqldf)
B = sqldf("select * FROM A WHERE SALARY > (SELECT AVG(SALARY) FROM A)")

write.csv(B,file = "YAHOO.csv")








