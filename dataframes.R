#-------------CREATE DATAFRAME FROM VECTORS--------------

eid = c(101,102,103,104)
fn = c("Swati","Shashank","Nikhil","Ajay")
ln = c("Bhalerao","Kumar","Dhayarikar","Kadam")
sal = c(5000,6000,7000,8000)

employees = data.frame(eid,fn,sal,ln)
View(A)

#---------

eid = c(101,102,103,104)
fn = c("Swati","Shashank","Nikhil","AJay")
ln = c("Bhalerao","Kumar","Dhayarikar","Kadam")
sal = c(5000,6000,7000,8000)
did = c(10,10,20,20)

employees = data.frame(eid,fn,sal,ln,stringsAsFactors = FALSE)
str(employees)

#------------ADDING A NEW COL TO DF / REMOVING A COL FROM DATA FRAME---------------


employees = data.frame(employees,did)
employees

employees = employees[,c(-6)]
#-------------data refrence--------------

str(employees)
employees = data.frame(employees,did)
employees$fn
employees$did.1
employees
employees$fn
employees
employees[1,]
employees[c(1,2),]
employees[c(1,2,3),]
employees[,]
employees
employees[,c(2,3)]

#------------CREATE DF FROM CSV---------------

A = read.csv("/Users/bajajvbh/Desktop/Vaibhav/abc2.csv")

A = read.csv("/Users/bajajvbh/Desktop/Vaibhav/abc2.csv",header = FALSE,col.names = c("eid","fn","sal","ln","did"))

A = read.csv(file.choose(),header = FALSE,col.names = c("eid","fn","sal","ln","did"))

write.csv(employees,"/Users/bajajvbh/Desktop/Vaibhav/abc.csv")
