library(readxl)
library(plyr)
library(qcc)
library(data.table)
library(stringr)
library(autoimage)
library(ggplot2)
library(tidyr)
opar <- par()


Traveler_Errors <- read_excel("C:/Users/Hpalcich/Desktop/Traveler Errors.xlsx", 
                              col_types = c("text", "date", "text", 
                                            "text", "text", "text", "text", "text", 
                                            "text"))

#make each job a factor
Traveler_Errors$Description <- as.factor(Traveler_Errors$Description)
Traveler_Errors$`DSQR Missed` = as.factor(toupper(Traveler_Errors$`DSQR Missed`))


Operations = unique(Traveler_Errors$Description)




errortypes = unite(Traveler_Errors,Totalerrors,`Type of error(s)`:X__3)
errortypes = separate_rows(errortypes,Totalerrors, sep = '_')


for (i in 1:length(Operations)){
  operationerror = subset(errortypes,errortypes$`Description` == as.character(Operations[i]))
  operationalerror = count(operationerror$Totalerrors)
  
  j <- sapply(operationalerror, is.factor)
  operationalerror[j] <- lapply(operationalerror[j], as.character)
  operationalerror = operationalerror[ grep("NA", operationalerror$x, invert = TRUE) , ]
  operationerrors = operationalerror[,2]
  names(operationerrors) = operationalerror[,1]
  pareto.chart(operationerrors,col=rainbow(length(operationerrors)), main = Operations[i] )
  dev.copy(pdf,paste(Operations[i],'Traveler Errors',".pdf"))
  dev.off()
}


sapply(Traveler_Errors, class)


#count the total error by tak and make a pareto chart
errors_by_task = count(Traveler_Errors$Description)
errors = errors_by_task[,2]
names(errors) = errors_by_task[,1]
pareto.chart(errors,col=rainbow(length(errors)))

#lets check how errors occur by date
dates = table(Traveler_Errors$Deadline)

pareto.chart(dates,col=rainbow(length(dates)))

#how about day of the week
errordates = as.data.frame(dates)
colnames(errordates)[colnames(errordates)=="Var1"] <- "Date of errors"
errordates$day <- weekdays(as.Date(errordates$`Date of errors`))

dayofweek = table(errordates$day)
pareto.chart(dayofweek,col=rainbow(length(dayofweek)))


#lets check how much the DSQR's miss

DSQRmissrate = count(Traveler_Errors$`DSQR Missed`)

Kylemissrate = subset(Traveler_Errors,Traveler_Errors$`DSQR Missed` == "Y" & Traveler_Errors$`DSQR Name` == 'KBL')
Kyletotal = subset(Traveler_Errors,Traveler_Errors$`DSQR Name` == 'KBL')

Brianmissrate = subset(Traveler_Errors,Traveler_Errors$`DSQR Missed` == "Y" & Traveler_Errors$`DSQR Name` == 'BBE')
Briantotal = subset(Traveler_Errors,Traveler_Errors$`DSQR Name` == 'BBE')

DSQRmissrate = subset(Traveler_Errors,Traveler_Errors$`DSQR Missed` == "Y")

Brianpercenterror = (nrow(Brianmissrate)/nrow(Briantotal))*100

Kylepercenterror = (nrow(Kylemissrate)/nrow(Kyletotal))*100

BrianOperations = table(Brianmissrate$Description)

KyleOperations = table(Kylemissrate$Description)

DSQROperations = table(DSQRmissrate$Description)

test = cbind(BrianOperations,KyleOperations)
test = as.data.frame(test)

par(mar=c(10,4,4,2))

barplot(t(test[c('BrianOperations','KyleOperations')]), main="Operations missed by DSQR", 
        xlab="", col=c('blue','red'), las=2,names.arg=row.names(test),beside=T, legend.text=c('Brian','Kyle'))
mtext(text = "Operation",
      side = 1,#side 1 = bottom
      line = 8)


barplot(BrianOperations, main="Operations missed by Brian", 
        xlab="", col=rainbow(length(dayofweek)), las=2)
mtext(text = "Operation",
      side = 1,#side 1 = bottom
      line = 8)

barplot(KyleOperations, main="Operations missed by Kyle", 
        xlab="", col=rainbow(length(dayofweek)), las=2)
mtext(text = "Operation",
      side = 1,#side 1 = bottom
      line = 8)

barplot(DSQROperations, main="Operations missed by DSQR's", 
        xlab="", col= 'red', las=2)
mtext(text = "Operation",
      side = 1,#side 1 = bottom
      line = 8)

print(paste("Brian''s error rate is: ", as.character(round(Brianpercenterror)),'%'))

print(paste("Kyle''s error rate is: ", as.character(round(Kylepercenterror)),'%'))


#time to check if theres any specific jobs that have the most errors

jobs = str_split_fixed(Traveler_Errors$`Job #`,"-",2)

Parts = count(jobs[,1])

jobs = Parts[,2]
names(jobs) = Parts[,1]
pareto.chart(jobs,col=rainbow(length(jobs)))