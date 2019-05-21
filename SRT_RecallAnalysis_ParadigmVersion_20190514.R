# table 9 is recall, sequence (blocks 2 & 3)
seq_files <- list.files('.','*-table9.csv')
for( f in 1:length(seq_files) ) {
  out_table <- read.csv(seq_files[f],header=TRUE)
  out_table$cond = 'seq'
  out_table$subj = substr(seq_files[f],1,6)
  out_table$Block[out_table$Trial<=40] <- 2
  out_table$Block[out_table$Trial>40] <- 3
  out_table$PrevTrial <- as.factor(c('0',out_table$TrialTable.Correct[1:(dim(out_table)[1]-1)]))
  
  if(f==1){ mydata_seq <- out_table}
  else {mydata_seq <- rbind(mydata_seq,out_table)}
}
names(mydata_seq)[7]<-'IsCorrect'
names(mydata_seq)[8]<-'ResponseTime'
names(mydata_seq)[9]<-'Responses'

# table 10 is recall, random (blocks 4 & 5)
ran_files = list.files('.','*-table10.csv')
for( f in 1:length(ran_files) ) {
  out_table <- read.csv(ran_files[f],header=TRUE)
  out_table$cond = 'ran'
  out_table$subj = substr(ran_files[f],1,6)
  out_table$Block[out_table$Trial<=40] <- 4
  out_table$Block[out_table$Trial>40] <- 5
  out_table$PrevTrial <- as.factor(c('0',out_table$TrialTable.Correct[1:(dim(out_table)[1]-1)]))

  if(f==1){ mydata_ran <- out_table}
  else{mydata_ran <- rbind(mydata_ran,out_table)}
}
names(mydata_ran)[7]<-'IsCorrect'
names(mydata_ran)[8]<-'ResponseTime'
names(mydata_ran)[9]<-'Responses'

mydata <- rbind(mydata_seq,mydata_ran)

# Filter some bad cases
mydata$ResponseTime[mydata$IsCorrect!=1] <- NA
mydata$ResponseTime[mydata$ResponseTime<150] <- NA
mydata$ResponseTime[mydata$ResponseTime>3000] <- NA
mydata$Block <- as.factor(mydata$Block)


library(lmerTest)
RT_model <- lmer(ResponseTime ~ cond + (1|subj) + (TrialTable.Correct|PrevTrial),mydata[mydata$Block==3 | mydata$Block==4,])
summary(RT_model)

library(plyr)
subjlevel <- ddply(mydata,c('subj','cond','Block'),summarise, subjRT = mean((ResponseTime),na.rm=TRUE))
plot(subjlevel$Block,subjlevel$subjRT)
