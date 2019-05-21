# table 2 is Dec Encoding, Real vs. Imaginary
dec_enc_files <- list.files('.','*-table2.csv')
# table 7 is Dec Recall, Seen vs. New
dec_rec_files <- list.files('.','*-table7.csv')

# Import and process the Dec Encoding data (Real vs. Imaginary judgments)
for( f in 1:length(dec_enc_files) ) {
  out_table <- read.csv(dec_enc_files[f],header=TRUE)
  
  # These will be the aggregated variables to use in analyses, so you can ignore the other stuff
  out_table$DecEncACC <- NA
  out_table$DecEncRT <- NA
  
  # If response comes in during the Object presentation (first 500 ms), record that
  out_table$DecEncACC[out_table$Dec_RetenObject.TouchScreen.Responses != 'None'] <-
    out_table$Dec_RetenObject.TouchScreen.IsCorrect[out_table$Dec_RetenObject.TouchScreen.Responses != 'None']
  out_table$DecEncRT[out_table$Dec_RetenObject.TouchScreen.Responses != 'None'] <-
    out_table$Dec_RetenObject.TouchScreen.ResponseTime[out_table$Dec_RetenObject.TouchScreen.Responses != 'None']
  
  # If NO response comes in during the Object presentation (first 500 ms), record the Waiting period response
  out_table$DecEncACC[
    out_table$Dec_RetenObject.TouchScreen.Responses == 'None' &
                        out_table$Dec_RetenWait4Resp.TouchScreen.Responses != 'None'] <-
    out_table$Dec_RetenWait4Resp.TouchScreen.IsCorrect[
      out_table$Dec_RetenObject.TouchScreen.Responses == 'None' &
        out_table$Dec_RetenWait4Resp.TouchScreen.Responses != 'None']
  
  out_table$DecEncRT[
    out_table$Dec_RetenObject.TouchScreen.Responses == 'None' &
      out_table$Dec_RetenWait4Resp.TouchScreen.Responses != 'None'] <-
    out_table$Dec_RetenWait4Resp.TouchScreen.ResponseTime[
      out_table$Dec_RetenObject.TouchScreen.Responses == 'None' &
        out_table$Dec_RetenWait4Resp.TouchScreen.Responses != 'None'] + 500
  
  out_table$subj <- as.factor(substr(dec_enc_files[f],1,6))
  out_table$CorrectResponse <- NA
  out_table$CorrectResponse[out_table$TrialTable.CorrectButton=='DecLeftElement'] <- 'Real'
  out_table$CorrectResponse[out_table$TrialTable.CorrectButton=='DecRightElement'] <- 'Imaginary'
  out_table$CorrectResponse <- as.factor(out_table$CorrectResponse)
  
  if(f==1){ mydata_enc <- out_table}
  else {mydata_enc <- rbind(mydata_enc,out_table)}
}

# Import and process the Dec Recall data (Seen vs. New judgments)
for( f in 1:length(dec_rec_files) ) {
  out_table <- read.csv(dec_rec_files[f],header=TRUE)
  
  # These will be the aggregated variables to use in analyses, so you can ignore the other stuff
  out_table$DecRecACC <- NA
  out_table$DecRecRT <- NA
  
  # If response comes in during the Object presentation (first 500 ms), record that
  out_table$DecRecACC[out_table$Dec_RetenObject.TouchScreen.Responses != 'None'] <-
    out_table$Dec_RetenObject.TouchScreen.IsCorrect[out_table$Dec_RetenObject.TouchScreen.Responses != 'None']
  out_table$DecRecRT[out_table$Dec_RetenObject.TouchScreen.Responses != 'None'] <-
    out_table$Dec_RetenObject.TouchScreen.ResponseTime[out_table$Dec_RetenObject.TouchScreen.Responses != 'None']
  
  # If NO response comes in during the Object presentation (first 500 ms), record the Waiting period response
  out_table$DecRecACC[
    out_table$Dec_RecogObject.TouchScreen.Responses == 'None' &
      out_table$Dec_RecogWait4Resp.TouchScreen.Responses != 'None'] <-
    out_table$Dec_RecogWait4Resp.TouchScreen.IsCorrect[
      out_table$Dec_RecogObject.TouchScreen.Responses == 'None' &
        out_table$Dec_RecogWait4Resp.TouchScreen.Responses != 'None']
  
  out_table$DecRecRT[
    out_table$Dec_RecogObject.TouchScreen.Responses == 'None' &
      out_table$Dec_RecogWait4Resp.TouchScreen.Responses != 'None'] <-
    out_table$Dec_RecogWait4Resp.TouchScreen.ResponseTime[
      out_table$Dec_RecogObject.TouchScreen.Responses == 'None' &
        out_table$Dec_RecogWait4Resp.TouchScreen.Responses != 'None'] + 500
  
  out_table$subj <- as.factor(substr(dec_rec_files[f],1,6))
  out_table$CorrectResponse <- NA
  out_table$CorrectResponse[out_table$TrialTable.CorrectButton=='DecLeftElement'] <- 'Oui'
  out_table$CorrectResponse[out_table$TrialTable.CorrectButton=='DecRightElement'] <- 'Non'
  out_table$CorrectResponse <- as.factor(out_table$CorrectResponse)
  
  if(f==1){ mydata_rec <- out_table}
  else {mydata_rec <- rbind(mydata_rec,out_table)}
}

# Some tests
library(plyr)
subj_encode <- ddply(mydata_enc,c('subj','TrialTable.CorrectButton','TrialTable.RealMadeUp','CorrectResponse'),
                    summarise,
                    ACC = mean(DecEncACC,na.rm=TRUE),
                    RT = mean(DecEncRT,na.rm=TRUE),
                    N = sum(DecEncACC>=0,na.rm=TRUE))
subj_recog <- ddply(mydata_rec,c('subj','TrialTable.CorrectButton','TrialTable.RealMadeUp','CorrectResponse'),
                    summarise,
                    ACC = mean(DecRecACC,na.rm=TRUE),
                    RT = mean(DecRecRT,na.rm=TRUE),
                    N = sum(DecRecACC>=0,na.rm=TRUE))

library(lmerTest)
model1 <- lmer(ACC ~ TrialTable.RealMadeUp + (1|subj) , subj_encode, weights=N)
summary(model1)
model2 <- lmer(ACC ~ CorrectResponse * TrialTable.RealMadeUp + (1|subj) , subj_recog, weights=N)
summary(model2)

# Encoding Phase plots
plot(subj_encode$TrialTable.RealMadeUp, subj_encode$ACC)
plot(subj_encode$TrialTable.RealMadeUp, subj_encode$RT)
plot(subj_encode$CorrectResponse, subj_encode$ACC)
plot(subj_encode$CorrectResponse, subj_encode$RT)

# Recognition Phase plots
plot(subj_recog$TrialTable.RealMadeUp, subj_recog$ACC)
plot(subj_recog$TrialTable.RealMadeUp, subj_recog$RT)
plot(subj_recog$CorrectResponse, subj_recog$ACC)
plot(subj_recog$CorrectResponse, subj_recog$RT)