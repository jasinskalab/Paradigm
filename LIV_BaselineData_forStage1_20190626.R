reqd_packages = c('readxl','janitor','stringr','data.table','ggplot2','tidyr','plyr','psych')
for (pack_i in (reqd_packages)) {
  if (!length(find.package(pack_i,quiet=TRUE))) {install.packages(pack_i)}
  library(pack_i,character.only=TRUE) 
}

setwd('~/Documents/Projects/LIV/Baseline_vs_Interim_Comparisons_May2019/')
LIV.data <- clean_names(read.csv('LIVPhoneBasedLiterac_DATA_2019-06-04_1339.csv'))
Baseline.data <- LIV.data[LIV.data$redcap_event_name %like% 'baseline_arm_',]

#####
# French words - Redcap marks (1) the incorrect words up until the time ran out (french_word_crochet(_lite)?)
# In the case of Autostop (french_word_autostop(_lite)?==1), the total number of words is zero
# Otherwise, the total number of words read is the number of 0's UP TO the crochet point.
Baseline.data$frenchwords <- ifelse(
  apply(is.na(Baseline.data[,names(Baseline.data) %like% '^motsfamilier(s)?\\d+_\\d+_\\d+']),1,all), NA, 
  pmax( # number of words attempted (crochet) - number of words incorrect (1)
  pmin(Baseline.data$french_word_crochet,50) -
  rowSums(Baseline.data[,names(Baseline.data) %like% 'motsfamilier(s)?\\d+_\\d+_\\d+']==1,na.rm=TRUE)
  ,0))
#Baseline.data$frenchwords[Baseline.data$french_word_autostop==1] <- 0

#####
# French pseudowords - Redcap marks (1) the incorrect words up until the time ran out (french_pseudoword_crochet(_lite)?)
# In the case of Autostop (french_word_autostop(_lite)?==1), the total number of words is zero
# Otherwise, the total number of words read is the number of 0's UP TO the crochet point.
Baseline.data$pseudowords <- ifelse(
  apply(is.na(Baseline.data[,names(Baseline.data) %like% '^motsinvente(s)?\\d+_\\d+_\\d+']),1,all), NA, 
  pmax( # number of words attempted (crochet) - number of words incorrect (1)
    pmin(Baseline.data$french_pseudoword_crochet,50) -
      rowSums(Baseline.data[,names(Baseline.data) %like% 'motsinvente(s)?\\d+_\\d+_\\d+']==1,na.rm=TRUE)
    ,0))
#Baseline.data$pseudowords[Baseline.data$french_pseudoword_autostop==1] <- 0

#####
# French graphemes - Follows the same format as words
Baseline.data$graphemes <- ifelse(
  apply(is.na(Baseline.data[,names(Baseline.data) %like% 'grapheme(s)?\\d+_\\d+_\\d+']),1,all), NA, 
  pmax( # number of graphemes attempted (crochet) - number of graphemes incorrect (1)
  pmin(Baseline.data$french_letters_crochet,100) -
    rowSums(Baseline.data[,names(Baseline.data) %like% 'grapheme(s)?\\d+_\\d+_\\d+']==1,na.rm=TRUE)
  ,0))
#Baseline.data$graphemes[Baseline.data$french_letters_autostop==1] <- 0

#####
# French phoneme awareness
# this approach ignores the autostop because there are no default values for missing responses.
# we can count ONLY the 1's (correct) and ignore the 2's (incorrect) and 3's (no response) and NAs (no information recorded)
Baseline.data$phonemes <- ifelse(
  apply(is.na(Baseline.data[,names(Baseline.data) %like% '^french_((id)|(int)|(fin)|(seg))\\d+$']),1,all),
  NA, rowSums(Baseline.data[,names(Baseline.data) %like% '^french_((id)|(int)|(fin)|(seg))\\d+$']==1,na.rm=TRUE))

#####
# French sentences (passage comprehension)
Baseline.data$sentenceCorr <- scale(
  (pmin(Baseline.data$french_passcomp_crochet,60,na.rm=T)
   - rowSums(Baseline.data[,names(Baseline.data) %like% 'french_passcomp_incorrect\\d_\\d+$']==1,na.rm=TRUE))
  *(as.numeric(1-Baseline.data$french_passcomp_autostop)),
  center=T, scale=T)
Baseline.data$sentenceCorr[is.na(Baseline.data$sentenceCorr)] <- 0 # Many children have no sentenceCorr data, so mean impute here

# Reading times
Baseline.data[,names(Baseline.data) %like% 'french_passcomp(2?)_time(_lite)?'] <- lapply(
  Baseline.data[,names(Baseline.data) %like% 'french_passcomp(2?)_time(_lite)?'], 
  function(x) pmin(as.numeric(gsub("[^0-9.]", "",x)),60)) # restrict times to a maximum of 60
Baseline.data[,names(Baseline.data) %like% 'french_passcomp(2?)_time(_lite)?'] <- lapply(
  Baseline.data[,names(Baseline.data) %like% 'french_passcomp(2?)_time(_lite)?'], 
  function(x) as.numeric(x<=10)*60 + as.numeric(x>10)*x) # times of 10 sec or faster are improbable, set to cutoff of 60s
Baseline.data$sentenceTime <- scale(
  -rowMeans(Baseline.data[,names(Baseline.data) %like% 'french_passcomp(2?)_time(_lite)?'],na.rm=T) 
  ,center=T,scale=T) # z-score and reverse so faster is higher-z and slower is lower-z

# Comprehension questions
Baseline.data$sentenceQAcc <- scale(
  ifelse(apply(is.na(Baseline.data[,names(Baseline.data) %like% 'french_passcomp(2?)q\\d+$']),1,all),NA
         , rowSums(Baseline.data[,names(Baseline.data) %like% 'french_passcomp(2?)q\\d+$']==1,na.rm=TRUE))
  , center=T,scale=T)

# Combine the passcomp fields using a PCA
Baseline.data$sentences <- principal(
  Baseline.data[,names(Baseline.data) %like% '^sentence((Time)|(QAcc)|(Corr))$']
  , nfactors=sum(names(Baseline.data) %like% '^sentence((Time)|(QAcc)|(Corr))$')
  , rotate="none", scores=T, missing=F)$scores[,1]

#####
keep_cols <- which(names(Baseline.data) %in% c('record_id',
                                             'child_name_french',
                                             'frenchwords',
                                             'pseudowords',
                                             'graphemes',
                                             'phonemes',
                                             'sentences',
                                             'sentenceTime',
                                             'sentenceQAcc',
                                             'sentenceCorr'))

short.data <- Baseline.data[,keep_cols]

setwd('~/Documents/Projects/STL/Stage1_mansucript_June2019/')
write.csv(short.data,'STL_BaselineLIV_forStage1_20190628.csv')


### Plot phoneme awareness, letter, word, sentence reading abilities of the LIV children. 
##Four panels, one for each of the measures, histograms in each panel 
##(accuracy for phoneme, letter, and word; timing for sentences)

### long df
library(ggplot2)
gather(short.data, key = "Literacy", value = "Accuracy", "phonemes", "graphemes", "pseudowords","frenchwords","sentenceTime","sentenceQAcc") ->longacc 


#Group Means
mu1 <- plyr::ddply(.data=longacc, .(Literacy), summarise, grp.mean1=mean(Accuracy, na.rm=TRUE))
head(mu1)

## Plot
acchist<-ggplot(longacc, aes(x=Accuracy)) +
  geom_histogram(position="identity", alpha=0.5) +
  geom_vline(data=mu1, aes(xintercept=grp.mean1)) +
  labs(title="Accuracy for Each Literacy Assessment") +
  facet_wrap(~Literacy, scales = 'free_x')

acchist
