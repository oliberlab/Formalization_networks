library(tidyverse)
library(QCA)
#Replace path with path in your own directory. 
path <- "/Users/olivier/Library/CloudStorage/SynologyDrive-Olli/Job/Coding/Projects/GitHub/Formalization_networks/data/AllCases20220605.csv"
#Upload dataset
cases <- read.csv(path, sep=";")
#Study 0 - Tests et essais
cases_FP <- cases %>% filter(.$Q23_Start_for.profit == "Yes" |
                            .$Q23_Middle_for.profit == "Yes" |  
                            .$Q23_End_for.profit == "Yes")
#Delete superfluous information
cases_FP <- cases_FP %>%
  select(-File.name,
         -Authors,
         -Institution,
         -e.mail.address.first.author)
#Delete cases with missing data on central items
cases_FP<-subset(cases_FP, 
            Q24_DK != "Yes" & 
              Q25_DK != "Yes" &
              Q26_DK != "Yes" &
              Q27_DK != "Yes" &
              Q39_DK != "Yes" &
              Q48_DK != "Yes" &
              Q49_DK != "Yes" &
              Q50_DK != "Yes" &
              Q51.1_DK != "Yes" &
              Q51.2_DK != "Yes" &
              Q51.3_DK != "Yes" &
              Q51.4_DK != "Yes")
#Replace all values set to "Off" with "NA", then NA with 0.
#Same procedure for "Yes", set to 5.
cases_FP[cases_FP == "Off"] <- NA
cases_FP[is.na(cases_FP)] <- 0
cases_FP[cases_FP == "Yes"] <- NA
cases_FP[is.na(cases_FP)] <- 5
#Create sets using extant items.
compute_score2 <- function(x){
  x <- as.numeric(as.character(x))
  x <- x/2
}
compute_score3 <- function(x){
  x <- as.numeric(as.character(x))
  x <- x/3
}
compute_score4 <- function(x){
  x <- as.numeric(as.character(x))
  x <- x/4
}
cases_FP <- cases_FP %>%
  mutate(., RUL = as.numeric(as.character(Q24_End)),
         ., CEN = as.numeric(as.character(Q39_End)),
         ., TRA = as.numeric(as.character(Q27_End)),
         ., MON = as.numeric(as.character(Q50_End)),
         ., GOA = as.numeric(as.character(Q48_End)),
         ., STR = as.numeric(as.character(Q49_End)),
         ., ACC = pmax(
           as.numeric(as.character(Q51.1_Middle)), 
           as.numeric(as.character(Q51.2_Middle)), 
           as.numeric(as.character(Q51.3_Middle)),
           as.numeric(as.character(Q51.4_Middle)),
           as.numeric(as.character(Q51.1_End)), 
           as.numeric(as.character(Q51.2_End)), 
           as.numeric(as.character(Q51.3_End)),
           as.numeric(as.character(Q51.4_End))),
         ., CON = as.numeric(as.character(Q32_End)),
         ., FOC = as.numeric(as.character(Q33_End)),
         ., MIT = as.numeric(as.character(Q34_End)),
         ., CRE = as.numeric(as.character(Q35_End)),
         ., LEG = as.numeric(as.character(Q58.1_End)),
         ., CAP = as.numeric(as.character(Q58.2_End)),
         ., SOC = as.numeric(as.character(Q58.3_End)),
         ., EFF = pmax(
           as.numeric(as.character(Q57.1_Middle)),
           as.numeric(as.character(Q57.1_End)),
           as.numeric(as.character(Q57.2_Middle)),
           as.numeric(as.character(Q57.2_End)),
           as.numeric(as.character(Q57.3_Middle)),
           as.numeric(as.character(Q57.3_End))),
         ., CHA = compute_score4(LEG) + 
           compute_score4(CAP) + 
           compute_score4(SOC) +
           compute_score4(EFF))
#Calibrate using recode. 
cases_FP_fs <- cases_FP %>%
  mutate(., cRUL = (recode(RUL,
                           cuts = "2, 3, 4",
                           values =  "0, 0.33, 0.66, 1")),
         ., cCEN = (recode(CEN,
                           cuts = "2, 3, 4",
                           values =  "0, 0.33, 0.66, 1")),
         ., cTRA = (recode(TRA,
                           cuts = "2, 3, 4",
                           values =  "0, 0.33, 0.66, 1")),
         ., cMON = (recode(MON,
                           cuts = "2, 3, 4",
                           values =  "0, 0.33, 0.66, 1")),
         ., cGOA = (recode(GOA,
                           cuts = "2, 3, 4",
                           values =  "0, 0.33, 0.66, 1")),
         ., cSTR = (recode(GOA, 
                          cuts = "2, 3, 4", 
                          values =  "0, 0.33, 0.66, 1")),
         ., cACC = (recode(ACC,
                           cuts = "2, 3, 4",
                           values =  "0, 0.33, 0.66, 1")),
         ., cLEG = (recode(LEG,
                           cuts = "2, 3, 4",
                           values =  "0, 0.33, 0.66, 1")),
         ., cCAP = (recode(CAP,
                           cuts = "2, 3, 4",
                           values =  "0, 0.33, 0.66, 1")),
         ., cSOC = (recode(SOC,
                           cuts = "2, 3, 4",
                           values =  "0, 0.33, 0.66, 1")),
         ., cEFF = (recode(EFF,
                           cuts = "2, 3, 4",
                           values =  "0, 0.33, 0.66, 1")),
         ., cCHA = (recode(CHA,
                           cuts = "2, 3, 4",
                           values =  "0, 0.33, 0.66, 1")))
cases_FP_fs_an <- cases_FP_fs %>%
  dplyr::select(cRUL:cCHA)
#Check for necessity for CHA
superSubset(cases_FP_fs_an, 
            outcome = "cACC", 
            neg.out = FALSE,
            relation = "sufficiency",
            incl.cut = 0.90,
            ron.cut = 0.70)


#Run analysis for top performers ("CHA") and all conditions.
ttACC <- truthTable(cases_FP_fs_an, outcome = "cCHA",
                    conditions = "cRUL, cCEN, cTRA, cMON, cGOA, cSTR, cACC",
                    incl.cut = 0.87,
                    show.cases = TRUE,
                    dcc = TRUE,
                    sort.by = "OUT, n")
ttACC

#Compute minimization of truth table and print conservative solution:
solacc_con <- minimize(ttACC, 
                       details = TRUE)
solacc_con

#Compute parsimonious solution and print results:
solacc_par <- minimize(ttACC,
                       details = TRUE,
                       include = "?",
                       show.cases = TRUE)
solacc_par

#Compute intermediary solution and print results:
solcha_int <- minimize(ttACC,
                       details = TRUE,
                       include = "?",
                       show.cases = TRUE,
                       dir.exp = "~cCEN")
solcha_int

#Study 1 - Accountability general
cases_FP <- cases %>% filter(.$Q23_Start_for.profit == "Yes" |
                               .$Q23_Middle_for.profit == "Yes" |  
                               .$Q23_End_for.profit == "Yes")
#Delete superfluous information
cases_FP <- cases_FP %>%
  select(-File.name,
         -Authors,
         -Institution,
         -e.mail.address.first.author)
#Delete cases with missing data on central items
cases_FP<-subset(cases_FP, 
                 Q24_DK != "Yes" & 
                   Q25_DK != "Yes" &
                   Q26_DK != "Yes" &
                   Q27_DK != "Yes" &
                   Q39_DK != "Yes" &
                   Q48_DK != "Yes" &
                   Q49_DK != "Yes" &
                   Q50_DK != "Yes" &
                   Q51.1_DK != "Yes" &
                   Q51.2_DK != "Yes" &
                   Q51.3_DK != "Yes" &
                   Q51.4_DK != "Yes")
#Replace all values set to "Off" with "NA", then NA with 0.
#Same procedure for "Yes", set to 5.
cases_FP[cases_FP == "Off"] <- NA
cases_FP[is.na(cases_FP)] <- 0
cases_FP[cases_FP == "Yes"] <- NA
cases_FP[is.na(cases_FP)] <- 5
#Create sets using extant items.
compute_score2 <- function(x){
  x <- as.numeric(as.character(x))
  x <- x/2
}
compute_score4 <- function(x){
  x <- as.numeric(as.character(x))
  x <- x/4
}
cases_FP <- cases_FP %>%
  mutate(., RUL = as.numeric(as.character(Q24_End)),
         ., CEN = as.numeric(as.character(Q39_End)),
         ., TRA = as.numeric(as.character(Q27_End)),
         ., MON = as.numeric(as.character(Q50_End)),
         ., GOA = as.numeric(as.character(Q48_End)),
         ., STR = as.numeric(as.character(Q49_End)),
         ., ACC = pmax(
           as.numeric(as.character(Q51.1_Middle)), 
           as.numeric(as.character(Q51.2_Middle)), 
           as.numeric(as.character(Q51.3_Middle)),
           as.numeric(as.character(Q51.4_Middle)),
           as.numeric(as.character(Q51.1_End)), 
           as.numeric(as.character(Q51.2_End)), 
           as.numeric(as.character(Q51.3_End)),
           as.numeric(as.character(Q51.4_End))),
         ., CON = as.numeric(as.character(Q32_End)),
         ., FOC = as.numeric(as.character(Q33_End)),
         ., MIT = as.numeric(as.character(Q34_End)),
         ., CRE = as.numeric(as.character(Q35_End)),
         ., LEG = as.numeric(as.character(Q58.1_End)),
         ., CAP = as.numeric(as.character(Q58.2_End)),
         ., SOC = as.numeric(as.character(Q58.3_End)),
         ., EFF = pmax(
           as.numeric(as.character(Q57.1_Middle)),
           as.numeric(as.character(Q57.1_End)),
           as.numeric(as.character(Q57.2_Middle)),
           as.numeric(as.character(Q57.2_End)),
           as.numeric(as.character(Q57.3_Middle)),
           as.numeric(as.character(Q57.3_End))),
         ., CHA = compute_score4(LEG) + 
           compute_score4(CAP) + 
           compute_score4(SOC) +
           compute_score4(EFF))
#Calibrate using recode. 
cases_FP_fs <- cases_FP %>%
  mutate(., cRUL = (recode(RUL,
                           cuts = "2, 3, 4",
                           values =  "0, 0.33, 0.66, 1")),
         ., cCEN = (recode(CEN,
                           cuts = "2, 3, 4",
                           values =  "0, 0.33, 0.66, 1")),
         ., cTRA = (recode(TRA,
                           cuts = "2, 3, 4",
                           values =  "0, 0.33, 0.66, 1")),
         ., cMON = (recode(MON,
                           cuts = "2, 3, 4",
                           values =  "0, 0.33, 0.66, 1")),
         ., cGOA = (recode(GOA,
                           cuts = "2, 3, 4",
                           values =  "0, 0.33, 0.66, 1")),
         ., cSTR = (recode(GOA, 
                           cuts = "2, 3, 4", 
                           values =  "0, 0.33, 0.66, 1")),
         ., cACC = (recode(ACC,
                           cuts = "2, 3, 4",
                           values =  "0, 0.33, 0.66, 1")),
         ., cLEG = (recode(LEG,
                           cuts = "2, 3, 4",
                           values =  "0, 0.33, 0.66, 1")),
         ., cCAP = (recode(CAP,
                           cuts = "2, 3, 4",
                           values =  "0, 0.33, 0.66, 1")),
         ., cSOC = (recode(SOC,
                           cuts = "2, 3, 4",
                           values =  "0, 0.33, 0.66, 1")),
         ., cEFF = (recode(EFF,
                           cuts = "2, 3, 4",
                           values =  "0, 0.33, 0.66, 1")),
         ., cCHA = (recode(CHA,
                           cuts = "2, 3, 4",
                           values =  "0, 0.33, 0.66, 1")))
cases_FP_fs_an <- cases_FP_fs %>%
  dplyr::select(cRUL:cCHA)
#Check for necessity for CHA
superSubset(cases_FP_fs_an, 
            outcome = "cACC", 
            neg.out = FALSE,
            relation = "sufficiency",
            incl.cut = 0.90,
            ron.cut = 0.70)


#Run analysis for top performers ("CHA") and all conditions.
ttACC <- truthTable(cases_FP_fs_an, outcome = "cACC",
                    conditions = "cRUL, cCEN, cTRA, cMON, cGOA, cSTR",
                    incl.cut = 0.85,
                    show.cases = TRUE,
                    dcc = TRUE,
                    sort.by = "OUT, n")
ttACC

#Compute minimization of truth table and print conservative solution:
solacc_con <- minimize(ttACC, 
                       details = TRUE)
solacc_con

#Compute parsimonious solution and print results:
solacc_par <- minimize(ttACC,
                       details = TRUE,
                       include = "?",
                       show.cases = TRUE)
solacc_par

#Compute intermediary solution and print results:
solcha_int <- minimize(ttACC,
                       details = TRUE,
                       include = "?",
                       show.cases = TRUE,
                       dir.exp = "~cCEN")
solcha_int
######

#Study 1 - Accountability general
library(tidyverse)
library(QCA)
#Replace path with path in your own directory. 
path <- "/Users/olivier/Library/CloudStorage/SynologyDrive-Olli/Job/Coding/Projects/GitHub/Formalization_networks/data/AllCases20220605.csv"
#Upload dataset
cases_all <- read.csv(path, sep=";")
cases_FP <- cases_all %>% filter(.$Q23_Start_for.profit == "Yes" |
                               .$Q23_Middle_for.profit == "Yes" |  
                               .$Q23_End_for.profit == "Yes")
#Delete superfluous information
cases_FP <- cases_FP %>%
  select(-File.name,
         -Authors,
         -Institution,
         -e.mail.address.first.author)
#Delete cases with missing data on central items
cases_FP<-subset(cases_FP, 
                 Q24_DK != "Yes" &
                   Q26_DK != "Yes" &
                   Q33_DK != "Yes" &
                   Q39_DK != "Yes" &
                   Q41_DK != "Yes" &
                   Q48_DK != "Yes" &
                   Q49_DK != "Yes" &
                   Q50_DK != "Yes" &
                   Q58_Support_DK != "Yes" &
                   Q58_positive_DK != "Yes" &
                   Q58_joint_DK != "Yes")
#Replace all values set to "Off" with "NA", then NA with 0.
#Same procedure for "Yes", set to 5.
cases_FP[cases_FP == "Off"] <- NA
cases_FP[is.na(cases_FP)] <- 0
cases_FP[cases_FP == "Yes"] <- NA
cases_FP[is.na(cases_FP)] <- 5
#Create sets using extant items.
cases_FP_fs <- subset(cases_FP, 
                      select = c(Q24_Start, Q24_Middle, Q24_End,
                                 Q26_Start, Q26_Middle, Q26_End,
                                 Q33_Start, Q33_Middle, Q33_End,
                                 Q39_Start, Q39_Middle, Q39_End,
                                 Q41_Start, Q41_Middle, Q41_End,
                                 Q48_Start, Q48_Middle, Q48_End,
                                 Q49_Start, Q49_Middle, Q49_End,
                                 Q50_Start, Q50_Middle, Q50_End,
                                 Q51.1_Start, Q51.1_Middle, Q51.1_End,
                                 Q51.2_Start, Q51.2_Middle, Q51.2_End,
                                 Q51.3_Start, Q51.3_Middle, Q51.3_End,
                                 Q57.1_Start, Q57.1_Middle, Q57.1_End,
                                 Q57.2_Start, Q57.2_Middle, Q57.2_End,
                                 Q57.3_Start, Q57.3_Middle, Q57.3_End,
                                 Q58.1_Start, Q58.1_Middle, Q58.1_End,
                                 Q58.2_Start, Q58.2_Middle, Q58.2_End,
                                 Q58.3_Start, Q58.3_Middle, Q58.3_End))
cases_FP_fs <- cases_FP_fs %>% mutate_all(as.character)
cases_FP_fs <- cases_FP_fs %>% mutate_all(as.numeric)
cases_FP_fs <- cases_FP_fs %>%
  mutate(., RUL = rowMeans(select(., c(Q24_Start, Q24_Middle))),
         ., CEN = rowMeans(select(., c(Q39_Start, Q39_Middle))),
         ., MON = rowMeans(select(., c(Q50_Start, Q50_Middle))),
         ., GOA = rowMeans(select(., c(Q48_Start, Q48_Middle))),
         ., STR = rowMeans(select(., c(Q49_Start, Q49_Middle))),
         ., ACC = pmax(Q51.1_Middle, Q51.2_Middle, Q51.3_Middle, Q51.1_End, 
           Q51.2_End, Q51.3_End),
         ., CON = rowMeans(select(., c(Q26_Start, Q26_Middle))),
         ., FOC = rowMeans(select(., c(Q33_Start, Q33_Middle))),
         ., LEG = Q58.1_End,
         ., KNO = rowMeans(select(., c(Q41_Start, Q41_Middle))),
         ., EFF = pmax(Q57.1_Middle, Q57.1_End, Q57.2_Middle, Q57.2_End,
           Q57.3_Middle, Q57.3_End),
         ., CAP = Q58.2_End,
         ., SOC = Q58.3_End,
         ., CHA = rowMeans(select(., c(CAP, LEG, SOC))))
#Calibrate Likert scores using recode. 
cases_FP_fs <- cases_FP_fs %>%
  mutate(., cRUL = (recode(RUL,
                           cuts = "2, 3, 4",
                           values =  "0, 0.33, 0.66, 1")),
         ., cCEN = (recode(CEN,
                           cuts = "2, 3, 4",
                           values =  "0, 0.33, 0.66, 1")),
         ., cMON = (recode(MON,
                           cuts = "2, 3, 4",
                           values =  "0, 0.33, 0.66, 1")),
         ., cGOA = (recode(GOA,
                           cuts = "2, 3, 4",
                           values =  "0, 0.33, 0.66, 1")),
         ., cSTR = (recode(STR, 
                           cuts = "2, 3, 4", 
                           values =  "0, 0.33, 0.66, 1")),
         ., cACC = (recode(ACC,
                           cuts = "2, 3, 4",
                           values =  "0, 0.33, 0.66, 1")),
         ., cFOC = (recode(FOC,
                           cuts = "3, 3.5, 4",
                           values =  "0, 0.33, 0.66, 1")),
         ., cLEG = (recode(LEG,
                           cuts = "2, 3, 4",
                           values =  "0, 0.33, 0.66, 1")),
         ., cKNO = (recode(KNO,
                           cuts = "2, 3, 4",
                           values =  "0, 0.33, 0.66, 1")),
         ., cEFF = (recode(EFF,
                           cuts = "2, 3, 4",
                           values =  "0, 0.33, 0.66, 1")),
         ., cCHA = (recode(CHA,
                           cuts = "2, 3, 4",
                           values =  "0, 0.33, 0.66, 1")))
cases_FP_fs_an <- cases_FP_fs %>%
  select(cRUL:cCHA)
#Check for necessity for outcome
superSubset(cases_FP_fs_an, 
            outcome = "cCHA", 
            neg.out = FALSE,
            relation = "necessity",
            incl.cut = 0.90,
            ron.cut = 0.60)
pof("cCHA <- cGOA", data = cases_FP_fs_an)
XYplot(cGOA, cCHA, 
       data = cases_FP_fs_an, 
       jitter = TRUE,
       relation = "necessity")
#Run analysis for all conditions.
ttACC <- truthTable(cases_FP_fs_an, outcome = "cCHA",
                    conditions = "cRUL, cCEN, cMON, cGOA, cSTR, cFOC, cACC",
                    incl.cut = 0.90,
                    show.cases = TRUE,
                    dcc = TRUE,
                    sort.by = "OUT, n")
ttACC
#Compute minimization of truth table and print conservative solution:
solacc_con <- minimize(ttACC, 
                       details = TRUE)
solacc_con
#Compute parsimonious solution and print results:
solacc_par <- minimize(ttACC,
                       details = TRUE,
                       include = "?",
                       show.cases = TRUE)
solacc_par
#Compute intermediary solution and print results:
solcha_int <- minimize(ttACC,
                       details = TRUE,
                       include = "?",
                       show.cases = TRUE,
                       dir.exp = "cRUL, cCEN, cMON, cGOA, cSTR, cFOC, cACC")
solcha_int

#Study Beta - Systemic change as mvQCA
library(tidyverse)
library(QCA)
#Replace path with path in your own directory. 
path <- "/Users/olivier/Library/CloudStorage/SynologyDrive-Olli/Job/Coding/Projects/GitHub/Formalization_networks/data/AllCases20220605.csv"
#Upload dataset
cases <- read.csv(path, sep=";")
cases_FP <- cases %>% filter(.$Q23_Start_for.profit == "Yes" |
                               .$Q23_Middle_for.profit == "Yes" |  
                               .$Q23_End_for.profit == "Yes")
#Delete superfluous information
cases_FP <- cases %>%
  select(-File.name,
         -Authors,
         -Institution,
         -e.mail.address.first.author)
#Delete cases with missing data on central items
cases_FP<-subset(cases_FP, 
                 Q24_DK != "Yes" &
                   Q26_DK != "Yes" &
                   Q33_DK != "Yes" &
                   Q39_DK != "Yes" &
                   Q41_DK != "Yes" &
                   Q48_DK != "Yes" &
                   Q49_DK != "Yes" &
                   Q50_DK != "Yes" &
                   Q58_Support_DK != "Yes" &
                   Q58_positive_DK != "Yes" &
                   Q58_joint_DK != "Yes")
#Replace all values set to "Off" with "NA", then NA with 0.
#Same procedure for "Yes", set to 5.
cases_FP[cases_FP == "Off"] <- NA
cases_FP[is.na(cases_FP)] <- 0
cases_FP[cases_FP == "Yes"] <- NA
cases_FP[is.na(cases_FP)] <- 5
#Create sets using extant items.
cases_FP_fs <- subset(cases_FP, 
                      select = c(Q24_Start, Q24_Middle, Q24_End,
                                 Q26_Start, Q26_Middle, Q26_End,
                                 Q33_Start, Q33_Middle, Q33_End,
                                 Q39_Start, Q39_Middle, Q39_End,
                                 Q41_Start, Q41_Middle, Q41_End,
                                 Q48_Start, Q48_Middle, Q48_End,
                                 Q49_Start, Q49_Middle, Q49_End,
                                 Q50_Start, Q50_Middle, Q50_End,
                                 Q51.1_Start, Q51.1_Middle, Q51.1_End,
                                 Q51.2_Start, Q51.2_Middle, Q51.2_End,
                                 Q51.3_Start, Q51.3_Middle, Q51.3_End,
                                 Q57.1_Start, Q57.1_Middle, Q57.1_End,
                                 Q57.2_Start, Q57.2_Middle, Q57.2_End,
                                 Q57.3_Start, Q57.3_Middle, Q57.3_End,
                                 Q58.1_Start, Q58.1_Middle, Q58.1_End,
                                 Q58.2_Start, Q58.2_Middle, Q58.2_End,
                                 Q58.3_Start, Q58.3_Middle, Q58.3_End))
cases_FP_fs <- cases_FP_fs %>% mutate_all(as.character)
cases_FP_fs <- cases_FP_fs %>% mutate_all(as.numeric)
cases_FP_fs <- cases_FP_fs %>%
  mutate(., RUL = rowMeans(select(., c(Q24_Start, Q24_Middle))),
         ., CEN = rowMeans(select(., c(Q39_Start, Q39_Middle))),
         ., MON = rowMeans(select(., c(Q50_Start, Q50_Middle))),
         ., GOA = rowMeans(select(., c(Q48_Start, Q48_Middle))),
         ., STR = rowMeans(select(., c(Q49_Start, Q49_Middle))),
         ., ACC = pmax(Q51.1_Middle, Q51.2_Middle, Q51.3_Middle, Q51.1_Start, 
                       Q51.2_Start, Q51.3_Start),
         ., CON = rowMeans(select(., c(Q26_Start, Q26_Middle))),
         ., FOC = rowMeans(select(., c(Q33_Start, Q33_Middle))),
         ., SUP = Q58.1_End,
         ., KNO = rowMeans(select(., c(Q41_Start, Q41_Middle))),
         ., INO = pmax(Q57.1_Middle, Q57.1_End, Q57.2_Middle, Q57.2_End,
                       Q57.3_Middle, Q57.3_End),
         ., CAP = Q58.2_End,
         ., SOC = Q58.3_End,
         ., CHA = rowMeans(select(., c(CAP, SUP, SOC))))
#Calibrate Likert scores using multi-values. 
cases_FP_fs <- cases_FP_fs %>%
  mutate(., cRUL = (calibrate(RUL,
                           type = "crisp",
                           thresholds = "2, 3, 4")),
         ., cCEN = (calibrate(CEN,
                           type = "crisp",
                           thresholds = "2, 3, 4")),
         ., cMON = (calibrate(MON,
                           type = "crisp",
                           thresholds = "2, 3, 4")),
         ., cGOA = (calibrate(GOA,
                           type = "crisp",
                           thresholds = "2, 3, 4")),
         ., cSTR = (calibrate(STR, 
                              type = "crisp",
                              thresholds = "2, 3, 4")),
         ., cACC = (calibrate(ACC,
                              type = "crisp",
                              thresholds = "2, 3, 4")),
         ., cFOC = (calibrate(FOC,
                              type = "crisp",
                              thresholds = "3, 4")),
         ., cSUP = (calibrate(SUP,
                           type = "crisp",
                           thresholds = "2, 3, 4")),
         ., cKNO = (calibrate(KNO,
                              type = "crisp",
                              thresholds = "2, 3, 4")),
         ., cINO = (calibrate(INO,
                              type = "crisp",
                              thresholds = "2, 3, 4")),
         ., cCHA = (calibrate(CHA,
                           type = "crisp",
                           thresholds = "2, 3, 4")))
cases_FP_fs_an <- cases_FP_fs %>%
  select(cRUL:cCHA)
#Check for necessity for outcome
superSubset(cases_FP_fs_an, 
            outcome = "cCHA", 
            neg.out = FALSE,
            relation = "necessity",
            incl.cut = 0.8)
#Run analysis for all conditions.
ttCHA <- truthTable(cases_FP_fs_an, 
                    outcome = "cCHA{3}",
                    conditions = "cRUL, cCEN, cMON, cGOA, cSTR, cFOC, cACC",
                    show.cases = TRUE,
                    dcc = TRUE,
                    sort.by = "OUT, n")
ttCHA
#Compute minimization of truth table and print conservative solution:
solcha_con <- minimize(ttCHA, 
                       details = TRUE)
solcha_con
#Compute parsimonious solution and print results:
solacc_par <- minimize(ttCHA,
                       details = TRUE,
                       include = "?",
                       show.cases = TRUE)
solacc_par
#Compute intermediary solution and print results:
solcha_int <- minimize(ttCHA,
                       details = TRUE,
                       include = "?",
                       show.cases = TRUE,
                       dir.exp = "cRUL{3}, cCEN{3}, cGOA{3}, cSTR{3}, cFOC{2}, cACC{3}")
solcha_int


