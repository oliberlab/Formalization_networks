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
                   Q27_DK != "Yes" &
                   Q39_DK != "Yes" &
                   
                   Q48_DK != "Yes" &
                   Q49_DK != "Yes" &
                   Q50_DK != "Yes" &
                   Q51.1_DK != "Yes" &
                   Q51.2_DK != "Yes" &
                   Q51.3_DK != "Yes" &
                   Q51.4_DK != "Yes" &
                   Q58_Support_DK != "Yes")
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
cases_FP <- cases_FP %>%
  mutate(., RUL = as.numeric(as.character(Q24_End)),
         ., CEN = as.numeric(as.character(Q39_End)),
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
         ., LEG = compute_score3(as.numeric(as.character(Q58.1_Start))) +
                  compute_score3(as.numeric(as.character(Q58.2_Middle))) +
                  compute_score3(as.numeric(as.character(Q58.3_End))),
         ., EFF = pmax(
           as.numeric(as.character(Q57.1_Middle)),
           as.numeric(as.character(Q57.1_End)),
           as.numeric(as.character(Q57.2_Middle)),
           as.numeric(as.character(Q57.2_End)),
           as.numeric(as.character(Q57.3_Middle)),
           as.numeric(as.character(Q57.3_End))),
         ., CAP = as.numeric(as.character(Q58.2_End)),
         ., SOC = as.numeric(as.character(Q58.3_End)),
         ., CHA = compute_score3(CAP) + 
           compute_score3(SOC) +
           compute_score3(EFF))
#Calibrate using recode. 
cases_FP_fs <- cases_FP %>%
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
         ., cSTR = (recode(GOA, 
                           cuts = "2, 3, 4", 
                           values =  "0, 0.33, 0.66, 1")),
         ., cACC = (recode(ACC,
                           cuts = "2, 3, 4",
                           values =  "0, 0.33, 0.66, 1")),
         ., cLEG = (recode(LEG,
                           cuts = "2, 3, 4",
                           values =  "0, 0.33, 0.66, 1")),
         ., cCHA = (recode(CHA,
                           cuts = "2, 3, 4",
                           values =  "0, 0.33, 0.66, 1")))
cases_FP_fs_an <- cases_FP_fs %>%
  dplyr::select(cRUL:cCHA)
#Check for necessity for CHA
superSubset(cases_FP_fs_an, 
            outcome = "cCHA", 
            neg.out = FALSE,
            relation = "necessity",
            incl.cut = 0.90,
            ron.cut = 0.60)
#Run analysis for top performers ("CHA") and all conditions.
ttACC <- truthTable(cases_FP_fs_an, outcome = "cCHA",
                    conditions = "cRUL, cCEN, cMON, cGOA, cSTR, cACC",
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
                       dir.exp = "cLEG")
solcha_int


