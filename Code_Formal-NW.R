library(tidyverse)
library(QCA)
#Replace path with path in your own directory. 
path <- "/Users/olivier/Library/CloudStorage/SynologyDrive-Olli/Job/Coding/Projects/GitHub/Formalization_networks/data/AllCases20220605.csv"
#Study 1 - Legitimacy FP
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
                   Q27_DK != "Yes" &
                   Q39_DK != "Yes" &
                   Q48_DK != "Yes" &
                   Q49_DK != "Yes" &
                   Q50_DK != "Yes")
#Replace all values set to "Off" with "NA", then NA with 0.
#Same procedure for "Yes", set to 5.
cases_FP[cases_FP == "Off"] <- NA
cases_FP[is.na(cases_FP)] <- 0
cases_FP[cases_FP == "Yes"] <- NA
cases_FP[is.na(cases_FP)] <- 5
#Create sets using extant items.
cases_FP_fs <- subset(cases_FP, 
                      select = c(Q24_Start, Q24_Middle, Q24_End,
                                 Q27_Start, Q27_Middle, Q27_End,
                                 Q39_Start, Q39_Middle, Q39_End,
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
  mutate(., RUL = Q24_Middle,
         ., TRA = Q27_Middle,
         ., CEN = Q39_Middle,
         ., MON = Q50_Middle,
         ., GOA = Q48_Middle,
         ., STR = Q49_Middle,
         ., ACC = pmax(Q51.1_Middle, Q51.2_Middle, Q51.3_Middle, Q51.1_End, 
                       Q51.2_End, Q51.3_End),
         ., LEG = Q58.1_End,
         ., EFF = pmax(Q57.1_Middle, Q57.1_End, Q57.2_Middle, Q57.2_End,
                       Q57.3_Middle, Q57.3_End),
         ., CAP = Q58.2_End,
         ., SOC = Q58.3_End,
         ., CHA = (CAP + LEG + SOC)/3)
#Calibrate Likert scores using recode. 
cases_FP_fs <- cases_FP_fs %>%
  mutate(., cRUL = (recode(RUL,
                           cuts = "2, 3, 4",
                           values =  "0, 0.33, 0.66, 1")),
         ., cTRA = (recode(TRA,
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
         ., cLEG = (recode(LEG,
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
            outcome = "cEFF", 
            neg.out = FALSE,
            relation = "necessity",
            incl.cut = 0.90,
            ron.cut = 0.50)
#Run analysis for all conditions.
ttFP <- truthTable(cases_FP_fs_an, outcome = "cACC",
                    conditions = "cRUL, cTRA, cCEN, cMON, cGOA",
                    incl.cut = 0.80,
                    show.cases = TRUE,
                    dcc = TRUE,
                    sort.by = "OUT, n")
ttFP
#Compute minimization of truth table and print conservative solution:
solfp_con <- minimize(ttFP, 
                       details = TRUE)
solfp_con
#Compute parsimonious solution and print results:
solfp_par <- minimize(ttFP,
                       details = TRUE,
                       include = "?",
                       show.cases = TRUE)
solfp_par
#Compute intermediary solution and print results:
solfp_int <- minimize(ttFP,
                       details = TRUE,
                       include = "?",
                       show.cases = TRUE,
                       dir.exp = "cRUL, cTRA, cCEN, cMON, cGOA")
solfp_int

#Study all but for profit
cases_NP <- cases_all %>% filter(.$Q23_Start_for.profit == "Off" |
                                   .$Q23_Middle_for.profit == "Off" |  
                                   .$Q23_End_for.profit == "Off")
#Delete superfluous information
cases_NP <- cases_NP %>%
  select(-File.name,
         -Authors,
         -Institution,
         -e.mail.address.first.author)
#Delete cases with missing data on central items
cases_NP<-subset(cases_NP, 
                 Q24_DK != "Yes" &
                   Q27_DK != "Yes" &
                   Q39_DK != "Yes" &
                   Q48_DK != "Yes" &
                   Q49_DK != "Yes" &
                   Q50_DK != "Yes")
#Replace all values set to "Off" with "NA", then NA with 0.
#Same procedure for "Yes", set to 5.
cases_NP[cases_NP == "Off"] <- NA
cases_NP[is.na(cases_NP)] <- 0
cases_NP[cases_NP == "Yes"] <- NA
cases_NP[is.na(cases_NP)] <- 5
#Create sets using extant items.
cases_NP_fs <- subset(cases_NP, 
                      select = c(Q24_Start, Q24_Middle, Q24_End,
                                 Q27_Start, Q27_Middle, Q27_End,
                                 Q39_Start, Q39_Middle, Q39_End,
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
cases_NP_fs <- cases_NP_fs %>% mutate_all(as.character)
cases_NP_fs <- cases_NP_fs %>% mutate_all(as.numeric)
cases_NP_fs <- cases_NP_fs %>%
  mutate(., RUL = Q24_Middle,
         ., TRA = Q27_Middle,
         ., CEN = Q39_Middle,
         ., MON = Q50_Middle,
         ., GOA = Q48_Middle,
         ., STR = Q49_Middle,
         ., ACC = pmax(Q51.1_Middle, Q51.2_Middle, Q51.3_Middle, Q51.1_End, 
                       Q51.2_End, Q51.3_End),
         ., LEG = Q58.1_End,
         ., EFF = pmax(Q57.1_Middle, Q57.1_End, Q57.2_Middle, Q57.2_End,
                       Q57.3_Middle, Q57.3_End),
         ., CAP = Q58.2_End,
         ., SOC = Q58.3_End,
         ., CHA = (CAP + LEG + SOC)/3)
#Calibrate Likert scores using recode. 
cases_NP_fs <- cases_NP_fs %>%
  mutate(., cRUL = (recode(RUL,
                           cuts = "2, 3, 4",
                           values =  "0, 0.33, 0.66, 1")),
         ., cTRA = (recode(TRA,
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
         ., cLEG = (recode(LEG,
                           cuts = "2, 3, 4",
                           values =  "0, 0.33, 0.66, 1")),
         ., cEFF = (recode(EFF,
                           cuts = "2, 3, 4",
                           values =  "0, 0.33, 0.66, 1")),
         ., cCHA = (recode(CHA,
                           cuts = "2, 3, 4",
                           values =  "0, 0.33, 0.66, 1")))
cases_NP_fs_an <- cases_NP_fs %>%
  select(cRUL:cCHA)
#Check for necessity for outcome
superSubset(cases_NP_fs_an, 
            outcome = "cACC", 
            neg.out = FALSE,
            relation = "necessity",
            incl.cut = 0.90,
            ron.cut = 0.60)
#Run analysis for all conditions.
ttNP <- truthTable(cases_NP_fs_an, outcome = "cACC",
                    conditions = "cRUL, cTRA, cCEN, cMON, cGOA",
                    incl.cut = 0.87,
                    show.cases = TRUE,
                    dcc = TRUE,
                    sort.by = "OUT, n")
ttNP
#Compute minimization of truth table and print conservative solution:
solnp_con <- minimize(ttNP, 
                       details = TRUE)
solnp_con
#Compute parsimonious solution and print results:
solnp_par <- minimize(ttNP,
                       details = TRUE,
                       include = "?",
                       show.cases = TRUE)
solnp_par
#Compute intermediary solution and print results:
solnp_int <- minimize(ttNP,
                       details = TRUE,
                       include = "?",
                       show.cases = TRUE,
                       dir.exp = "cRUL, cTRA, cCEN, cMON, cGOA")
solnp_int
#Select C1P2 because it has better fit values and primo implicants are same.
#Hence select P2 for discussions. 


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
cases_MV <- cases %>%
  select(-File.name,
         -Authors,
         -Institution,
         -e.mail.address.first.author)
#Delete cases with missing data on central items
cases_MV<-subset(cases_MV, 
                 Q24_DK != "Yes" &
                   Q26_DK != "Yes" &
                   Q27_DK != "Yes" &
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
cases_MV[cases_MV == "Off"] <- NA
cases_MV[is.na(cases_MV)] <- 0
cases_MV[cases_MV == "Yes"] <- NA
cases_MV[is.na(cases_MV)] <- 5
#Create sets using extant items.
cases_CS_mv <- subset(cases_MV, 
                      select = c(Q17_Start, Q17_Middle, Q17_End,
                                 Q24_Start, Q24_Middle, Q24_End,
                                 Q26_Start, Q26_Middle, Q26_End,
                                 Q27_Start, Q27_Middle, Q27_End,
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
cases_CS_mv <- cases_CS_mv %>% mutate_all(as.character)
cases_CS_mv <- cases_CS_mv %>% mutate_all(as.numeric)
cases_CS_mv <- cases_CS_mv %>%
  mutate(., POW = Q17_Middle,
         ., RUL = Q24_Middle,
         ., TRA = Q27_Middle,
         ., CEN = Q39_Middle,
         ., MON = Q50_Middle,
         ., GOA = Q48_Middle,
         ., STR = Q49_Middle,
         ., ACC = pmax(Q51.1_Middle, Q51.2_Middle, Q51.3_Middle, Q51.1_End, 
                       Q51.2_End, Q51.3_End),
         ., CON = Q26_Middle,
         ., FOC = Q33_Middle,
         ., LEG = Q58.1_End,
         ., KNO = Q41_Middle,
         ., EFF = pmax(Q57.1_Middle, Q57.1_End, Q57.2_Middle, Q57.2_End,
                       Q57.3_Middle, Q57.3_End),
         ., CAP = Q58.2_End,
         ., SOC = Q58.3_End,
         ., CHA = (CAP + LEG + SOC)/3)
#Calibrate Likert scores using multi-values. 
cases_CS_mv <- cases_CS_mv %>%
  mutate(., cPOW = (calibrate(POW,
                              type = "crisp",
                              thresholds = "3, 4")),
         ., cRUL = (calibrate(RUL,
                           type = "crisp",
                           thresholds = "2, 3, 4")),
         ., cTRA = (calibrate(TRA,
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
         ., cCHA = (calibrate(CHA,
                           type = "crisp",
                           thresholds = "2, 3, 4")))
cases_CS_mv_an <- cases_CS_mv %>%
  select(cPOW:cCHA)
#Check for necessity for outcome
superSubset(cases_CS_mv_an, 
            outcome = "cACC", 
            neg.out = FALSE,
            relation = "necessity",
            incl.cut = 0.8)
#Run analysis for all conditions.
ttMV <- truthTable(cases_CS_mv_an, 
                    outcome = "cACC{3}",
                    conditions = "cRUL, cCEN, cMON, cGOA",
                    show.cases = TRUE,
                    dcc = TRUE,
                    sort.by = "OUT, n")
ttMV
#Compute minimization of truth table and print conservative solution:
solmv_con <- minimize(ttMV, 
                       details = TRUE)
solmv_con
#Compute parsimonious solution and print results:
solmv_par <- minimize(ttMV,
                       details = TRUE,
                       include = "?",
                       show.cases = TRUE)
solmv_par
#Compute intermediary solution and print results:
solmv_int <- minimize(ttMV,
                       details = TRUE,
                       include = "?",
                       show.cases = TRUE,
                       dir.exp = "cRUL{3}, cCEN{3}, cMON{3}, cGOA{3}")
solmv_int

#Study for all
cases_all <- read.csv(path, sep=";")
#Delete superfluous information
cases_CS <- cases_all %>%
  select(-File.name,
         -Authors,
         -Institution,
         -e.mail.address.first.author)
#Delete cases with missing data on central items
cases_CS<-subset(cases_CS, 
                 Q17_DK != "Yes" &
                   Q24_DK != "Yes" &
                   Q26_DK != "Yes" &
                   Q27_DK != "Yes" &
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
cases_CS[cases_CS == "Off"] <- NA
cases_CS[is.na(cases_CS)] <- 0
cases_CS[cases_CS == "Yes"] <- NA
cases_CS[is.na(cases_CS)] <- 5
#Create sets using extant items.
cases_CS_fs <- subset(cases_CS, 
                      select = c(Q17_Start, Q17_Middle, Q17_End,
                                 Q24_Start, Q24_Middle, Q24_End,
                                 Q26_Start, Q26_Middle, Q26_End,
                                 Q27_Start, Q27_Middle, Q27_End,
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
cases_CS_fs <- cases_CS_fs %>% mutate_all(as.character)
cases_CS_fs <- cases_CS_fs %>% mutate_all(as.numeric)
cases_CS_fs <- cases_CS_fs %>%
  mutate(., POW = Q17_Middle,
         ., RUL = Q24_Middle,
         ., TRA = Q27_Middle,
         ., CEN = Q39_Middle,
         ., MON = Q50_Middle,
         ., GOA = Q48_Middle,
         ., STR = Q49_Middle,
         ., ACC = pmax(Q51.1_Middle, Q51.2_Middle, Q51.3_Middle, Q51.1_End, 
                       Q51.2_End, Q51.3_End),
         ., CON = Q26_Middle,
         ., FOC = Q33_Middle,
         ., LEG = pmax (Q58.1_Middle, Q58.1_End),
         ., KNO = Q41_Middle,
         ., EFF = pmax(Q57.1_Middle, Q57.1_End, Q57.2_Middle, Q57.2_End,
                       Q57.3_Middle, Q57.3_End),
         ., CAP = pmax (Q58.1_Middle, Q58.1_End),
         ., SOC = pmax (Q58.1_Middle, Q58.1_End),
         ., CHA = (CAP + LEG)/2)
#Calibrate Likert scores using recode. 
cases_CS_fs <- cases_CS_fs %>%
  mutate(., cPOW = (recode(POW,
                           cuts = "3, 3.5, 4",
                           values =  "0, 0.33, 0.66, 1")),
         ., cRUL = (recode(RUL,
                           cuts = "2, 3, 4",
                           values =  "0, 0.33, 0.66, 1")),
         ., cTRA = (recode(TRA,
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
cases_CS_fs_an <- cases_CS_fs %>%
  select(cPOW:cCHA)
#Check for necessity for outcome
superSubset(cases_CS_fs_an, 
            outcome = "cEFF", 
            neg.out = FALSE,
            relation = "necessity",
            incl.cut = 0.90,
            ron.cut = 0.50)
#Run analysis for all conditions.
ttCS <- truthTable(cases_CS_fs_an, outcome = "cCHA",
                   conditions = "cRUL, cCEN, cGOA, cMON, cSTR",
                   incl.cut = 0.80,
                   show.cases = TRUE,
                   dcc = TRUE,
                   sort.by = "OUT, n")
ttCS
#Compute minimization of truth table and print conservative solution:
solcs_con <- minimize(ttCS, 
                      details = TRUE)
solcs_con
#Compute parsimonious solution and print results:
solcs_par <- minimize(ttCS,
                      details = TRUE,
                      include = "?",
                      show.cases = TRUE)
solcs_par
#Compute intermediary solution and print results:
solcs_int <- minimize(ttCS,
                      details = TRUE,
                      include = "?",
                      show.cases = TRUE,
                      dir.exp = "cRUL, cCEN, cGOA, cMON, cSTR")
solcs_int
#Run analysis for all conditions and negative outcome.
ttNoCS <- truthTable(cases_CS_fs_an, outcome = "~cACC",
                     conditions = "cRUL, cCEN, cGOA, cMON, cSTR",
                     incl.cut = 0.80,
                     show.cases = TRUE,
                     dcc = TRUE,
                     sort.by = "OUT, n")
ttNoCS
#Compute minimization of truth table and print conservative solution:
solno_con <- minimize(ttNoCS, 
                      details = TRUE)
solno_con
#Compute parsimonious solution and print results:
solno_par <- minimize(ttNoCS,
                      details = TRUE,
                      include = "?",
                      show.cases = TRUE)
solno_par
#Compute intermediary solution and print results:
solno_int <- minimize(ttNoCS,
                      details = TRUE,
                      include = "?",
                      show.cases = TRUE,
                      dir.exp = "cRUL, cCEN, cGOA, cMON, cSTR")
solno_int
