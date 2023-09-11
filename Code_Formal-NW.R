library(tidyverse)
library(QCA)
#Replace path with path in your own directory. 
path <- "/Users/olivier/Library/CloudStorage/SynologyDrive-Olli/Job/Coding/Projects/GitHub/Formalization_networks/data/AllCases20220605.csv"
#Upload dataset
cases <- read.csv(path, sep=";")
#Delete superfluous information
cases <- cases %>%
  select(-File.name,
         -Authors,
         -Institution,
         -e.mail.address.first.author)
#Delete cases with missing data on central items
cases2<-subset(cases, 
            Q24_DK != "Yes" & 
              Q25_DK != "Yes" &
              Q26_DK != "Yes" &
              Q27_DK != "Yes" &
              Q39_DK != "Yes" &
              Q48_DK != "Yes" &
              Q49_DK != "Yes" &
              Q50_DK != "Yes")
#Replace all values set to "Off" with "NA", then NA with 0.
#Same procedure for "Yes", set to 5.
cases2[cases2 == "Off"] <- NA
cases2[is.na(cases2)] <- 0
cases2[cases2 == "Yes"] <- NA
cases2[is.na(cases2)] <- 5
which(cases2 == 6, arr.ind=TRUE)
#Create sets using extant items.
compute_score2 <- function(x){
  x <- as.numeric(as.character(x))
  x <- x/2
}
compute_score3 <- function(x){
  x <- as.numeric(as.character(x))
  x <- x/3
}
cases2 <- cases2 %>%
  mutate(., RUL = compute_score2(Q24_Middle) + compute_score2(Q25_Middle),
         ., CEN = as.numeric(as.character(Q39_Middle)),
         ., DEC = as.numeric(as.character(Q27_Middle)),
         ., MON = as.numeric(as.character(Q50_Middle)),
         ., GOA = compute_score2(Q48_Middle) + compute_score2(Q49_Middle),
         ., LEG = compute_score2(Q58.1_Middle) + compute_score2(Q58.1_End),
         ., CAP = as.numeric(as.character(Q58.2_End)),
         ., SOC = as.numeric(as.character(Q58.3_End)),
         ., EFF = pmax(
           (compute_score2(Q57.1_Middle) + compute_score2(Q57.1_End)),
           (compute_score2(Q57.2_Middle) + compute_score2(Q57.2_End)),
           (compute_score2(Q57.3_Middle) + compute_score2(Q57.3_End))),
         ., CHA = compute_score3(LEG) + compute_score3(CAP) + compute_score3(SOC))

#Calibrate using recode. 
cases2_fs <- cases2 %>%
  mutate(., cRUL = (recode(RUL,
                           cuts = "2, 3, 4",
                           values =  "0, 0.33, 0.66, 1")),
         ., cCEN = (recode(CEN,
                           cuts = "2, 3, 4",
                           values =  "0, 0.33, 0.66, 1")),
         ., cDEC = (recode(DEC,
                           cuts = "2, 3, 4",
                           values =  "0, 0.33, 0.66, 1")),
         ., cMON = (recode(MON,
                           cuts = "2, 3, 4",
                           values =  "0, 0.33, 0.66, 1")),
         ., cGOA = (recode(GOA,
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
cases2_fs_analysis <- cases2_fs %>%
  dplyr::select(cRUL:cCHA)

#Check for necessity for CHA
superSubset(cases2_fs_analysis, 
            outcome = "cCHA", 
            neg.out = FALSE,
            conditions = "cRUL, cCEN, cDEC, cMON, cGOA",
            relation = "necessity",
            incl.cut = 0.80)

#Run analysis for top performers ("CHA") and all conditions.
ttTOP <- truthTable(cases2_fs_analysis, outcome = "cEFF",
                    conditions = "cRUL, cCEN, cDEC, cMON, cGOA",
                    incl.cut = 0.80,
                    show.cases = TRUE,
                    dcc = TRUE,
                    sort.by = "OUT, n")
ttTOP

#Compute minimization of truth table and print conservative solution:
soltop_com <- minimize(ttTOP, 
                       details = TRUE)
soltop_com

#Compute parsimonious solution and print results:
soltop_par <- minimize(ttTOP,
                       details = TRUE,
                       include = "?",
                       show.cases = TRUE)
soltop_par

#Compute intermediary solution and print results:
soltop_int <- minimize(ttTOP,
                       details = TRUE,
                       include = "?",
                       show.cases = TRUE,
                       dir.exp = "cEFF")
soltop_int
