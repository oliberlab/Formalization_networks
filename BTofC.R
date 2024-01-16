#Study for all, behavioral theory of CSP
path <- "/Users/olivier/Library/CloudStorage/SynologyDrive-Olli/Job/Coding/Projects/GitHub/Formalization_networks/data/AllCases20220605.csv"
cases_all <- read.csv(path, sep=";")
#Delete superfluous information
cases_BTC <- cases_all %>%
  select(-File.name,
         -Authors,
         -Institution,
         -e.mail.address.first.author)
#Delete cases with missing data on central items
cases_BTC<-subset(cases_BTC, 
                 Q24_DK != "Yes" &
                   Q25_DK != "Yes" &
                   Q35_DK != "Yes" &
                   Q40_DK != "Yes" &
                   Q42_DK != "Yes" &
                   Q43_DK != "Yes" &
                   Q48_DK != "Yes" &
                   Q50_DK != "Yes" &
                   Q58_Support_DK != "Yes" &
                   Q58_positive_DK != "Yes" &
                   Q58_joint_DK != "Yes")
#Replace all values set to "Off" with "NA", then NA with 0.
#Same procedure for "Yes", set to 5.
cases_BTC[cases_BTC == "Off"] <- NA
cases_BTC[is.na(cases_BTC)] <- 0
cases_BTC[cases_CS == "Yes"] <- NA
cases_BTC[is.na(cases_BTC)] <- 5
#Create sets using extant items.
cases_BTC_fs <- subset(cases_BTC, 
                      select = c(Q24_Start, Q24_Middle, Q24_End,
                                 Q25_Start, Q25_Middle, Q25_End,
                                 Q35_Start, Q35_Middle, Q35_End,
                                 Q40_Start, Q40_Middle, Q40_End,
                                 Q42_Start, Q42_Middle, Q42_End,
                                 Q43_Start, Q43_Middle, Q43_End,
                                 Q48_Start, Q48_Middle, Q48_End,
                                 Q50_Start, Q50_Middle, Q50_End,
                                 Q57.1_Start, Q57.1_Middle, Q57.1_End,
                                 Q57.2_Start, Q57.2_Middle, Q57.2_End,
                                 Q57.3_Start, Q57.3_Middle, Q57.3_End,
                                 Q58.1_Start, Q58.1_Middle, Q58.1_End,
                                 Q58.2_Start, Q58.2_Middle, Q58.2_End,
                                 Q58.3_Start, Q58.3_Middle, Q58.3_End))
cases_BTC_fs <- cases_BTC_fs %>% mutate_all(as.character)
cases_BTC_fs <- cases_BTC_fs %>% mutate_all(as.numeric)
cases_BTC_fs <- cases_BTC_fs %>%
  mutate(., QRC = (Q42_Middle + Q48_Middle)/2,
         ., UNA = (Q24_Middle + Q25_Middle)/2,
         ., PBS = (Q40_Middle + Q50_Middle)/2,
         ., ORL = (Q43_Middle + Q35_Middle)/2,
         ., LEG = pmax (Q58.1_Middle, Q58.1_End),
         ., EFF = pmax(Q57.1_Middle, Q57.1_End, Q57.2_Middle, Q57.2_End,
                       Q57.3_Middle, Q57.3_End),
         ., CAP = pmax (Q58.1_Middle, Q58.1_End),
         ., SOC = pmax (Q58.1_Middle, Q58.1_End),
         ., CHA = (CAP + LEG)/2)
#Calibrate Likert scores using recode. 
cases_BTC_fs <- cases_BTC_fs %>%
  mutate(., cQRC = (recode(QRC,
                           cuts = "3, 3.5, 4",
                           values =  "0, 0.33, 0.66, 1")),
         ., cUNA = (recode(UNA,
                           cuts = "2, 3, 4",
                           values =  "0, 0.33, 0.66, 1")),
         ., cPBS = (recode(PBS,
                           cuts = "2, 3, 4",
                           values =  "0, 0.33, 0.66, 1")),
         ., cORL = (recode(ORL,
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
cases_BTC_fs_an <- cases_BTC_fs %>%
  select(cQRC:cCHA)
#Check for necessity for outcome
superSubset(cases_BTC_fs_an, 
            outcome = "cCHA", 
            neg.out = FALSE,
            relation = "necessity",
            incl.cut = 0.90,
            ron.cut = 0.50)
#Run analysis for all conditions.
ttBTC <- truthTable(cases_BTC_fs_an, outcome = "cCHA",
                   conditions = "cQRC, cUNA, cPBS, cORL",
                   incl.cut = 0.90,
                   show.cases = TRUE,
                   dcc = TRUE,
                   sort.by = "OUT, n")
ttBTC
#Compute minimization of truth table and print conservative solution:
solbt_con <- minimize(ttBTC, 
                      details = TRUE)
solbt_con
#Compute parsimonious solution and print results:
solbt_par <- minimize(ttBTC,
                      details = TRUE,
                      include = "?",
                      show.cases = TRUE)
solbt_par
#Compute intermediary solution and print results:
solbt_int <- minimize(ttBTC,
                      details = TRUE,
                      include = "?",
                      show.cases = TRUE,
                      dir.exp = "cQRC, cUNA, cPBS, cORL")
solbt_int
#Run analysis for all conditions and negative outcome.
ttNoBT <- truthTable(cases_BTC_fs_an, outcome = "~cCHA",
                     conditions = "cQRC, cUNA, cPBS, cORL",
                     incl.cut = 0.80,
                     show.cases = TRUE,
                     dcc = TRUE,
                     sort.by = "OUT, n")
ttNoCS
#Compute minimization of truth table and print conservative solution:
solno_con <- minimize(ttNoBT, 
                      details = TRUE)
solno_con
#Compute parsimonious solution and print results:
solno_par <- minimize(ttNoBT,
                      details = TRUE,
                      include = "?",
                      show.cases = TRUE)
solno_par
#Compute intermediary solution and print results:
solno_int <- minimize(ttNoBT,
                      details = TRUE,
                      include = "?",
                      show.cases = TRUE,
                      dir.exp = "cQRC, cUNA, cPBS, cORL")
solno_int
