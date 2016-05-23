library(dplyr)
library(stringr)

setwd("C:/project/Daksh/dataWrangling/medak")

c1 <- read.csv("../dataInput/Medak_Case.csv",header=T,as.is=T)
h1 <- read.csv("../dataInput/Medak_Hearing.csv",header=T,as.is=T)

c <- c1[,c("combined_case_number","case_type","court_name",
           "court_state","court_district","court_type","district",
           "current_stage","stage_of_case","current_status","nature_of_disposal",
           "under_acts","under_sections",
           "year","date_filed","registration_date","first_hearing_date","next_hearing_date","decision_date",
           "petitioner","petitioner_advocate","respondent","respondent_advocate","before_honourable_judges","petitioner_address",
           "respondent_address","police_station","f_i_r_no")]

h <- h1[,c("combined_case_number","current_stage",
           "business_on_date","hearing_date","next_hearing_date","purpose_of_hearing",
           "court_name","district",
           "petitioner_advocate","respondent_advocate","before_honourable_judges")]

###
# check blank filed date then make it as reg date
###

c$date_filed <- ifelse(c$date_filed == "",c$registration_date,c$date_filed)
c$date_filed <- ifelse(c$date_filed > c$registration_date,c$registration_date,c$date_filed)

###
# Convert date time string to Date
###

c$date_filed <- as.Date(strptime(c$date_filed, "%Y-%m-%d %H:%M:%S"))
c$registration_date <- as.Date(strptime(c$registration_date, "%Y-%m-%d %H:%M:%S"))
c$first_hearing_date <- as.Date(strptime(c$first_hearing_date, "%Y-%m-%d %H:%M:%S"))
c$next_hearing_date <- as.Date(strptime(c$next_hearing_date, "%Y-%m-%d %H:%M:%S"))
c$decision_date <- as.Date(strptime(c$decision_date, "%Y-%m-%d %H:%M:%S"))


h$business_on_date <- as.Date(strptime(h$business_on_date, "%Y-%m-%d %H:%M:%S"))
h$hearing_date <- as.Date(strptime(h$hearing_date, "%Y-%m-%d %H:%M:%S"))
h$next_hearing_date <- as.Date(strptime(h$next_hearing_date, "%Y-%m-%d %H:%M:%S"))

##################################################################
### calculations
##################################################################

###
# create the key - for merging
###
h$key <- paste(h$combined_case_number,h$district,h$ court_name,sep="/")
c$key <- paste(c$combined_case_number,c$district,c$ court_name,sep="/")

###
# calculate the differene in hearing days
###
h$DaysBetweenHearing <- as.integer(difftime(h$hearing_date,h$business_on_date,units = "days"))

###
# calculate time to first hearing
###
c$DaysToFirstHearing <- as.integer(difftime(c$first_hearing_date,c$date_filed,units = "days"))

###
# calculate num of hearing and avg days between hearing
###

hNum <- h %>% group_by(key) %>%
  summarise(AvgDaysBetweenHearing = round(mean(DaysBetweenHearing,na.rm=T),0), 
            NumHearing = n())
###
# To mark cases in case table for which hearing has not started
# mark all hearing table reecords with Yes
###
hNum$HearingExists <- "Yes"

###
# merge cases and the newly calculated values
###
d <- merge(x = c, y = hNum, by = "key", all = TRUE)

###
# Calculate Age in Days
###
d$AgeinDays <- ifelse(d$current_status == "Pending",
                      as.integer(difftime(as.Date(strptime(Sys.time(), "%Y-%m-%d %H:%M:%S")),d$date_filed,units = "days")),
                      as.integer(difftime(d$decision_date,d$date_filed,units = "days"))
                      )

###
# Mark Hearing Exists as No for the NA's
###
d["HearingExists"][is.na(d["HearingExists"])] <- "No"

###
# Impute New Status
###
d$NewStatus <- ifelse(d$current_status=="Disposed","Disposed",
                      ifelse(d$current_status == "Pending" & d$HearingExists == "No","Pending","In Process"))

###
# Fix Before Judge
###
d$before_honourable_judges <- ifelse(d$before_honourable_judges == "","No Judge",d$before_honourable_judges)

###
# Fix Petitioner app
###
d$petitioner_advocate <- ifelse(d$petitioner_advocate == "app","APP",
                                ifelse(d$petitioner_advocate == "A.P.P","APP",
                                       ifelse(d$petitioner_advocate == "APP.","APP",
                                              ifelse(d$petitioner_advocate == "App","APP",
                                                     ifelse(d$petitioner_advocate == "A.P.P","APP",
                                                            ifelse(d$petitioner_advocate == "A.P.P.","APP",
                                                                   d$petitioner_advocate))))))

d$petitioner_advocate <- ifelse(d$petitioner_advocate == "","No Petitioner Advocate",d$petitioner_advocate)

d$respondent_advocate <- ifelse(d$respondent_advocate == "","No Respondent Advocate",d$respondent_advocate)

###
# Mark Civil or Criminal 
###

### Clean under_acts
d$UnderActsNew <- d$under_acts
# If blank, mark as IPC
d$UnderActsNew <- ifelse(d$UnderActsNew == "","IPC",d$UnderActsNew)
#View(as.data.frame(table(d$UnderActsNew)) %>% arrange(desc(Freq)))
d$UnderActsNew <- str_replace_all(d$UnderActsNew,"INDIAN PENAL CODE","IPC")
d$UnderActsNew <- str_replace_all(d$UnderActsNew,"MOTOR VEHICLE ACT","IPC")
d$UnderActsNew <- str_replace_all(d$UnderActsNew,"CODE OF CRIMINAL PROCEDURE","IPC")
#d$UnderActsNew <- str_replace_all(d$UnderActsNew,"MV ACT","IPC")

d$CivilOrCriminal <- ifelse(str_detect(d$UnderActsNew,"IPC"),"Criminal","Civil")
table(d$CivilOrCriminal)

###
# Check, 1511 rows, 37 cols
###
dim(d)