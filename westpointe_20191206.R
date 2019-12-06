library(dplyr)
install.packages("odbc")
library(odbc)
library(tidyverse)

#con = dbConnect(odbc::odbc(), "REDPROD",
#                uid = "jwinitzk@slcc.edu",
#                pwd = "St3ph3ns!")

#select fsch.term_code, fsch.course_subject, fsch.course_number, ds.campus, ds.begin_time_1, ds.end_time_1, ds.meeting_days_1, 
#fsch.pidm, address_zip, ethnicity, gender, age_on_first_day, reg_status as registration_status, cum_ug_credits, first_generation_ind, 
#pell_eligible_ind, term_program_code, term_program_name
#from wsrpmgr.fact_slcc_course_history fsch
#JOIN wsrpmgr.dim_section ds	
#USING(dim_section_key)
#JOIN wsrpmgr.dim_student dstu
#USING(dim_student_key)
#JOIN wsrpmgr.dim_student_term dst
#ON fsch.pidm = dst.pidm
#AND fsch.term_code = dst.term_code
#LEFT JOIN wsrpmgr.dim_pgm_of_study_term dpost
#USING(dim_pgm_of_study_term_key)
#where fsch.term_code between '201820' and '201950'
#and concurrent_section_ind = 'N'
#order by fsch.pidm, fsch.term_code, fsch.course_subject, fsch.course_number;

data <- read.csv("westpointe_student_profile.csv")
data <- westpointe_student_profile
rm(westpointe_student_profile)

#count of students per semester
data %>% group_by(TERM_CODE, PIDM) %>%
  slice(1) %>%
  group_by(TERM_CODE) %>%
  summarise(count = n())


#westpointe student flag
data <- data %>%
  mutate(westpointe3 = ifelse(CAMPUS == "Westpointe Center", 1, 0)) %>%
  group_by(PIDM) %>%
  mutate(westpointe2 = sum(westpointe3)) %>%
  mutate(westpointe = ifelse(westpointe2 > 0, 1, 0))

data$westpointe3 <- NULL
data$westpointe2 <- NULL

#unique pidms
unique <- data %>% 
  group_by(TERM_CODE,PIDM) %>% 
  slice(1)

unique %>% 
  group_by(TERM_CODE) %>%
  summarise(count = n())

# A tibble: 8 x 2
#TERM_CODE count
#<dbl> <int>
#1    201820 23011
#2    201830 12091
#3    201840 23668
#4    201850     6
#5    201920 22021
#6    201930 12047
#7    201940 24010
#8    201950     3

studenttypetable <- unique %>% 
  group_by(TERM_CODE, REGISTRATION_STATUS) %>%
  summarise(count = n())

#create returning student flag
data <- data %>% mutate(returning = ifelse(REGISTRATION_STATUS == "Continuing Student", 1,
                                   ifelse(REGISTRATION_STATUS == "Returning Student", 1, 0)))

campus_studenttype <- data %>% group_by(CAMPUS, REGISTRATION_STATUS) %>%
#    filter(CAMPUS == "Westpointe Center") %>%
    summarise(n = n()) %>%
  mutate(pc = n/sum(n))

#no way to tell from this data if non-matriculated students are new or returning. Per Abby,
# I need FIRST_TERM_NON_CONCURRENT_IND field as well.

