#This scipt is for data ingestion and cleaning
library(readxl)
library(tidyverse)
library(gghighlight)

Activity_TS_Raw <- read_xls("data/Monthly-AE-Time-Series-October-2024.xls",skip = 13,sheet=1)[,1:13]

Perf_TS_Raw <- read_xls("data/Monthly-AE-Time-Series-October-2024.xls",skip = 13,sheet=2)[,1:9]

Activity_Clean <- Activity_TS_Raw %>%
  rename_with(~ c("Period",
                  "Att_T1","Att_T2","Att_T3","Att_Tot",
                  "Adm_T1","Adm_T2","Adm_T34","Adm_Tot_AE","Adm_Oth","Adm_Tot",
                  "Over_4hrs","Over_12hrs"
                  )) %>%
  mutate(Period = as.Date(Period),year=year(Period),month=month(Period))

Perf_Clean <- Perf_TS_Raw %>%
  rename_with(~ c("Period",
                  "Un4_T1","Un4_T2","Un4_T3","Un4_Tot",
                  "Ov4_T1","Ov4_T2","Ov4_T3","Ov4_Tot"
  )) %>%
  mutate(Period = as.Date(Period),year=year(Period),month=month(Period))


Activity_Winters <- Activity_Clean %>%
  filter(month %in% c(1,2,12)) %>%
  mutate(winter = ifelse(month<12,paste0(year-2001,"-",year-2000),paste0(year-2000,"-",year-1999))) %>%
  group_by(winter) %>%
  select(-Period,-year,-month) %>%
  summarise(across(everything(),sum)) 

Perf_Winters <- Perf_Clean %>%
  filter(month %in% c(1,2,12)) %>%
  mutate(winter = ifelse(month<12,paste0(year-2001,"-",year-2000),paste0(year-2000,"-",year-1999))) %>%
  group_by(winter) %>%
  select(-Period,-year,-month) %>%
  summarise(across(everything(),sum)) %>%
  mutate(Perc_Ov4_T1 = Ov4_T1/(Ov4_T1+Un4_T1),
         Perc_Ov4_T2 = Ov4_T2/(Ov4_T2+Un4_T2),
         Perc_Ov4_T3 = Ov4_T3/(Ov4_T3+Un4_T3),
         Perc_Ov4_Tot = Ov4_Tot/(Ov4_Tot+Un4_Tot))

