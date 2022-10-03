#? Created on 2022-09-30 16:27:17 by Jyri Lillemets with R version 4.2.1 (2022-06-23).
#? This script cleans data sets for a more smooth application in the project.

# Load packages
library('magrittr')
library('tidyverse')

# Countries
riigid <- read.csv('assets/countries.csv')
names(riigid) %<>% strsplit('\\.') %>% sapply(`[`, 1)
names(riigid)[names(riigid) == 'Net'] <- 'Migration'
write.csv(riigid, 'andmed/countries.csv', row.names = F)

# IT survey
tasu <- read.csv('assets/itsalary.csv', na.strings = '')
tasu %<>% 
  select(Gender, 
         Yearly.brutto.salary..without.bonus.and.stocks..in.EUR, 
         Total.years.of.experience) %>% 
  setNames(c('gender', 'salary', 'experience'))
äärmus <- quantile(tasu$salary, .75) + IQR(tasu$salary)*1.5
tasu %<>% filter(salary < äärmus)
tasu$experience %<>% sub(',', '.', .) %>% as.numeric
tasu %<>% na.omit
äärmus <- quantile(tasu$experience, .75) + IQR(tasu$experience)*1.5
tasu %<>% filter(experience < äärmus)
write.csv(tasu, 'andmed/itsalary.csv', row.names = F)
