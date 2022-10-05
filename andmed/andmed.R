#? Created on 2022-09-30 16:27:17 by Jyri Lillemets with R version 4.2.1 (2022-06-23).
#? This script cleans data sets for a more smooth application in the project.

# Load packages
library('magrittr')
library('tidyverse')

# Diabetes
# test_03_seosed
tervis <- read.csv('assets/diabetes.csv')
set.seed(0)
tervis %<>% slice_sample(n = 1e4)
write.csv(tervis, 'andmed/diabetes.csv', row.names = F)

# restaurantorders.csv
# test_03_seosed
orders <- read.csv('assets/restaurantorders.csv')
orders %<>% 
  subset(select = c('Order.Number', 'Item.Name')) %>% 
  setNames(c('order', 'food'))
write.table(orders, 'andmed/orders.csv', sep = ',', quote = F, row.names = F, col.names = F)

# Countries
# 03_seosed
riigid <- read.csv('assets/countries.csv')
names(riigid) %<>% strsplit('\\.') %>% sapply(`[`, 1)
names(riigid)[names(riigid) == 'Net'] <- 'Migration'
write.csv(riigid, 'andmed/countries.csv', row.names = F)

# IT survey
# 04_usaldusvahemikud
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

# Heart disease indicators
# praktikum_04_usaldusvahemikud
bmi <- read.csv('assets/heartdisease.csv')
Kus <- (nrow(bmi)/500+1) %>% round %>% rep(500) %>% cumsum # Take every nth observation so that there are 1000 observations
bmi <- bmi[Kus, ]
#t.test(BMI ~ PhysicalActivity, bmi)
bmi %<>% select(BMI, HeartDisease, PhysicalHealth)
write.csv(bmi, 'andmed/bmi.csv', row.names = F)

# Lunch offers in Estonia
# test_04_usaldusvahemikud
lunch <- read.csv('assets/lunchoffers.csv')
lunch %<>% filter(price_euro < 20)
set.seed(0); lunch %<>% slice_sample(n = 1e4)
lunch %<>% select(date, city, name, offer, price_euro)
lunch %<>% rename(price = price_euro)
write.csv(lunch, 'andmed/lunchoffers.csv', row.names = F)
