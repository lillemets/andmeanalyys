#? Created on 2022-09-30 16:27:17 by Jyri Lillemets with R version 4.2.1 (2022-06-23).
#? This script cleans data sets for a more smooth application in the project.

# Load packages
library('magrittr')
library('dplyr')

# Read, clean and save data sets (2022-10-23 09:30:09) ----------

# World Values Survey
# hierarhiline
wvs <- read.csv('assets/WVSbycountry.csv')
wvs %<>% 
  select(Country, survself, TRADRAT5, Wave) %>%  #, X011, X025, X044)
  na.omit %>% 
  arrange(desc(Wave)) %>% distinct(Country, .keep_all=TRUE) %>% 
  select(-Wave)
names(wvs) <- c('country', 'survexp', 'tradsec') #, 'children', 'education', 'savings')
write.csv(wvs, 'andmed/wvsmap.csv', row.names = F)

# Vehicles 1983
autod <- read.csv('assets/vehicles1983.csv')
autod$origin %<>% recode(`1` = 'American', `2` = 'European', `3` = 'Japanese')
write.csv(autod, 'andmed/vehicles1983.csv', row.names = F)

# Wholesale customers
# test_08_klasterdamine
kliendid <- read.csv('assets/wholesalecustomers.csv')
kliendid %<>% lapply(function(x) ifelse(x %in% boxplot.stats(x)$out, NA, x)) %>% # Eemalda erindid
  as.data.frame
kliendid$Channel %<>% recode(`1` = 'HoReCa', `2` = 'Retail')
kliendid$Region %<>% recode(`1` = 'Lisnon', `2` = 'Oporto', `3` = 'Other')
write.csv(kliendid, 'andmed/wholesalecustomers.csv', row.names = F)

# Estonia passengers
# otsusepuu
estonia <- read.csv('assets/estoniapassengers.csv')
names(estonia) %<>% tolower
write.csv(estonia, 'andmed/estoniapassengers.csv', row.names = F)

# Heart disease indicators
# test_07_klassifitseerimine
süda <- read.csv('assets/heartdisease.csv')
set.seed(6); süda %<>% sample_n(1e3)
set.seed(0); süda$GenHealth[sample(1:nrow(süda), 100)] <- NA
#süda$Diabetic[süda$Diabetic == 'No, borderline diabetes'] <- 'Borderline'
süda$Diabetic %<>% recode('No, borderline diabetes' = 'Borderline')
write.csv(süda, 'andmed/heartdisease.csv', row.names = F)

# Gender classification
# praktikum_07_lähinaabrid
lemmik <- read.csv('assets/genderclass.csv')
write.csv(lemmik, 'andmed/genderclass.csv', row.names = F)

# Handwritten ZIP code digits
# praktikum_07_lähinaabrid
zip <- read.csv('assets/zipdigits.csv', header = F)
names(zip) <- c('digi]ot', paste0('pixel', 1:256))
set.seed(0); zip$digit[sample(1:nrow(zip), 1e3)] <- NA
write.csv(zip, 'andmed/zipdigits.csv', row.names = F)

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

# IT salary
# usaldusvahemikud
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
#Kus <- (nrow(bmi)/500+1) %>% round %>% rep(500) %>% cumsum # Take every nth observation so that there are 1000 observations
#bmi <- bmi[Kus, ]
set.seed(0); bmi %<>% slice_sample(n = 1e3)
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

# Housing prices
# linreg
majad <- read.csv('assets/housingprices.csv')
mudel <- lm(price ~ area, majad)
majad %<>% filter(cooks.distance(mudel) < .0005) #%>% plot(price ~ area, .)
majad %<>% filter(price < min(boxplot.stats(majad$price)$out))
majad$price <- majad$price / 1e6 # Teisendame hinna miljonitesse
majad$area <- majad$area / 10.764 # Teisendame ruutjalad ruutmeetriteks
write.csv(majad, 'andmed/housingprices.csv', row.names = F)

# Spotify 2000
# praktikum_05_linreg
laulud <- read.csv('assets/spotify2000.csv')
## Tõlgime veergude nimetused
names(laulud) <- c('nr','pealkiri', 'esitaja', 'liik', 'aasta', 
                   'taktsagedus', 'elavus', 'tantsitavus', 'valjusus', 
                   'publik', 'rõõmsus', 'pikkus', 'akustilisus', 'kõne', 
                   'menukus')
## Jäta lineaarselt seotud
mudel <- lm(menukus ~ ., laulud[c(5:11, 13:15)])
laulud %<>% filter(cooks.distance(mudel) < .0001) #%>% plot(Popularity ~ Loudness..dB. , .)
## Vähenda liike
liigid <- table(laulud$liik) %>% sort %>% tail(20) %>% names
laulud$liik <- ifelse(laulud$liik %in% liigid, laulud$liik, 'other')
write.csv(laulud, 'andmed/spotify2000.csv', row.names = F)

# Belarus used cars
# test_05_linreg
autod <- read.csv('assets/belaruscars.csv')
autod <- autod[, -12]
names(autod) <- c(
  'make', 'model', 'price.usd', 'year', 'condition', 'mileage.km', 'fuel', 
  'volume.l', 'color', 'transmission', 'drive')
autod$volume.l %<>% `/`(1e3)
set.seed(0); autod %<>% slice_sample(n = 1e3)
write.csv(autod, 'andmed/belaruscars.csv', row.names = F)

# Credit card default prediction
# logreg
laenud <- read.csv('assets/creditdefault.csv', skip = 1)
set.seed(0); laenud %<>% slice_sample(n = 1e3)
laenud %<>% .[c(2:6,25)]
names(laenud) <- c('credit', 'gender', 'education', 'maritalstatus', 'age', 'default')
laenud$gender %<>% factor(levels = 1:2, labels = c('Male', 'Female'))
laenud$education %<>% factor(levels = 1:4, labels = c('Graduate school', 'University', 'High school', 'Others'))
laenud$maritalstatus %<>% factor(levels = 1:3, labels = c('Married', 'Single', 'Others'))
write.csv(laenud, 'andmed/creditdefault.csv', row.names = F)

# Instagram users
# praktikum_06_logreg
insta <- read.csv('assets/instagramusers.csv')
write.csv(insta, 'andmed/instagramusers.csv', row.names = F)

# Starbucks customer survey
# test_06_logreg
sb <- read.csv('assets/starbuckssurvey.csv')
sb <- sb[, c(13:19, 21)]
names(sb) <- c('coffee.quality', 'price', 'promotions', 'ambiance', 'wifi.quality', 'service', 'meetings', 'continue')
write.csv(sb, 'andmed/starbuckssurvey.csv', row.names = F)