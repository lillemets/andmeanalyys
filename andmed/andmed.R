#? Created on 2022-09-30 16:27:17 by Jyri Lillemets with R version 4.2.1 (2022-06-23).
#? This script cleans data sets for a more smooth application in the project.

# Load packages
library('magrittr')
library('dplyr')


# Read, clean and save data sets (2022-10-23 09:30:09) ----------

# Presidentide kõned
# test_10_tekstikaeve
library('rjson')
kõned <- fromJSON(file = 'assets/kõned.json')
kõned %<>% bind_rows
## Vali
set.seed(1); kõned %<>% sample_n(200)
## Korrasta
kõned$aasta <- substr(kõned$date, 7,10) %>% as.numeric
kõned %<>% select(pealkiri = title, president, aasta, text)
## Täpsusta korduvad pealkirjad
korduvad <- duplicated(kõned$pealkiri)
kõned$pealkiri[korduvad] %<>% paste(kõned$aasta[korduvad])
write.csv(kõned, 'andmed/kõned.csv', row.names = F)

# praktikum_10_tekstikaeve
library('rvest')
xtee <- list(pealkiri = '//*[@id="aspect_artifactbrowser_ItemViewer_div_item-view"]/div/h2/text()', 
             liik = '//*[@id="aspect_artifactbrowser_ItemViewer_div_item-view"]/div/div/div[2]/div[3]/ul/li/a', 
             aasta = '//*[@id="aspect_artifactbrowser_ItemViewer_div_item-view"]/div/div/div[1]/div[2]/text()[2]', 
             tekst = '//*[@id="aspect_artifactbrowser_ItemViewer_div_item-view"]/div/div/div[2]/div[1]/div/text()[1]')
allUrls <- paste0('https://dspace.emu.ee/handle/10492/', 1000:7000)
set.seed(0); urls <- sample(allUrls, 200)
dokidLs <- lapply(urls, function(x) {
  tryCatch({
    leht <- read_html(x)
    lapply(xtee, function(y) html_elements(leht, xpath = y) %>% html_text)
    #Sys.sleep(1)
    }, error = function() message(x, '\n'))
})
dokid <- lapply(dokidLs, as.data.frame) %>% do.call(rbind, .)
#dokid %<>% filter(!grepl('Vol.', dokid$liik))
#dokid %<>% filter(!grepl(' the ', dokid$tekst))
write.csv(dokid, 'andmed/dspace.csv', row.names = F)

# Comparative Manifestos Project
# tekstikaeve
#nimed <- list.files('andmed/cmp') %>% sub('\\..+' , '', .)
#progs <- lapply(list.files('andmed/cmp', full = T), read.csv) %>% 
#  lapply(select, 1) %>% lapply(unlist) %>% lapply(trimws) %>% 
#  lapply(paste, collapse = ' ')
#for (i in seq_along(progs)) writeLines(progs[[i]], paste0('andmed/cmp/', nimed[i], '.txt'))
#library('manifestoR')
#mp_setapikey(key = '5d3ca6b25bf8fe42b5de91660224b086')
#progs <- mp_corpus(countryname == "Estonia" & edate > as.Date("2007-03-01"))


# Young people
# test_09_mõõtmevähendus
noored <- read.csv('assets/youngpeople.csv', na.strings = '')
## Nimitunnused arvudeks
Tasemed <- list(Smoking = c('never smoked', 'tried smoking', 'former smoker', 'current smoker'), 
                Alcohol = c('never', 'social drinker', 'drink a lot'), 
                Punctuality = c('i am often running late', 'i am always on time', 'i am often early'), 
                Lying = c('never', 'only to avoid hurting someone', 'sometimes', 'everytime it suits me'), 
                Internet.usage = c('no time at all', 'less than an hour a day', 'few hours a day', 'most of the day'), 
                Education = c('currently a primary school pupil', 'primary school', 'secondary school', 
                              'college/bachelor degree', 'masters degree', 'doctorate degree'))
for (i in names(Tasemed)) {
  noored[, i] <- noored[, i] %>% factor(levels = Tasemed[[i]]) %>% as.numeric
}
## Korrasta veeru nimetused
names(noored) %<>% tolower %>% gsub('\\.+', '\\.', .)
## Salvesta nimetused
#writeLines(names(noored), 'andmed/youngpeople.txt')
write.csv(noored, 'andmed/youngpeople.csv', row.names = F)

# Eesti ettevõtete majandusnäitajad
# praktikum_09_mõõtmevähendus
## Päri
library('pxweb')
aadress <- 'https://andmed.stat.ee/api/v1/et/stat/EM001'
#pxweb_get(aadress)$variables
valikud <- list(
  'Näitaja' = '*', 
  'Tegevusala' = '*', 
  'Tööga hõivatud isikute arv' = 'TOTAL', 
  'Vaatlusperiood' = '2020') %>% 
  lapply(as.character)
päring <- pxweb_get_data(url = aadress, query = valikud)
ettev <- päring
## Korrasta
ettev %<>% select(näitaja = Näitaja, tegevusala = Tegevusala, 
                  väärtus = 'EM001: Ettevõtete majandusnäitajad')
## Vali 
set.seed(2)
jätta <- unique(ettev$näitaja) %>% sample(20)
ettev %<>% filter(näitaja %in% jätta)
jätta <- unique(ettev$tegevusala) %>% sample(50)
ettev %<>% filter(tegevusala %in% jätta)
## Laienda
library('tidyr')
ettev %<>% pivot_wider(names_from = näitaja, values_from = väärtus)
## Salvesta
write.csv(ettev, 'andmed/tegevusalad.csv', row.names = F)

# Death causes
# faktoranalüüs
surm <- read.csv('assets/deathcause.csv')
## Asedna absoluutväärtused osakaaludega
surm[, -1] <- surm[, -1] / rowSums(surm[, -1], na.rm = T)
## Korrasta
vaheta <- c('Cirrhosis.and.other.chronic.liver.diseases', 
            'Environmental.heat.and.cold.exposure')
names(surm)[names(surm) %in% vaheta] <- c('Liver.diseases', 'Heat.and.cold')
write.csv(surm, 'andmed/deathcause.csv', row.names = F)

# Decathlon
# peakomponendid
kv <- read.csv('assets/decathlon2004.csv')
kv %<>% filter(Competition == 'OlympicG')
kv %<>% select(Athlets, 
               run100m = X100m, run400m = X400m, run1500m = X1500m, 
               run110m.hurdle = X110m.hurdle, 
               Long.jump, High.jump, Pole.vault, 
               Shot.put, Discus, Javeline)
names(kv) %<>% tolower %>% sub('\\.', '', .)
write.csv(kv, 'andmed/decathlon2004.csv', row.names = F)

# Eesti vallad
# praktikum_08_klasterdamine
## Päri
library('pxweb')
aadress <- 'https://andmed.stat.ee/api/v1/et/stat/RL21422'
#pxweb_get(aadress)$variables
valikud <- list(
  'Aasta' = 2021, 
  'Vanuserühm' = 2:6, 
  'Elukoht' = '*', 
  'Sugu' = 2:3, 
  'Kodakondsus' = 2:6) %>% 
  lapply(as.character)
päring <- pxweb_get_data(url = aadress, query = valikud)
ov <- päring
## Jäta ainult vallad ja linnad
ov %<>% subset(grepl('\\slinn$|\\svald$|Tallinn$', ov$Elukoht))
## Korrasta
names(ov)[names(ov) == 'Elukoht'] <- 'kov'
ov$kov <- sub('^\\.\\.', '', ov$kov)
names(ov)[ncol(ov)] <- 'väärtus'
ov %<>% select(-Aasta)
## Arvuta hilisemaks kogu elanike arv KOVides
kokku <- aggregate(väärtus ~ kov, ov, sum)
## Laienda
library('tidyr')
#ov %>% pivot_wider(names_from = c('Sugu', 'Kodakondsus', 'Vanuserühm'), values_from = väärtus)
ov <- lapply(ov[, c('Sugu', 'Kodakondsus', 'Vanuserühm')], function(x) {
  aggregate(ov$väärtus, list(kov = ov$kov, tunnus = x), sum) %>% 
    pivot_wider(names_from = tunnus, values_from = x)
})
ov <- do.call('cbind', ov)
## Korrasta veerud
ov[, grep('kov', names(ov))[-1]] <- NULL
names(ov)[grep('kov', names(ov))] <- 'kov'
## Arvuta osakaalud
ov[, -1] %<>% sapply(`/`, kokku$väärtus)
## Salvesta
write.csv(ov, 'andmed/kovrahvastik.csv', row.names = F)

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
# kkeskmised
autod <- read.csv('assets/vehicles1983.csv')
autod$origin %<>% recode(`1` = 'American', `2` = 'European', `3` = 'Japanese')
write.csv(autod, 'andmed/vehicles1983.csv', row.names = F)

# Wholesale customers
# test_08_klasterdamine
kliendid <- read.csv('assets/wholesalecustomers.csv')
kliendid %<>% lapply(function(x) ifelse(x %in% boxplot.stats(x)$out, NA, x)) %>% # Eemalda erindid
  as.data.frame
kliendid$Channel %<>% recode(`1` = 'horeca', `2` = 'retail')
kliendid$Region %<>% recode(`1` = 'Lisbon', `2` = 'Oporto', `3` = 'Other')
names(kliendid) <- c('channel', 'region', 'fresh', 'milk', 'groceries', 'frozen', 'cleaning', 'meat')
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
riigid[, 1] %<>% trimws
names(riigid) %<>% strsplit('\\.') %>% sapply(`[`, 1)
names(riigid) %<>% tolower
names(riigid)[names(riigid) == 'net'] <- 'netmigration'
riigid %<>% select(country, region, population, area, 
                   netmigration, gdp, literacy, phones, 
                   arable, agriculture)
riigid$region %<>% recode(
  "ASIA (EX. NEAR EAST)" = 'Asia', 
  "BALTICS" = 'Eastern Europe', 
  "C.W. OF IND. STATES" = 'Asia', 
  "EASTERN EUROPE" = 'Eastern Europe', 
  "LATIN AMER. & CARIB" = 'South America', 
  "NEAR EAST" = 'Asia', 
  "NORTHERN AFRICA" = 'Africa', 
  "NORTHERN AMERICA" = 'North America', 
  "OCEANIA" = 'Oceania', 
  "SUB-SAHARAN AFRICA" = 'Africa', 
  "WESTERN EUROPE" = 'Western Europe'
)
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
