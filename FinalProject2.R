dat = read.csv("D:/Darren/Spring_2019/CST_383/finalProject/Accidental_Drug_Related_Deaths_2012-2018.csv")
dat2 = read.csv("D:/Darren/Spring_2019/CST_383/finalProject/chsi_dataset/RISKFACTORSANDACCESSTOCARE.csv")
dat3 = read.csv("D:/Darren/Spring_2019/CST_383/finalProject/County_Age_Sex_Race_Ethn_2010-2017/A18SRH_cty_icen0009_pcen1017.csv")

# Website where dat datsets were found
# https://data.ct.gov/Health-and-Human-Services/Accidental-Drug-Related-Deaths-2012-2018/rybz-nyjw
# https://authoring.ct.gov//DPH/Health-Information-Systems--Reporting/Population/Connecticut-Population-Datasets-in-SAS-Format

# Removes Fentanyl column, because there are only 4 fatalities


# Turns 'Other' to ones and zeroes - this column had strings (drug names) rather than simple "Y" to 
# indicate drug fatalities, so is handled differently than the other drug columns below
dat$Other = ifelse(dat$Other != "", 1, 0)

# Turns each drug column of "Y"s and "N"s into ones and zeroes - except for 'Other', which is handled above
colNames = c("Heroin", "Cocaine", "Fentanyl", "FentanylAnalogue", 
             "Oxycodone", "Oxymorphone", "Ethanol", "Hydrocodone",
             "Benzodiazepine", "Methadone", "Amphet", "Tramad", 
             "Morphine_NotHeroin", "Hydromorphone", 
             "OpiateNOS", "AnyOpioid")
dat[,colNames] = apply(dat[,colNames], 2, FUN = function(x) ifelse(x == "Y", 1, 0))

# Removes all non-Connecticut rows from the dataset
dat2 = subset(dat2, State_FIPS_Code == 9)

# Removes all pre 2012 rows from the dataset to better match the dat1 dataset
dat3 = subset(dat3, year >= 2012)

# Calculates overall rate of each drug's involvement in total number of drug fatalities
HeroinRate = sum(dat$Heroin)/length(dat$Heroin)
drugRates = HeroinRate

CocaineRate = sum(dat$Cocaine)/length(dat$Cocaine)
drugRates = append(drugRates, CocaineRate, after = length(x))

FentanylRate = sum(dat$Fentanyl)/length(dat$Fentanyl)
drugRates = append(drugRates, FentanylRate, after = length(x))

FentanylAnalogueRate = sum(dat$FentanylAnalogue)/length(dat$FentanylAnalogue)
drugRates = append(drugRates, FentanylAnalogueRate, after = length(x))

OxycodoneRate = sum(dat$Oxycodone)/length(dat$Oxycodone)
drugRates = append(drugRates, OxycodoneRate, after = length(x))

OxymorphoneRate = sum(dat$Oxymorphone)/length(dat$Oxymorphone)
drugRates = append(drugRates, OxymorphoneRate, after = length(x))

EthanolRate = sum(dat$Ethanol)/length(dat$Ethanol)
drugRates = append(drugRates, EthanolRate, after = length(x))

HydrocodoneRate = sum(dat$Hydrocodone)/length(dat$Hydrocodone)
drugRates = append(drugRates, HydrocodoneRate, after = length(x))

BenzodiazepineRate = sum(dat$Benzodiazepine)/length(dat$Benzodiazepine)
drugRates = append(drugRates, BenzodiazepineRate, after = length(x))

MethadoneRate = sum(dat$Methadone)/length(dat$Methadone)
drugRates = append(drugRates, MethadoneRate, after = length(x))

AmphetRate = sum(dat$Amphet)/length(dat$Amphet)
drugRates = append(drugRates, AmphetRate, after = length(x))

TramadRate = sum(dat$Tramad)/length(dat$Tramad)
drugRates = append(drugRates, TramadRate, after = length(x))

Morphine_NotHeroinRate = sum(dat$Morphine_NotHeroin)/length(dat$Morphine_NotHeroin)
drugRates = append(drugRates, Morphine_NotHeroinRate, after = length(x))

HydromorphoneRate = sum(dat$Hydromorphone)/length(dat$Hydromorphone)
drugRates = append(drugRates, HydromorphoneRate, after = length(x))

OtherRate = sum(dat$Other)/length(dat$Other)
drugRates = append(drugRates, OtherRate, after = length(x))

OpiateNOSRate = sum(dat$OpiateNOS)/length(dat$OpiateNOS)
drugRates = append(drugRates, OpiateNOSRate, after = length(x))

AnyOpioidRate = sum(dat$AnyOpioid)/length(dat$AnyOpioid)
drugRates = append(drugRates, AnyOpioidRate, after = length(x))

drugRates

Drugs = c("HeroinRate", "CocaineRate", "FentanylRate", "FentanylAnalogueRate", 
          "OxycodoneRate", "OxymorphoneRate", "EthanolRate", "HydrocodoneRate",  
          "BenzodiazepineRate", "MethadoneRate", "AmphetRate", "TramadRate", 
          "Morphine_NotHeroinRate", "HydromorphoneRate", "OtherRate", "OpiateNOSRate", 
          "AnyOpioidRate")
DrugDeaths = data.frame(Drugs, drugRates)
DrugDeaths

par(las = 2, mar = c(9, 4, 4, 4))
plot(DrugDeaths)

# Creates barplot of heroin deaths by race
tbl = table(dat$Heroin, dat$Race)
barplot(tbl, col="darkblue", main="Deaths by Heroin")

# Groups and sums heroin deaths by race 
aggregate(Heroin ~ Race, data=dat, sum)
sum(dat$Heroin)

# Lists races with heroin deaths for each
sum(dat[dat$Age > 35,]$Heroin)

# Shows percentage of heroin deaths that are white people
2006/sum(dat$Heroin)

174/sum(dat$Heroin)


# Shows total rate of each drug involved in fatalities overall
dat[,colNames] = apply(dat[,colNames], 2, sum(dat))



table(aggregate(dat3$pop(unique(dat3$cty_name))))

tapply(dat3, list(group[row(cty_name)], col(pop)), sum)

# total <- merge(dat, d, by=c("DeathCounty"))

# sum(nrow(dat[dat$Heroin == "Y", ]))/nrow(dat)

sum(nrow(dat[dat$Heroin == "Y", ]))/nrow(d)

# Creates dataset of 2017 data
dat2017 = dat3[dat3$year == 2017 ,]

dat2017

# Gives county population totals
aggregate(dat2017$pop, by=list(Category=dat2017$cty_name), FUN=sum)
# Gives state population total
sum(dat2017$pop)

split = split_data(dat)
tr_dat = split[[1]]
te_dat = split[[2]]

fit = naiveBayes(output ~ age + maxhr, data = tr_dat)

predicts = predict(fit, newdata = te_dat)

conf_mtx = table(predicts, te_dat$output)
conf_mtx
mean(predicts == te_dat$output)

summary(conf_mtx)

plot(density(dat&Age[, dat$Heroin == "y"], col = "green"))
     lines(density(dat$Age), dat$Heroin == "", col = "red")

     # Splits data into test and training
     tr_rows = sample(nrow(dat), 0.7 * nrow(dat), replace = FALSE)
     
     tr_dat = dat[tr_rows,]
     
     te_dat = dat[-tr_rows,]
     
     # If equals zero, data was split correctly done
     (nrow(tr_dat) + nrow(te_dat)) - nrow(dat)
     
     drug_fatalities = dat$Heroin %in% c(" ", "ALCOHOL PRESENT",

      "ILLEGAL DRUG CONTRIBUTED", "ILLEGAL DRUG PRESENT")
     daytime = ifelse(dat$timestamp$hour > 18 | dat$timestamp$hour < 5, "night", "day")
     
     tbl = table(dat$Heroin, dat$Sex)
     tbl1 = apply(tbl, 2, function(x) x/sum(x))
     barplot(tbl1, main="Heroin", xlab="Gender", legend=rownames(tbl))
     
     tbl = table(dat$AnyOpiates, dat$Race)
     tbl1 = apply(tbl, 2, function(x) x/sum(x))
     barplot(tbl1, main="Heroin", xlab="Race", legend=rownames(tbl))
     
     head(dat, 2)

      dat$Heroin = dat$Heroin == "Y"
      HeroinRate = sum(dat$Heroin)/length(dat$Heroin)
      HeroinRate
      
      dat$Heroin = dat$Heroin == "Y"
      HeroinRate = sum(dat$Heroin)/length(dat$Heroin)
      HeroinRate
      
      vec = c("Y", "", "", "Y")
      ifelse(vec == "Y", 1, 0)


