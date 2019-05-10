
d = read.csv("Accidental_Drug_Related_Deaths_2012-2018.csv")
dats = read.csv("C:/Users/gilda/Documents/CSUMBSpring2019/CST383/Final_Project/chsi_dataset/RISKFACTORSANDACCESSTOCARE.csv")



#from https://www.census.gov/data/datasets/time-series/demo/popest/intercensal-2000-2010-counties.html
conCensus = read.csv("C:/Users/gilda/Documents/CSUMBSpring2019/CST383/Final_Project/co-est00int-alldata-09.csv")

summary(conCensus)
summary(d)
summary(dats)


sum(is.na(dats))
par(mfrow= c(2,1))
dats[dats == -1111.1] <- NA     #cleaned data a bit on health data           
plot((dats[dats$CHSI_State_Name == "California",]$Obesity ~ dats[dats$CHSI_State_Name == "California",]$No_Exercise))
plot(table(dats$Obesity ~ dats$CHSI_State_Name))


plot((dats[dats$CHSI_State_Name == "California",]$Obesity ~ dats[dats$CHSI_State_Name == "California",]$No_Exercise),ylim = c(0,40))
plot((dats[dats$CHSI_State_Name == "Alabama",]$Obesity ~ dats[dats$CHSI_State_Name == "Alabama",]$No_Exercise),ylim = c(0,40))

plot(dat[,])


#plot by sex



d$age_range[d$Sex == "Male"] = cut(d$Age[d$Sex == "Male"], breaks=c(0,20,25,30,35,40,45,50,55,60,100), labels=c("< 20", "20-25","25-30","30-35","35-40","40-45","45-50","50-55", "50-60", "> 60"))
plot(d$age_range[d$Sex == "Male"] , col = "green", ylim= c(0,500))
plot(d$age_range[d$Sex == "Female"], col = "red", ylim= c(0,500))


#drugs_alcohol = dat$Driver.Substance.Abuse %in% c("ALCOHOL
#CONTRIBUTED", "ALCOHOL PRESENT", "ILLEGAL DRUG CONTRIBUTED", "ILLEGAL DRUG PRESENT")

sex = d$Sex  

tbl = table( sex, d$age_range)
tbl1 = apply(tbl, 2, function(x) x/sum(x))

barplot(tbl1, xlab="Male to female ratio", legend=rownames(tbl),col=rainbow(4))

# by race 

# Install
install.packages("wesanderson")
# Load
library(wesanderson)

unique(d$Race)

tbls = table( d$Race, d$age_range)
tbls1 = apply(tbls, 2, function(x) x/sum(x))
barplot(tbls1, xlab="race ratio", col=rainbow(12))

# death by age, naive bayes predictor

str(d)
unique(d$DeathCounty)
deathCity = unique(d$DeathCityGeo)
deathCity = sort(deathCity)
deathCity
install.packages("e1071")
library(e1071)

tr_rows = sample(nrow(d), 0.8 * nrow(d))
tr_dat = d[tr_rows,]
te_dat = d[-tr_rows,]

d$age_range = cut(d$Age, breaks=c(0,20,25,30,35,40,45,50,55,60,100), labels=c("< 20", "20-25","25-30","30-35","35-40","40-45","45-50","50-55", "50-60", "> 60"))
plot(d$age_range)

nrow(tr_dat)
length(tr_dat$Sex)
fit = naiveBayes(deathCity ~  Race , data=tr_dat)



################
#clustering by type of drugs
head(d[,21:27])
plot(d[,21] ~ d[,22])

num_clusters = 5
fit = kmeans(d[,21:27], num_clusters)
library(cluster)

sil = silhouette(fit$cluster, dist(dat))
# plot the silhouette
plot(sil)
# average silhouette width
sw = mean(sil[,3])


# perform hierarchical clustering
hc = hclust(dist(d), method="complete")
# plot the "dendrogram"
plot(hc)


########
# split race into white an non_white as the output
table(d$Race)

#make new column of 0 or 1 for white


##############

plot(table(d$Age))

plot(d[,21:25])

names = names(d[,21:25])
names
is.character(names)
par(las = 2)
hist(table(d[,21:25]))
table(d[,21:25])

n = list(names(d[,21:25]))
table(d[,21:25])
aggregate(d[,21:25], by=n, FUN=sum)


num = c(21:37)
colCounts = 0;
for(n in num ){
  curr = nrow(d[d[,n] == "Y",])
  if(n == 21){
    colCounts = c(curr)
  }else if ( n == 35){
    colCounts = c(colCounts,nrow(d[d$Other != "",]))
  }else {
    colCounts = c(colCounts,curr)
  }
}

name =  names(d[,21:37])

par(las = 2,mar= c(9,4,3,1)+0.1)
plot(colCounts, xaxt = "n", ylab = "Number of deaths", xlab = "", type = "l", col = "blue", main = "Number of deaths by drug type")
axis(1, at=1:17, labels=name)

nrow(d[d[,21] == "Y",])
nrow(d[d$Cocaine == "Y",])
nrow(d[d$Fentanyl == "Y",])
nrow(d[d$FentanylAnalogue == "Y",])
nrow(d[d$Oxycodone == "Y",])
nrow(d[d$Oxymorphone == "Y",])
nrow(d[d$Ethanol == "Y",])
nrow(d[d$Hydrocodone == "Y",])
nrow(d[d$Benzodiazepine == "Y",])
nrow(d[d$Methadone == "Y",])
nrow(d[d$Amphet == "Y",])
nrow(d[d$Tramad == "Y",])
nrow(d[d$Morphine_NotHeroin == "Y",])
nrow(d[d$Hydromorphone == "Y",])
nrow(d[d$Other != "",])
nrow(d[d$OpiateNOS == "Y",])
nrow(d[d$AnyOpioid == "Y",])


unique(d$Other)
unique(d$OpiateNOS)
unique(d$AnyOpioid)
d$Other = factor(d$Other)
sum(d$Other != "")


d$factored = factor(d$Heroin)
length(d$factored[d$factored == "Y"])

nrow(d)
names(d[,21:37])
unique(d$Heroin)


aggregate(d[,21], by = list(Category = d[,21]),FUN =  nrow)
aggregate(conCensus$TOT_POP, by=list(Category=conCensus$CTYNAME), FUN=sum)



colnames(d[21])

unique(d$Fentanyl)
sel = c(21:37)
#names(d[,sel])
for(val in sel){
  
  s = sum(nrow(d[d[,val] == "Y",]))/nrow(d)
  print(names(d[val]))
  print(s)
  
}

selected = dats[,c("CHSI_County_Name","Obesity")]
total <- merge(data =  d,data =  selected, by=c("DeathCounty","CHSI_county_name"))
counties = unique(conCensus$COUNTY)

lapply(conCensus, FUN = sum )


pop2010 = dat[dat$year == 2018,]