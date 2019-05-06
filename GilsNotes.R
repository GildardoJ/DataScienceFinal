
d = read.csv("Accidental_Drug_Related_Deaths_2012-2018.csv")
dats = read.csv("C:/Users/gilda/Documents/CSUMBSpring2019/CST383/Final_Project/chsi_dataset/RISKFACTORSANDACCESSTOCARE.csv")



#from https://www.census.gov/data/datasets/time-series/demo/popest/intercensal-2000-2010-counties.html
conCensus = read.csv("C:/Users/gilda/Documents/CSUMBSpring2019/CST383/Final_Project/co-est00int-alldata-09.csv")

summary(conCensus)
summary(d)
summary(dats)
unique(d$DeathCounty)

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
d$Other = factor(d$Other)
sum(d$Other != "")


d$factored = factor(d$Heroin)
length(d$factored[d$factored == "Y"])

nrow(d)
names(d[,21:37])
unique(d$Heroin)


aggregate(d[,21], by = list(Category = d[,21]),FUN =  nrow)
aggregate(conCensus$TOT_POP, by=list(Category=conCensus$CTYNAME), FUN=sum)




s = 5

for(val in sel){
  s = sum(nrow(d[d$Heroin == "Y",]))/nrow(d)
  print(s)
}

selected = dats[,c("CHSI_County_Name","Obesity")]


total <- merge(data =  d,data =  selected, by=c("DeathCounty","CHSI_county_name"))



counties = unique(conCensus$COUNTY)

lapply(conCensus, FUN = sum )


pop2010 = dat[dat$year == 2018,]