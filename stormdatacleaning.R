### Storm data processing
## note: EXP columns: k=thousands, M=millions, B=billions
# http://ire.org/nicar/database-library/databases/storm-events/
#read in the data
setwd("~/Documents/Coursera courses/Data Science Specialization/Reproducible Research/RepData_PeerAssessment2")
library(ggplot2)
library(plyr)
library(stringr)
library(maps)
library(datasets)
library(mapproj)
library(gridExtra)
storm <- read.csv("repdata-data-StormData.csv", stringsAsFactors=FALSE)
# variables of interest: BGN_DATE, END_DATE, FATALITIES, INJURIES, CROPDMG
# CROPDMGEXP, PROPDMG, PROPDMGEXP, EVTYPE, STATE
# col numbers: 2(date), 7(char), 8 (char), 12 (date), 23 (num),24 (num),25(num),26(char), 27 (num), 28 (char) 28:37 NULL
# 1. subset based on variables needed for the analysis to make dataset more manageable
int <- c("BGN_DATE", "FATALITIES", "INJURIES", "CROPDMG", "CROPDMGEXP", "PROPDMG", "PROPDMGEXP", "EVTYPE", "STATE")
stormsub <- storm[, colnames(storm) %in% int]
rm(storm)
# 2. subset based on fatalities, injuries, property and crop damage
stormdmg <- subset(stormsub, stormsub$FATALITIES > 0 | stormsub$INJURIES > 0 |stormsub$PROPDMG > 0 | stormsub$CROPDMG >0)
rm(stormsub)
# 3. eliminate the times from the date columns, 
#    make a separate year column, and turn the dates into Date objects
dates <- sapply(stormdmg$BGN_DATE, FUN=function(x) {strsplit(x, " ")[[1]][1]})
years <- sapply(dates, FUN=function(x) {strsplit(x, "/")[[1]][3]})
#yr <- gsub("19", "", years) orders the years starting with 00 
#yr <- gsub("20", "", yr) ##cutting it down to 2 digits is better for graphing
stormdmg$YEAR <- years

# 4. transform multiply damage variables by their appropriate multiplier
codes <- c('K'=1000, 'k'=1000, 'M'=1000000, 'm'=1000000, 'B'=1000000000, 'b'=1000000000)
multi <- c(rep(1000, 2), rep(1000000, 2), rep(1000000000, 2))
propmul <- data.frame(PROPDMGEXP=codes, PROPDMGMULT=multi)
cropmul <- data.frame(CROPDMGEXP=codes, CROPDMGMULT=multi)
new1 <- merge(stormdmg, propmul, all.x=TRUE)
new2 <- merge(new1, cropmul, all.x=TRUE)
new2$PROPDMGMULT[is.na(new2$PROPDMGMULT)] <- 1##to eliminate NAs and make calculations possible
new2$CROPDMGMULT[is.na(new2$CROPDMGMULT)] <- 1
new2$NEWPROPDMG <- new2$PROPDMG * new2$PROPDMGMULT
new2$NEWCROPDMG <- new2$CROPDMG * new2$CROPDMGMULT

# 6. combine similiar event types in order to query by event
new2$COMBEVTYPE <- new2$EVTYPE
new2$COMBEVTYPE <- toupper(new2$COMBEVTYPE)
new2$COMBEVTYPE[str_detect(string=new2$COMBEVTYPE, pattern="^AVAL")] <- "AVALANCHE"
new2$COMBEVTYPE[str_detect(string=new2$COMBEVTYPE, pattern="^AVAL")] <- "AVALANCHE"
new2$COMBEVTYPE[new2$COMBEVTYPE=='COLD AIR TORNADO'] <- "TORNADO"
new2$COMBEVTYPE[str_detect(string=new2$COMBEVTYPE, pattern="^TORN")] <- "TORNADO"
new2$COMBEVTYPE[str_detect(string=new2$COMBEVTYPE, pattern="FREEZE")] <- "FREEZE"
new2$COMBEVTYPE[str_detect(string=new2$COMBEVTYPE, pattern="FROST")] <- "FREEZE"
new2$COMBEVTYPE[str_detect(string=new2$COMBEVTYPE, pattern="^THU")] <- "THUNDERSTORM"
new2$COMBEVTYPE[str_detect(string=new2$COMBEVTYPE, pattern="^TSTM")] <- "THUNDERSTORM"
new2$COMBEVTYPE[str_detect(string=new2$COMBEVTYPE, pattern="FREEZING")] <- "FREEZING RN" #SO THAT RAIN DOESN'T GET PICKED UP
new2$COMBEVTYPE[str_detect(string=new2$COMBEVTYPE, pattern="HURRICANE")] <- "HURRICANE"
new2$COMBEVTYPE[str_detect(string=new2$COMBEVTYPE, pattern="DROUGHT")] <- "DROUGHT"
new2$COMBEVTYPE[str_detect(string=new2$COMBEVTYPE, pattern="HEAT")] <- "HEAT"
new2$COMBEVTYPE[str_detect(string=new2$COMBEVTYPE, pattern="WARM")] <- "HEAT"
new2$COMBEVTYPE[str_detect(string=new2$COMBEVTYPE, pattern="HAIL")] <- "HAIL"
new2$COMBEVTYPE[str_detect(string=new2$COMBEVTYPE, pattern="FLASH")] <- "FLASH FLD"
new2$COMBEVTYPE[str_detect(string=new2$COMBEVTYPE, pattern="FLOOD")] <- "FLOOD"
new2$COMBEVTYPE[str_detect(string=new2$COMBEVTYPE, pattern="RAPID")] <- "FLOOD"
new2$COMBEVTYPE[str_detect(string=new2$COMBEVTYPE, pattern="URBAN/SML STREAM FLD")] <- "FLOOD"
new2$COMBEVTYPE[str_detect(string=new2$COMBEVTYPE, pattern="RAIN")] <- "RAIN"
new2$COMBEVTYPE[str_detect(string=new2$COMBEVTYPE, pattern="SNOW")] <- "SNOW"
new2$COMBEVTYPE[str_detect(string=new2$COMBEVTYPE, pattern="LIG")] <- "LIGHTNING"
new2$COMBEVTYPE[str_detect(string=new2$COMBEVTYPE, pattern="^MUD")] <- "MUDSLIDE"
new2$COMBEVTYPE[str_detect(string=new2$COMBEVTYPE, pattern="LANDSLIDE")] <- "LANDSLIDE"
new2$COMBEVTYPE[str_detect(string=new2$COMBEVTYPE, pattern="TROPICAL")] <- "TROPICAL STORM"
new2$COMBEVTYPE[str_detect(string=new2$COMBEVTYPE, pattern="LANDSLIDE")] <- "LANDSLIDE"
new2$COMBEVTYPE[str_detect(string=new2$COMBEVTYPE, pattern="WATER")] <- "WATERSPOUT"
new2$COMBEVTYPE[str_detect(string=new2$COMBEVTYPE, pattern="FOG")] <- "FOG"
new2$COMBEVTYPE[str_detect(string=new2$COMBEVTYPE, pattern="COLD")] <- "COLD"
new2$COMBEVTYPE[str_detect(string=new2$COMBEVTYPE, pattern="TEMP")] <- "COLD"
new2$COMBEVTYPE[str_detect(string=new2$COMBEVTYPE, pattern="FIRE")] <- "FIRE"
new2$COMBEVTYPE[str_detect(string=new2$COMBEVTYPE, pattern="BLIZ")] <- "BLIZZARD"
new2$COMBEVTYPE[str_detect(string=new2$COMBEVTYPE, pattern="SLEET")] <- "SLEET"
new2$COMBEVTYPE[str_detect(string=new2$COMBEVTYPE, pattern="WHIR")] <- "WHIRLWND"
new2$COMBEVTYPE[str_detect(string=new2$COMBEVTYPE, pattern="WIND")] <- "WIND"

# 6. Eliminate variables that I won't use in order to keep the data set smaller
new2 <- new2[names(new2) %in% c("STATE", "COMBEVTYPE", "FATALITIES", "INJURIES", "NEWPROPDMG", "NEWCROPDMG", "YEAR")]
rm(new1)
# 7. create a data frame of state abbreviations and names in order to create maps
states_map <- map_data("state")
stabb <- unique(as.character(new2$STATE))
StateName <- data.frame(STATE=stabb, region=tolower(state.name[match(stabb, state.abb)]))

#8. What event type has caused the most damage over time?
#cropdmg
cropd <- aggregate(new2$NEWCROPDMG, list(new2$COMBEVTYPE), sum)
cropd <- head(arrange(cropd, desc(x)))
totcropdg <- ggplot(cropd, aes(x=Group.1, y=x)) + geom_bar(stat='identity') + labs(x='Weather Event', y='Total Crop Damage', title= 'Crop Damage \n per Weather event')
#propdmg
propd <- aggregate(new2$NEWPROPDMG, list(new2$COMBEVTYPE), sum)
propd <- head(arrange(propd, desc(x)))
propdg <- ggplot(propd, aes(x=Group.1, y=x)) + geom_bar(stat='identity') + labs(x='Weather Event', y='Total Property Damage', title= 'Property Damage \n per Weather event')
#propdmg
#fatalities
fat <- aggregate(new2$FATALITIES, list(new2$COMBEVTYPE), sum)
fat <- head(arrange(fat, desc(x)))
fatg <- ggplot(fat, aes(x=Group.1, y=x)) + geom_bar(stat='identity') + labs(x='Weather Event', y='Total Fatalities Caused', title= 'Fatalities \n per Weather event')
#injuries
inj <- aggregate(new2$INJURIES, list(new2$COMBEVTYPE), sum)
inj <- head(arrange(inj, desc(x)))
injg <- ggplot(inj, aes(x=Group.1, y=x)) + geom_bar(stat='identity') + labs(x='Weather Event', y='Total Injuries Caused', title= 'Injuries \n per Weather event')
grid.arrange(totcropdg, fatg, propdg, injg, nrow=2, ncol=2)
#or using ggplot and facets
colnames(cropd) <- c("Event.Type", "Damage")
cropd$Damage.Type <- rep("Crop Damage", 6)
colnames(propd) <- c("Event.Type", "Damage")
propd$Damage.Type <- rep("Property Damage", 6)
colnames(fat) <- c("Event.Type", "Damage")
fat$Damage.Type <- rep("Fatalities", 6)
colnames(inj) <- c("Event.Type", "Damage")
inj$Damage.Type <- rep("Injuries", 6)
top6dmg <-rbind(cropd, propd,fat, inj) 
top6dmgg <- ggplot(top6dmg, aes(x=Event.Type, y=Damage, color=Event.Type)) + facet_wrap(~ Damage.Type)


# 7. explore maxes by reordering in order to easily see the top 10 of each category
#byfat <- arrange(new2, desc(FATALITIES))
#byinj <- arrange(new2, desc(INJURIES))
#bycropdmg <- arrange(new2, desc(NEWCROPDMG))
#bypropdmg <- arrange(new2, desc(NEWPROPDMG))

# 8. explore by state, year and type of harm
new2$STATE <- as.factor(new2$STATE)
statesum <- as.data.frame(table(new2$STATE))
byeventtotal <- arrange(statesum, desc(Freq))

# yearly number of events recorded increases drastically in 1993
evperyear <- as.data.frame(table(new2$YEAR))
yreveg <- ggplot(evperyear, aes(x=Var1, y=Freq)) +geom_bar(stat='identity')
yreveg91 <- ggplot(evperyear[42:62,], aes(x=Var1, y=Freq)) +geom_bar(stat='identity')
mean(evperyear$Freq[1:43]) # 636.65
mean(evperyear$Freq[44:62]) #11960.89

# yearly crop damage disaggregated by state
cropsub <- new2[new2$NEWCROPDMG > 0,]
yrstcrop <- with(cropsub, aggregate(NEWCROPDMG, list(YEAR, STATE), sum))
colnames(yrstcrop) <- c("YEAR", "STATE", "TotalCropDMG")
yrstcroporder <- yrstcrop[order(yrstcrop[,3], decreasing=TRUE),] #the highest for one state in one year

#total damage broken down by year and event type
## crop damage
yearlycropdmg2 <- with(cropsub, aggregate(NEWCROPDMG, list(YEAR, COMBEVTYPE), sum))
colnames(yearlycropdmg2) <- c("YEAR", "COMBEVTYPE", "TotalCropDMG")
cropg2 <- ggplot(yearlycropdmg2, aes(x=YEAR, y=TotalCropDMG, fill=COMBEVTYPE)) + geom_bar(stat='identity') + geom_text(aes(label=yearlycropdmg2$YEAR), size=3) + theme(axis.ticks.x = element_blank(), axis.text.x=element_blank())
##property damage
propsub <- new2[new2$NEWCROPDMG > 0,]
yearlypropdmg <- with(propsub, aggregate(NEWPROPDMG, list(YEAR, COMBEVTYPE), sum))
colnames(yearlypropdmg) <- c("YEAR", "COMBEVTYPE", "TotalPropDMG")
yrlpropg <- ggplot(yearlypropdmg, aes(x=YEAR, y=TotalPropDMG, fill=COMBEVTYPE)) + geom_bar(stat='identity') 
# taking 2006 out by setting a y limit gives us a better look at the distribution of events and property damage:
yrlypropg2 <- ggplot(yearlypropdmg, aes(x=YEAR, y=TotalPropDMG, fill=COMBEVTYPE)) + geom_bar(stat='identity') + ylim(0, 30000000000) + geom_text(aes(label=YEAR), vjust=-0.5, size=3) + theme(axis.ticks.x = element_blank(), axis.text.x=element_blank())
# fatalities
fatsub <- new2[new2$FATALITIES > 0,]
yearlyfat <- with(fatsub, aggregate(FATALITIES, list(YEAR, COMBEVTYPE), sum))
colnames(yearlyfat) <- c("YEAR", "COMBEVTYPE", "FATALITIES")
yrlyfatg <- ggplot(yearlyfat, aes(x=YEAR, y=FATALITIES, fill=COMBEVTYPE)) + geom_bar(stat='identity') 
yrlyfatgzoom <- ggplot(yearlyfat[yearlyfat$YEAR>=1993,], aes(x=YEAR, y=FATALITIES, fill=COMBEVTYPE)) + geom_bar(stat='identity') 
#injuries
injsub <- new2[new2$INJURIES > 0,]
yearlyinj <- with(injsub, aggregate(INJURIES, list(YEAR, COMBEVTYPE), sum))
colnames(yearlyinj) <- c("YEAR", "COMBEVTYPE", "INJURIES")
yrlyinjg <- ggplot(yearlyinj, aes(x=YEAR, y=INJURIES, fill=COMBEVTYPE)) + geom_bar(stat='identity') 
yrlyinjgzoom <- ggplot(yearlyinj[yearlyinj$YEAR>1993,], aes(x=YEAR, y=INJURIES, fill=COMBEVTYPE)) + geom_bar(stat='identity') 


yrlytotbyevent <- ddply(cropsub, 'YEAR', function(z){
    y <-with(z, aggregate(NEWCROPDMG, list(COMBEVTYPE), sum))
    y <- arrange(y, desc(x))
    y[1:2,]
}
)
#crop damage by event
cropbyevent <- with(cropsub, aggregate(NEWCROPDMG, list(COMBEVTYPE), sum))
colnames(cropbyevent) <- c("COMBEVTYPE", "TotalCROPDMG")
cropbyeventord <- arrange(cropbyevent, desc(TotalCROPDMG))
cropbyeventg <- ggplot(cropbyevent, aes(x=COMBEVTYPE, y=TotalCROPDMG)) + geom_bar(stat='identity')
cropbyeventgzoom <- ggplot(cropbyeventord[1:10,], aes(x=COMBEVTYPE, y=TotalCROPDMG)) + geom_bar(stat='identity')
#mean yearly crop damage by event
yrlymeanbyevent <- ddply(cropsub, 'YEAR', function(z){
    y <-with(z, aggregate(NEWCROPDMG, list(COMBEVTYPE), mean))
    y <- arrange(y, desc(x))
    y[1,]
}
)

#yearly property damage by state
propsub <- new2[new2$NEWPROPDMG > 0,]
yrstprop <- with(propsub, aggregate(NEWPROPDMG, list(YEAR, STATE), sum))
#yrstprop2 <- merge(yrstprop, StateName, x.all=TRUE)
colnames(yrstprop) <- c("YEAR", "STATE", "TotalPropDMG")
yrstproporder <- yrstprop[order(yrstprop[,3], decreasing=TRUE),]
### 1. flood, CA, 2006, 2. Katrina (storm surge), LA, 2005, 3. MS 2005, 4 FL04, 5. FL05
#total yearly property damage
yearlypropdmg <- with(propsub, aggregate(NEWPROPDMG, list(YEAR), sum))
colnames(yearlypropdmg) <- c("YEAR", "TotalPropDMG")
propg <- ggplot(yearlypropdmg, aes(x=YEAR, y=TotalPropDMG)) + geom_bar(stat='identity')
# in 1993 property damage begins to increase the max being 2006 and second being 2005
propgzoom <- ggplot(yearlypropdmg[42:62,], aes(x=YEAR, y=TotalPropDMG)) + geom_bar(stat='identity')
yrlymean1 <- mean(yearlypropdmg$TotalPropDMG[1:43])
yrlymean2 <- mean(yearlypropdmg$TotalPropDMG[56:57])
yrlymean3 <- mean(yearlypropdmg$TotalPropDMG[c(43:55, 58:62)])
propdmg05 <- propsub[propsub$YEAR == 2005,]
propdmgperevt05 <- with(propdmg05, aggregate(NEWPROPDMG, list(COMBEVTYPE), sum)) 
propdmgperevt05 <- arrange(propdmgperevt05, desc(x))
#propdmg06 <- propsub[propsub$YEAR == 2006,]
#propdmgperevt06 <- with(propdmg06, aggregate(NEWPROPDMG, list(COMBEVTYPE), sum)) 
#propdmgperevt06 <- arrange(propdmgperevt06, desc(x))
#propdmg06stna <- merge(propdmg06, StateName, x.all=TRUE, y.all=TRUE)
#propdmg06map <- merge(states_map, propdmg06stna)
#propdmg06map <- arrange(propdmg06map, group, order)
#mappropdmg <- ggplot(propdmg06map, aes(x=long, y=lat, group=group, fill=TotalPropDMG)) + geom_polygon(color='black') + coord_map('polyconic')
#yearly fatalities
fatsub <- new2[new2$FATALITIES > 0,]
yrstfat <- with(fatsub, aggregate(FATALITIES, list(YEAR, STATE), sum))
colnames(yrstfat) <- c("YEAR", "STATE", "TotalFATAL")
yrstfatorder <- yrstfat[order(yrstfat[,3], decreasing=TRUE),] ### 1. IL, 1995, 2. AL, 2011, 3. PA, 1995
fatg <- ggplot(yrstfat, aes(x=YEAR, y=TotalFATAL)) + geom_bar(stat='identity') 
##more fatalities more consistenly since 1993
yearlyfat <- with(yrstfat, aggregate(TotalFATAL, list(YEAR), sum))
colnames(yearlyfat) <- c("YEAR", "TotalFATAL")
fatg91 <- ggplot(yearlyfat[42:62,], aes(x=YEAR, y=TotalFATAL)) + geom_bar(stat='identity') 
fat500 <- yearlyfat[yearlyfat$TotalFATAL > 500,] 
#8 years have seen more than 500 fatalities, 1995 being the most at 1491,
# and 2011 coming in second at 1002

#yearly injuries
injsub <- new2[new2$INJURIES >0,]
yrstinj <- with(injsub, aggregate(INJURIES, list(YEAR, STATE), sum))
colnames(yrstinj) <- c('YEAR', 'STATE', 'TotalINJ')
yrstinjorder <- arrange(yrstinj, desc(TotalINJ))
yearlyinj <- with(yrstinj, aggregate(TotalINJ, list(YEAR), sum))
colnames(yearlyinj) <- c("YEAR", "TotalINJ")
injg <- ggplot(yearlyinj, aes(x=YEAR, y=TotalINJ)) + geom_bar(stat='identity') ## similar to FATALITIES
inj3000 <- yearlyinj[yearlyinj$TotalINJ > 3000,] #12 years, 11177 in 1998

## Which state has the most entries?
statetotals <- as.data.frame(table(stormdmg$STATE))
stateorder <- arrange(statetotals, desc(Freq))
g <- ggplot(stateorder, aes(x=Var1, y=Freq, label=rownames(stateorder))) + 
    geom_bar(stat='identity') + geom_text(aes(label=Var1), vjust=-0.2, size=3)


