hd <- "~" #update with filepath of "home" directory
fp <- "Data" # update with location of the 6 csv files used in this class: country.csv, drugtreatment.csv, empdat.csv, instructorevals.csv, lowbwt.csv, states.csv

### READS IN ALL DATA AND FUNCTIONS FOR LABS AND CLASS ASSIGNMENTS

# function to get mode:
getmode <- function(x) {
	z <- table(x)
	names(z)[z == max(z)]
}

# function to make tidier tables
freqtable <- function(x) {
	zf <- as.data.frame(table(x))
	zp <- as.data.frame(prop.table(table(x)))
	z <- cbind(zf, zp[2])
	z$x <- factor(z$x, levels = c(levels(z$x), "N", "NA / missing", "Total"))
	colnames(z)[1] <- deparse(substitute(x))
	colnames(z)[3] <- "ValidProp"
	sumf <- sum(z[2])
	sump <- sum(z[3])
	z[nrow(z)+1, 2] <- sumf
	z[nrow(z), 1] <- "N"
	z[nrow(z), 3] <- sump
	z[nrow(z)+1, 1] <- "NA / missing"
	z[nrow(z), 2] <- sum(is.na(x))
	z$Proportion <- z$Freq / (sumf + sum(is.na(x)))
	z[nrow(z)+1, 1] <- "Total"
	z[nrow(z), 2] <- sumf + sum(is.na(x))
	z[nrow(z), 4] <- 1
	print.data.frame(z, row.names=FALSE)
}

setwd(hd)
setwd(fp)

################## COUNTRY #############################################################

country <- read.csv("country.csv")

# clean country names, encoding problems for special characters
country$country <- gsub("\xca", " ", country$country)
country$country <- gsub("\xfc\xbe\x8e\x93\xa4\xbc", "o", country$country)
country$country <- gsub("\xfc\xbe\x99\x93\xa0\xbc", "e", country$country)

# remove case variable
country$case <- NULL

################## STATES #############################################################

states <- read.csv("states.csv")

################## LOWBWT #############################################################

lowbwt <- read.csv("lowbwt.csv")

lowbwt$LOW <- (lowbwt$LOW == 1)
lowbwt$RACE <- factor(lowbwt$RACE, labels = c("white", "black", "other"))
lowbwt$SMOKE <- (lowbwt$SMOKE == 1)
lowbwt$HT <- (lowbwt$HT == 1)
lowbwt$UI <- (lowbwt$UI == 1)

# prop.table(table(lowbwt$SMOKE2, lowbwt$SMOKE))

################## INSTRUCTOREVALS #################################################

instructorevals <- read.csv("instructorevals.csv")

################## DRUGTREATMENT ###################################################

drugtreatment <- read.csv("drugtreatment.csv")

### FIXES ###

drugtreatment$DFREE <- (drugtreatment$DFREE == 1)
drugtreatment$IVHX <- factor(drugtreatment$IVHX, labels = c("never", "previous", "recent"))
drugtreatment$RACE <- factor(drugtreatment$RACE, labels = c("white", "other"))
drugtreatment$TREAT <- factor(drugtreatment$TREAT, labels = c("Short", "Long"))
drugtreatment$SITE <- factor(drugtreatment$SITE, labels = c("A", "B"))

################## EMPDAT ##########################################################

empdat <- read.csv("empdat.csv")

### FIX DATE (convert from factor to date; correct two-year date problem) ###

# convert factor to date class
empdat$dob.datex <- as.Date(empdat$dob, format = "%m/%d/%y")

# set all dates to 20th century
empdat$dob.date <- as.Date(ifelse(empdat$dob.datex > Sys.Date(), 
  format(empdat$dob.datex, "19%y-%m-%d"), 
  format(empdat$dob.datex)))

# delete raw and interim date variables 
empdat$dob.datex <-NULL
empdat$dob <-NULL

################## END #############################################################

#check naming conventions

#names <- c(colnames(country), colnames(states), colnames(lowbwt), colnames(empdat), colnames(drugtreatment), colnames(instructorevals))
#namestab <- table(names)
#namestab

####################################################################################

METADATA <- read.csv("metadata.csv")
metadataRachelData <- read.csv("metadataRachelData.csv")
METADATA <- rbind (METADATA, metadataRachelData)

setwd(hd)
rm(hd)
rm(fp)
