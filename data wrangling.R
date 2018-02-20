#libraries
library(readr)
library(dplyr)
library(tidyr)

FUL <- read_csv("Federal_Upper_Limits_-_2017_12.csv")
View(FUL)

#rename columns to make them easier to use
names(FUL)[names(FUL) == "ACA FUL"] <- "Full"
names(FUL)[names(FUL) == "Weighted Average AMPs"] <- "AMPs"

#Add column for Price Paid by Customer (Paid)
FUL <- mutate(FUL, Paid = Full - AMPs)

#keep only one row of each ingredient
FUL <- FUL %>%
  group_by(Ingredient) %>%
  summarise(min(Paid))

#medications
#diuretics: Hyrdochlorothiazide
#beta blockers: Acebutolol and Atenolol
#ACE inhibitors: Lisinopril, Benazepril, Captopril
#ARBs: Candesartan, Losartan
#calcium channel blockers: Amlodipine and Diltiazem

#Add column for Drug Type
FUL$Type <- NA

#Fill in column with approprite Drug Type
FUL[which(FUL$Ingredient == "HYDROCHLOROTHIAZIDE"), "Type"] <- "diuretic"
FUL[which(FUL$Ingredient == "ACEBUTOLOL HYDROCHLORIDE"), "Type"] <- "beta blocker"
FUL[which(FUL$Ingredient == "ATENOLOL"), "Type"] <- "beta blocker"
FUL[which(FUL$Ingredient == "LISINOPRIL"), "Type"] <- "ACE inhibitor"
FUL[which(FUL$Ingredient == "BENAZEPRIL HYDROCHLORIDE"), "Type"] <- "ACE inhibitor"
FUL[which(FUL$Ingredient == "CAPTOPRIL"), "Type"] <- "ACE inhibitor"
FUL[which(FUL$Ingredient == "CANDESARTAN CILEXETIL"), "Type"] <- "ARB"
FUL[which(FUL$Ingredient == "LOSARTAN POTASSIUM"), "Type"] <- "ARB"
FUL[which(FUL$Ingredient == "AMLODIPINE BESYLATE"), "Type"] <- "CCB"
FUL[which(FUL$Ingredient == "DILTIAZEM HYDROCHLORIDE"), "Type"] <- "CCB"

#remove all other rows
types <- c("diuretic", "beta blocker", "ACE inhibitor", "ARB", "CCB")
FUL <- filter(FUL, Type %in% types)

#create file
write.csv(FUL, "coverage_clean.csv")



#Notes
#I was having a little bit of trouble with trying to get the string to contain the string rather than matching the exact string.
#I tried writing it as %ATENOLOL% or trying the contains function but kept getting syntax errors.
#I was able to successfully use the filter function but figured it was easier to enter in the medications once and then use the !is.na function for all rows missing values in the Type column.
#However, I kept getting syntax errors when using is.na, so I just made a vector of the types.
#Basically, the data looks the way I want it to look, but the code is still kind of tedious.




#unused mess down here--disregard
#create function to check for each medication
findmed <- function(medication, ...) {
  filter(Federal_Upper_Limits_2017_12, Ingredient == medication)
}

filter(Federal_Upper_Limits_2017_12, Ingredient == "HYDROCHLOROTHIAZIDE")
       , Ingredient == "Acebutolol", Ingredient == "Lisinopril", Ingredient == "Benazepril", Ingredient == "Captopril", Ingredient == "Candesartan", Ingredient == "Losartan", Ingredient == "Amlodipine", Ingredient == "Diltiazem")

#filter table to only conntain relevant hypertension medications
Federal_Upper_Limits_2017_12 <- findmed("Hydrochlorothiazide", "Acebutolol", "Atenolol", "Lisinopril", "Benazepril", "Captopril", "Candesartan", "Losartan", "Amlodipine", "Diltiazem")

#get rid of unneeded up columns
#Month and Year are not needed, because they are all from 12/17
FUL <- select(FUL, -Year, -Month)

#filter table to only include hypertension medications
medication <- c("HYDROCHLOROTHIAZIDE", "ACEBUTOLOL", "ATENOLOL", "LISINOPRIL", "BENAZEPRIL", "CAPTOPRIL", "CANDESARTAN", "LOSARTAN", "AMLODIPINE", "DILTIAZEM")
FUL <- FUL %>%
  filter(Ingredient %in%
           medication)