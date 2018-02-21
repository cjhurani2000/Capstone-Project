#libraries
library(readr)
library(dplyr)
library(tidyr)

FUL <- read_csv("Federal_Upper_Limits_-_2017_12.csv")
View(FUL)

#The year and month are the same for all, and we don't care about the NDC. Delete those.
FUL <- select(FUL, -NDC, -Year, -Month)

#rename some columns to make it easier to delete those too
names(FUL)[names(FUL) == "Package Size"] <- "Size"
names(FUL)[names(FUL) == "Product Group"] <- "Group"
names(FUL)[names(FUL) == "Multiplier Greater Than 175 Percent of Weighted Avg of AMPs"] <- "Delete2"
names(FUL)[names(FUL) == "A-Rated"] <- "Rating"
names(FUL)[names(FUL) == "MDR Unit Type"] <- "Unit"

FUL <- select(FUL, -Size, -Group, -Delete2, -Rating, -Route, -Unit)

#keep the distinct rows from here
FUL <- distinct(FUL)

#rename columns to make them easier to use
names(FUL)[names(FUL) == "ACA FUL"] <- "Full"
names(FUL)[names(FUL) == "Weighted Average AMPs"] <- "AMPs"

#Add column for Price Paid by Customer (Paid)
FUL <- mutate(FUL, Paid = Full - AMPs)

#What is needed is the price per miligram. Isolate the strength and go from there
FUL <- separate(FUL, 3, c("Strength", "Delete"), sep = "MG")
FUL <- select(FUL, -Delete)
FUL <- mutate(FUL, PPMG = Paid/as.numeric(Strength))

#keep one row of each ingredient
FUL <- FUL %>%
  group_by(Ingredient) %>%
  mutate(min(PPMG))

names(FUL)[names(FUL) == "min(PPMG)"] <- "min_PPMG"
FUL <- filter(FUL, PPMG == min_PPMG)


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

#pure/not pure
FUL$Pure <- "N"
pure <- c("CAPTOPRIL", "LISINOPRIL", "ATENOLOL", "HYDROCHLOROTHIAZIDE")
FUL[which(FUL$Ingredient %in% pure), "Pure"] <- "Y"

#create file
write.csv(FUL, "coverage_clean.csv")