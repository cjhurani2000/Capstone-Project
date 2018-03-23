## PART 1: ADDING A NEW VARIABLE

#load libraries
library(readr)
library(dplyr)
library(ggplot2)

#load dataset
coverage_clean <- read_csv("coverage_clean.csv")
View(coverage_clean)

#create new table with the mean PPMG
freq_cov <- coverage_clean %>%
  group_by(Type) %>%
  summarize(mean(PPMG))

#add column for frequency
freq_cov$Frequency <- NA

#fill in values
#diuretic = 0.1990
#ARB = 0.3357
#ACEI= 0.1679
#BB = 0.1363
#CCB = 0.1191
frequencies <- c(0.1679, 0.3357, 0.1363, 0.1191, 0.1990)
freq_cov$Frequency <- frequencies

#rename column so it is easier to use
names(freq_cov)[names(freq_cov) == "mean(PPMG)"] <- "Mean_PPMG"

#create file
write.csv(freq_cov, "coverage_summary.csv")
       
##########

## PART TWO: MACHINE LEARNING

freq_cov <- read_csv("coverage_summary.csv")
View(coverage_summary)

#summary and correlation of the mean price and the frequency of prescription
sub.mean.freq <- subset(freq_cov, select = c("Mean_PPMG", "Frequency"))
summary(sub.mean.freq)
cor(sub.mean.freq)

#scatterplot
plot(sub.mean.freq)

#linear regression model
freq.mod <- lm(Frequency ~ Mean_PPMG, data=freq_cov)

#summarize results
summary(freq.mod)

#results show * next to both the intercept and the Mean_PPMG, so they are both significant.
#multiple R-squared is 0.8156 and adjusted R-squared is 0.7541, so there is a moderately strong positive correlation between the price of the drug and the frequency it is prescribed.
#p-value is 0.03568

anova(freq.mod)
#The * next to the Mean_PPMG shows that the correlation is significant.

#find coefficients for linear regression line
coef(summary(freq.mod))

# resulting line y = 13.34x + 0.14
