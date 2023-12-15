
#Set the working directory (you will need to edit this to be the directory where you have stored the data files for this Chapter)
setwd("D:\\PC Files\\Documents")
imageDirectory<-"D:\\PC Files\\Documents"

setwd("D:\\PC Files\\Documents")
imageDirectory<-"D:\\PC Files\\Documents"

#Initiate packages
library(compute.es)
library(ez)
library(ggplot2)
library(multcomp)
library(nlme)
library(pastecs)
library(reshape)
library(WRS)
library(car)

# Read data and reshape it
dateData<-read.delim("LooksOrPersonality.dat", header = TRUE)

speedData<-melt(dateData, id = c("participant","gender"), measured = c("att_high", "av_high", "ug_high", "att_some", "av_some", "ug_some", "att_none", "av_none", "ug_none"))
names(speedData)<-c("participant", "gender", "groups", "dateRating")

# Define factors

speedData$personality<-gl(3, 60, labels = c("Charismatic", "Average", "Dullard"))
speedData$looks<-gl(3,20, 180, labels = c("Attractive", "Average", "Ugly"))

speedData<-speedData[order(speedData$participant),]

# Use Levene's test to check the equality of variances
levene_result <- leveneTest(dateRating ~ looks * gender, data = speedData)
print(levene_result)

#Enter data by hand

participant<-gl(20, 9, labels = c("P01", "P02", "P03", "P04", "P05", "P06", "P07", "P08", "P09", "P10", "P11", "P12", "P13", "P14", "P15", "P16", "P17", "P18", "P19", "P20" ))

participant<-gl(20, 9, labels = c(paste("P", 1:20, sep = "_")))


gender<-gl(2, 90, labels = c("Male", "Female"))
personality<-gl(3, 3, 180, labels = c("Charismatic", "Average", "Dullard"))
looks<-gl(3, 1, 180, labels = c("Attractive", "Average", "Ugly"))
dateRating<-c(86, 84, 67, 88, 69, 50, 97, 48, 47, 91, 83, 53, 83, 74, 48, 86, 50, 46, 89, 88, 48, 99, 70, 48, 90, 45, 48, 89, 69, 58, 86, 77, 40, 87, 47, 53, 80, 81, 57, 88, 71, 50, 82, 50, 45, 80, 84, 51, 96, 63, 42, 92, 48, 43, 89, 85, 61, 87, 79, 44, 86, 50, 45, 100, 94, 56, 86, 71, 54, 84, 54, 47, 90, 74, 54, 92, 71, 58, 78, 38, 45, 89, 86, 63, 80, 73, 49, 91, 48, 39, 89, 91, 93, 88, 65, 54, 55, 48, 52, 84, 90, 85, 95, 70, 60, 50, 44, 45, 99, 100, 89, 80, 79, 53, 51, 48, 44, 86, 89, 83, 86, 74, 58, 52, 48, 47, 89, 87, 80, 83, 74, 43, 58, 50, 48, 80, 81, 79, 86, 59, 47, 51, 47, 40, 82, 92, 85, 81, 66, 47, 50, 45, 47, 97, 69, 87, 95, 72, 51, 45, 48, 46, 95, 92, 90, 98, 64, 53, 54, 53, 45, 95, 93, 96, 79, 66, 46, 52, 39, 47)

speedData<-data.frame(participant, gender, personality, looks, dateRating)

#Examining the Main Effects

# Create and save boxplot for attractiveness, colored by personality, faceted by gender
dateBoxplot <- ggplot(speedData, aes(looks, dateRating, colour = personality))
dateBoxplot + geom_boxplot() + labs(x = "Attractiveness", y = "Mean Rating of Date", colour = "Charisma") + facet_wrap(~gender)
imageFile <- paste(imageDirectory,"Speed Date Boxplot.png",sep="/")
ggsave(file = imageFile)

# Create a bar plot for 'Attractiveness' (looks) and save the image
looksBar <- ggplot(speedData, aes(looks, dateRating))
looksBar + stat_summary(fun = mean, geom = "bar", fill = "White", colour = "Black") + stat_summary(fun.data = mean_cl_boot, geom = "pointrange") + labs(x = "Attractiveness", y = "Mean Rating of Date") 
imageFile <- paste(imageDirectory,"Speed Date Looks.png",sep="/")
ggsave(file = imageFile)


# Create a bar plot for 'Charisma' (personality) and save the image
charismaBar <- ggplot(speedData, aes(personality, dateRating))
charismaBar + stat_summary(fun = mean, geom = "bar", fill = "White", colour = "Black") + stat_summary(fun.data = mean_cl_boot, geom = "pointrange") + labs(x = "Charisma", y = "Mean Rating of Date") 
imageFile <- paste(imageDirectory,"Speed Date Charisma.png",sep="/")
ggsave(file = imageFile)

# Create a bar plot for 'Gender' and save the image
genderBar <- ggplot(speedData, aes(gender, dateRating))
genderBar + stat_summary(fun = mean, geom = "bar", fill = "White", colour = "Black") + stat_summary(fun.data = mean_cl_boot, geom = "pointrange") + labs(x = "Gender", y = "Mean Rating of Date") 
imageFile <- paste(imageDirectory,"Speed Date Gender.png",sep="/")
ggsave(file = imageFile)

# Create a plot for the interaction between 'Attractiveness' and 'Gender' and save the image
genderLooks <- ggplot(speedData, aes(looks, dateRating, colour = gender))
genderLooks + stat_summary(fun = mean, geom = "point") + stat_summary(fun = mean, geom = "line", aes(group= gender)) + stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + labs(x = "Attractiveness", y = "Mean Rating of Date", colour = "Gender") + scale_y_continuous(limits = c(0,100)) 
imageFile <- paste(imageDirectory,"looks gender.png",sep="/")
ggsave(file = imageFile)

# Create a plot for the interaction between 'Charisma' and 'Gender' and save the image
genderCharisma <- ggplot(speedData, aes(personality, dateRating, colour = gender))
genderCharisma + stat_summary(fun = mean, geom = "point") + stat_summary(fun = mean, geom = "line", aes(group= gender)) + stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + labs(x = "Charisma", y = "Mean Rating of Date", colour = "Gender") + scale_y_continuous(limits = c(0,100)) 
imageFile <- paste(imageDirectory,"personality gender.png",sep="/")
ggsave(file = imageFile)

# Create a plot for the interaction between 'Attractiveness' and 'Charisma' and save the image
looksCharisma <- ggplot(speedData, aes(looks, dateRating, colour = personality))
looksCharisma + stat_summary(fun = mean, geom = "point") + stat_summary(fun = mean, geom = "line", aes(group= personality)) + stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + labs(x = "Attractiveness", y = "Mean Rating of Date", colour = "Charisma") + scale_y_continuous(limits = c(0,100)) 
imageFile <- paste(imageDirectory,"personality  looks.png",sep="/")
ggsave(file = imageFile)

# Create a plot for the interaction between 'Attractiveness', 'Charisma', and 'Gender' and save the image
looksCharismaGender <- ggplot(speedData, aes(looks, dateRating, colour = personality))
looksCharismaGender + stat_summary(fun = mean, geom = "point") + stat_summary(fun = mean, geom = "line", aes(group= personality)) + stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + labs(x = "Attractiveness", y = "Mean Rating of Date", colour = "Charisma") + scale_y_continuous(limits = c(0,100)) + facet_wrap(~gender)
imageFile <- paste(imageDirectory,"three way interaction.png",sep="/")
ggsave(file = imageFile)

#using ezAnova
options(digits = 3)
# Descriptive statistics for different levels of 'looks'
by(speedData$dateRating, speedData$looks, stat.desc, basic = FALSE)

# Descriptive statistics for different levels of 'personality'
by(speedData$dateRating, speedData$personality, stat.desc, basic = FALSE)

# Descriptive statistics for different levels of 'gender'
by(speedData$dateRating, speedData$gender, stat.desc, basic = FALSE)

# Descriptive statistics for combinations of 'looks' and 'gender'
by(speedData$dateRating, list(speedData$looks, speedData$gender), stat.desc, basic = FALSE)

# Descriptive statistics for combinations of 'personality' and 'gender'
by(speedData$dateRating, list(speedData$personality, speedData$gender), stat.desc, basic = FALSE)

# Descriptive statistics for combinations of 'looks', 'personality', and 'gender'
by(speedData$dateRating, list(speedData$looks, speedData$personality, speedData$gender), stat.desc, basic = FALSE)
options(digits = 7)

# Create a boxplot of 'dateRating'
boxplot(speedData$dateRating, main = "Boxplot of dateRating", ylab = "dateRating")

# Identify potential outliers based on a threshold (e.g., 1.5 times the interquartile range)
Q1 <- quantile(speedData$dateRating, 0.25)
Q3 <- quantile(speedData$dateRating, 0.75)
IQR_value <- Q3 - Q1
lower_threshold <- Q1 - 1.5 * IQR_value
upper_threshold <- Q3 + 1.5 * IQR_value

# Identify outliers
outliers <- which(speedData$dateRating < lower_threshold | speedData$dateRating > upper_threshold)

# Display cases corresponding to identified outliers
outlierData <- speedData[outliers, c("participant", "looks", "personality", "gender", "dateRating")]

print(outlierData)

# Create a Q-Q plot of 'dateRating'
qqnorm(speedData$dateRating)
qqline(speedData$dateRating)

# Shapiro-Wilk test for normality
shapiro_test <- shapiro.test(speedData$dateRating)
print(shapiro_test)
#using ezAnova
# Define contrasts for ezANOVA: Some vs. None and High vs. Average
SomevsNone <- c(1, 1, -2)
HivsAv <- c(1, -1, 0)

# Apply contrasts to 'personality' and 'looks'
contrasts(speedData$personality) <- cbind(SomevsNone, HivsAv)
AttractivevsUgly <- c(1, 1, -2)
AttractvsAv <- c(1, -1, 0)
contrasts(speedData$looks) <- cbind(AttractivevsUgly, AttractvsAv)

# Set options for digit display
options(digits = 3)

# Run ezANOVA with specified factors and contrasts
speedModel <- ezANOVA(data = speedData, dv = .(dateRating), wid = .(participant),
                      between = .(gender), within = .(looks, personality),
                      type = 3, detailed = TRUE)

# Display the results of the ezANOVA
speedModel

# Reset options for digit display
options(digits = 7)
