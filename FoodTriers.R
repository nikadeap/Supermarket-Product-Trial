
#Read the LasagnaTriers.csv file
setwd('C:/Users/Harish/Desktop/BDA/Assignment')

LasagnaTriers <- read.csv("Lasagna Triers.csv", header = TRUE)
library(ggplot2)
library(e1071)


print("*****************Question 5.a************************")

#Question 5.a
#Calculate Kurtosis and skew of variable person
kurtosis <- kurtosis(LasagnaTriers$Person)
print(kurtosis)
skew <- skewness(LasagnaTriers$Person)
print(skew)
print("As Person is a categorical variable, calculation skew and kurtosis is not meaningful")

print("*****************Question 5.d************************")
#This question is R-implementation of question 5.b and 5.c

#Create age buckets based on the age given in csv file

library(questionr)
#Create an Age Bucket using cut function

LasagnaTriers$Agebucket <-
  cut((LasagnaTriers$Age),
      c(20, 24, 29, 34, 39, 44, 49, 54, 59, 64),
      include.lowest = FALSE)


print(tapply(LasagnaTriers$Age, LasagnaTriers$Agebucket, FUN = length))


#Group data as per Age bucket and classify average of weight in terms of Lasgna Triers and non-Triers

Triers <- subset(LasagnaTriers, LasagnaTriers$Have.Tried == 'Yes')

NonTriers <- subset(LasagnaTriers, LasagnaTriers$Have.Tried == 'No')

#Count of Lasgna Triers and non-Triers as per Age bucket
AgeCount <- data.frame()
AgeCount <- with(LasagnaTriers, wtd.table(Agebucket, Have.Tried))
print("Count of Lasagna Triers and non-Triers within age buckets")
print(AgeCount)

#Average weights as per age buckets for Lasgna Triers and nonTriers
#Triers_wt < data.frame()
Triers_wt <-
  aggregate(Triers$Weight,
            by = list(Triers$Agebucket),
            FUN = mean)
colnames(Triers_wt) <- c('AgeBucket', 'LasgnaTriers-Weight')


nonTriers_wt <-
  aggregate(NonTriers$Weight,
            by = list(NonTriers$Agebucket),
            FUN = mean)
colnames(nonTriers_wt) <- c('AgeBucket', 'LasgnaNonTriers-Weight')

total_wt <- merge(Triers_wt, nonTriers_wt, by = "AgeBucket")
print("Classifying weight in categories Lasagna Triers and non-Triers as per Age bucket")
print(total_wt)

for (i in 1:length(total_wt$AgeBucket)) {
  if (total_wt$`LasgnaTriers-Weight`[i] > total_wt$`LasgnaNonTriers-Weight`[i]) {
    print("Average weight of Lasagna triers is greater than non-Triers")
  }
  else{
    print("Average weight of Lasagna non-triers is greater than Triers")
  }
}

print("Conclusion:As per the Trend shown in result we can accpet that Lasgna Triers tend to put on weight as they grow older")

#We can visually analyse the trends in Triers vs NonTriers in barplot
subset <-
  t(data.frame(
    total_wt$`LasgnaTriers-Weight`,
    total_wt$`LasgnaNonTriers-Weight`
  ))
barplot(
  subset,
  legend = c("Triers", "NonTriers"),
  names.arg = total_wt$AgeBucket,
  beside = TRUE,
  main = "comparison of weight in Lasagna triers vs Non-Triers"
)


# Visualizing realationship between Weight and Age
#ggplot to visualize relationship between weight and age
ggplot(data = LasagnaTriers) +
  geom_point(mapping = aes(x = Age, y = Weight), col = "red")

correlation <- cor(LasagnaTriers$Age, LasagnaTriers$Weight)

print(
  paste(
    "Correlation coefficient between Age and Weight is",
    round(correlation, digits = 2),
    "As the correlation coeffieicent can be rounded off to zero, Age and Weight share nonlinear relationship"
  )
)

#***********************************************************************************************************************
print("***************Question 5.e**********************************")
#Question5.e

set.seed(800)

par(mfrow = c(1, 2))
# Simulating a normal distribution
normvar <- rnorm(100,mean= mean(Triers$Weight,sd=sd(Triers$Weight)))
normvar_df <- data.frame(normvar)

#Draw histogram
hist(normvar,xlim=c(190,200),ylim=c(0,20))

#Draw a qq plot to show normal distribution of random variable 

qqnorm(normvar, col = "red") # QQ-plot
qqline(normvar, col = "blue", lwd = 3)

print("conclusion: based on QQ-plot, weights of the Lasagna Triers(Have.Tried=Yes) are normally distributed")


#Calculate the skewness of the Weight Variable
Skew <- skewness(Triers$Weight)
print(paste("Skewness of the variable weight is ", round(Skew, digits = 2)))
print(paste("positive value of skew implies a right skewed distribution"))

#Calculate the Kurtosos of the Weight Variable
Kurt <- kurtosis(Triers$Weight)
print(paste("Kurtosis of the variable weight is ", round(Kurt, digits =
                                                           2)))
print(
  paste(
    "negative value of kurtosis implies flatter distribution than the normal distribution i.e platykurtic"
  )
)
