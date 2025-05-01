getwd()

setwd("C:\\Users\\MSI\\Desktop\\Lab 04-20250305")

getwd()

#import the dataset 
#csv file import 
data<-read.table("DATA 4.txt",header = TRUE,sep="")
data

fix(data)

#rename the columns
names(data)<-c("Team","Attendance","Salary","Years")
data

fix(data)

# Attendance
boxplot(team$Attendance, main = "Boxplot - Attendance")
hist(team$Attendance, main = "Histogram - Attendance")
stem(team$Attendance)

# Salary
boxplot(team$Salary, main = "Boxplot - Salary")
hist(team$Salary, main = "Histogram - Salary")
stem(team$Salary)

# Years
boxplot(team$Years, main = "Boxplot - Years")
hist(team$Years, main = "Histogram - Years")
stem(team$Years)


#q2
boxplot(Attendance,main="Boxplot for Attendance",outline=TRUE,xlab="Attendance",horizontal=TRUE)

stem(Years)


#q3 summary status

summary_stats <- function(x) {
  cat("Mean:", mean(x), "\n")
  cat("Median:", median(x), "\n")
  cat("Standard Deviation:", sd(x), "\n\n")
}

summary_stats(team$Attendance)
summary_stats(team$Salary)
summary_stats(team$Years)

quantile(team$Attendance, probs = c(0.25, 0.75))
quantile(team$Salary, probs = c(0.25, 0.75))
quantile(team$Years, probs = c(0.25, 0.75))


IQR(team$Attendance)
IQR(team$Salary)
IQR(team$Years)

find_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Check mode for "Years"
find_mode(team$Years)

find_outliers <- function(x) {
  Q1 <- quantile(x, 0.25)
  Q3 <- quantile(x, 0.75)
  IQR_val <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR_val
  upper_bound <- Q3 + 1.5 * IQR_val
  outliers <- x[x < lower_bound | x > upper_bound]
  return(outliers)
}



# Check for each variable
find_outliers(team$Attendance)
find_outliers(team$Salary)
find_outliers(team$Years)


#quartile
quantile(Attendance)
IQR(Attendance)