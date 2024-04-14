#This R Project looks at statistics related to Premier League footballers shooting, in it I look to showcase descriptive statistics, data cleaning, inferential statistics and Supervised Machine Learning.
#First we import the data
data <- read.csv("C:/Users/jammy/OneDrive/Desktop/player_shooting_2023_2024.csv")
#Now to make sure that it sent over I will look for the first few rows
head(data)
#To check the structure of the data
str(data)
#To get a summary of the data
summary(data)
#Data Cleaning
#Get rid of duplicates
data <- data[!duplicated(data), ]
#Check for null values
any(is.na(data))
#Get rid of null values
cleaned_data <- data[complete.cases(data), ]
#double check
any(is.na(cleaned_data))
#check for outliers
boxplot(cleaned_data)
#doesn't work as there's non-numeric data i.e player names
#get the numeric data
numeric_cleaned_data <- cleaned_data[, sapply(cleaned_data, is.numeric)]
#Try again
boxplot(numeric_cleaned_data)
#nothing looks out of the ordinary, let's implement IQR just to be sure
Q1 <- quantile(numeric_cleaned_data$variable, 0.25)
Q3 <- quantile(numeric_cleaned_data$variable, 0.75)
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
outliers <- numeric_cleaned_data$variable[numeric_cleaned_data$variable < lower_bound | numeric_cleaned_data$variable > upper_bound]
outliers
#there are not outliers, but it's still good practice to check - now the data is ready for some descriptive analysis
#before we do anything else lets remove the X column as it is obsolete
cleaned_data <- cleaned_data[, -1]
cleaned_data
#it may make sense to also drop the next column, as the rank has discrepensies due to the removal of null values
cleaned_data <- cleaned_data[, -2]
cleaned_data
#now for some descriptive statistics - lets look at the numeric values again
summary_stats <- summary(numeric_cleaned_data)
print(summary_stats)
#now we have a basic overview of the descriptive statistics for each column
#i want to see use a hypothesis test to see if its true that forwards get the most goals using the anova test, or does every position get simialr amount of goals
library(stats)
anova_result <- aov(Gls ~ Pos, data=cleaned_data)
print(summary(anova_result))
#the p value is very small, and less that 0.05, so we can reject the hypothesis that there isn't a difference in the amount of goals scored by each position
#now we will use linear regression to see how age, shots taken and distance covered are related to the amount of goals scored
linearmodel <- lm(Gls ~ Age + Sh + Dist, data = cleaned_data)
summary(linearmodel)
#from here we look at the p values and compare them to the interval p = 0.05
#we can see that goals scored isnt necesarilly related to age as its p value is 0.9853
#as expected, shots taken had a significant positive impact on goals scored, with a p value of <2e-16***. With a coefficent of .1441, this means for every shot taken theres an expected increase of goals by .1441
#similarly, distance covered had a significant negativeimpact on goals scored, with a p value of 8.09e-10. For every unit of distance covered, this led to a .113 decrease in goals.
#Now let's try and graph some things - goals firstly, and then expected goals (xG)
barplot(cleaned_data$Gls, names.arg = cleaned_data$Player, las = 2, xlab = "Player", ylab = "Goals Scored", main = "Goals Scored by Player")
#while this does showcase what I want, its a bit jumbled up, so it might be an idea to focus on the top 20 players.
sorted_data <- cleaned_data[order(cleaned_data$Gls, decreasing = TRUE), ]
top_twenty <- head(sorted_data, 20)
barplot(top_twenty$Gls, names.arg = top_twenty$Player, las = 2, xlab = "Player", ylab = "Goals Scored", main = "Top Twenty Scorers")
#now lets compare their xG with their goals to see who is the most prolific
index <- seq_along(top_twenty$Player)
plot(index, top_twenty$Gls, type = "o", col = "blue", xlab = "Player", ylab = "Goals/Expected Goals", main = "Goals vs Expected Goals for Top Twenty Scorers", xaxt = "n")
lines(index, top_twenty$xG, type = "o", col = "red")
axis(1, at = index, labels = top_twenty$Player, las = 2)
legend("topright", legend = c("Goals", "Expected Goals (xG)"), col = c("blue", "red"), lty = 1, cex = 0.8)
#Let's finish up with some Machine Learning
#I want to use position classification to try and predict a players position by stats like goals, shots taken, expected goals
#To do this I will implement a random trees classifier
#First we must combine all these and make Position a factor
data <- cleaned_data[, c("Sh", "Gls", "xG", "Pos")]
data$Pos <- as.factor(data$Pos)
#we then split data into training and test sets
set.seed(123)
train_index <- sample(1:nrow(data), 0.8 * nrow(data))
test_index <- setdiff(1:nrow(data), train_index)
X_train <- data[train_index, -ncol(data)]  
X_test <- data[test_index, -ncol(data)]    
y_train <- data[train_index, "Pos"]        
y_test <- data[test_index, "Pos"]
#we then train the model and make predictions
install.packages("randomForest")
library(randomForest)
model <- randomForest(x = X_train, y = y_train, ntree = 100)
predictions <- predict(model, X_test)
#Then we check how accurate model is 
accuracy <- sum(predictions == y_test) / length(predictions)
print(accuracy)
#the model has an accuracy of 29.1% - there is room for improvement but it is more accurate than randomly guessing