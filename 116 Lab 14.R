# Name: Faye Bandet
# Date: 12/5/19
# ISTA 116 Section B || Section Leader : Jacob Heller
# Lab Assignment 14
# Collaborator(s): Nick Ackerman

download.file("http://www.openintro.org/stat/data/mlb11.RData", destfile = "mlb11.RData")
load("mlb11.RData")

#1
plot(mlb11$runs, y = mlb11$at_bats)
# The correlation looks at least moderately linear. The distribution isn't very strong of a linear relationship/association compared to stronger statistics. 

#2
cor(mlb11$at_bats, mlb11$runs)
# The correlation coefficient is 0.610627, as shown in the previous graph. Correlation coefficients closest to 1 are the strongest.

#3
plot_ss(x = mlb11$at_bats, y = mlb11$runs)
#Test 1
#Coefficients:
#(Intercept)       x  
#-2923.3517       0.6522  
#Sum of Squares:  130263.9

#Test 2
#Coefficients:
#  (Intercept)      x  
# -24690.615        4.667  
#Sum of Squares:  7750302

#Test 3
#Coefficients:
# (Intercept)      x  
# -1259.5623       0.3504  
#Sum of Squares:  147708.4

# The lowest Sum of Squares I got was 130263.9.

#4
model1 <-lm(mlb11$runs~mlb11$at_bats, mlb11)
model1
#Coefficients:
#(Intercept)  mlb11$at_bats  
# -2789.2429         0.6305 
# This regression line is different than the one I did by hand because it is more accurate than me guesing points.

#5
summary(model1)
# I reused model1 which is runs by at_bats, and checked the correlation between them which is correlated and explained  because there is a potential run each time a player is up to bat. The at_bats estimate is 0.6305, which is fairly close to 1 so it shows strong correlation.

#6
plot(model1)
summary(model1)
# I used the summary function to check the residuals (66.47 on 28 degrees of freedom), constant variability, and plot to see the linearity.

#7
model2 <- lm(mlb11$runs~mlb11$homeruns, mlb11)
model2
# created model of the regression line, (415.239 + 1.835x)

#8
plot(model2)
summary(model2)
#I think that homeruns is a better prediction of runs than at_bats because the data has stronger correlation. We should interpret the slope of at_bats as positive because it goes up the abline on the Q-Q plot, and homeruns as positive for each regression line. The R^2 value of homeruns(0.6132 vs. 0.3505) is nearly two times closer to 1 so its stronger. P-value is 1.9e-07.

#9
model3 <- lm(mlb11$runs~mlb11$hits, mlb11)
model4 <- lm(mlb11$runs~mlb11$bat_avg, mlb11)
model5 <- lm(mlb11$runs~mlb11$strikeouts, mlb11)
model6 <- lm(mlb11$runs~mlb11$stolen_bases, mlb11)
model7 <- lm(mlb11$runs~mlb11$wins, mlb11)
summary(model3)
summary(model4)
summary(model5)
summary(model6)
summary(model7)
# The best predictor of runs is bat_avg because it has the strongest correlation, checked the Q-Q, and lowest p-value (5.877e-08). This makes sense because players who actually hit the ball can run.

#10
model8 <- lm(runs ~ at_bats + hits + homeruns + bat_avg + strikeouts + stolen_bases + wins, data = mlb11)
summary(model8)
#This model explains more of the variance than the single variable regression models I trained in the previous steps.Individual tests will be more accurate to varience than an overall test. I think this is because it performed multiple tests at once, and compared all the variables, which results in a higher degree of overall varience. This is because there were more arguments to test in this regression function.
