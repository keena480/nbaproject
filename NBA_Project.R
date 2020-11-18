library(corrplot)
library(MASS)
library(dplyr)

nba_ori <- read.csv('nba_new.csv')
nba <- nba_ori[c(1:21)]

#Exclude Rookie from the data
nba <- subset(nba, Rookie.Scale. == 0)
nba <- nba[-19]

#Correlation Graph
nba_cor <- cor(nba[3:20], use="pairwise", method="spearman")
res1 <- cor.mtest(nba_cor, conf.level = .95)
corrplot(nba_cor, p.mat = res1$p, method="square", 
         insig = "label_sig", pch.col = "white", # Add * to statistical significant one
         tl.col = "blue", tl.srt = 90, # Text label color and rotation
         type = "upper", diag = FALSE)  # hide correlation coefficient on the principal diagonal

#Linear Regression with all variables
salary_all <- lm(Salary~.-Max-Player.Name-Player.ID,data=nba)
summary(salary_all)

#Variables selection
stepAIC(salary_all, direction="both")

#Linear Regression with essential variables
salary_essential <- lm(formula = Salary ~ REB + VORP + eFG + OR. + TO. + Usage + Age, data = nba)
summary(salary_essential)

#Prediction
salary_pred <- predict(salary_essential, newdata = nba)

#Value Check Function
value_determination <- function(playerdata){
  salary_pred <- predict(salary_essential, newdata = playerdata)
  if (playerdata$VORP >= mean(nba$VORP) && playerdata$Salary > salary_pred) {print("Expensive Talent")}
  else if(playerdata$VORP >= mean(nba$VORP) && playerdata$Salary <= salary_pred) {print("Good Investment")}
  else if(playerdata$VORP < mean(nba$VORP) && playerdata$Salary > salary_pred) {print("Over Price")}
  else if(playerdata$VORP < mean(nba$VORP) && playerdata$Salary <= salary_pred) {print("Bench Warmer")}
}

#Test Function
playerdata <- subset(nba, nba$Player.Name=="LeBron James", select = c(1:20))
value_determination(playerdata)

salary_pred <- predict(salary_essential, newdata = playerdata)
print(c(playerdata$Salary, salary_pred))
mean(nba$VORP)


# ###Useless
# dataset = dataset[c(3,9,13)]
# # install.packages('rpart')
# library(rpart)
# regressor = rpart(formula = Salary ~ PER,
#                   data = dataset,
#                   method="class",
#                   control = rpart.control(minsplit = 1))
# # Fitting Random Forest Regression to the dataset
# # install.packages('randomForest')
# library(randomForest)
# set.seed(1234)
# regressor = randomForest(x = dataset[2],
#                          y = dataset$Salary,
#                          ntree = 5)
# 
# # Predicting a new result with Decision Tree Regression
# y_pred = predict(regressor, data.frame(PER = 5))
# 
# 
# # Visualising the Decision Tree Regression results (higher resolution)
# # install.packages('ggplot2')
# library(ggplot2)
# x_grid = seq(min(dataset$PER), max(dataset$PER), 0.05)
# ggplot() +
#   geom_point(aes(x = dataset$PER, y = dataset$Salary),
#              colour = 'red') +
#   geom_line(aes(x = x_grid, y = predict(regressor, newdata = data.frame(PER=x_grid))),
#             colour = 'blue') +
#   ggtitle('Random Forest Regression)') +
#   xlab('VORP') +
#   ylab('Salary')
# 
# # Plotting the tree
# plot(regressor)
# text(regressor)
# 
# cforest(Salary ~ .,data = dataset, controls=cforest_control(mtry=2, mincriterion=0))
# getTree(regressor,1,labelVar=TRUE)
# regressor = ctree(formula = Salary ~ PER,
#                   data = dataset)
# 
# plot(regressor, type="simple")
