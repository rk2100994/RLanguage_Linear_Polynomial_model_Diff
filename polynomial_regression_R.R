#Polynomial Regression
getwd()

#Importing datasest
dataset = read.csv("Position_Salaries.csv")
dataset = dataset[2:3]
dataset

#Fitting Linear Regression to the dataset
lin_reg = lm(formula = Salary ~., data = dataset)
summary(lin_reg)

#Fitting Polynomial Linear Regression to the dataset
dataset$Level2 = dataset$Level^2
dataset$Level2 = dataset$Level^3
dataset$Level4 = dataset$Level^4
poly_reg = lm(formula = Salary ~., data = dataset)
summary(poly_reg)

#Visualizing Linear Regression Results
install.packages('ggplot2')
library(ggplot2)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour ='red')+
  geom_line(aes(x = dataset$Level, y  = predict(lin_reg, newdata = dataset)),
            colour = 'blue') +
  ggtitle('Linear Regressor') +
  xlab('Level') +
  ylab('Salary')

#Visualizing Polynomial Regression Results
library(ggplot2)
x_grid = seq(min(dataset$Level), max(dataset$Level),0.1)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour ='red')+
  geom_line(aes(x = dataset$Level, y  = predict(poly_reg, newdata = dataset)),
            colour = 'blue') +
  ggtitle('Polynomial Regressor') +
  xlab('Level') +
  ylab('Salary')


#Predict for linear regressor
y_pred = predict(lin_reg,data.frame(Level = 6.5))


#Predict for linear regressor
y_pred = predict(poly_reg,data.frame(Level = 6.5,
                                     Level2 = 6.5^2,
                                     Level3 = 6.5^3,
                                     Level4 = 6.5^4))
