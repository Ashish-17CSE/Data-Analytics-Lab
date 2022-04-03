#install.packages("gapminder")
library(gapminder)
data("gapminder")
plot(gapminder)
View(gapminder)

#install.packages("dplyr")
library(dplyr)

## ------------One-Sample T-testing-------------##
mean(gapminder$lifeExp)
t.test(gapminder$lifeExp, mu = 5, alternative = "less", conf.level = 0.99)
  
## -------------Tow-Sample T-test---------------##
View(gapminder)
  df1 <- gapminder %>%
    select(country,lifeExp)%>%
    filter(country == "South Africa" | country == "Ireland")
  t.test(data = df1, lifeExp ~ country)

##  -------------Paired t-test------------------##
  t.test(gapminder$lifeExp,gapminder$pop,paired = TRUE, alternative = "two.sided")

## --------------One Sample z-test--------------##
  library(gapminder)
  x <- rnorm(gapminder)
  z.test(x,sigma.x = 2)
  
## -------------Two-Sample Z-Test---------------##
  x <- rnorm(gapminder)
  y <- rnorm(gapminder)
  z.test(x,sigma.x = 0.5, y, sigma.y = 0.5, mu=2)
  z.test(x, sigma.x=0.5, y, sigma.y=0.5, conf.level=0.90)

  
###############----------F-test----------------------#
  #* Method 1
  x <- gapminder$year
  y <- gapminder$lifeExp
  var.test(x, y, alternative = "two.sided")
 
  #* or Method 2
   var.test(lifeExp ~ country, data = df1, alternative = "two.sided")
   
  #* or Method 3
  res.ftest <- var.test(lifeExp ~ country, data = df1)
  res.ftest
  
  
  chisq.test(data_frame$treatment, data_frame$improvement, correct=FALSE)
  
###-------------Chi-squared test---------------------#
  data_frame <- read.csv("https://goo.gl/j6lRXD")  #Reading CSV
  table(data_frame$treatment, data_frame$improvement)
  chisq.test(data_frame$treatment, data_frame$improvement, correct=FALSE)
  data("mtcars")
  table(mtcars$carb, mtcars$cyl)
  chisq.test(mtcars$carb, mtcars$cyl)