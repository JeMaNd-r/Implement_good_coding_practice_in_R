# Good coding practice 
# Bad example    

setwd("C:/Users/idivrz90seqi/Documents/Projects/Coding Club/Implement_good_coding_practice_in_R")

library(tidyverse)
library(ggplot2)
library(psych)

data(mtcars)
head(mtcars)

# plot histogram
hist(mtcars$cyl)

# influence of cyl on mpg, disp, hp, vs and gear
lm1 <- lm( mpg ~ cyl, data = mtcars )
plot(mtcars)
str(mtcars$cyl)
summary(lm1)
lm2 <- lm( disp ~ cyl, data = mtcars )
summary(lm2)
lm3 <- lm( hp ~ cyl, data = mtcars )
summary(lm3)
lm4 <- lm( vs ~ cyl, data = mtcars )
summary(lm4)
lm1.1 <- lm( gear ~ cyl, data = mtcars )
summary(lm1.1)

# plot x and y with geom_smooth function
ggplot(data=mtcars,aes(x=cyl, y=mpg))+geom_point()+geom_smooth()
ggplot(data=mtcars,
       aes(x=cyl, y=disp))+
  geom_point()+
  geom_smooth()
ggplot(data=mtcars,    aes(x=cyl, y=hp))+   geom_point()+
  geom_smooth()
ggplot(mtcars,
       aes(x=cyl, y=vs))+   geom_point()+   geom_smooth()
ggplot(data=mtcars,aes(x=cyl, y=gear))+
  geom_point()+geom_smooth()


