#read in data
install.packages("car")
library(car)
data(Prestige)
help(Prestige)

#Question 1
#a.Delete NA of type 
Prestige <- na.omit(Prestige)
#Creat a new dummy variable by coding the variable type
Prestige$professional <- ifelse(Prestige$type == "prof",1,0)

#b.Creat a dummy for progression
#run a linear model with prestige as an outcome
model_b <- lm(prestige~income*professional,data = Prestige)
summary(model_b)


#Question 2
#a.Ho:There is no effect of yard signs in a precinct on vote share
#  H1:There is effect of yard signs in a precinct on vote share
#Calculate t-statistics and p-value
TS_1 <- 0.042/0.016

#df_1=n-k-1=131-2-1=128
df_1 <-128
p_value1 <- 2*pt(abs(TS_1),df_1,lower.tail = F)
cat("The p-value of hypothsis is:",round(p_value1,4))

#Ho:There is no effect of being next to precints with these yard signs on vote share
#H1:There is effect of being next to precints with these yard signs on vote share
TS_2 <- 0.042/0.013

#df_1=n-k-1=131-2-1=128
df_2 <-128
p_value2 <- 2*pt(abs(TS_2),df_2,lower.tail = F)
cat("The p-value of hypothsis is:",round(p_value2,4))
