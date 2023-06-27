View(airline)
require(lolcat)
require(tidyr)
# Percentage of people that paid less than $930
u = mean(airline$Price)
s = sd(airline$Price)
ans1 = pnorm(q = 930, mean = u, sd = s) * 100
ans1

#For the data associated with Number of Cancelled Flights per Day per 500:
#If I took 20 flights during the period of time represented by these data, 
#what is the probability that I had at least 2 flights canceled?

c = airline$Number_Cancelled_per_Day_per_500[0:450]
u = mean(c)
u
p = u/500
p
q = 1 - p
q
ans2 = dbinom(x = 18 , size = 20 , prob = q)
1 - ans2

d = airline$Bag_Delivery_Time[0:75]
u = mean(d)


ans1 = pexp(q = 40, rate = 1/u, lower.tail = T)
ans1 * 100

p = 8/480
round(p,4)


#---------------------------------------------------------#
# QUIZ

ans1 = pbinom(q = 4 , size= 400, prob = 0.02 , lower.tail = T )
round(ans1,4)

ans2 = ppois(q = 50, lambda = 65, lower.tail = F)
round(ans2,4)

lower = pnorm(q = 153, mean = 153, sd = 10, lower.tail = T)
upper = pnorm(q = 164, mean = 153, sd = 10, lower.tail = F)
ans3 = lower + upper
round(ans3,4)


ans4 = pexp(q = 10000, rate = 1/40000, lower.tail = T)
round(ans4,4)




















