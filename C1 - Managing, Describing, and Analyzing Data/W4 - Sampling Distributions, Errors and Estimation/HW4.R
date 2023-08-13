# Random Sampling Distribution Simulation
require(lolcat)
nqtr<-function(x,d){noquote(t(round.object(x, d)))}

# First, set seed so we will get the same results
set.seed(100)

# Next, create / simulate a random sampling distribution
n<-100
reps<-5000

samples <- replicate(reps, rexp(n, rate=5))
sampleavgs <- colMeans(samples)

# Calculate the descriptive statistics of the RSD
nqtr(summary.continuous(sampleavgs, stat.sd=T),5)
# 6
10 / sqrt(15)

# 7
x = 1.580
u = 1.575
n = 5
sd = 0.01

z = (x -u) / (sd / sqrt(n))
pnorm(z)

# 8
n = 5
x = 1.572
z = (x -u) / (sd / sqrt(n))

# 11
(xbar<-mean(preforms$weight))
(sd<-sd(preforms$weight))

# 12,13
ro<-round.object
nqtr<-function(x,d){noquote(t(round.object(x, d)))}
options(scipen=999)

ro(t.test.onesample.simple(sample.mean = xbar
                        ,sample.variance = sd^2
                        ,sample.size = 20
                        ,conf.level = 0.95),4)


# 15,16
ro(variance.test.onesample.simple(sample.variance = sd^2
                               ,sample.size = 20
                               ,conf.level = 0.90),4)


# 18

n = 500
m = 15
p = m/n
ci = 0.95
ro(proportion.test.onesample.exact.simple(sample.proportion = p
                                          ,sample.size = n
                                          , conf.level = 0.95),4)



#

ro(poisson.test.onesample.simple(sample.count = 250 * 2.58
                              ,sample.size = 250
                              ,conf.level = 0.90),4)

