#===============================================================
#====================Experiment-1(CRD)==========================
#===============================================================


# p-value < 0.05 → Significant (Treatment golor moddhy parthikko ache) (Reject H0)
# p-value ≥ 0.05 → insignificant (Accept H0)

#install.packages("gmodels")
#install.packages("agricolae")
library(gmodels)
library(agricolae)

Feed_1 <- c(179, 186, 200, 182, 187, 197)
Feed_2 <- c(169, 188, 179, 189, 186, 168, 182)
Feed_3 <- c(182, 191, 243, 214, 213, 202, 180, 214)
Feed_4 <- c(210,220,226,245,241,225)
Feed_5 <- c(210, 214, 220, 192, 188, 189, 190, 224)
Feed_6 <- c(155, 176, 159, 161, 165)


group <- c(Feed_1, Feed_2, Feed_3, Feed_4, Feed_5, Feed_6)

levels <- factor(rep(paste0("Feed_", 1:6), c(6, 7, 8, 6, 8, 5)))

data <- list(score=group, Treatment=levels)
names(data)

# (a) Analyze the data and test the significance of the difference.

model <- aov(score ~ Treatment, data=data)
summary(model)


#(b) Test the hypothesis that the mean weights of chickens given diets 3 and 5 are equivalent.

#(c) Test the hypothesis that there is a significant difference between the mean weights of chickens fed diets 3 and 6.

con <- matrix(c(0,0,1,0,0,-1,
                0,0,1,0,-1,0), nrow = 2, byrow = TRUE)

rownames(con) <- c("Feed_3 VS Feed_6", "Feed_3 VS Feed_5")
con

fit.contrast(model, "Treatment", con,conf.int = 0.95)
# Feed 3 vs. Feed 5:p-value>0.05 (insignificant).So We fail to reject the null hypothesis.
#The average weight difference is only 1.5 units, and the p-value is very high (0.83). This means the mean weights of chickens given diets 3 and 5 are statistically equivalent

# 
# Feed 3 vs. Feed 6:p-value<0.001 (Highly Significant). So We reject the null hypothesis. 
#Chickens on Diet 3 weighed, on average, 41.68 units more than those on Diet 6. This means the mean weights of chickens given diets 3 and 6 are statistically significant.


#(d) Test among the difference between all pairs.

# This compares the means of all groups within the "Treatment" factor
LSD_Test <- LSD.test(model, "Treatment",)
LSD_Test$groups

# jodi same alphabet(akta) hoy tahole poroshpor insignificant.
# jodi different alphabet(akta) hoy tahole poroshpor significant

# jodi different alphabet(doita) hoy tahole poroshpor insignificant.kinto bakider shathe significant







#===============================================================
#====================Experiment-2(RBD)==========================
#===============================================================

# H0:There is no significant difference between the block means.
# H1:At least one block mean differs  significantly.

# H0:All the diet are equal
# H1:At least two 2ui's are unequal

# (a)	Test whether there is a significant difference between the treatments.



y1 <- c(33.5, 33, 32, 30, 28.5)
y2 <- c(38.9, 37.8, 37, 36, 34.8)
y3 <- c(40.9, 39.6, 39, 38.3, 37.5)
y4 <- c(41.9, 40.4, 39.9, 39.5, 38.7)
y5 <- c(41.9, 40.5, 39.8, 39.2, 38.8)
y6 <- c(42.8, 41.9, 40.7, 39.8, 39.4)

y <- c(y1, y2, y3, y4, y5, y6)
 
Treatment <- factor(rep(LETTERS[1:6], each = 5))
#[1:6] = Number of treatment ( A,B,...,F)
# each = 5 :- number of blocks (Block1,Block2, ..., Block5)


Block <- factor(rep(1:5, times = 6))
# times = 6  :- number of treatment
# 1:5  :- number of blocks

Data <- data.frame(y, Treatment, Block)

mod.rbd <- aov(y ~ Treatment + Block, data = Data)
summary(mod.rbd)



#(b)	Conduct a hypothesis to see if there is a statistically significant difference between treatments D and E. 

#(c)	Test the hypothesis that whether the treatments A and B are significantly different. 

con <- matrix(c(0, 0, 0, 1, -1, 0,
                1, -1, 0, 0, 0, 0),nrow = 2, byrow = TRUE)

rownames(con) <- c("Treatment_D vs Treatment_E", "Treatment_A vs Treatment_B")
con

fit.contrast(mod.rbd, "Treatment", con,conf.int=0.95)

# D vs E= insignificant
# A vs B= highly significant


#(d)	Compare each pair of means using Duncan's test and Fisher's LSD test.

duncan_test <- duncan.test(mod.rbd, "Treatment",console = TRUE)
duncan_test$groups

# DNMRT <- duncan.test(Data$y, Data$Treatment,
#                      DFerror = 20,
#                      MSerror = 0.2)
# DNMRT$groups 

LSD_Test <- LSD.test(mod.rbd, "Treatment",console=TRUE)
LSD_Test$groups

# (e)	Test whether there is a significant difference between the blocks.
mod.rbd <- aov(y ~ Block + Treatment, data = Data)
summary(mod.rbd)

#Conclusion:There is a significant difference between the blocks.

# (f)	Comment on whether blocking is necessary in this experiment.


# Blocking is used in an RBD to reduce experimental error caused by variability among blocks.

# Since the ANOVA test showed that the block effect is significant, it means that the blocks differ from each other and contribute to variability in the response variable.

# Conclusion: Blocking is necessary and effective in this experiment because it successfully accounts for variation among blocks and helps improve the precision of treatment comparisons.
#===============================================================
#====================Experiment-3(LSD) ==============================
#===============================================================

#(i) Perform analysis of variance of data and test the significance of diet effects.


Row <- factor(rep(paste0("cow", 1:4), each = 4))
Column <- factor(rep(paste0("period", 1:4), 4))
Treatment <- factor(paste0("diet", c(1,2,3,4,
                                     2,4,1,3,
                                     3,1,4,2,
                                     4,3,2,1)))

# amon o hote pare
# Treatment<-factor(paste0("fertiliser",(c("A","B","C","D","E",
#                                          "B","C","D","E","A",
#                                          "C","D","E","A","B",
#                                          "D","E","A","B","C",
#                                          "E","A","B","C","D"))))
yields<-c(192, 190, 214, 221,
          195, 203, 139, 152,
          292, 218, 245, 204,
          249, 210, 163, 134)
 
data<-data.frame(yields, Row, Column, Treatment)
names(data)

myfit<-aov(yields ~ Row+Column+Treatment,data=data)

#General model summary
summary(myfit)
# Analysis of Variance Table
fit<-anova(myfit)
fit

#(ii) Which diet would you prefer?
out <- LSD.test(model,"Treatment")
out$groups

#Diet 4 is significantly different from either of the diets 1 and 2. Diet 3 is also significantly different from either of the diets 1 and 2. Diets 3 and 4, and diets 1 and 2 are not significantly different. Die produces the highest yield.



#===============================================================
#====================Experiment-4 ==============================
#===============================================================

set.seed(125)

pimc <- function(a, b, g, n){    # a = lower and b = upper limits, g = integral function
  x <- runif(n, a, b)            # n = replicate
  k <- g(x)
  c <- max(k)
  y <- runif(n, 0, c)
  vec <- rep(0, n)
  for(i in 1:n){
    if(y[i] > k[i]){
      vec[i] <- 0
    }
    else{ vec[i] <- 1 }
  }
  nH <- sum(vec)
  pimc <- c*(b-a)*nH/n
  pimc
}

g <- function(x){(sin(pi*x))^2}
pimc(0, 1, g, 10000)
# [1] 0.4967      # Result will slightly vary for different seeds


# Actual result
#   sin^2(π x) = (1 - cos(2π x)) / 2
#   (1/2 * ∫ from 0 to 1 of 1 dx) minus (1/2 * ∫ from 0 to 1 of cos(2π x) dx)
#   [x / 2 minus sin(2π x) / (4π)] evaluated from 0 to 1
#0.5-0=0.5

#===============================================================================
#============== Experiment-5 / Linear Congruential Method ======================
#===============================================================================

ran<-function(start,a,c,m,n){ 
  # start= seed
  # a = multiplier
  # c= increment
  # m = modulus
  # n= size of sequence
  vec<-c()
  for( i in 1:n){ 
    z<-(a*start+c)%%m 
    start<-z 
    vec<-c(vec,z) 
  } 
  list("The sequence of random numbers: "=vec,
       "The sequence of random numbers between 0 and 1"=vec/m) 
}
ran(start =52 , a = 21, c = 53, m = 100, n = 5)

#===============================================================
#====================Experiment-6 ==============================
#===============================================================

rand <- function(m, a, seed, n){   # m and a are vectors of length k
  k <- length(m)
  start <- rep(seed, k)
  vec <- c()
  for(i in 1:n){
    z <- rep(0, k)
    sum <- 0
    for(j in 1:k){
      z[j] <- (a[j] * start[j]) %% m[j]
      start[j] <- z[j]
      sum <- sum + ((-1)^(j-1)) * z[j]
    }
    x <- sum %% (m[1]-1)
    if(x>0)
      R <- (x + m[1]) else (m[1]-1)/m[1]
    vec <- c(vec, R)
  }
  vec
}

m <- c(2147483563, 2147483399)
a <- c(40014, 40692)
seed <- 73
n <- 10

rand(m, a, seed, n)





#================================= Lab Mid ==============================
#==============================================================================
y1 <- c(24.8,27.4,38.5,28.6)
y2 <- c(20.7,28.9,39.6,32.1)
y3 <- c(27.8,22.4,36.9,40)
y4 <- c(16.3,15.1,19.7,14.2)
y5 <- c(16.3,17.1,15.5,17.8)
y6 <- c(24.5,22.6,26.4,22.7)

y <- c(y1, y2, y3, y4, y5, y6)

Treatment <- factor(rep(LETTERS[1:6], each = 4))
Block <- factor(rep(1:4, times = 6))

Data <- data.frame(y, Treatment, Block)

mod.rbd <- aov(y ~ Treatment + Block, data = Data)
summary(mod.rbd)


#================================= Mid squared method ==============================
#==============================================================================
Midsq<- function(seed, n) {
  k <- nchar(as.integer(seed))
  temp <- seed
  
  i <- 0:n
  Zi <- numeric(n + 1)
  Ri <- numeric(n + 1)
  Zi_sq <- numeric(n + 1)
  
  Zi[1] <- temp
  Ri[1] <- c("-")  
  Zi_sq[1] <- temp^2
  
  for(j in 1:n) {
    squared <- temp^2
    squared_str <- sprintf(paste0("%0", 2 * k, "d"), squared)
    start_pos <- floor(k / 2) + 1
    temp <- as.integer(substr(squared_str, start_pos, start_pos + k - 1))
    
    Zi[j + 1] <- temp
    Ri[j + 1] <- temp / 10^k
    Zi_sq[j + 1] <- temp^2
  }
  return(data.frame(i, Zi, Ri, Zi_sq))
}

Midsq(9876, 5)



#================================= Binomial Random Variable Generation ==============================
#==============================================================================

n <- 6
p <- 0.5

pmf <- dbinom(0:n, size = n, prob = p)
cdf <- cumsum(pmf)
lower_bounds <- c(0, cdf[-length(cdf)])
sample_size <- n+1

rand_nums <- c("5075", "3068", "4269", "6931", "0851", "2254", "2745", "2723", "6158", "4297")

selected_rand_nums <- rand_nums[1:sample_size]
U <- as.numeric(selected_rand_nums) / 10000

X <- findInterval(U, lower_bounds) - 1

results <- data.frame(
  i       = 0:n,
  LVal    = round(lower_bounds, 3), 
  UVal    = round(cdf, 3),
  RandNum = selected_rand_nums,
  U       = U,
  X       = X
)
results

#The genetated random sample of size 5 from X~binomial(6,0.5) is {3,2,3,4,1} 




#================================================================================
#================================================================================
#================================================================================
#================================================================================
#================================================================================




#===============================================================================
#======================= Control Variate Approach ==================================
#===============================================================================
set.seed(123)
m <- 1000000
u <- runif(m)

#functions
f <- function(u) exp(-0.5)/(1+u^2)
g <- function(u) exp(-u)/(1+u^2)

# f <- function(u) exp(0.5 * u)
# g <- function(u) exp(u)

A <- f(u)
B <- g(u)

a <- -cov(f(u),g(u)) / var(f(u))

mu<- integrate(f, 0, 1)$value

T1 <- g(u)
T2 <- B + a * (f(u) - mu)


c(mean(T1), mean(T2))

(var(T1) - var(T2)) / var(T1)

#===============================================================================
#======================= Control Variates and Regression ==================================
#===============================================================================

set.seed(123)
m<-1000000
u<-runif(m)
f<-function(u)exp(-0.5)/(1+u^2)
g<-function(u)exp(-u)/(1+u^2)

mu<-integrate(f,0,1)$value
L<-lm(g(u)~f(u))

t.hat<-sum(L$coeff*c(1,mu))
t.hat

cat("Area under the curve g(u):",t.hat,
    "\nVariance:",summary(L)$sigma^2,
    "\nThe amount of variance it deduced:",summary(L)$r.squared*100,"%")







#===============================================================================
#======================= Exponential Distribution ==================================
#===============================================================================
n <- 1000
rate <- 2

rexpon <- function(size, lambda) {
  X <- rep(0, size)   
  
  for(i in 1:size) {
    U <- runif(1)                
    X[i] <- -(1 / lambda) * log(U)  
  }
  return(X)}
# eSamp <- rexp(n, rate)
eSamp <- rexpon(n, rate)

hist(eSamp, prob = TRUE, main = "Exponential Distribution")
lines(density(eSamp))

# 3. Mean and Variance Agreement
# Theoretical Mean = 1 / rate = 0.5
# Theoretical Variance = 1 / (rate^2) = 0.25
mean(eSamp)
var(eSamp)

# H0:mu = 0.5(The true population mean is equal to the theoretical mean)
t.test(eSamp, mu = 1 / rate)

#conclusion: Since the p-value > 0.05, the test is statistically insignificant. Therefore, we fail to reject the null hypothesis.thus, the true population mean is equal to the theoretical mean

#===============================================================================
#======================= Gamma Distribution ==================================
#===============================================================================
n <- 1000
alpha <- 2  # shape
beta <- 3   # rate

rgamma_custom <- function(size, shape, rate) {
  X <- rep(0, size)                 
  
  for(i in 1:size) {
    U <- runif(shape) 
    
    X[i] <- sum(-(1 / rate) * log(U))
  }
  return(X)}


gSamp <- rgamma_custom(n, shape = alpha, rate = beta)
# gSamp <- rgamma(n, shape = alpha, rate = beta)

hist(gSamp, prob = TRUE, main = "Gamma Distribution")
lines(density(gSamp))

# 3. Mean and Variance Agreement
# Theoretical Mean = shape / rate = 2 / 3 = 0.666
# Theoretical Variance = shape / (rate^2) = 2 / 9 = 0.222
mean(gSamp)
var(gSamp)

# H0:mu = 0.006(The true population mean is equal to the theoretical mean)
t.test(gSamp, mu = alpha / beta)

#conclusion: Since the p-value > 0.05, the test is statistically insignificant. Therefore, we fail to reject the null hypothesis.thus, the true population mean is equal to the theoretical mean



#===============================================================================
#======================= Normal Distribution ==================================
#===============================================================================
n <- 10000
mu <- 10
sigma <- 2

rnorm_custom <- function(size, mean , sd) {
  X <- rep(0, size) 
  
  for(i in 1:size) {
    U1 <- runif(1)      
    U2 <- runif(1)                  
    
    Z <- sqrt(-2 * log(U1)) * cos(2 * pi * U2)
    X[i] <- mean + (sd * Z)
  }
  
  return(X)}

nSamp <- rnorm_custom(n, mean = mu, sd = sigma)
# nSamp <- rnorm(n, mean = mu, sd = sigma)

hist(nSamp, prob = TRUE, main = "Normal Distribution")
lines(density(nSamp))

# 3. Mean and Variance Agreement
# Theoretical Mean = mu = 10
# Theoretical Variance = sigma^2 = 4
mean(nSamp)
var(nSamp)

# 4. Hypothesis Testing (H0: mu = 10)
t.test(nSamp, mu = mu)



#conclusion: Since the p-value > 0.05, the test is statistically insignificant. Therefore, we fail to reject the null hypothesis.thus, the true population mean is equal to the theoretical mean


#===============================================================================
#======================= Uniform Distribution ==================================
#===============================================================================
n <- 10000

runif_custom <- function(size, min = 0, max = 1) {
  X <- rep(0, size)
  for(i in 1:size) {
    U <- runif(1)
    
    X[i] <- min + (max - min) * U
  }
  
  return(X)}

uSamp <- runif_custom (n)
# uSamp <- runif(n)

hist(uSamp, prob = TRUE, main = "Uniform Distribution")
lines(density(uSamp))

# 3. Mean and Variance Agreement
# Theoretical Mean = (min + max) / 2 = 0.5
# Theoretical Variance = (max - min)^2 / 12 = 0.0833
mean(uSamp)
var(uSamp)

# 4. Hypothesis Testing about the Mean (H0: mu = 0.5)
t.test(uSamp, mu = 0.5)


#conclusion: Since the p-value > 0.05, the test is statistically insignificant. Therefore, we fail to reject the null hypothesis.thus, the true population mean is equal to the theoretical mean



#===============================================================================
#======================= Binomial Distribution ==================================
#===============================================================================
n <- 10000
trials <- 10
p <- 0.5

rbinom_custom <- function(size, trials, prob) {
  X <- rep(0, size) 
  
  for(i in 1:size) {
    successes <- 0
    for(t in 1:trials) {
      U <- runif(1)                 
      if(U <= prob) {
        successes <- successes + 1  
      }
    }
    
    X[i] <- successes 
  }
  
  return(X)}

bSamp <- rbinom_custom(n, trials= trials, prob = p)
# bSamp <- rbinom(n, size = trials, prob = p)

hist(bSamp, prob = TRUE, main = "Binomial Distribution")
lines(density(bSamp))

# 3. Mean and Variance Agreement
# Theoretical Mean = trials * p = 10 * 0.5 = 5
# Theoretical Variance = trials * p * (1 - p) = 10 * 0.5 * 0.5 = 2.5
mean(bSamp)
var(bSamp)

# 4. Hypothesis Testing about the Mean (H0: mu = 5)
t.test(bSamp, mu = trials * p)


#conclusion: Since the p-value > 0.05, the test is statistically insignificant. Therefore, we fail to reject the null hypothesis.thus, the true population mean is equal to the theoretical mean



#===============================================================================
#======================= Poison Distribution ===================================
#===============================================================================

n <- 10000
lambda <- 2

rpois_custom <- function(size, lambda) {
  X <- rep(0, size) 
  L <- exp(-lambda)
  
  for(i in 1:size) {
    k <- 0
    p <- 1
    
    while(p > L) {
      k <- k + 1
      U <- runif(1)
      p <- p * U
    }
    
    X[i] <- k - 1
  }
  
  return(X)}

pSamp <- rpois_custom(n, lambda)
# pSamp <- rpois(n, lambda)

hist(pSamp, prob = TRUE, main = "Poisson Distribution")
lines(density(pSamp))

# 3. Mean and Variance Agreement
# Theoretical Mean = lambda =2
# Theoretical Variance = lambda = 2
mean(pSamp)
var(pSamp)


t.test(pSamp, mu=lambda)


#conclusion: Since the p-value > 0.05, the test is statistically insignificant. Therefore, we fail to reject the null hypothesis.thus, the true population mean is equal to the theoretical mean


#===============================================================================
#======================= Geomatric Distribution ==================================
#===============================================================================
n <- 10000
p <- 0.3

rgeom_custom <- function(size, prob) {
  X <- rep(0, size) 
  
  for(i in 1:size) {
    failures <- 0
    
    while(TRUE) {
      U <- runif(1)
      if (U <= prob) {
        break 
      } else {
        failures <- failures + 1
      }}
    
    X[i] <- failures
  }
  return(X)}

geomSamp <- rgeom_custom(n, prob = p)
# geomSamp <- rgeom(n, prob = p)

hist(geomSamp, prob = TRUE, main = "Geometric Distribution")
lines(density(geomSamp))

# 3. Mean and Variance Agreement
# Theoretical Mean = (1 - p) / p = 0.7 / 0.3 = 2.333
# Theoretical Variance = (1 - p) / (p^2) = 0.7 / 0.09 = 7.777
mean(geomSamp)
var(geomSamp)

# 4. Hypothesis Testing about the Mean (H0: mu = 2.333)
t.test(geomSamp, mu = (1 - p) / p)



#conclusion: Since the p-value > 0.05, the test is statistically insignificant. Therefore, we fail to reject the null hypothesis.thus, the true population mean is equal to the theoretical mean