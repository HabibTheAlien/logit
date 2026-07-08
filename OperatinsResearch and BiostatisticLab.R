# install.packages("lpSolve")
# install.packages("IBFS")
library(lpSolve)
library(IBFS)

# install.packages("survival")
# install.packages("survminer")
# install.packages("broom")
# install.packages("pROC")
# install.packages("tidyverse")
# install.packages("marginaleffects ")
# install.packages("car")
# install.packages("ResourceSelection")
library(survival)
library(survminer)
#===============================================================================
#============================= Graphical Method ==================================
#===============================================================================

x<-seq(0,400,1)
cotton <- (34000-100*x)/100
dacron <- (36000-200*x)/300
linen <- (32000-200*x)/200

plot(x,cotton,type="n",xlab="X1",ylab="X2")
lines(x,cotton,col="red")
lines(x,dacron,col="blue")
lines(x,linen,col="green")

#The corner poins (160,0),(120,40),(0,120)

df <- data.frame(x1=c(160,120,0),x2=c(0,40,120))
df$z <- 100*df$x1 + 120*df$x2


df[which.max(df$z),]


#===============================================================================
#============================= Simplex Method ==================================
#===============================================================================

obj<-c(50,70,60)
const<-matrix(c(
                -4,-5,-3,
                -6,-3,-5,
                -2,-3,-4), nrow=3, byrow=TRUE)
rhs<-c(-6,-9,-4)
#simplex method hole chinho always "<=" korte hobe "-" dara gun kore

dir<-c("<=","<=","<=")
sol<-lp(
        direction= "min",
        objective.in = obj,
        const.mat= const,
        const.dir= dir,
        const.rhs=rhs)
cat("The optimal values are:",
    "\nx1=",sol$solution[1], 
    "\nx2=",sol$solution[2],
    "\nx3=",sol$solution[3],
    "\nMinimum cost for the food mixture=",sol$objval)

#===============================================================================
#============================= Two-Phase Method ==================================
#===============================================================================
#s,e,a

# "<=" -- s
# "=" -- a
# ">=" -- -e,+a


#objective function e shudhu "a" er value 1 thakbe baki "x" abong "e" er value 0 hobe
obj1<-c(0,0,0,0,1,1)
const1<-matrix(c(
  2,4,-1,0,1,0,
  1,7,0,-1,0,1
),nrow=2,byrow=TRUE)

#Two-Phase method hole chinho always "=" 
dir1<-c("=","=")
rhs1<-c(4,7)

phase1<-lp(
  direction="min",
  objective.in =obj1,
  const.mat=const1,
  const.rhs= rhs1,
  const.dir= dir1)
cat("the optimal values are:
    \na1=", phase1$solution[5],
    "\na2=",phase1$solution[6],
    "\nThe minimize value of Z is=",phase1$objval)

# zodi Z,a er man "0" pawa jy tahole Phase-II te jabo . 

#Since the optimal value and all the artificial variables are zero in Phase-I, the original LP problem has a feasible solution.Now, we can proceed to Phase-II


# zodi Z,a er man "0" na peye onno positive value pawa jy tahole Phase-II te jabo na .

#Since the optimal value and all the artificial variables are not zero in Phase-I, the original LP problem has no feasible solution.

#phase-I er por "a" gula bad pore jabe objective and subjective function theke


obj2<-c(1,1,0,0)
const2<-matrix(c(
  2,4,-1,0,
  1,7,0,-1
),nrow=2,byrow=TRUE)

dir2<-c("=","=")
rhs2<-c(4,7)

phase2<-lp(
  direction="min",
  objective.in =obj2,
  const.mat=const2,
  const.rhs= rhs2,
  const.dir= dir2
)
cat("the optimal values are:
    \nx1=", phase2$solution[1],
    "\nx2=",phase2$solution[2],
    "\nThe minimize value of Z is=",phase2$objval)

#===============================================================================
#============================= Big-M Method ==================================
#===============================================================================
# s,e,a
# 
# "<=" -- s
# "=" -- a
# ">=" -- -e,a

M<-1e7
obj<-c( 5,3,0,0,M,M)
const<-matrix(c(
  2,4,1,0,0,0,
  2,2,0,0,1,0,
  5,2,0,-1,0,1
),nrow=3, byrow=TRUE)
dir<-c("=","=","=")
rhs<-c(12,10,10)

sol<-lp(
  objective.in=obj,
  direction="min",
  const.mat=const,
  const.dir=dir,
  const.rhs=rhs
  
)
cat("the optimal values are:
    \nx1=", sol$solution[1],
    "\nx2=",sol$solution[2],
    "\nThe minimize value of Z is=",sol$objval)

#===============================================================================
#============================= Transportation Problem ==================================
#===============================================================================

library(lpSolve)
cost <- matrix(c(4, 8, 8, 
                 16, 24, 16, 
                 8, 16, 24), nrow = 3, byrow=TRUE)
cost
# rsig: Row signs 
# shob somoy "<" hobe
rsig <- rep("<", 3)

# rrhs: Row right-hand side
rrhs <- c(76, 82, 77)

# csig: Column signs
# shob somoy ">" hobe
csig <- rep(">", 3)

# crhs: Column right-hand side
crhs <- c(72, 102, 41)

sol <- lp.transport(
  cost.mat=cost, 
  direction="min", 
  row.sign=rsig,
  row.rhs= rrhs, 
  col.sign=csig,
  col.rhs= crhs)
sol
sol$solution

#===============================================================================
#============================= Gomory cutting plane method ==================================
#===============================================================================
obj <- c(1, 1)
const <- matrix(c(3, 2,  
                  0, 1), nrow = 2, byrow = TRUE) 
dir <- c("<=", "<=") 
rhs <- c(5, 2) 

sol <- lp(
  direction = "max", 
  objective.in = obj,
  const.mat = const, 
  const.dir = dir, 
  const.rhs = rhs
) 

x <- sol$solution
cat("Initial LP Solution: x1 =", x[1], ", x2 =", x[2])

int_sol <- lp("max", obj, const, dir, rhs, all.int = TRUE)

x_int <- int_sol$solution

cat("Integer Solution (via Gomory cuts):",
    "\nx1 =", x_int[1],", x2 =", x_int[2], ", Max Z =", int_sol$objval)

#===============================================================================
#============================= NWCM, VAM ==================================
#===============================================================================




library(IBFS)
# Last column = Supply, Last row = Demand
transport_matrix <- data.frame(
  D1 = c(6, 3, 4, 20),
  D2 = c(4, 8, 4, 95),
  D3 = c(1, 7, 2, 35),
  Supply = c(50, 40, 60, 150), # 150 at the bottom right is the total balanced sum
  row.names = c("S1", "S2", "S3", "Demand")
)

# 1. Solve using Northwest Corner Method
NWCM(transport_matrix)

# 2. Solve using Vogel's Approximation Method
VAM(transport_matrix)



#===============================================================================
#============================= Biostatistics ==================================
#===============================================================================




#===============================================================================
#============================= Mid-2024 ==================================
#===============================================================================

setwd("C:/Users/ASUS/OneDrive/Documents/code/Exam")
data<-read.csv("biostat.csv")

data<-within(data,{
  frac<-factor(fracture,levels = c("no fracture","fracture"))
  gen<-factor(sex,levels = c("F","M"))
  bmi<-(weight_kg)/( height_cm/100)^2
  medi<-factor(medication,levels = c("No medication",
                                     "Anticonvulsant", 
                                     "Glucocorticoids"))
})


#a
#Generalized Linear Model(glm)
model<-glm(frac ~ age + gen + bmi + medi + waiting_time + bmd , 
           data=data, family=binomial(link = "logit")) 

# er moddhy OR= estimate
# tidy use korbo clean data frame pawar jonno
library(broom)
tidy(model,conf.int=TRUE,exponentiate=TRUE)
summary(model) 

# Gender (genM): OR = 2.74, p = 0.060 (Significant at the 10% level).
# Interpretation: male patients have 2.74 times higher odds of experiencing a hip fracture compared to female patients.
#   
#   
#Medication (mediGlucocorticoids): OR = 0.16, p = 0.035 (Statistically significant at alpha = 0.05).
#Interpretation:patients using Glucocorticoids have 0.16 times the odds of experiencing a hip fracture compared to patients on no medication.
# 
# Bone Mineral Density (bmd): OR = 4.27 x 10^-8, p < 0.001 (Highly statistically significant).
# Interpretation:a one-unit increase in bone mineral density is associated with a profound, near-total reduction in the odds of a hip fracture, it is making BMD the strongest protective factor in the model.




#b
#convert log-odds into actual probabilities between 0 and 1
pred<-predict(model,type="response") 

# If the predicted probability is greater than 0.5, it classifies the patient as a 1 (Fracture).
# 
# If the probability is 0.5 or less, it classifies the patient as a 0 (No Fracture).
pred<-ifelse(pred>0.5,1,0) 
pred<-factor(pred,levels=c(0,1),labels=c("No","Yes")) 

#Creating the Confusion Matrix
cls<-table(Actual=data$frac,Predict=pred) 
cls 
TN<-cls[1,1] 
FP<-cls[1,2] 
FN<-cls[2,1] 
TP<-cls[2,2] 
sen<-TP/(TP+FN) 
spc<-TN/(TN+FP) 
ppv<-TP/(TP+FP) 
npv<-TN/(TN+FN) 
acc<-(TP+TN)/(TP+TN+FP+FN) 
cat("\n Sensitivity =",sen,"\n", "Specificity =",spc, "\n","PPV =",ppv,"\n", "NPV =",npv,"\n", "Accuracy =",acc,"\n") 


# Sensitivity (0.78)
# the model correctly identified 78% of the patients who actually had a fracture
# 
# Specificity (0.916)
#the model correctly identified 91.6% of the patients who did not have a fracture,
# 
# PPV - Positive Predictive Value (0.795)
# all the patients whom the model predicted would have a fracture, 79.5% of them actually had one.
# 
# NPV - Negative Predictive Value (0.908)
# all the patients whom the model predicted would not have a fracture, 90.8% of them actually did not have one.
# 
# Accuracy (0.875)
# The model correctly classified 87.5% of all cases (both fractures and non-fractures) in the data set.



#To find area under curve (AUC);
# install.packages("pROC")
library(pROC) 
pred<-predict(model, type="response") 
roc.obj<-roc(data$frac,pred) 
auc(roc.obj)


# Area Under the Curve (AUC) = 0.9269
# means there is a 92.69% chance that the model will assign a higher predicted probability of fracture to a randomly chosen patient with fracture than than without fracture


#========================================
#====================== END ==================
#========================================

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(
  tidyverse,        # data wrangling & ggplot2
  marginaleffects,  # avg_slopes() for AME
  car,              # vif()
  ResourceSelection,# hoslem.test()
  pROC,             # roc() and AUC
  broom,            # tidy model output
  ggplot2
)

setwd("C:/Users/ASUS/OneDrive/Documents/code")
HT <- read.csv("Hypertension_Lab.csv",header = T, sep = ",")
str(HT)

#data file এর মধ্যে নতুন একটা column তৈরি করবে 
HT$bmi_cat <- ifelse(HT$bmi < 25, "Normal",ifelse(HT$bmi < 30, "Overweight","Obese"))
HT$bmi_cat

#column টি কে factor বানাবে 
HT$bmi_cat <- factor(HT$bmi_cat,levels = c("Normal", "Overweight", "Obese"))
HT$bmi_cat

#numarical value গুলোকে lebeling করবে নতুন column এ 
HT <- within(HT, {
  sex <- factor(Gender, labels = c("Male", "Female"))
  ht <- factor(hypert, labels = c("No","Yes"))})
str(HT)
# data এর মধ্যে থাকা male ও female এর সংখ্যা দেখাবে 
table(HT$sex)

#data এর মধ্যে থাকা male ও female এর অনুপাত দেখাবে 
prop.table(table(HT$sex))


# ht group এর সাপেক্ষে information গুলো দেখাবে
HT |>
  group_by(ht) |>
  summarise(
    n          = n(),
    mean_bmi   = mean(bmi),
    sd_bmi     = sd(bmi),
    mean_sleep = mean(sleep),
    sd_sleep   = sd(sleep),
  ) |>
  print()

# ht group এর সাপেক্ষে  male ও female এর সংখ্যা দেখ
table(HT$ht,HT$sex)

#logistic model fit করা হচ্ছে
model <- glm(
  ht ~ bmi + sleep + sex,
  data   = HT,
  family = binomial(link = "logit")
)

summary(model)
model

# Tidy output (OPtional) 
# organized একটা টেবিল পাওয়ার জন্য 
tidy(model, conf.int = TRUE, conf.level = 0.95) |> print()


#===================================================================================
# Calulate Sensitivity, Specificity, PVP,NPV, Precision, Recall, Accuracy, F1-score:
#===================================================================================

predict_prob <- predict(model,type="response")
predict_prob <- ifelse(predict_prob > 0.5,1,0)
classification_table <- table(Actual = HT$ht, Predict = predict_prob)

TN <- classification_table[1,1]
FP <- classification_table[1,2]
FN <- classification_table[2,1]
TP <- classification_table[2,2]

calculate_all <- function(TP, FP, TN, FN) {
  matrics <- list(
    TP = TP,
    FP = FP,
    TN = TN,
    FN = FN,
    Total = TP + FP + TN + FN,
    Sensitivity = TP / (TP + FN),
    Specificity = TN / (TN + FP),
    PPV = TP / (TP + FP),
    NPV = TN / (TN + FN),
    Precision = TP / (TP + FP),
    Recall = TP / (TP + FN),
    Accuracy = (TP + TN) / (TP + FP + TN + FN),
    F1_score = (2 * ((TP / (
      TP + FP
    )) * TP / (TP + FN))) / ((TP / (TP + FP)) + (TP / (TP + FN)))
  )
}

print(calculate_all(TP,FP,TN,FN))



# Variance Inflation Factor (multicollinearity) 
#It makes sure your predictors (BMI, sleep, sex) aren't so highly correlated with each other that they confuse the model. (A score under 5 is usually good
cat("VIF:\n")
vif(model)
## No Multicollinearity

# --- Hosmer–Lemeshow goodness-of-fit (g = 10 groups) --------
#tests "Goodness of Fit." It checks if the predicted frequencies match the observed frequencies
hl <- hoslem.test(
  x = as.numeric(HT$ht) - 1,
  y = fitted(model),
  g = 10
)

cat("\nHosmer-Lemeshow test:\n")
print(hl)
# if p > 0.05 → acceptable fit

# Model fits the data well.

# ============================================================
# ROC curve ROC(Receiver Operating Characteristic) 
# ============================================================

roc_obj <- roc(
  response = HT$ht,
  predictor = fitted(model),
  levels = c("No","Yes")
  
)
roc_obj

cat("AUC:",round(auc(roc_obj),4),"\n")

# AUC=0.685 means: If you randomly pick one positive case (e.g., has disease) 
# and one negative case (e.g., no disease), the model will assign 
# a higher predicted probability to the positive case about 68.5% of the time.


#This measures how well the model can distinguish between someone with hypertension and someone without.

#An AUC of 0.685 (as noted in your comments) means the model is about 68.5% accurate at ranking a random sick person higher than a random healthy person.

# ============================================================
# Plot ROC curve
# ============================================================

plot(roc_obj,
     col = "red",
     lwd = 2,
     main = "ROC Curve",
     print.auc=TRUE
)
abline(a=0,b=1,lty=2)

# ============================================================



#Deviance and AIC(Akaike Information Criterion) 
cat("\nNull deviance   :", model$null.deviance,  "df =", model$df.null)
cat("\nResidual deviance:", model$deviance,       "df =", model$df.residual)
cat("\nAIC             :", AIC(model), "\n")



# McFadden's pseudo-R² 
mcfadden_r2 <- 1 - (model$deviance / model$null.deviance)
cat("\nMcFadden R²:", round(mcfadden_r2, 4), "\n")


# ============================================================
# AVERAGE MARGINAL EFFECTS  (AME)
# ============================================================
# avg_slopes() evaluates the slope for each observation
# at its own covariate values and then averages — this is
# the standard AME interpretation for probability.

ame <- avg_slopes(model)
print(ame)

## AME (BMI)=0.02186: A one-unit increase in BMI (from 25 to 26)  
# is associated with an average 2.2 percentage point increase in the 
# probability of having hypertension, holding all other variables constant.


# Marginal effects at the mean (MEM) — alternative approach
mem <- slopes(model, newdata = datagrid())
print(mem) 


# ============================================================
# ODDS RATIOS WITH 95% CONFIDENCE INTERVALS
# ============================================================

or_table <- tidy(model, conf.int = TRUE, exponentiate = TRUE) 

or_table







#=========================================================================================================
# Semester
#=========================================================================================================

# ==============================================================================
# STA4206L Exam Script: Hypertension Data Analysis
# ==============================================================================

# 1. SETUP AND IMPORT
# ------------------------------------------------------------------------------
install.packages("haven")
library(haven)

# IMPORT data HYPERT.csv
HYPERT <- read.csv("HYPERT.csv")

# Randomly select 650 cases using ID as seed
# REPLACE '12345678' with your actual Student ID number
set.seed(12345678) 
HYPERT2 <- HYPERT[sample(nrow(HYPERT), size = 650, replace = FALSE), ]

# Export the data into SPSS file
write_sav(HYPERT2, "HYPERT.sav")

# 2. ANALYSIS
# ------------------------------------------------------------------------------

# (a) Are there any missing observations? If yes, how many?
missing_total <- sum(is.na(HYPERT2))
missing_by_var <- colSums(is.na(HYPERT2))

print("--- Question (a): Missing Observations ---")
print(paste("Total missing values:", missing_total))
print(missing_by_var)


# (b) Calculate BMI and recode into BMICLASS
# Formula: weight(kg) / [height(m)]^2
HYPERT2$BMI <- HYPERT2$weight / (HYPERT2$height / 100)^2

# Recode into 4 categories
HYPERT2$BMICLASS <- cut(HYPERT2$BMI, 
                        breaks = c(-Inf, 20, 25, 30, Inf), 
                        labels = c(1, 2, 3, 4), 
                        right = FALSE)

print("--- Question (b): BMI Classification Distribution ---")
table(HYPERT2$BMICLASS)


# (c) Risk of Isolated Systolic Hypertension (ISH) in 1993 for women
# Note: ISII is the variable for ISH in 1993; sex: 1 = female
women_data <- subset(HYPERT2, sex == 1)
n_women <- nrow(women_data)
cases_ish <- sum(women_data$ISII == 1, na.rm = TRUE)

risk_women <- cases_ish / n_women

# 95% Confidence Interval Calculation (Manual)
# Formula: p +/- 1.96 * sqrt(p*(1-p)/n)
se <- sqrt((risk_women * (1 - risk_women)) / n_women)
ci_lower <- risk_women - (1.96 * se)
ci_upper <- risk_women + (1.96 * se)

print("--- Question (c): Risk for Women ---")
print(paste("Risk of ISH for women:", round(risk_women, 4)))
print(paste("95% CI: [", round(ci_lower, 4), ",", round(ci_upper, 4), "]"))


# (d) Association between ISII and BMICLASS
print("--- Question (d): Association ---")
# Chi-square test
chi_test <- chisq.test(HYPERT2$ISII, HYPERT2$BMICLASS)
print(chi_test)

# Highest probability calculation
prob_table <- prop.table(table(HYPERT2$BMICLASS, HYPERT2$ISII), margin = 1)
print("Proportion Table (Rows = BMICLASS, Col 2 = ISH Probability):")
print(prob_table)


# (e) Logistic Regression
# Response: ISII, Explanatory: age, sex, smoke93, BMICLASS
# Note: Categorical variables should be treated as factors
logit_model <- glm(ISII ~ age + factor(sex) + factor(smoke93) + factor(BMICLASS), 
                   data = HYPERT2, family = "binomial")

print("--- Question (e): Logistic Regression Output ---")
summary(logit_model)

# Odds Ratios (OR)
print("Odds Ratios:")
exp(coef(logit_model))




#For weibull distribution

alpha <- 1.25 # shape parameter (α)
lambda <- 3000 # scale parameter (λ)
pdf_scale <- (alpha /lambda)*(x/lambda)^(alpha - 1) * exp(-(x/lambda)^alpha)
m <- lambda * gamma((1 / alpha) + 1)
v <- lambda^2 * (gamma((2 / alpha) + 1) - (gamma((1 / alpha) + 1))^2)

alpha <- 1.25  # shape parameter (α)
lambda <- 3000 # rate parameter (λ)
pdf_rate <- (alpha *lambda)*(lambda*x)^(alpha - 1) * exp(-(lambda * x)^alpha)
m <- (1 / lambda) * gamma((1 / alpha) + 1)
v <- (1 / lambda^2) * (gamma((2 / alpha) + 1) - (gamma((1 / alpha) + 1))^2)


# ============================================================
# ================= Survival Analysis ===========================
# ============================================================



setwd("E:/study/8th Semester/4203")
#Read data
usage_data <- read.csv("Automobile.csv",sep=",")
str(usage_data)

library(survival)
library(ggplot2)
library(survminer)

# Basic Descriptive Statistics
summary(usage_data)
mean(usage_data$Age, trim = 0.05)
sd(usage_data$Age)
CV<- sd(usage_data$Age)/mean(usage_data$Age)*100
CV
IQR(usage_data$Age)

mean(usage_data$Usage, trim = 0.05)
sd(usage_data$Usage)
CV<-sd(usage_data$Usage)/mean(usage_data$Usage)*100
CV
IQR(usage_data$Usage)
cor(usage_data$Age,usage_data$Usage)


# Plot of probability mass function and distribution function.
par(mfrow=c(1,2))
x <- 0:5
px <- c(0.05, 0.15, 0.25, 0.30, 0.20, 0.05)
plot(x, px, type = "h", 
     lwd=2, 
     xlab="X", 
     ylab="P(x)", 
     main="Probability mass function for X")
points(x, px, pch=19)

Fx <- cumsum(px)
plot(x, Fx, type="s",
     xlab="X", 
     ylab="F(x)", 
     main = "Probability distribution function for X")
par(mfrow=c(1,1))

# Example-1:Exponential distribution—survival probability, mean, quantile, etc.

# (i) The probability of surviving 10,000 hours can be obtained from the relationship S(t)=exp(-lambda*t)
lambda <- 0.00025
S.10000 <- exp(-lambda*10000)
S.10000
# (ii) Memoryless property
S.20000_10000 <- exp(-lambda*30000)/exp(-lambda*10000)
S.20000 <- exp(-lambda*20000)
S.20000
#(iii)
MTTF <- 1/lambda
MTTF
#(iv)
t_0.5 <- -1/lambda*log(1-0.5)
#(v)
t_0.3 <- -1/lambda*log(1-0.3)
t_0.3
#(vi)
t <- -log(1-0.6321)/(lambda)
t

# Example 2: Weibull distribution—survival probability, mean, quantile, etc.
# The pdf of Weibull distribution is
# f(t)=β/η*(t/η)^(β-1)exp*[-(t/η)^β ], t≥0 

eta <- 4000
beta <- 1.50
mean.T <- eta*gamma(1+1/beta)
var.T <- eta^2 * (gamma(1+2/beta) - (gamma(1+1/beta))^2)
var.T 
S.5000 <- exp(-(5000/eta)^beta)
S.7000 <- exp(-(7000/eta)^beta)
S.2000_5000 <- S.7000/S.5000

# Hazard function at 5000h
h.5000 <- (beta/eta)*((5000/eta)^beta)
h.5000
# Let us consider λ = 1/η = 0.00025 and β = 1, then the Weibull distribution 
# reduces to an exponential distribution where the memoryless property 
# of the exponential distribution can be used.

lambda <- 1/eta
S.2000.Exp <- exp(-1/eta*2000)


#====================================================================
# ==================== Example 3: Schizophrenia data ================
#====================================================================
# install.packages("survival")
# install.packages("survminer")

library(survival)
library(survminer)


setwd("C:/Users/ASUS/OneDrive/Documents/code/Exam")
schizo <- read.csv("Schizophrenia.csv") 

schizo <- within(schizo, { 
  sex <- factor(Gender, labels = c("Male", "Female")) 
  censor <- factor(Censor, labels = c("Censor", "Death")) 
  marital <- factor(Marital, labels = c("Single", "Married", "Alone again")) 
  Time <- as.numeric(Time)}) 

#a.The total number of censored observations is 117 and events (deaths) is 163.

table(schizo$censor) 
#prop.table(table(schizo$censor))

#By marital status the number of censors is 50, 35 and 32 for Single, Married and Alone again. 
table(schizo$censor, schizo$marital) 
#prop.table(table(schizo$censor, schizo$marital))

#By gender the number of censors is 86 and 31 for Male and Female. 
table(schizo$censor, schizo$sex)


#b. Plotting Kaplan-Meier estimate of the survival curve for these patients.
schizo_surv <- survfit(Surv(Time,Censor)~1,
                       data = schizo,
                       type = "kaplan-meier", 
                       conf.type = "log-log")
plot(schizo_surv,
     xlab = "Days of follow-up",    
     ylab="Survival Probability",   
     main= "Overall survival curve")

#c. The median survival time together with a 95% CI is 
print(schizo_surv) # Median Survival time

#The median survival time is 933 days. 


#the survival time at 6 months, 18 months and 36 months togather with 95% CI is 
print(summary(schizo_surv, time=c(6*30, 18*30, 36*30)))

#the estimated survival probabilities at 6 months, 18 months and 36 months are 0.881, 0.672 and 0.459 respectively. This means that approximately 88.1%, 67.2% and 45.9% of patients survive beyond 6 months, 18 months and 36 months respectively. 


#d. The median survival time with 95% CI by marital status. 
KM_schizo_mar <- survfit(Surv(Time,Censor) ~ marital,
                         data=schizo,type="kaplan-meier", 
                         conf.type="log-log")
print(KM_schizo_mar)

plot(KM_schizo_mar, 
     col = c("red", "darkgreen", "lightskyblue"), 
     main = "Kaplan-Meier Survival Curves by Marital Status",
     xlab = "Time",
     ylab = "Survival Probability")
legend("topright",legend = c("Single ", "Married", "Alone again"), 
       col = c("red", "darkgreen", "lightskyblue"), 
       lty = 1)

#Since p-value <0.0001, we conclude that highly significant differences in survival times among the three marital status groups. Patients who are 'Alone Again' have the poorest survival (median 539 days), while married patients have the best survival (median 1310 days).  


#e. The median survival time with 95% CI by gender. 
KM_schizo_gender <- survfit(Surv(Time,Censor) ~ sex,
                            data=schizo,type="kaplan-meier", 
                            conf.type="log-log")

print(KM_schizo_gender) # Median Survival time for gender of patients.

#Females have a substantially lower median survival time compared to males,indicating that females in this cohort have poorer survival outcomes.Kaplan-Meier estimate of the survival curve by gender. 

plot(KM_schizo_gender, 
     col = c("red", "lightskyblue"), 
     main = "Kaplan-Meier Survival Curves by Marital Status",
     xlab = "Time",
     ylab = "Survival Probability")
legend("topright",legend = c("Male", "Female"), 
       col = c("red","lightskyblue"), 
       lty = 1)

#The Kaplan-Meier curves show that females have consistently lower survival probabilities than males throughout the follow-up period, with a log-rank p-value < 0.0001 confirming a statistically significant difference. 


#f. The comparison between the Kaplan-Meier Estimator and Nelson-Aalen Estimator estimates at timepoints 90, 180, 270 and 365 days.

na_fit <- survfit(Surv(Time, Censor) ~ 1,
                  data= schizo,
                  type= "fh",     # Fleming-Harrington = Nelson-Aalen
                  conf.type = "log-log")    

schizo_surv <- survfit(Surv(Time, Censor) ~ 1, 
                       data = schizo, 
                       type = "kaplan-meier", 
                       conf.type = "log-log") 

na_summary <- summary(na_fit, times = c(90, 180, 270, 365)) 
km_summary <- summary(schizo_surv, times = c(90, 180, 270, 365)) 

comparison <- data.frame( 
  Time = na_summary$time, 
  KM_S = round(na_summary$surv,  4), 
  KM_lower = round(km_summary$lower,  4), 
  KM_upper = round(km_summary$upper,  4), 
  NA_S = round(na_summary$surv,  4), 
  NA_lower = round(na_summary$lower, 4), 
  NA_upper = round(na_summary$upper, 4)) 

print(comparison) 


#In general, NA is greater or equal. This result indicates that both methods provide very similar estimates of the survival experience of the patients. 










#################################################################################
### Example 4: Parameter Estimation Under Type-I Censoring Using R's lung Dataset
#################################################################################

# Recode status to survival convention
lung$status2 <- ifelse(lung$status == 2, 1, 0)
n <- nrow(lung)   # n = 228
names(lung)
table(lung$status2)

lung$status_n <- ifelse(lung$status == 2, 1, 0)
table(lung$status_n)

tau <- 365  # fixed censoring time 365 days (1 year)

D_total <- sum(lung$status2)         # number of events
t_total <- sum(lung$time)            # total survival time days
t1 <- pmin(lung$time, tau)     # observed times capped at τ
s1 <- as.integer(lung$time <= tau & lung$status2 == 1)  # event indicator

D <- sum(s1)        # 121 events
tt1 <- sum(t1)        # 55,657 days (total time on test)

cat("\n── Type-I censoring summary ──\n")
cat("τ (cutoff) =", tau, "days\n")
cat("Events (D) =", D, "| Censored =", n-D, "\n")

# ── MLE: closed-form ─────────────────────────────────
# λ_hat = D / Σtᵢ*    (failures / total survival time)
# μ_hat = 1/λ_hat
lambda_hat <- D / ttl
mu_hat     <- 1 / lambda_hat

# ── Variance & 95% CI for λ̂ ──────────────────────────
# Var(λ_hat) = λ²/D   →   SE(λ_hat) = λ_hat/√D
se_lambda  <- lambda_hat / sqrt(D)
ci_lambda  <- lambda_hat + c(-1,1) * 1.96 * se_lambda

# CI for μ_hat = 1/λ_hat via delta method: SE(μ_hat) = μ_hat/√D
se_mu      <- mu_hat / sqrt(D)
ci_mu      <- mu_hat + c(-1,1) * 1.96 * se_mu

cat("\n── Exponential MLE (Type-I) ──\n")
cat(sprintf("λ_hat = %.6f  SE = %.6f  95%%CI [%.6f, %.6f]\n",
            lambda_hat, se_lambda, ci_lambda[1], ci_lambda[2]))
cat(sprintf("μ_hat = %.2f days  SE = %.2f  95%%CI [%.2f, %.2f]\n",
            mu_hat, se_mu, ci_mu[1], ci_mu[2]))

# ── Exact chi-square CI for λ (preferred) ────────────
# Based on: 2Dλ/λ_hat ~ χ²(2D)
ci_exact_lambda <- c(
  qchisq(0.025, 2*D) / (2*ttl),
  qchisq(0.975, 2*D) / (2*ttl)
)
cat(sprintf("\nExact 95%%CI for λ: [%.6f, %.6f]\n",
            ci_exact_lambda[1], ci_exact_lambda[2]))

##########################################################################














