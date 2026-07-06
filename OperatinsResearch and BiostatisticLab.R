#===============================================================================
#============================= Simplex Method ==================================
#===============================================================================
library(lpSolve)
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
#============================= Biostatistics ==================================
#===============================================================================






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
















