
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


