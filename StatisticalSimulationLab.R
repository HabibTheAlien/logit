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

options(digits = 5)
fit.contrast(model, "Treatment", con)


#(d) Test among the difference between all pairs.

# This compares the means of all groups within the "Treatment" factor
LSD_Test <- LSD.test(model, "Treatment")
LSD_Test$groups


#===============================================================
#====================Experiment-2(RBD)==========================
#===============================================================

# H0:There is no significant difference between the block means.
# H1:At least one block mean differs  significantly.

# H0:All the diet are equal
# H1:At least two 2ui's are unequal

# (a)	Test whether there is a significant difference between the treatments.

library(gmodels)
library(agricolae)
options(digits = 5)

y1 <- c(33.5, 33, 32, 30, 28.5)
y2 <- c(38.9, 37.8, 37, 36, 34.8)
y3 <- c(40.9, 39.6, 39, 38.3, 37.5)
y4 <- c(41.9, 40.4, 39.9, 39.5, 38.7)
y5 <- c(41.9, 40.5, 39.8, 39.2, 38.8)
y6 <- c(42.8, 41.9, 40.7, 39.8, 39.4)

y <- c(y1, y2, y3, y4, y5, y6)

Treatment <- factor(rep(LETTERS[1:6], each = 5))

# each = 5 :- number of blocks


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

fit.contrast(mod.rbd, "Treatment", con)

# D vs E= insignificant
# A vs B= highly significant


#(d)	Compare each pair of means using Duncan's test and Fisher's LSD test.

# duncan_test <- duncan.test(mod.rbd, "Treatment", console = TRUE)
# duncan_test$groups

DNMRT <- duncan.test(Data$y, Data$Treatment,
                     DFerror = 20,
                     MSerror = 0.2)
DNMRT$groups

LSD_Test <- LSD.test(mod.rbd, "Treatment", console = TRUE)
LSD_Test$groups

# (e)	Test whether there is a significant difference between the blocks.
mod.rbd <- aov(y ~ Treatment + Block, data = Data)
summary(mod.rbd)

#Conclusion:There is a significant difference between the blocks.

# (f)	Comment on whether blocking is necessary in this experiment.


# Blocking is used in an RBD to reduce experimental error caused by variability among blocks.

# Since the ANOVA test showed that the block effect is significant, it means that the blocks differ from each other and contribute to variability in the response variable.

# Conclusion: Blocking is necessary and effective in this experiment because it successfully accounts for variation among blocks and helps improve the precision of treatment comparisons.
#===============================================================
#====================Experiment-3 ==============================
#===============================================================

#(i) Perform analysis of variance of data and test the significance of diet effects.
Column<-factor(paste0(rep("Period",4),1:4))

Row<-factor(rep(paste0("Cow",1:4),each=4))

Treatment<-factor(paste0("diet", (c(1,2,3,4,
                                    2,4,1,3,
                                    3,1,4,2,
                                    4,3,2,1))))


# amon o hote pare
# Treatment<-factor(paste0("fertiliser",(c("A","B","C","D","E",
#                                           "B","C","D","E","A",
#                                           "C","D","E","A","B",
#                                           "D","E","A","B","C",
#                                           "E","A","B","C","D"))))
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
out <- LSD.test(y = myfit,
                trt = "Treatment",
                DFerror = myfit[4,1],
                MSerror = myfit[4,3],
                alpha = 0.05)
out$groups

#Diet 4 is significantly different from either of the diets 1 and 2. Diet 3 is also significantly different from either of the diets 1 and 2. Diets 3 and 4, and diets 1 and 2 are not significantly different. Die produces the highest yield.