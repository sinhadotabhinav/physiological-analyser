# clean up environment
rm(list = ls(all=TRUE))

# import libraries
library(foreign)
library(descr)
library(dplyr)
library(mosaic)
library(ggplot2)
library(pROC)
library(Hmisc)
library(lmtest)

# read data
ds <- read.dta("~/male-grip-strength.dta")
# analyse data and data structure
names(ds)
str(ds)
head(ds)
class(ds[,"Hwid"])
class(ds[,"Hlen"])
class(ds[,"Fcirc"])
class(ds[,"Bcirc"])
class(ds[,"Bskin"])
class(ds[,"Grip"])
class(ds[,"HEIGHT"])
class(ds[,"WEIGHT"])

##############################################
# part a: parsimonous model for grip strength
##############################################

# create BMI categorical variable
# convert height into metres
ds$height_meters <- (ds$HEIGHT/100)
ds$bmi <- (ds$WEIGHT)/(ds$height_meters*ds$height_meters)
favstats(ds$bmi)
histogram(ds$bmi, type="percent", xlab = "Body Mass Index")
# create an empty variable and create categories and labels
ds$bmi_categories <- NA
ds$bmi_categories[ds$bmi < 18.5] <- 1
ds$bmi_categories[ds$bmi >= 18.5 & ds$bmi <= 25.0] <- 2
ds$bmi_categories[ds$bmi >= 25.0 & ds$bmi <= 30.0] <- 3
ds$bmi_categories[ds$bmi >= 30] <- 4
ds$bmi_categories <- factor(ds$bmi_categories, levels = c(1,2,3,4),
                            labels = c("Underweight", "Normal", "Overweight", "Obese"))
freq(ds$bmi_categories, plot = F)
str(ds)
# create a new data frame with variables bmi levels, Hwid, Hlen, Fcirc, Bcirc, Fskin, Bskin
# following correct naming conventions
new_ds <- cbind.data.frame(grip = ds$Grip, bmi_categories = ds$bmi_categories, 
                        hand_width = ds$Hwid, hand_length = ds$Hlen,
                        forearm_circ = ds$Fcirc, bicep_circ = ds$Bcirc,
                        forearm_skin = ds$Fskin, bicep_skin = ds$Bskin)
new_ds <- new_ds[complete.cases(new_ds),]
str(new_ds)
head(new_ds)
class(new_ds$grip)
class(new_ds$bmi_categories)
class(new_ds$hand_width)
class(new_ds$hand_length)
class(new_ds$forearm_circ)
class(new_ds$bicep_circ)
class(new_ds$forearm_skin)
class(new_ds$bicep_skin)
freq(new_ds$bmi_categories)
# since obese bmi cat has no data set, we can ignore it
pairs(~ grip + bmi_categories + hand_width + hand_length,
      upper.panel = NULL, data = new_ds, cex.labels = 1, pch = 19, cex = 0.5)
pairs(~ grip + forearm_circ + bicep_circ + forearm_skin + bicep_skin,
      upper.panel = NULL, data = new_ds, cex.labels = 1, pch = 19, cex = 0.5)
xyplot(new_ds$grip ~ new_ds$bmi_categories, ylab="Grip Strength", xlab="BMI categories", pch = 19)
# an initial look shows all relations could be linear
# write about individual graphs and how string the linearity looks
# we will investigate all variables one by one later
# more plots like boxplots and histograms
boxplot(new_ds$grip ~ new_ds$bmi_categories, ylab = "Grip Strength (Kg)", frame = FALSE)
favstats(new_ds$grip)
freq(new_ds$bmi_categories)
favstats(new_ds$hand_width)
favstats(new_ds$hand_length)
favstats(new_ds$forearm_circ)
favstats(new_ds$bicep_circ)
favstats(new_ds$forearm_skin)
favstats(new_ds$bicep_skin)
favstats(new_ds$grip ~ new_ds$bmi_categories)
# compare with boxpot and other plots - repeat for all associations
# lets find correlation coefficients
correlation_setA <- rcorr(cbind(grip = new_ds$grip, bmi_categories = new_ds$bmi_categories,
            hand_width = new_ds$hand_width, hand_length = new_ds$hand_length), type = "spearman")
correlation_setB <- rcorr(cbind(grip = new_ds$grip, forearm_circ = new_ds$forearm_circ,
                                bicep_circ = new_ds$bicep_circ, forearm_skin = new_ds$forearm_skin,
                                bicep_skin = new_ds$bicep_skin), type = "spearman")
correlation_fullset <- rcorr(cbind(grip = new_ds$grip, bmi_categories = new_ds$bmi_categories,
                                   hand_width = new_ds$hand_width, hand_length = new_ds$hand_length,
                                   forearm_circ = new_ds$forearm_circ, bicep_circ = new_ds$bicep_circ,
                                   forearm_skin = new_ds$forearm_skin, bicep_skin = new_ds$bicep_skin),
                             type = "spearman")
correlation_setA$r
correlation_setB$r
correlation_fullset$r
# write about the nature of correlation between grip and others and also strong 
# associations among the independent variables and also p values using -$P
# sunmmary:
# bmi_categories, hand_width, hand_length show +ve mild correlations with grip ~ 0.22-0.24
# forearm_circ and bicep_circ show +ve significant correlations with grip ~ 0.56-0.63
# forearm_skin shows +ve weak correlation with grip ~ 0.08 almost no correlation
# bicep_skin shows -ve poor correlation with grip ~ -0.03 almost no correlation
# forearm_circ and bicep_circ have strong +ve correlation between each other ~ 0.84
# forearm_skin and bicep_skin have significant +ve correlation between each other ~ 0.62
# highly correlated variables can cause problems in the regression model so we may have to chose
# only one in the two pairs of variables above. We will do this by looking at correlations or running
# regression of grip with variables and compare.

model_forearm_circ <- lm(grip ~ forearm_circ, data = new_ds)
summary(model_forearm_circ)
# r-squared = 0.39
model_bicep_circ <- lm(grip ~ bicep_circ, data = new_ds)
summary(model_bicep_circ)
# r-squared = 0.34
# which implies that 39 % variation in forearm_circ is captured in the relationship with grip 
# vs. the 34 % variation in bicep_circ which is lesser so we will have to chose forearm_circ in
# our final model since they have strong correlations between them
model_forearm_skin <- lm(grip ~ forearm_skin, data = new_ds)
summary(model_forearm_skin)
# r-squared = 0.02
model_bicep_skin <- lm(grip ~ bicep_skin, data = new_ds)
summary(model_bicep_skin)
# r-squared = 0.0007
# which implies that 2 % variation in forearm_skin is captured in the relationship with grip 
# vs. the 0.07 % variation in bicep_skin which is poorer so we will have to chose forearm_skin in
# our final model since they have strong correlations between them
linear_model_one <- lm(grip ~ bmi_categories + hand_width + hand_length +
                      forearm_circ + forearm_skin, data = new_ds)
summary(linear_model_one)
linear_model_one_fit <- linear_model_one$fitted.values
favstats(linear_model_one_fit)
# bmi_categoriesOverweight, hand_length and forearm_circ are the only significant variables 
# with P < 0.05. However, bmi_categoriesNormal, forearm_skin and hand_width
# are not significant. For overall significance of bmi_categories, we will be doing
# the LR test to obtain a single P value. This is done by creating two models:
# model_lrtestA with bmi_categories and model_lrtestB without.
model_lrtestA <- lm(grip ~ bmi_categories + hand_width + hand_length +
                      forearm_circ + forearm_skin, data = new_ds)
model_lrtestB <- lm(grip ~ hand_width + hand_length +
                      forearm_circ + forearm_skin, data = new_ds)
lrtest(model_lrtestA, model_lrtestB)
# the output tells us that the P value for bmi_categories is 0.044 < 0.05
# implying that the overall effect of bmi_categories on grip is strong suggesting us
# to include it in the regression model for predicting grip strength. Hence, the final
# regression model should include bmi_categories, hand_length and forearm_circ only.
# before running the final model we will relevel the bmi_categories variable to change ref to Normal BMI
new_ds$bmi_categories <- relevel(new_ds$bmi_categories, ref = "Normal")
linear_model_final <- lm(grip ~ forearm_circ + hand_length + bmi_categories, data = new_ds)
summary(linear_model_final)
# This regression model explains 47.78 % of the variation in grip strength using
# bmi, hand length and forearm circumference as predictor variables. Not a good prediction model

#####################################
# part b: coefficient interpretation
#####################################
# the above linear regression model can be expressed in the form of the equation
# Y{f(X)} = ß0 + ß1*X1 + ß2*X2 + ß3*X3 + ß4*X4
# Y is the predicted value of Grip Strength is Kg
# ß0 is the intercept or constant term is the estimated mean Y value for all X=0. Here it is the mean Grip Strength for
# a male who belongs to the Normal BMI category
# ß1 is the change in mean Grip Strength for a male who belongs to Underweight BMI category relative to Normal BMI category
# i.e. its effects on Grip Strength is 5.85 Kg/m2 lower compared to Normal BMI category
# X1 is the dummy variable = {1, if male belongs to Underweight BMI category and 0, otherwise}
# ß2 is the change in mean Grip Strength for a male who belongs to Overweight BMI category relative to Normal BMI category
# i.e. its effects on Grip Strength is 4.69 Kg/m2 higher compared to Normal BMI category
# X2 is the dummy variable = {1, if male belongs to Overweight BMI category and 0, otherwise}
# ß3 is the estimated effect of forearm circumference on Grip Strength keeping all other variables constant
# X3 is the provided value for forearm circumference
# ß4 is the estimated effect of hand length on Grip Strength keeping all other variables constant
# X4 is the provided value for hand length
# Looking at the coefficients in the summary of the regression model or by issuing the coeffiencts command we can say that,
coefficients(linear_model_final)
confint(linear_model_final, level = 0.95)
# ß0 = -56.3083 (intercept), ß1 = -5.8454 (çoefficient of underweight BMI category),
# ß2 = 4.6865 (çoefficient of overweight BMI category), ß3 = 2.8204 (çoefficient of forearm circumference variable)
# and ß4 = 1.4897 (çoefficient of hand length variable)
# which means that the estimated mean Y value is -56.3083 for all predictor variables = 0,
# for a male in Underweight BMI category, on average the Grip Strength is 5.85 Kg/m2 lower compared to Normal BMI category,
# for a male in Overweight BMI category, on average the Grip Strength is 4.69 Kg/m2 higher compared to Normal BMI category,
# with an increase in forearm circumference by 1cm, the Grip Strength increases by 2.82 Kg/m2 keeping all other variables constant,
# with an increase in hand length by 1cm, the Grip Strength increases by 1.49 Kg/m2 keeping all other variables constant
# so, Grip Strength(Kg) = -56.3083 - 5.8454*X1 + 4.6865*X2 + 2.8204*X3 + 1.4897*X4
# the grip strength for a future male student can be predicted using the above linear equation
# given the bmi category of the student (or the availability of height and weight stats), forearm circumeference and 
# hand length of the male student.
# Also, this model does not handle males which belong to Obese category since there were no data sets on 
# obese candidates to test the regression model

ggplot(data=new_ds, aes(x=fitted(linear_model_final),y=grip)) +
  geom_point() +
  geom_smooth(se=F, col='red') +
  ylab("Residual") +
  xlab("Fitted values") +
  ggtitle("Residual v Fitted plot")
ggsave("male-grip-strength.pdf")
#################################
# part c: validating assumptions
#################################

par(mfrow = c(1,1))
plot(linear_model_final)
plot(linear_model_final)
# Write about the 4 assumptions of linear regression
# 1 Linearity: The relationship between X and the mean of Y is linear
# 2 Homoscedasticity: The variance of residual is the same for any value of X
# 3 Normality: For any fixed value of X, Y is normally distributed
# 4 Independence: Observations are independent of each other: depends on the knowldege of study design and data collection (assumed to be true)
# check these assumptions by examining plots of residuals/ errors using plot command
# Residuals vs Fitted Plot
# residual plot show that the relationship between grip and bmi levels,hand length and 
# forearm circumeference is approximately linear except the first few data points: satisifies Linearity
# Scale-Location Plot
# the spread location plot checks the homogeneity of variance of the residuals (homoscedasticity)
# a roughly horizontal line with equally spread points is a good indication of homoscedasticity but according 
# to the graph the variance of residuals are not truly homogenous (problem of heteroscedasticity)
# Normal Q-Q Plot
# the residuals are normally ditributed since it follows a roughly diagonal line
# grip strength given bmi levels, hand length and forearm circumeference is approximately Normality
# Residuals vs Leverage
# identifies influential cases, that is extreme values that might influence the regression results when included or excluded from the analysis.
# the plot does not indicate any data points outside of the dashed line, Cook’s distance.
# there are no points to exclude to improve the regression model
residuals <- linear_model_final$residuals
hist(residuals, freq = FALSE, col = "red")
curve(dnorm(x, mean = mean(residuals), sd = sd(residuals)), col = "blue", add = TRUE)
 # the normal distribution of the residuals in the histogram also suggest that the model is okay
