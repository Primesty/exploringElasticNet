sink("742DietProject.log", append=FALSE, split=TRUE)

## Read in data

diet_data <- read.csv(file.choose(), header = TRUE)

head(diet_data)

names(diet_data)


## Subset the data  exclude variables + standardize

diet_food <- diet_data[,1:50]
diet_demo <- diet_data[,c(1,53:59, 61:66)]
diet_health <- diet_data[, c(1,60,68:101)]

names(diet_health)

# Exclude variables - data cleaning

not_these_food <- c(2, 3, 8, 9, 10, 19, 21, 22, 23, 24, 25, 26, 39, 41, 42, 46, 47)
not_these_demo <- c(9, 11, 12, 14)
not_these_health <- c(12, 19, 23, 26, 32)

diet_food <- diet_food[,-not_these_food]
diet_demo <- diet_demo[,-not_these_demo]
diet_health <- diet_health[,-not_these_health]

colnames(diet_food) <- tolower(colnames(diet_food))
colnames(diet_demo) <- tolower(colnames(diet_demo))
colnames(diet_health) <- tolower(colnames(diet_health))

# standardized z-scores

clmns_f <- 2:(ncol(diet_food))
clmns_d <- 2:(ncol(diet_demo))
clmns_h <- 2:(ncol(diet_health))

diet_food.z <- as.data.frame(scale(diet_food[, clmns_f], center = TRUE, scale = TRUE))
diet_demo.z <- as.data.frame(scale(diet_demo[, clmns_d], center = TRUE, scale = TRUE))
diet_health.z <- as.data.frame(scale(diet_health[, clmns_h], center = TRUE, scale = TRUE))

# Add country row again - maybe reduntant since not used for analyis

diet_food.z$country <- diet_data[,1]
diet_food.z <- diet_food.z[,c(33, 1:32)] # reorder columns

diet_demo.z$country <- diet_data[,1]
diet_demo.z <- diet_demo.z[,c(10, 1:9)]

diet_health.z$country <- diet_data[,1]
diet_health.z <- diet_health.z[,c(31, 1:30)]

qqnorm(diet_food$protein..g.day.)
qqline(diet_food$protein..g.day., col = 2)

# Summarize male female columns by mean value

library(dplyr)

diet_health.z <- mutate(diet_health.z, cholesterol_mf = (mean.total.cholesterol..men..mg.dl...2005+mean.total.cholesterol..female..mg.dl...2005)/2)
diet_health.z <- mutate(diet_health.z, bloodpressure_mf = (diet_health.z$systolic.blood.pressure..adults.aged.15.and.above..men..mmhg.+diet_health.z$systolic.blood.pressure..adults.aged.15.and.above..female..mmhg.)/2)
diet_health.z <- mutate(diet_health.z, obesity_mf = (diet_health.z$obesity.prevalence..men....+diet_health.z$obesity.prevalence..female....)/2)
diet_health.z <- mutate(diet_health.z, adultmort_mf = (diet_health.z$adult.mortality.rate..probability.of.dying.between.15.to.60.years.per.1000.population..male+diet_health.z$adult.mortality.rate..probability.of.dying.between.15.to.60.years.per.1000.population..female)/2)
diet_health.z <- mutate(diet_health.z, lifeexpect_birth_mf = (diet_health.z$life.expectancy.at.birth..years..male+diet_health.z$healthy.life.expectancy..hale..at.birth..years..female)/2)
diet_health.z <- mutate(diet_health.z, infantmort_mf = (diet_health.z$infant.mortality.rate..per.1.000.live.births..male+diet_health.z$infant.mortality.rate..per.1.000.live.births..female)/2)
diet_health.z <- mutate(diet_health.z, healthy_lifeexpect_mf = (diet_health.z$healthy.life.expectancy..hale..at.birth..years..male+diet_health.z$healthy.life.expectancy..hale..at.birth..years..female)/2)
diet_health.z <- mutate(diet_health.z, under5mort_mf = (diet_health.z$under.5.mortality.rate..probability.of.dying.by.age.5.per.1000.live.births..male+diet_health.z$under.5.mortality.rate..probability.of.dying.by.age.5.per.1000.live.births..female)/2)
diet_health.z <- mutate(diet_health.z, tobaccouse_mf = (diet_health.z$prevalence.of.current.tobacco.use.among.adults....15.years......male..2005+diet_health.z$prevalence.of.current.tobacco.use.among.adults....15.years......female..2005)/2)

head(diet_health.z)
names(diet_health.z)

# Get rid off redundant male/female columns - only combined columns left
diet_health.z.clean <- diet_health.z[,-c(3,4,5,6,8,9,10,11,12,13,18,19,21,22,23,24,28,29)]

diet_demo.z.clean <- diet_demo.z[,-c(5,6,7)]

## Check correlation between outcome variables - multivariate analysis DVs should be at lease somewhat correlated

cor.test(diet_health.z.clean$years.of.life.lost.to.communicable.diseases......2002,diet_health.z.clean$years.of.life.lost.to.non.communicable.diseases......2002, method = c("pearson"))

library(glmnet)

names(diet_food.z)

### Run Elastic Net
## Predictors for glmnet

x1 <- as.matrix(diet_demo.z.clean[,2:7])
x2 <- as.matrix(diet_food.z[,2:33])

 # use mgaussian for multivariate elastic net
y1 <- as.matrix(diet_health.z.clean[,c(12,13)])


# Fit model 1 - years of life lost to communicable and non-communicable diseases~demo

mfit <- glmnet(x1, y1, family="mgaussian", standardize=FALSE, alpha=0.5)

# summarize the fit
summary(mfit)
print(mfit)

plot(mfit, label=TRUE)
plot(mfit, xvar = "lambda", label = TRUE, type.coef = "2norm")
plot(mfit, xvar = "lambda", label = TRUE, type.coef = "coef")
plot(mfit, xvar = "dev", label = TRUE, type.coef = "coef")

### Cross validate model (k-fold cross validation) nfolds = number of times run

cvmfit = cv.glmnet(x1, y1, family = "mgaussian", standardize = FALSE, type.measure="mse", nfolds = 10)
plot(cvmfit)
cvmfit$lambda.min #0.03300866
cvmfit$lambda.1se
coef(cvmfit, s=0.03300866) # which variables do not have a strong relationship with DVs?
# The onse with . are done!

## Test hypothesis, which variables are significantly related to DVs??


## Get rid of variables

diet_demo.z.clean.red <- diet_demo.z.clean[,-6]

# Reduced dataset

health.demo.z <- cbind(diet_health.z.clean[,c(1,12,13)], diet_demo.z.clean.red[,-1])


# Run multivariate regression - IVs and DVs must be in same dataset!! Look into that
# don't forget vif() - does not work with mulitvariate regression: solution two univariate
# multiple regressions

## Run univariate regressions to use vif to assess collinearity

library(car)

uni.reg1 <- lm(health.demo.z$years.of.life.lost.to.communicable.diseases......2002~total.fertility.rate..per.female.+
                 gross.national.income.per.capita..ppp.international...+
                 population.annual.growth.rate....+population.in.urban.areas....+
                 total.fertility.rate..per.female.+
                 population.with.sustainable.access.to.improved.sanitation.....total, data=health.demo.z)

uni.reg2 <- lm(health.demo.z$years.of.life.lost.to.non.communicable.diseases......2002~
                 gross.national.income.per.capita..ppp.international...+
                 population.annual.growth.rate....+population.in.urban.areas....+
                 total.fertility.rate..per.female.+
                 population.with.sustainable.access.to.improved.sanitation.....total, data=health.demo.z)
Anova(uni.reg1)
summary(uni.reg1)
vif(uni.reg1)

Anova(uni.reg2)
summary(uni.reg2)
vif(uni.reg2)

### Run multivariate regression

multivariate.reg<-lm(cbind(years.of.life.lost.to.communicable.diseases......2002, years.of.life.lost.to.non.communicable.diseases......2002)~
                       total.fertility.rate..per.female.+
                       gross.national.income.per.capita..ppp.international...+
                       population.annual.growth.rate....+population.in.urban.areas....+
                       total.fertility.rate..per.female.+
                       population.with.sustainable.access.to.improved.sanitation.....total, data=health.demo.z) # cbind DVs together model on IVs additive model



Anova(multivariate.reg) # Anova command on output object gives us output
multivariate.reg
# Checks if IVs are significantly related to the set of DVs - we reject H0 (no relationship)

# Now look at individual regression follow up - for which DVs is there a significant relation

summary(multivariate.reg) # gives us several outputs for all DVs
attributes(multivariate.reg)

multivariate.reg$residuals[,1] # for first response
plot(density(multivariate.reg$residuals[,1]))
qqline(multivariate.reg$residuals[,1])
# resid(multivariate.reg)

## Scatterpolts of residuals
par(mfrow=c(2,2))
plot(health.demo.z$gross.national.income.per.capita..ppp.international..., multivariate.reg$residuals[,1], xlab="gross national income", ylab = "years of life lost communicable diseases")
abline(multivariate.reg, lwd=1, col="red")
abline(h=0)

### Plot fitted values vs. residuals to check for linearity and homoscedasticity

plot(fitted(multivariate.reg), residuals(multivariate.reg))
abline(h=0, col="red", lwd=2, lty=2)

sink()