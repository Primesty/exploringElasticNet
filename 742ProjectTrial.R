sink("742ProjectTrial.log", append=FALSE, split=TRUE)

## Read in data from csv file

data <- read.csv(file.choose(), header = TRUE)

data2 <- read.csv(file.choose(), header = TRUE)

diet<-read.csv(file.choose(), header = TRUE)

head(diet)

colnames(data) <- tolower(colnames(data))

## Cleaning and subsetting the data

head(data)

sub_data <- data[ , c("subject.", "country", "sample", "sample.", "hrprop", "lowreports", "highreports", "highcheat", "female", "age", "minority", "earn", "howrel", "trust")]

head(sub_data)

sub_data.us <- data[data$countries == "USA", c("subject.", "country", "sample", "sample.", "hrprop", "lowreports", "highreports", "highcheat", "female", "age", "minority", "earn", "howrel", "trust")]

head(sub_data.us)

# Get rid of missing data

sub_data.nomis <- na.omit(sub_data)

head(sub_data.nomis)
tail(sub_data.nomis)

# Rename female column to gender column (female = 0)

names(sub_data.nomis)[names(sub_data.nomis) == 'female'] <- 'gender'

summary(sub_data.nomis)

qqnorm(sub_data.nomis$lowreports)
qqline(sub_data.nomis$lowreports, col = 2)

sub_data.nomis$age.log <- log(sub_data.nomis$age)

## Standardize variables

sub_data.nomis$age_z <- scale(sub_data.nomis$age.log, center = FALSE, scale = TRUE)
sub_data.nomis$highreport_z <- scale(sub_data.nomis$highreports, center = FALSE, scale = TRUE)
sub_data.nomis$lowreports_z <- scale(sub_data.nomis$lowreports, center = FALSE, scale = TRUE)

yvar_z$social <- scale(yvar_z$social, center = FALSE, scale = TRUE)
yvar_z$women <- scale(yvar$women, center = TRUE, scale = TRUE)
yvar_z$immigration <- scale(yvar$immigration, center = TRUE, scale = TRUE)

library(cluster)


## Hierarchical cluster analysis

## Plot dendrogram

cheat.cluster<-hclust(d=dist(sub_data.nomis[,c(5, 6, 7, 10, 12, 13, 14)]),method=c("ward.D")) # calculate the distance

plot(cheat.cluster)

lg.clust <- hclust(d=dist(data2[, 2:6]), method = c("ward"))
plot(lg.clust)

### Three clusters - high, mid, and low cheat

cheat.cluster.kmeans3 <- kmeans(sub_data.nomis[,5:14], centers=3, nstart=2)

cheat.cluster.kmeans3

lg.clust.kmeans3 <- kmeans(data2[,2:6], centers = 3, nstart = 2)
lg.clust.kmeans3

table(data2$Country, lg.clust.kmeans3$cluster)

lg.clust.kmeans2 <- kmeans(data2[,2:6], centers = 2, nstart = 2)
lg.clust.kmeans2


library(NbClust)

lg.mat <- as.matrix(data2[, 2:6])

lg.clust.nb <- NbClust(data = lg.mat, distance = "euclidean", min.nc = 2, max.nc = 5,
                       method = "complete", index = 'all')
  

library(foreign)
library(asbio)
library(MASS)
library(psych)
library(DiscriMiner)

# Three DVs: lowreports, highreports and highcheat - conceptually related

sub_data.nomis$highcheat <- as.factor(sub_data.nomis$highcheat)
sub_data.nomis$gender <- as.factor(sub_data.nomis$gender)
sub_data.nomis$minority <- as.factor(sub_data.nomis$minority)


y <- cbind(sub_data.nomis$lowreports, sub_data.nomis$highreports)
y.z <- cbind(sub_data.nomis$lowreports_z, sub_data.nomis$highreport_z)

# Factorial MANOVA

cheat.manova<-manova(y.z~sub_data.nomis$earn)
cheat.manova

xmdl <- aov(sub_data.nomis$highreports~sub_data.nomis$age + sub_data.nomis$gender + sub_data.nomis$highcheat)
xmdl
summary(cheat.manova, test = "Wilks")

sink()