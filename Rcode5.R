# Load necessary libraries for data analysis and visualization
library(foreign)     # To read SPSS data files
library(MASS)         # For negative binomial regression
library(ggplot2)      # For creating plots and visualizations
library(pscl)         # For zero-inflated models (ZIP)
library(bbmle)        # For maximum likelihood estimation
library(VGAM)         # For vector generalized linear models
library(extraDistr)   # For additional distribution functions
library(ggpubr)       # For arranging multiple ggplot objects in a grid

# Reading the data from an SPSS file
anc <- read.spss('C:\\Users\\user\\Desktop\\dataggplot\\ANC 2.sav')

# View dataset structure to understand the variables
View(anc)          
str(anc)

# Create frequency tables for different categorical variables with respect to 'Visit' 
table(anc$Visit, anc$Age)
table(anc$Visit, anc$Region)
table(anc$Visit, anc$Residence)
table(anc$Visit, anc$Religion)
table(anc$Visit, anc$wealthIndex)
table(anc$Visit, anc$Educationlevel)
table(anc$Visit, anc$Employmentstatus)
table(anc$Visit, anc$partnereducation)
table(anc$Visit, anc$maritalstatus)
table(anc$Visit, anc$exposertomedia)

# Plot the relationship between Age of mothers and their ANC visits
ag1 <- read.spss('C:\\Users\\user\\Desktop\\dataggplot\\age.sav')
data1 <- data.frame(age, numbervisit, percent, visitingstatus)
p7 <- ggplot(data = data1, aes(x = age, Number, y = percent, col = numbervisit)) +
  geom_point(size = 3) +       # Adding points to the plot
  facet_grid(visitingstatus ~ .) +
  ylim(0, 95) +                # Y-axis range for percentage
  labs(x = "Age of mothers", y = "Percentage of mothers", colour = "Number of ANC visits")

# Plot for media exposure data
med <- read.spss('C:\\Users\\user\\Desktop\\dataggplot\\Media.sav')
data10 <- data.frame(Media, numbervisit, percent, visitingstatus)
p8 <- ggplot(data = data10, aes(x = Media, Number, y = percent, col = numbervisit)) +
  geom_point(size = 3) +       
  facet_grid(visitingstatus ~ .) + 
  ylim(0, 95) +
  scale_color_hue(l = 60) +    # Adjust color range
  labs(x = "Exposure to media", y = "Percentage of mothers", colour = "Number of ANC visits")

# Arrange the two plots side by side
ggarrange(p7, p8, ncol = 2, nrow = 1, common.legend = TRUE, legend = "bottom")

# Visualization for region-wise distribution of ANC visits
reg <- read.spss('C:\\Users\\user\\Desktop\\dataggplot\\region.sav')
data2 <- data.frame(Region, numbervisit, percent, visitingstatus)
p <- ggplot(data = data2, aes(x = Region, Number, y = percent, col = numbervisit)) +
  geom_point(size = 5) + 
  facet_grid(visitingstatus ~ .) + 
  ylim(0, 95) + 
  labs(x = "Region of mothers", y = "Percentage of mothers", colour = "Number of ANC visits")
p

# Visualization for mother's residence location
res <- read.spss('C:\\Users\\user\\Desktop\\dataggplot\\Residence.sav')
data3 <- data.frame(Residence, numbervisit, percent, visitingstatus)
p3 <- ggplot(data = data3, aes(x = Residence, Number, y = percent, col = numbervisit)) +
  geom_point(size = 3) + 
  facet_grid(visitingstatus ~ .) +
  ylim(0, 95) + 
  labs(x = "Residence of mothers", y = "Percentage of mothers", colour = "Number of ANC visits")

# Employment status data visualization
emp <- read.spss('C:\\Users\\user\\Desktop\\dataggplot\\Employment.sav')
data7 <- data.frame(Employment, numbervisit, percent, visitingstatus)
p4 <- ggplot(data = data7, aes(x = Employment, Number, y = percent, col = numbervisit)) +
  geom_point(size = 3) + 
  facet_grid(visitingstatus ~ .) + 
  ylim(0, 95) + 
  labs(x = "Mothers Employment status", y = "Percentage of mothers", colour = "Number of ANC visits")

# Arrange Residence and Employment visualizations side by side
ggarrange(p3, p4, ncol = 2, nrow = 1, common.legend = TRUE, legend = "bottom")

# Religion-wise data visualization for ANC visits
rel <- read.spss('C:\\Users\\user\\Desktop\\dataggplot\\Religion.sav')
data4 <- data.frame(Religion, numbervisit, percent, visitingstatus)
p <- ggplot(data = data4, aes(x = Religion, Number, y = percent, col = numbervisit)) +
  geom_point(size = 3) + 
  facet_grid(visitingstatus ~ .) + 
  ylim(0, 95) + 
  labs(x = "Religion of mothers", y = "Percentage of mothers", colour = "Number of ANC visits")
p

# Visualizing wealth index vs. ANC visits
wel <- read.spss('C:\\Users\\user\\Desktop\\dataggplot\\Wealth.sav')
data5 <- data.frame(Wealth, numbervisit, percent, visitingstatus)
p5 <- ggplot(data = data5, aes(x = Wealth, Number, y = percent, col = numbervisit)) +
  geom_point(size = 3) + 
  facet_grid(visitingstatus ~ .) +
  ylim(0, 95) + 
  labs(x = "Wealth index", y = "Percentage of mothers", colour = "Number of ANC visits")

# Marital status data visualization
mst <- read.spss('C:\\Users\\user\\Desktop\\dataggplot\\Mstatus.sav')
data9 <- data.frame(Mstatus, numbervisit, percent, visitingstatus)
p6 <- ggplot(data = data9, aes(x = Mstatus, Number, y = percent, col = numbervisit)) +
  geom_point(size = 3) + 
  facet_grid(visitingstatus ~ .) + 
  ylim(0, 95) + 
  labs(x = "Mothers marital status", y = "Percentage of mothers", colour = "Number of ANC visits")

# Arrange Wealth and Marital Status side by side
ggarrange(p5, p6, ncol = 2, nrow = 1, common.legend = TRUE, legend = "bottom")

# Education and Partner Education level comparison
educ <- read.spss('C:\\Users\\user\\Desktop\\dataggplot\\Education.sav')
data6 <- data.frame(Education, numbervisit, percent, visitingstatus)
p1 <- ggplot(data = data6, aes(x = Education, Number, y = percent, col = numbervisit)) +
  geom_point(size = 3) + 
  facet_grid(visitingstatus ~ .) + 
  ylim(0, 95) + 
  labs(x = "Mothers education level", y = "Percentage of mothers", colour = "Number of ANC visits")

pedu <- read.spss('C:\\Users\\user\\Desktop\\dataggplot\\Peducation.sav')
data8 <- data.frame(Peducation, numbervisit, percent, visitingstatus)
p2 <- ggplot(data = data8, aes(x = Peducation, Number, y = percent, col = numbervisit)) + 
  geom_point(size = 3, alpha = 1) + 
  facet_grid(visitingstatus ~ .) + 
  ylim(0, 95) + 
  labs(x = "Partner education level", y = "Percentage of mothers", colour = "Number of ANC visits")

# Arrange Education and Partner Education side by side
ggarrange(p1, p2, ncol = 2, nrow = 1, common.legend = TRUE, legend = "bottom")

# Simulated data for zero-inflated models
sim <- read.spss('C:\\Users\\user\\Desktop\\dataggplot\\simulation.sav')
datasim <- data.frame(AIC, Model, Lambda, Zeros)
ggplot(data = datasim, aes(x = Lambda, y = AIC, col = Zeros, shape = Model)) +
  geom_point(size = 3) + 
  ylim(0, 1700) + 
  labs(colour = "Percentage of zeros", shape = "Models")

# Set up for fitting multiple zero-inflated models
par(mfrow = c(2, 2))   # Arrange multiple plots in 2x2 grid

# Zero-Inflated Poisson models with different specifications
model1 <- zeroinfl(Visit ~ Age + Residence + wealthIndex + partnereducation + maritalstatus + exposertomedia + Employmentstatus + Religion + Educationlevel, dist = "poisson", data = anc)
model2 <- zeroinfl(Visit ~ Age + Residence + wealthIndex + partnereducation + maritalstatus + exposertomedia + Employmentstatus + Religion + Educationlevel, dist = "negbin", data = anc)
model3 <- hurdle(Visit ~ Age + Residence + wealthIndex + partnereducation + maritalstatus + exposertomedia + Employmentstatus + Religion + Educationlevel, dist = "negbin", data = anc, link = "logit", zero.dist = "negbin")

# Poisson and Negative binomial regression models for ANC visits
model4 <- glm(Visit ~ Age + Residence + wealthIndex + partnereducation + maritalstatus + exposertomedia + Employmentstatus + Religion + Educationlevel, data = anc, family = poisson)
model5 <- glm.nb(Visit ~ Age + Residence + wealthIndex + partnereducation + maritalstatus + exposertomedia + Employmentstatus + Religion + Educationlevel, data = anc)

# Summarize model results
summary(model1)
summary(model2)
summary(model3)
summary(model4)
summary(model5)
# Likelihood calculations for different models
# Evaluate the log-likelihood of each model
logLik(model1)  # Log-likelihood for zero-inflated Poisson model (model1)
logLik(model2)  # Log-likelihood for zero-inflated Negative Binomial model (model2)
logLik(model3)  # Log-likelihood for hurdle model (model3)
logLik(model4)  # Log-likelihood for Poisson regression model (model4)
logLik(model5)  # Log-likelihood for Negative Binomial regression model (model5)

# Comparing models using AIC (Akaike Information Criterion)
AIC(model1, model2, model3, model4, model5)

# Comparing models using BIC (Bayesian Information Criterion)
BIC(model1, model2, model3, model4, model5)

# Vuong test for model comparison (to determine which model fits better)
# Vuong test compares two models to see if there's significant difference
-1.167e+04  # Log-likelihood or other result displayed

# Vuong test for all pairwise model comparisons
vuong(model1, model2)
vuong(model1, model3)
vuong(model1, model4)
vuong(model1, model5)
vuong(model2, model3)
vuong(model2, model4)
vuong(model2, model5)
vuong(model3, model4)
vuong(model3, model5)
vuong(model4, model5)

##############################################################################
### Simulation Code for Zero-Inflated Models 
###########################################

# DATA GENERATION: Simulating data from a Zero-Inflated Poisson model

N <- 1000  # Sample size
# Parameters for logistic regression and Poisson distribution
beta0 <- log(1.6)
beta1 <- log(2)
beta2 <- log(log(1.2))
beta3 <- log(2.5)
gamma0 <- log(log(3.8))
gamma1 <- log(log(6) / log(2))
gamma2 <- log(3.07)
gamma3 <- log(log(1.7))

set.seed(15)  # Set seed for reproducibility
X1 <- rbinom(N, 1, 0.5)  # Random binary data for predictor X1
X2 <- rbinom(N, 1, 0.6)  # Random binary data for predictor X2
X3 <- rbinom(N, 1, 0.7)  # Random binary data for predictor X3

# Generate zero-inflated binary outcomes and Poisson counts
Y0 <- rbinom(N, 1, 1 / (1 + exp(beta0 + beta1 * X1 + beta2 * X2 + beta3 * X3)))
Yc <- rpois(N, exp(gamma0 + gamma1 * X1 + gamma2 * X2 + gamma3 * X3))
Y <- ifelse(Y0 == 0, 0, Yc)  # Combine both for final zero-inflated outcome

# View proportion of zeros in the dataset
table(Y)  # Frequency table for outcome variable Y
prop.table(table(Y))  # Proportion of zeros

# Observed zero counts by level of X
obsprob0x0 <- sum(Y[X == 0] == 0) / sum(X == 0)
obsprob0x1 <- sum(Y[X == 1] == 0) / sum(X == 1)
cbind(obsprob0x0, obsprob0x1)  # Comparison for X = 0 vs X = 1

library(pscl)  # For fitting zero-inflated models

# Fit Zero-Inflated Poisson models
model1 <- zeroinfl(Y ~ X1 + X2 + X3, link = "logit", dist = "poisson")
summary(model1)  # Summary for Poisson ZIP model

model2 <- zeroinfl(Y ~ X1 + X2 + X3, link = "logit", dist = "negbin")
summary(model2)  # Summary for Negative Binomial ZIP model

# Fit hurdle model
model3 <- hurdle(Y ~ X1 + X2 + X3, link = "logit", dist = "poisson", zero.dist = "binomial")
summary(model3)  # Hurdle model summary

# Estimated probabilities for different levels of X in Zero-Inflated Poisson model
estprobx0 <- predict(model1, newdata = data.frame(X = 0), type = "prob")
estprobx1 <- predict(model1, newdata = data.frame(X = 1), type = "prob")

# Compare observed vs estimated frequencies for both levels of X
par(mfrow = c(1, 2))  # Create side-by-side histograms for visual comparison

# Histograms and comparison plots for X = 0
hist(Y[X == 0], prob = TRUE, xlab = "Y", ylab = "Relative Frequency", main = "X=0", breaks = seq(-0.5, 6.5, by = 1))
points(seq(0, 6, by = 1), estprobx0, pch = 1, col = 2, lwd = 2)
lines(seq(0, 6, by = 1), estprobx0, lty = 2, col = 2, lwd = 2)

# Histograms and comparison plots for X = 1
hist(Y[X == 1], prob = TRUE, xlab = "Y", ylab = "Relative Frequency", main = "X=1", breaks = seq(-0.5, 6.5, by = 1))
points(seq(0, 6, by = 1), estprobx1, pch = 1, col = 3, lwd = 2)
lines(seq(0, 6, by = 1), estprobx1, lty = 2, col = 3, lwd = 2)

# Simulation: Data generation for Zero-Inflated Poisson (ZIP) and Negative Binomial (ZINB) models

l <- seq(from = 0.2, to = 20, length = 1000)  # Lambda values for ZIP
n1 <- 1000  # Number of simulations
N <- 100  # Sample size in each simulation

# Empty vectors for storing AIC values and zero-proportion
m1 <- c(1:1000)
m2 <- c(1:1000)
m3 <- c(1:1000)
m4 <- c(1:1000)
m5 <- c(1:1000)
op <- c(1:1000)

# Simulate zero-inflated data across different lambda values
for (i in 1:n1) {
  # Generate binary and Poisson counts
  X1 <- rbinom(N, 1, 0.5)
  X2 <- rbinom(N, 1, 0.6)
  X3 <- rbinom(N, 1, 0.7)
  Y <- rzipois(N, l[i], 0.4)  # Zero-inflated Poisson data
  
  # Fit different models to the simulated data
  model1 <- zeroinfl(Y ~ X1 + X2 + X3, link = "logit", dist = "poisson")
  model2 <- zeroinfl(Y ~ X1 + X2 + X3, link = "logit", dist = "negbin")
  model3 <- hurdle(Y ~ X1 + X2 + X3, link = "logit", dist = "poisson", zero.dist = "binomial")
  model4 <- glm(Y ~ X1 + X2 + X3, family = "poisson")
  model5 <- glm.nb(Y ~ X1 + X2 + X3)  # Negative Binomial regression
  
  # Store AIC values for each model
  m1[i] = AIC(model1)
  m2[i] = AIC(model2)
  m3[i] = AIC(model3)
  m4[i] = AIC(model4)
  m5[i] = AIC(model5)
  
  # Store proportion of zeros in Y
  op[i] = (table(Y[Y == 0]) / N) * 100
}

# Output the data frame of AICs and zero proportions
data.frame(m1, m2, m3, m4, m5, l, op)

# Repeat similar process for Zero-Inflated Negative Binomial (ZINB) model simulations
k <- seq(from = 0.2, to = 0.7, length = 1000)  # K values for ZINB
m11 <- c(1:1000)
m22 <- c(1:1000)
m33 <- c(1:1000)
m44 <- c(1:1000)
m55 <- c(1:1000)
oq <- c(1:1000)

# Simulate ZINB distribution
for (i in 1:n1) {
  # Generate binary and Poisson counts
  X1 <- rbinom(N, 1, 0.5)
  X2 <- rbinom(N, 1, 0.6)
  X3 <- rbinom(N, 1, 0.7)
  Y <- rzinb(N, 11, 0.7, k[i])  # Zero-inflated Negative Binomial data
  
  # Fit models to simulated data
  model1 <- zeroinfl(Y ~ X1 + X2 + X3, link = "logit", dist = "poisson")
  model2 <- zeroinfl(Y ~ X1 + X2 + X3, link = "logit", dist = "negbin")
  model3 <- hurdle(Y ~ X1 + X2 + X3, link = "logit", dist = "poisson", zero.dist = "binomial")
  model4 <- glm(Y ~ X1 + X2 + X3, family = "poisson")
  model5 <- glm.nb(Y ~ X1 + X2 + X3)
  
  # Store AIC values and zero proportions
  m11[i] = AIC(model1)
  m22[i] = AIC(model2)
  m33[i] = AIC(model3)
  m44[i] = AIC(model4)
  m55[i] = AIC(model5)
  oq[i] = (table(Y[Y == 0]) / N) * 100
}

# Output the final data frame for ZINB simulation
data.frame(m11, m22, m33, m44, m55, k, oq)

# Examine the maximum proportion of zeros
max(oq)

# Generate examples of simulated data for ZINB and ZIP distributions
z <- rzinb(100, 20, 0.5, 0.94)  # Simulated ZINB data
table(z)

j <- rzip(1e5, 6, 0.33)  # Simulated ZIP data
table(f)

f <- rzinb(100, 20, 0.6, 0.33)
1e5
sample(c(0,1), 100, replace = TRUE)
sample(20,replace=T)
help(rhurdle)
??rhurdle
c<-seq(0,100,5)
v<-rzipois(1000,5,0.1)
m<-rzip(100, lambda = 2,0.3)
j<-rzinb(1000, 1,0.5,0.9)
table(j)
table(m)
hist(v)
hist(z)
table(z)
table(v)
rzinb(100,1,0.001,0.5)