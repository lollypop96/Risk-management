library(ggplot2)
library(plyr)
library(dplyr)
library(qcc)
library(QRM)
library(tibble)
library(sqldf)
library(xlsx)

usa = read.csv("C:/Users/liry9/Desktop/Risk Management/First Assignment/Dataset/us_accidents.csv", header=TRUE)

## We keep only the id, the city and the county
usa2 <- usa[c("ID","City","County")]

usacall <- sqldf("SELECT  City, COUNT(*) AS Count
              FROM usa2
              GROUP BY City
              ORDER BY Count DESC")

usacall <- usacall %>% mutate(id = row_number())

## We rename the columns with easier names
colnames(usacall)[1] <- "city"
colnames(usacall)[2] <- "total"
colnames(usacall)[3] <- "rank"

rm(usa)
rm(usa2)

## We add a column called "cum_sum" with the cumulative sums
usacall$cum_sum <- cumsum(usacall$total)

# Write the first data set in a new workbook
# write.csv(usacall,"C:/Users/liry9/Desktop/Risk Management/First Assignment/usa_accidents_for_excel.csv", row.names = FALSE)

## Add two columns with the logs of ranks and of the totals
usacall$log_rank <- log(usacall$rank)
usacall$log_total <- log(usacall$total)

### We also plot our data using the log-log scale which is useful to
# prove again if our distribution behaves like a Power-Law
plot(x=usacall$log_total, y=usacall$log_rank, col="blue")

### We start off by plotting the Mean Excess Function (ME)
# Mean excess function plot
MEplot(usacall$total)

# This is seful to determine if our distribution is heavy-tailed. In our
# case we can denote that it is heavy-tailed.

### Based on the ME plot we could set 3 to 4 thresholds at 6.000, 9.000, 11.000
# and 20.000
u_6000 <- 6000
u_8000 <- 8000
u_11000 <- 11000
u_18500 <- 18500
 
abline(v= c(u_6000,u_8000,u_11000,u_18500), col="blue")

#####################################################################################
                                    ### GDP model ###
#####################################################################################
##### We fit now with the fit.GPD function a generalized pareto distribution
# to our data. This is done via mle.
### GPD model
mod_u6000=fit.GPD(usacall$total, threshold=u_6000)
mod_u8000=fit.GPD(usacall$total, threshold=u_8000)
mod_u11000=fit.GPD(usacall$total, threshold=u_11000)
mod_u18500=fit.GPD(usacall$total, threshold=u_18500)

### We want to check now which one among the selected thresholds is the best one 
## Threshold at 6.000
xi_u6000=as.numeric(mod_u6000$par.ests[1])
beta_u6000=as.numeric(mod_u6000$par.ests[2])
 
## Threshold at 8.000
xi_u8000=as.numeric(mod_u8000$par.ests[1])
beta_u8000=as.numeric(mod_u8000$par.ests[2])
 
## Threshold at 11.000
xi_u11000=as.numeric(mod_u11000$par.ests[1])
beta_u11000=as.numeric(mod_u11000$par.ests[2])
 
## Threshold at 18.500 
xi_u18500=as.numeric(mod_u18500$par.ests[1])
beta_u18500=as.numeric(mod_u18500$par.ests[2])
 
xi_and_beta <- matrix(c(xi_u6000,beta_u6000,xi_u8000,beta_u8000,xi_u11000,beta_u11000,xi_u18500,beta_u18500), ncol=4)
colnames(xi_and_beta) <- c('u6000', 'u8000', 'u11000', 'u18500')
rownames(xi_and_beta) <- c('xi', 'beta')
xi_and_beta_table <- as.table(xi_and_beta)
xi_and_beta_table

#####################################################################################
                          ### Fitted GDP vs Empirical GDP ###
#####################################################################################
### We chose thresholds equal to 6000 and 8000, beacuse they are respectively
# the first and second kink of our Mean Excess Function.
# We have positive Xis, therefore our distribution behaves like a Pareto.
# And we can also control the values of the Betas.
## Threshold u=6.000
plotFittedGPDvsEmpiricalExcesses(usacall$total, threshold = u_6000)
legend("bottomright", bty = "n", lty = 0:1, pch = c(19, NA),
       col = c("blue","black"), legend= c('Empirical Excess df','Theoretical excell df GDP'))

## Threshold u=8.000
plotFittedGPDvsEmpiricalExcesses(usacall$total, threshold = u_8000)
legend("bottomright", bty = "n", lty = 0:1, pch = c(19, NA),
       col = c("blue","black"), legend= c('Empirical Excess df','Theoretical excell df GDP'))

## Threshold u=11.000
plotFittedGPDvsEmpiricalExcesses(usacall$total, threshold = u_11000)
legend("bottomright", bty = "n", lty = 0:1, pch = c(19, NA),
       col = c("blue","black"), legend= c('Empirical Excess df','Theoretical excell df GDP'))

## Threshold u=18.500
plotFittedGPDvsEmpiricalExcesses(usacall$total, threshold = u_18500)
legend("bottomright", bty = "n", lty = 0:1, pch = c(19, NA),
       col = c("blue","black"), legend= c('Empirical Excess df','Theoretical excell df GDP'))

### Now we perform a simultaneous comparison of different GPD models
# with a Confidence interval CI=0.95 (by default)
xiplot(usacall$total, models=50, start=mod_u6000$n.exceed, end=mod_u8000$n.exceed, reverse=TRUE)

# Threshold vs exceedences: the trade off

#####################################################################################
                                  ### QQplots ###
#####################################################################################
### In order to check now if the selected thresholds (at 6.000 and at
# 8.000) are the right ones we resolve to qqplots.
# Then we will plot the QQplot of quantile function of excedences
# u6000 compared to the hypotetical quantile function of GPD.
## QQplot of u=6.000
qf_u6000 <- function(p) 
qGPD(p, xi_u6000, beta_u6000)
 
excess_u6000 <- sort(usacall$total[usacall$total > u_6000] - u_6000)
N_6000 = mod_u6000$n.exceed
est_q6000 <- qf_u6000(c(1:N_6000)/(N_6000+1))
 
plot(excess_u6000, est_q6000)
abline(lm(est_q6000[1:(N_6000-3)]~excess_u6000[1:(N_6000-3)]), col="blue")

## QQplot of quantile function of excedences of u8000 compared to the 
# hypotetical quantile function of GPD
qf_u8000 <- function(p) # quantile function of df
qGPD(p, xi_u8000, beta_u8000)

excess_u8000 <- sort(usacall$total[usacall$total > u_8000] - u_8000)
N_8000 = mod_u8000$n.exceed
est_q8000 <- qf_u8000(c(1:N_8000)/(N_8000+1))

plot(excess_u8000, est_q8000)
abline(lm(est_q8000[1:(N_8000-3)]~excess_u8000[1:(N_8000-3)]), col="red")

## To understand if our distribution behaves like a power tail,
# we plot our data in Log-Log Scale
plot(x=usacall$log_total, y=usacall$log_rank, col="blue")

#####################################################################################
                  ### Comparison between quantile levels ###
#####################################################################################
### The riskmeasures function calculates Quantiles And Expected Shortfalls
# Makes a rapid calculation of point estimates of prescribed quantiles and
# expected shortfalls using the output of the function gpd.
### Threshold at 6.000:
RiskMeasures(mod_u6000, p = c(0.99, 0.995))

## The VaR (0,990) of our ditribution considering u = 6.000, tells us that 
# there is a 1% of probability that, in 3 years, there will be more 
# than 1.934,511 car accidents in any City of the Database.
## The VaR (0,995) of our ditribution considering u = 6.000, tells us that 
# there is a 0,05% of probability that, in 3 years, there will be more 
# than 7.286,135 car accidents in any City of the Database.

### Threshold at 8.000
RiskMeasures(mod_u8000, p = c(0.99, 0.995))

## The VaR (0,990) of our ditribution considering u8000, tells us that 
# there is a 1% of probability that, in 3 years, there will be more 
# than 1711,661 car accidents in any City of the Database.
## The VaR (0,995) of our ditribution considering u8000, tells us that 
# there is a 0,05% of probability that, in 3 years, there will be more 
# than 7194,097 car accidents in any City of the Database.

#####################################################################################
        ### Simultaneous comparison of VaRs under different GPD models ###
#####################################################################################
### This graph will tell us how much influence had the choice of the threshold in 
# the VaR computation.
VaRplot<-function(data=usacall$total,ab=c(6000,8000))
{alpha=0.99
a=c(ab[1]:ab[2])
for(i in c(ab[1]:ab[2]))
{
  a[i-ab[1]+1]=RiskMeasures(fit.GPD(data,threshold=i), alpha)[[2]]
}
plot(c(ab[1]:ab[2]),a,type="l", xlab="Thresholds",ylab=paste("VaR -",alpha))
}
VaRplot()
abline(h = 4894.956, lty = 3) #Var estimate under u6000
abline(h = 2792.148, lty = 3) #Var estimate under u8000

#####################################################################################
### Lastly we can plot the confidence intervals.
# Estimated Tail Probabilities
showRM(mod_u6000, alpha=0.99, RM="VaR" , method="BFGS")


showRM(mod_u8000, alpha=0.99, RM="ES", method="BFGS")

#####################################################################################