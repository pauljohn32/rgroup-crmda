## Title: Bootstrapping mediation
## Author: Alexander M. Schoemann <schoemann@ku.edu>
## Date posted: 02/17/12
## Depends: boot
## Description: This is an example of how to bootstrap 
## an indirect effect, it includes a function to be passed
## to the boot library and an example. 
## ----------------------------------------------------------------------------------------------------


## bootMed: a function to bootstrap an indirect effect in regression
## data = data to be used
## dv = name of the dependent variable in data (character)
## iv = name of the independent variable in data (character)
## med = name of the mediating variable in data (character)
## numBoot = number of bootstrap samples to draw
## returns = a boot object, can use boot.ci to get 
## confidence intervals for the indirect effect


bootMed <- function(data, dv, iv, med, numBoot) {
require(boot)

#make sure variable names are characters
# dv <- 'advance'
 # iv <- 'complaints'
 # med <- 'learning'
 # numBoot=50

#Create function to pass to boot
medReg <- function(data, i, y, x, md){

d <- data[i,]

#Regress M on X
m1 <- lm(as.formula(paste(md, '~', x)), data = d)

#Regress Y on X and M
m2 <- lm(as.formula(paste(y, '~', x, '+', md)), data = d)

ab <- (coef(m1)[2])*(coef(m2)[3])

return(as.numeric(ab))
}
#Bootstrap
medBoot <- boot(data, statistic=medReg, R=numBoot, sim = "ordinary", stype = "i", y=dv, x=iv, md=med)
return(medBoot)
}


##Example
summary(attitude)

test <- bootMed(data=attitude, dv='advance', iv='complaints', med='learning', numBoot=50)


## Get confidence intervals
boot.ci(medBoot)
