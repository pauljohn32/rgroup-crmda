## Paul Johnson
## 2010-03-24
####################USE MEMISC PACKAGE to load GSS #########


library(memisc)

### reads SPSS varnames and labels
idat <- spss.system.file("/home/pauljohn/ps/ps706/DataExample-GSS2006/gss2006.sav")
### Caution: Following is huge output!
# codebook(idat)

### Forces T to read whole numbers
idat2 <- as.data.set(idat)

### Watch out:
# names(idat2)
### will show 5137 variables

### Can push that into a standard R data frame
### 
dat <- as.data.frame(idat2)

### free up memory used by idat objects
rm(idat2)
rm(idat)

### R's table function. Blah!
table( dat$vote00)

### If you like crosstabs in SPSS
library(gmodels)

CrossTable(dat$vote00)

CrossTable(dat$vote00 ,dat$sex)

### I like gentable in memisc

genTable( ~ vote00, data=dat)


gt <- genTable( percent(vote00) ~ sex, data=dat )
gt

toLatex(gt)




### Which variables have fewer than 4500 NAP answers
goodVariable <-  apply( dat, 2, function(x) length(x[ x == "NAP"]) < 4500 )

dat <- subset(dat, select=goodVariable)

names(dat)

### Note huge amount of <NAP> in data
sout <- sapply( dat, function(x) length(x[x == "NAP"]))

### which variables have less than 4000 cases NAP
wsout <- which(sout < 4000)

### choose those variables
datnew.memisc <- subset(dat, select= wsout)

### Save to R data frame
save(datnew, file="gss-subset-memisc.Rda")

names(datnew)
