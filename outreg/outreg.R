### Paul Johnson
### Adapted from ideas in post in r-help by Dave Armstrong May 8, 2006


###tight means one column per fitted model
###not tight means 2 columns per fitted model

###incoming= either one regression model or a list of regresion models
###title = a string
###modelLabels= a VECTOR of character strings
### varLabels= a LIST of labels linked to variable names (see examples)
### tight= BOOLEAN, indicates results should be on one tight column or two for each model
### showAIC= BOOLEAN should the AIC be displayed for each model?
### lyx=create a table suitable for inclusion in a lyx float.

outreg <- function(incoming, title="My Regression", label="", modelLabels=NULL, varLabels=NULL, tight=TRUE, showAIC=TRUE, lyx=TRUE){

  modelList <- NULL
  
  ## was input just one model, or a list of models?  ###
  if ( "lm" %in% class(incoming)) { ##just one model input
    nmodels <- 1
    modelList <- list(modl1=incoming)
  } else {
    nmodels <- length(incoming)
    modelList <- incoming
  } 
  
  ##TODO modelLabels MUST have same number of items as "incoming"

  
  ## Get a regression summary object for each fitted model
  summaryList <- list()
  fixnames <- vector()
  myModelClass <- vector()
  
  i <-  1
  for (model in modelList){
    summaryList[[i]] <- summary(model)
    
    fixnames <- unique( c( fixnames, names(coef(model))))
    myModelClass[i] <- class(model)[1]
    i <- i+1
  }
  

  
  ###If you are just using LaTeX, you need these
if (lyx == FALSE){
 cat("\\begin{table}\n ")
  cat("\\caption{",title,"}\\label{",label,"}\n ")
 }
  cat("\\begin{center}\n ")
  nColumns <- ifelse(tight, 1+nmodels, 1 + 2*nmodels) 
  cat(paste("\\begin{tabular}{*{",nColumns,"}{l}}\n ", sep=""))
  cat("\\hline\n ")

  ### Put model labels on top of each model column, if modelLabels were given
  if (!is.null(modelLabels)){
    cat("     ")
    for (modelLabel in modelLabels){
      if (tight == T) {
        cat(paste("&", modelLabel))
      }else{
        cat(paste("&\\multicolumn{2}{c}{",modelLabel,"}",sep=""))
      }
    }
    cat (" \\\\\n ")
  }
  
  ### Print the headers "Estimate" and "(S.E.)", output depends on tight or other format 
  if (tight == T){
    cat("             ")
    for (i in 1:nmodels) { cat (" & Estimate ") }
    cat(" \\\\\n")
    
    cat("             ")
    for (i in 1:nmodels) {  cat (" & (S.E.) ") }
    cat(" \\\\\n")
  }else{
    
    cat("             ")
    for (i in 1:nmodels) { cat (" & Estimate & S.E.") }
    cat(" \\\\\n")
  }

  
  cat("\\hline \n \\hline\n ")


  ### Here come the regression coefficients
  for (regname in fixnames){
    if ( !is.null(varLabels[[regname]]) ) { cat(paste("",varLabels[[regname]]), sep="")}
    else {cat(paste("", regname), sep="")}
   
    for (model in modelList) {
      est <- coef(model)[regname]
      se <- sqrt(diag(vcov(model)))[regname]
      if ( !is.na(est) ) {
        cat (paste("   &   ", round(est,3)))
        pval <- pt(abs(est/se), lower.tail=F, df = model$df.residual)
        if (pval < 0.025) cat("*")
        
        if (tight == F) {
          cat (paste("   &   (", round(se,3),")",sep=""))
        }
      } else {
        cat ("   & . ")
        if (tight == F) cat (" & " )
      }
    }
    cat (" \\\\\n ")
    
    if (tight == T){
      for (model in modelList) {
        est <- coef(model)[regname]
        if (!is.na(est)) cat (paste("   &   (",round(sqrt(diag(vcov(model)))[regname],3)),")",sep="")
        else cat("   &  ")
      }
      cat (" \\\\\n ")
    }
   }
  cat("\\hline \n")


  ### Print a row for the number of cases
  cat(paste("N"), sep="")
  for (model in summaryList) {
    myDF <- sum( model$df[-3] ) #omit third value from df vector
    cat (paste("   &   ", myDF))
    if (tight == F) cat("    &")
  }
  cat (" \\\\\n ")


  ### Print a row for the root mean square error
  if ("lm" %in% myModelClass) {
       cat(paste("$RMSE$"),sep="")
       for (model in summaryList) {
         cat( paste("       &", if(is.numeric(model$sigma)) round(model$sigma,3)))
         if (tight == F) cat("    &")
       }
       cat ("  \\\\\n ")
     }
  
 
  ### Print a row for the R-square
  if ("lm" %in% myModelClass) {
     cat(paste("$R^2$"),sep="")
     for (model in summaryList) {
       cat( paste("       &", if(is.numeric(model$r.square))round(model$r.square,3)))
       if (tight == F) cat("    &")
     }
     cat ("  \\\\\n ")
   }

 
  ## Print a row for the model residual deviance
   if ("glm" %in% myModelClass) {
    cat(paste("$Deviance$"),sep="")
    for (model in summaryList) {
      cat (paste("      &", if(is.numeric(model$deviance))round(model$deviance,3)))
      if (tight == F) cat("      &")
    }
    cat ("  \\\\\n ")
  }

  ### Print a row for the model's fit, as -2LLR
  if ("glm" %in% myModelClass) {    
    cat (paste("$-2LLR (Model \\chi^2)$"),sep="")
    for (model in modelList) {
      if (is.numeric(model$deviance)){
        n2llr <- model$null.deviance - model$deviance 
        cat (paste("      &", round(n2llr,3)))
        gmdf <- model$df.null - model$df.residual + 1
       
        if (pchisq(n2llr, df= gmdf, lower.tail=F) < 0.05) {cat ("*")}
      }
    
      else {
        cat ("    &")
      }
      if (tight == F) cat("      &")
    }
    cat ("  \\\\\n ")
  }



  ## Print a row for the model's fit, as -2 LLR
  ### Can't remember why I was multiplying by -2
  
  if (showAIC == T) {
    cat(paste("$AIC$"),sep="")
    for (model in modelList) {
      cat (paste("      &", if(is.numeric(AIC(model)))round(AIC(model),3)))
      if (tight == F) cat("      &")
    }
    cat ("  \\\\\n ")
  }


  
   cat("\\hline\\hline\n")
   cat ("* $p \\le 0.05$")
   cat("\\end{tabular}\n")
   cat("\\end{center}\n")
   if (lyx == FALSE){ 
      cat("\\end{table}\n")
   }
 }



x1 <- rnorm(100)
x2 <- rnorm(100)
y1 <- 5*rnorm(100)+3*x1 + 4*x2

y2 <- rnorm(100)+5*x2
m1 <- lm (y1~x1)
m2 <- lm (y1~x2)
m3 <- lm (y1 ~ x1 + x2)
gm1 <- glm(y1~x1)



 outreg(m1,title="My One Tightly Printed Regression", lyx=F )


 outreg(m1,tight=F,modelLabels=c("Fingers"), title="My Only Spread Out Regressions" ,lyx=F)
 

        
 outreg(list(m1,m2),modelLabels=c("Mine","Yours"),varLabels=list(x1="Billie"), title="My Two Linear Regressions Tightly Printed" ,lyx=F)


        
 outreg(list(m1,m2),modelLabels=c("Whatever","Whichever"), title="My Two Linear Regressions Not Tightly  Printed", showAIC=F, lyx=F)


 outreg(list(m1,m2,m3),title="My Three Linear Regressions", lyx=F)
        
        
 outreg(list(m1,m2,m3),tight=F,modelLabels=c("I Love love love really long titles","Hate Long","Medium"), lyx=F)

 outreg(list(gm1),modelLabels=c("GLM"), lyx=F)


 outreg(list(m1,gm1),modelLabels=c("OLS","GLM"), lyx=F)


