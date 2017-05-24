logoddsFnc <- function(data_ind, data_dep, ind_varname, min.count=1){
  
  # Assumptions: x & y are numeric vectors of the same 
  # length, y is 0/1 varible.  This returns a vector
  # of breaks of the x variable where each bin has at 
  # least min.countnumber of y's
  bin.by.other.count <- function(x, other, min.cnt=1) {
    csum <- cumsum(tapply(other, x, sum))
    breaks <- numeric(0)
    
    i <- 1
    breaks[i] <- as.numeric(names(csum)[1])
    cursum <- csum[1]
    
    for ( a in names(csum) ) {
      if ( csum[a] - cursum >= min.cnt ) {
        i <- i + 1
        breaks[i] <- as.numeric(a)
        cursum <- csum[a]
      }
    }
    
    breaks
  }
  
  brks <- bin.by.other.count(data_ind, data_dep, min.cnt=min.count)
  
  # Visualizing binary categorical data
  var_cut <- cut(data_ind, breaks=brks, include.lowest=T)
  var_mean <- tapply(data_dep, var_cut, mean)
  var_median <- tapply(data_ind, var_cut, median)
  
  mydf <- data.frame(ind=data_ind, dep=data_dep)
  fit <- glm(dep ~ ind, data=mydf, family=binomial())
  pred <- predict(fit, data.frame(ind=min(data_ind):max(data_ind)), 
                  type="response", se.fit=T)
  
  # Plot 
  plot(x=var_median, y=var_mean, ylim=c(0,1.15), 
       xlab=ind_varname, ylab="Exp Prob", pch=21, bg="black")
  stripchart(data_ind[data_dep==0], method="stack", 
             at=0, add=T, col="grey")
  stripchart(data_ind[data_dep==1], method="stack", 
             at=1, add=T, col="grey")
  
  lines(x=min(data_ind):max(data_ind), 
        y=pred$fit, col="blue", lwd=2)
  lines(lowess(x=var_median, 
               y=var_mean, f=.30), col="red")
  
  lines(x=min(data_ind):max(data_ind), 
        y=pred$fit - 1.96*pred$se.fit, lty=2, col="blue")
  lines(x=min(data_ind):max(data_ind), 
        y=pred$fit + 1.96*pred$se.fit, lty=2, col="blue")
}




logoddsFnc(icu$age, icu$died, "age", min.count=3)
