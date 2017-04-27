visualize.continuous <-
function(dist, stat = c(0,1), params, section = "lower"){  
  #Perform the approriate scales to center the distribution.
  mean = dist$init(params)[[1]];var = dist$init(params)[[2]]
  
  #Do we have a mean and variance to work with?
  if(is.numeric(var)) {
    lb = -3.5*sqrt(var) + mean; ub = 3.5*sqrt(var) + mean
    mean = signif(mean, digits=3); var = signif(var, digits=3)
  } #axis length
  else {
    lb = -4*params[[2]] + params[[1]]; ub = 4*params[[2]] + params[[1]]
  }
 
  #Special scaling case.
  if(dist$name == "Exponential Distribution")
  {
    if(var>1) ub = .75*sqrt(var) + mean
    else ub = 13*sqrt(var) + mean
    lb = -.5; 
  }
  
  #Creates the center title by concatenating various bits of information.
  #This may need to be optimized at a later time.
  graphmain = paste(dist$name," \n")
  for(i in 1:length(params)){
    graphmain = paste(graphmain, dist$varsymbols[i]," = ",params[[i]], " ")
  }
  
  #Generate the initial PDF and plot it.
  x = seq(lb,ub,length=500)
  y = dist$density(x,params)
  plot(x,y, lwd=2, col="ORANGE", type="l", xlab="X Values", ylab="Probability Density", main=graphmain, axes=TRUE)

  #Evaluate based on section type. 
  if(section == "lower"){
    x=seq(lb,stat,length=300)
    y=dist$density(x,params)
    polygon(c(lb,x,stat),c(0,y,0),col="BLUE")
    prob = dist$probability(stat,params)
    subheader = paste("P( ",dist$variable," \u2264 ",stat, ") = ", signif(prob, digits=3))
  }
  else if(section == "bounded"){
    if(length(stat)!= 2) stop("Incorrect Number of Stat Parameters Supplied for Bounded Condition")
    ub = stat[[2]]; lb = stat[[1]]
    i = (x >= lb & x <= ub)
    lines(x, y)
    polygon(c(lb,x[i],ub), c(0,y[i],0), col="BLUE") 
    prob = dist$probability(ub,params) - dist$probability(lb,params)
    subheader = paste("P(",lb," \u2264 ",dist$variable," \u2264 ",ub,") =", signif(prob, digits=3))
  }
  else if(section == "upper"){
    x=seq(stat,ub,length=500)
    y=dist$density(x,params)
    polygon(c(stat,x,ub),c(0,y,0),col="BLUE")
    prob = 1-dist$probability(stat,params)
    subheader = paste("P( ",dist$variable," \u2265 ", stat, " ) =", signif(prob, digits=3))
  }
  else{
    stop("Section not specified. Please choose either lower, bounded, or upper.")
  }
  mtext(subheader,3)
  title(sub = paste("\u03BC = ", mean,", \u03C3\u00B2 = ", var))
}
