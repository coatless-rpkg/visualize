visualize.discrete <-
function(dist, stat = c(0,1), params, section = "lower"){
  
  stat = round(stat)
  if(length(stat)>1 & section != "bounded"){ section = "bounded"; cat("Supplied stat > 1, reverting to bounded.")}
  
  #Perform the approriate scales to center the distribution.
  mean = dist$init(params)[1];var = dist$init(params)[2]
  lb = max(0,round(-3*sqrt(var) + mean)); ub = round(3*sqrt(var) + mean) #axis length

  graphmain = paste(dist$name," \n")
  for(i in 1:length(params)){
    graphmain = paste(graphmain, dist$varsymbols[i]," = ",params[[i]], " ")
  }

  #Generate initial pmf graph
  x = seq(lb,ub,by=1)
  y = dist$density(x,params)
  plot(c(lb,ub),c(0,max(y)), type="n", xlab="Values", ylab="Probability", main=graphmain)
  lines(x, y, type = "h", col="ORANGE")
  abline(h=0,col="RED")
  
  #evaluate based on sections.
  if(section == "lower"){
    x=seq(lb,stat[[1]],1)
    y=dist$density(x,params)
    lines(x,y,type="h",col="BLUE")
    prob = dist$probability(stat,params)
    subheader = paste("P( ",dist$variable," \u2264 ",stat, ") = ", signif(prob, digits=3))
  }
  else if(section == "bounded"){
    if(length(stat)!= 2) stop("Incorrect Number of Stat Parameters Supplied for Bounded Condition")
    upper = stat[[2]]; lower = stat[[1]]
    x = seq(lower,upper,by=1)
    y = dist$density(x,params)
    lines(x,y,type="h",col="BLUE")
    prob = dist$probability(upper,params) - dist$probability(lower-1,params)
    subheader = paste("P(",lower," \u2264 ",dist$variable," \u2264 ",upper,") =", signif(prob, digits=3))
  }
  else if(section == "upper"){
    x=seq(stat[[1]],ub,by=1)
    y=dist$density(x,params)
    lines(x,y,type="h",col="BLUE")
    prob = 1-dist$probability(stat-1,params)
    subheader = paste("P( ",dist$variable," \u2265 ", stat, " ) =", signif(prob, digits=3))
  }
  else{
    stop("Section not specified. Please choose either lower, bounded, or upper.")
  }
  mtext(subheader,3)
  title(sub = paste("\u03BC = ", signif(mean, digits=3),", \u03C3\u00B2 = ",signif(var, digits=3)))
}
