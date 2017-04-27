visualize.discrete <-
function(dist, stat = c(0,1), params, section = "lower"){
  stat = round(stat)
  
  #Perform the approriate scales to center the distribution.
  mean = dist$init(params)[1];var = dist$init(params)[2]
  lower_bound = max(0,round(-3*sqrt(var) + mean));
  upper_bound = round(3*sqrt(var) + mean) #axis length
  
  x = seq(lower_bound,upper_bound,by=1)
  y = dist$density(x,params)
  ymax = max(y)+0.05

  graphmain = paste(dist$name," \n")
  for(i in 1:length(params)){
    graphmain = paste(graphmain, dist$varsymbols[i]," = ",params[[i]], " ")
  }
  
  #evaluate based on sections.
  if(section == "lower"){
    if(stat >= lower_bound) region = length(seq(lower_bound,stat[[1]],1))
    else region = 0
    #Build lower tail shade if stat is within bounds.
    i = region
    j = abs(region - upper_bound)+1
    
    barplot(y, ylim = c(0, ymax), col=c(rep("blue",i),rep("white",j)), axes = FALSE)
    barplot(y, ylim = c(0, ymax), xlab = "Values", ylab = "Probability", names.arg = x, main=graphmain, col=c(rep("orange",i),rep("white",j)), density=c(rep(3,i),rep(0,j)), add = TRUE)
    
    prob = dist$probability(stat,params)
    subheader = paste("P( ",dist$variable," \u2264 ",stat, ") = ", signif(prob, digits=3))
  }
  else if(section == "bounded"){
    if(length(stat)!= 2) stop("Incorrect Number of Stat Parameters Supplied for Bounded Condition")
    disupper = upper = stat[[2]]; dislower = lower = stat[[1]]
    
    #Map the bounds
    if(upper > upper_bound){
      upper = upper_bound
      if(lower > upper_bound) lower = upper_bound
    }
    else if(lower < lower_bound){
      lower = lower_bound
      if(upper < lower_bound) upper = lower_bound
    }
    
    #Build the grid.
    i = length(seq(lower_bound,lower,by=1)) - 1
    j = length(seq(lower,upper,by=1)) - 1*(dislower < lower_bound & disupper < lower_bound || disupper > upper_bound & dislower > upper_bound)
    k = length(seq(upper,upper_bound,by=1)) - 1
    
    #plot the distribution
    barplot(y, ylim = c(0, ymax), col=c(rep("white",i),rep("blue",j),rep("white",k)), axes = FALSE)
    barplot(y, ylim = c(0, ymax), xlab = "Values", ylab = "Probability", names.arg = x, main=graphmain, col=c(rep("white",i),rep("orange",j), rep("white",k)), density=c(rep(0,i),rep(3,j),rep(0,k)), add = TRUE)
    
    #Generate subtitle
    prob = dist$probability(disupper,params) - dist$probability(dislower-1,params)
    subheader = paste("P(",dislower," \u2264 ",dist$variable," \u2264 ",disupper,") =", signif(prob, digits=3))
  }
  else if(section == "upper"){
    if(stat <= upper_bound) region = length(seq(stat[[1]],upper_bound,1))
    else region = 0
    i = abs(region -(upper_bound-lower_bound+1)) #region-span of graph
    j = region
    barplot(y, ylim = c(0, ymax), col=c(rep("white",i),rep("blue",j)), axes = FALSE)
    barplot(y, ylim = c(0, ymax), xlab = "Values", ylab = "Probability", names.arg = x, main=graphmain, col=c(rep("white",i),rep("orange",j)), density=c(rep(0,i),rep(3,j)), add = TRUE)
    prob = 1-dist$probability(stat-1,params)
    subheader = paste("P( ",dist$variable," \u2265 ", stat, " ) =", signif(prob, digits=3))
  }
  else{
    stop("Section not specified. Please choose either lower, bounded, or upper.")
  }
  mtext(subheader,3)
  title(sub = paste("\u03BC = ", signif(mean, digits=3),", \u03C3\u00B2 = ",signif(var, digits=3)))
}
