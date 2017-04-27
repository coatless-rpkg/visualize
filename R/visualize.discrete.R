visualize.discrete <-
function(dist, stat = c(0,1), params, section = "lower", strict){
  
  stat = round(stat)
  
  #Perform the approriate scales to center the distribution.
  mean = dist$init(params)[1];var = dist$init(params)[2]
  lower_bound = max(0,round(-3*sqrt(var) + mean));
  upper_bound = round(3*sqrt(var) + mean) 
  
  #Builds data for graph
  x = seq(lower_bound,upper_bound,by=1)
  y = dist$density(x,params)
  ymax = max(y)+0.05

  #Creates Graph Title
  graphmain = paste(dist$name," \n")
  for(i in 1:length(params)){
    graphmain = paste(graphmain, dist$varsymbols[i]," = ",params[[i]], " ")
  }
  
  #evaluate based on sections.
  if(section == "lower"){
    if(stat >= lower_bound) region = stat[[1]]-lower_bound+1
    else region = 0
    
    #Build lower tail shade if stat is within bounds.
    i = region-1*(strict[[1]] == 1 & stat >= lower_bound)
    j = abs(region - upper_bound)+1+1*(strict[[1]] == 1 & stat >= lower_bound)
    
    barplot(y, ylim = c(0, ymax), col=c(rep("blue",i),rep("white",j)), axes = FALSE)
    barplot(y, ylim = c(0, ymax), xlab = "Values", ylab = "Probability", names.arg = x, main=graphmain, col=c(rep("orange",i),rep("white",j)), density=c(rep(3,i),rep(0,j)), add = TRUE)
    
    prob = dist$probability(stat-1*(strict[[1]] == 1),params)
    ineqsym = if(strict[[1]]==0){" \u2264 "}else{" < "}
    subheader = paste("P( ",dist$variable, ineqsym ,stat, ") = ", signif(prob, digits=3))
  }
  else if(section == "bounded"){
    if(length(stat)!= 2) stop("Incorrect Number of Stat Parameters Supplied for Bounded Condition")
    disupper = upper = stat[[2]]; dislower = lower = stat[[1]]
    
    #Map the bounds
    if(upper > upper_bound){
      upper = upper_bound
      if(lower > upper_bound) lower = upper_bound
    }
    if(lower < lower_bound){
      lower = lower_bound
      if(upper < lower_bound) upper = lower_bound
    }

    #Calculate necessary adjustments.
    #Figure out whether the stat is outside of the graph display
    bounds_adjust = -1*((dislower < lower_bound & disupper < lower_bound) || (disupper > upper_bound & dislower > upper_bound))
    lower_adjust = 1*(lower_bound == lower & upper_bound != upper);  upper_adjust = 1*(upper_bound == upper & lower_bound != lower)
    #Adjust if inequalities are strict, while checking stat with graph display.
    strict_adjust = -1*(strict[[1]]==1 & bounds_adjust != -1 & lower_adjust != 1) - 1*(strict[[2]]==1 & bounds_adjust != -1 & upper_adjust != 1)

    #Build the grid.
    i = abs(lower-lower_bound) + 1*(strict[[1]]==1 & dislower >= lower_bound)
    j = abs(upper-lower) + 1 + strict_adjust + bounds_adjust
    k = abs(upper_bound-upper) + 1*(strict[[2]]==1 & disupper <= upper_bound)

    #plot the distribution
    barplot(y, ylim = c(0, ymax), col=c(rep("white",i),rep("blue",j),rep("white",k)), axes = FALSE)
    barplot(y, ylim = c(0, ymax), xlab = "Values", ylab = "Probability", names.arg = x, main=graphmain, col=c(rep("white",i),rep("orange",j), rep("white",k)), density=c(rep(0,i),rep(3,j),rep(0,k)), add = TRUE)
    
    #Generate subtitle
    prob = dist$probability(disupper-1*(strict[[2]]==1),params) - dist$probability(dislower-1*(strict[[1]]==0),params)
    if(prob < 0) {prob = 0}
    low_ineq = if(strict[[1]]==0){" \u2264 "}else{" < "}
    upper_ineq = if(strict[[2]]==0){" \u2264 "}else{" < "}
    subheader = paste("P(",dislower,low_ineq,dist$variable,upper_ineq,disupper,") =", signif(prob, digits=3))
  }
  else if(section == "upper"){
    span = upper_bound-lower_bound+1
    if(stat <= upper_bound & stat >= lower_bound) region = upper_bound-stat[[1]]+1
    else if(stat < lower_bound) region = span
    else region = 0

    i = abs(region - (upper_bound-lower_bound+1))+1*(strict[[1]]==1 & stat >= lower_bound) #region-span of graph
    j = region-1*(strict[[1]]==1 & stat <= upper_bound & stat >= lower_bound)

    barplot(y, ylim = c(0, ymax), col=c(rep("white",i),rep("blue",j)), axes = FALSE)
    barplot(y, ylim = c(0, ymax), xlab = "Values", ylab = "Probability", names.arg = x, main=graphmain, col=c(rep("white",i),rep("orange",j)), density=c(rep(0,i),rep(3,j)), add = TRUE)
    prob = 1-dist$probability(stat-1*(strict[[1]] == 0),params)
    ineqsym = if(strict[[1]]==0){" \u2265 "}else{" > "}
    subheader = paste("P( ",dist$variable, ineqsym, stat, " ) =", signif(prob, digits=3))
  }
  else{ stop("Section not specified. Please choose either lower, bounded, or upper.") }
  mtext(subheader,3)
  title(sub = paste("\u03BC = ", signif(mean, digits=3),", \u03C3\u00B2 = ",signif(var, digits=3)))
}
