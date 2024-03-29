#' Graphing function for Discrete Distributions.
#' 
#' Handles how discrete distributions are graphed. Users should not use this
#' function. Instead, users should use \code{link{visualize.it}}.
#' 
#' 
#' @param dist contains the distribution from
#' \code{link{visualize.distributions}}.
#' @param stat a statistic to obtain the probability from. When using the
#' "bounded" condition, you must supply the parameter as `stat =
#' c(lower_bound, upper_bound)`. Otherwise, a simple `stat =
#' desired_point` will suffice.
#' @param params A list that must contain the necessary parameters for each
#' distribution. For example, `params = list(n = 5, prob = .25)` would be
#' for a binomial distribution with size 5 and probability .75. If you are not
#' aware of the parameters for the distribution, consider using the
#' `visualize.`*dist_name* functions listed under the "See Also"
#' section.
#' @param section Select how you want the statistic(s) evaluated via
#' `section=` either `"lower"`,`"bounded"`, `"upper"`,
#' or`"tails"`.
#' @param strict Determines whether the probability will be generated as a
#' strict (<, >) or equal to (<=, >=) inequality. `strict=` requires
#' either values = 0 or =FALSE for equal to OR values =1 or =TRUE for strict.
#' For bounded condition use: `strict=c(0,1)` or
#' `strict=c(FALSE,TRUE)`.
#' @author James Balamuta
#' @seealso [visualize.it()], [visualize.binom()],
#' [visualize.geom()], [visualize.hyper()],
#' [visualize.nbinom()], [visualize.pois()].
#' @export
#' @keywords visualize
#' @examples
#' 
#' # Function does not have dist look up, must go through visualize.it
#' visualize.it(dist='geom', stat = c(2,4), params = list(prob = .75), section = "bounded",
#'           strict = c(0,1))
#' 
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
  for(i in seq_along(params)){
    graphmain = paste(graphmain, dist$varsymbols[i]," = ",params[[i]], " ")
  }
  
  #evaluate based on sections, rewrite v5?
  if(section == "lower"){
    if(stat >= lower_bound) region = stat[[1]]-lower_bound+1
    else region = 0
    
    #Build lower tail shade if stat is within bounds.
    i = region-1*(strict[[1]] == 1 & stat >= lower_bound)
    j = abs(region - upper_bound)+1+1*(strict[[1]] == 1 & stat >= lower_bound)
    
    barplot(y, ylim = c(0, ymax), col=c(rep("blue",i),rep("white",j)), axes = FALSE)
    barplot(y, ylim = c(0, ymax), xlab = "Values", ylab = "Probability", names.arg = x, main=graphmain, col=c(rep("orange",i),rep("white",j)), density=c(rep(3,i),rep(0,j)), add = TRUE)
    
    prob = dist$probability(stat-1*(strict[[1]] == 1),params)
    ineqsym = if(strict[[1]]==0){" <= "}else{" < "}
    subheader = bquote(P( .(as.name(dist$variable)) ~ .(as.name(ineqsym)) ~ .(as.name(stat)) ) == ~.(signif(prob, digits=3)))
  }
  else if(section == "bounded"){
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
    low_ineq = if(strict[[1]]==0){" <= "}else{" < "}
    upper_ineq = if(strict[[2]]==0){" <= "}else{" < "}
    
    subheader = bquote(P( .(as.name(dislower)) ~ .(as.name(low_ineq)) ~ .(as.name(dist$variable)) ~ .(as.name(upper_ineq)) ~ .(as.name(disupper))  ) ==  .(signif(prob, digits=3)))
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
    ineqsym = if(strict[[1]]==0){">="}else{">"}
    subheader = bquote(P( .(as.name(dist$variable)) ~ .(as.name(ineqsym)) ~ .(as.name(stat)) ) == .(signif(prob, digits=3)))
  }
  else if(section == "tails") #implemented in v4.0
  {
    #set separate stats
    lower_stat = stat[[1]]; upper_stat = stat[[2]]
    
    #set strict values based on 1/0s or true/falses
    if(strict[[1]]) lower_strict = 1
    else lower_strict = 0
    if(strict[[2]]) upper_strict = 1
    else upper_strict = 0
    
    #determine the span
    span = upper_bound-lower_bound + 1
    
    #handle the case specific errors
    if(lower_stat >= lower_bound) lower_region = lower_stat - lower_bound + 1 - 1*(lower_strict==1)
    else if(lower_stat >= upper_bound) lower_region = span
    else lower_region = 0 #lower_stat < lower_bound
    
    if(upper_stat <= upper_bound & upper_stat >= lower_bound) 
          upper_region = upper_stat-lower_bound+1-1*(upper_strict==1)
    else upper_region = span #upper_stat < lower_bound
    
    #builds counts
    i = lower_region
    j = upper_region-i-1+2*(upper_strict==1)
    k = span-i-j

    #build graphs
    barplot(y, ylim = c(0, ymax), col=c(rep("blue",i),rep("white",j),rep("blue",k)), axes = FALSE)
    barplot(y, ylim = c(0, ymax), xlab = "Values", ylab = "Probability", names.arg = x, main=graphmain,
            col=c(rep("orange",i),rep("white",j), rep("orange",k)), density=c(rep(3,i),rep(0,j),rep(3,k)),
            add = TRUE)
    
    #handle legend information
    prob = 1-dist$probability(upper_stat-1*(!upper_strict),params)+dist$probability(lower_stat-1*(lower_strict),params)
    upper_ineqsym = if(!upper_strict){">="}else{">"}
    lower_ineqsym = if(!lower_strict){"<="}else{"<"}
    subheader = bquote(P( .(as.name(dist$variable)) ~ .(as.name(lower_ineqsym)) ~ .(as.name(lower_stat)) ) + P( .(as.name(dist$variable)) ~ .(as.name(upper_ineqsym)) ~ .(as.name(upper_stat))  ) == .(signif(prob, digits=3)))
  }
  else{ stop("Section not specified. Please choose either: `lower`, `bounded`, `tails`, or `upper`.") }
  
  if(length(stat)==1){
    axis(1,at=i+1,labels=bquote(eta[.(stat[[1]])]), line=.69)
  }
  else{
    axis(1,at=i+1,labels=bquote(eta[.(stat[[1]])]), line=.69)
    axis(1,at=i+j+2,labels=bquote(eta[.(stat[[2]])]), line=.69)
  }
  
  mtext(subheader,3)
  title(sub = bquote(mu ~ "=" ~ .(signif(mean, digits=3)) ~ ", " ~ sigma^2 ~ "=" ~ .(signif(var, digits=3))) )
}
