#' Graphing function for Continuous Distributions.
#' 
#' Handles how continuous distributions are graphed. Users should not use this
#' function. Instead, users should use [visualize.it()].
#' 
#' 
#' @param dist contains the distribution from
#' [visualize.distributions()].
#' @param stat a statistic to obtain the probability from. When using the
#' "bounded" condition, you must supply the parameter as `stat =
#' c(lower_bound, upper_bound)`. Otherwise, a simple `stat =
#' desired_point` will suffice.
#' @param params A list that must contain the necessary parameters for each
#' distribution. For example, `params = list(mu = 1, sd = 1)` would be for
#' a normal distribution with mean 1 and standard deviation 1. If you are not
#' aware of the parameters for the distribution, consider using the
#' `visualize.`*dist_name* functions listed under the "See Also"
#' section.
#' @param section Select how you want the statistic(s) evaluated via
#' `section=` either `"lower"`,`"bounded"`, `"upper"`,
#' or`"tails"`.
#' @author James Balamuta
#' @seealso [visualize.it()], [visualize.beta()],
#' [visualize.chisq()], [visualize.exp()],
#' [visualize.gamma()], [visualize.norm()],
#' [visualize.unif()], [visualize.cauchy()]\*,
#' [visualize.f()]\*, [visualize.lnorm()]\*,
#' [visualize.t()]\*, [visualize.wilcox()]\*,
#' [visualize.logis()]\*. \cr \* = added in v2.0.
#' @export
#' @keywords visualize
#' @examples
#' 
#' # Function does not have dist look up, must go through visualize.it
#' visualize.it(dist='norm', stat = c(0,1), params = list(mu = 1, sd = 1), section = "bounded")
#' 
visualize.continuous <-
function(dist, stat = c(0,1), params, section = "lower"){  
  #Perform the approriate scales to center the distribution.
  mean = dist$init(params)[[1]];var = dist$init(params)[[2]]
  
  #maybe open these up in the next release?
  line_width = 3
  line_style = 2
  
  #Do we have a mean and variance to work with?
  if(is.numeric(var)) {
    lb = -3.5*sqrt(var) + mean; ub = 3.5*sqrt(var) + mean
    mean = signif(mean, digits=3); var = signif(var, digits=3)
  } #axis length
  else {
    lb = -4*params[[2]] + params[[1]]; ub = 4*params[[2]] + params[[1]]
  }
  
  if(mean=="Undefined"){
    cat("Warning: df2 < 2, mean is not able to be generated.\n")
  }
  
  #Special axes
  if(dist$name != "Normal Distribution" && 
     dist$name != "Student t Distribution" && 
     dist$name != "Cauchy Distribution" &&
     dist$name != "Wilcox Rank Sum Distribution")
  {
    if(var>1 && is.numeric(var)) {
      ub = .75*sqrt(var) + mean
    }
    else if(!is.numeric(var) && dist$name=="F Distribution"){
      ub=3
      cat("Warning: df2 < 4, variance is not able to be generated.\n")
    }
    else ub = 13*sqrt(var) + mean
    lb = -0.008; 
    if(section=="tails"){
      cat("Warning: Abnormal request for tails condition supplied on nonsymmetric distribution.\n")
    }
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
  plot(x,y, lwd=2, col="black", type="l", xlab=paste(dist$variable,"- Statistic"), ylab="Probability Density", main=graphmain, axes=TRUE)

  
  #Evaluate based on section type. 
  if(section == "lower"){
    #handle cases outside of graph window
    if(lb > stat){
      lb = stat
    }
    path = seq(lb,stat,.01)
    polygon(c(lb,path,stat),
            c(0,dist$density(path,params),0),
            col="Blue", lty=line_style, lwd=line_width, border="Orange")    
    prob = dist$probability(stat,params)
    subheader = paste("P( ",dist$variable," \u2264 ",stat, ") = ", signif(prob, digits=3))
  }
  else if(section == "bounded"){
    start = stat[[1]]; end = stat[[2]];
    path=seq(start,end,.01)
    polygon(c(start,path,end),
            c(0,dist$density(path,params),0),
            col="Blue", lty=line_style, lwd=line_width, border="Orange");   
    prob = dist$probability(end,params) - dist$probability(start,params)
    subheader = paste("P(",start," \u2264 ",dist$variable," \u2264 ",end,") =", signif(prob, digits=3))
  }
  else if(section == "upper"){
    if(ub < stat){
      ub = stat
    }
    path = seq(stat,ub,.01)
    polygon(c(stat,path,ub),
            c(0,dist$density(path,params),0),
            col="Blue", lty=line_style, lwd=line_width, border="Orange")  
    prob = 1-dist$probability(stat,params)
    subheader = paste("P( ",dist$variable," \u2265 ", stat, " ) =", signif(prob, digits=3))
  }
  else if(section == "tails"){
    lower_stat = stat[[1]];upper_stat=stat[[2]];
    #make sure we have the right stats
    if(lower_stat>upper_stat){
      hold = lower_stat
      lower_stat = upper_stat
      upper_stat = hold
    }
    #handle cases outside of graph window
    if(ub < upper_stat){
      ub = upper_stat
    }
    if(lb > lower_stat){
      lb = lower_stat
    }
    
    #generate lower area
    lower_path = seq(lb,lower_stat,.01)
    polygon(c(lb,lower_path,lower_stat),
            c(0,dist$density(lower_path,params),0),
            col="Blue", lty=line_style, lwd=line_width, border="Orange")    
    
    
    #generate upper area
    upper_path = seq(upper_stat,ub,.01)
    polygon(c(upper_stat,upper_path,ub),
            c(0,dist$density(upper_path,params),0),
            col="Blue", lty=line_style, lwd=line_width, border="Orange")
    
    prob = 1-dist$probability(upper_stat,params)+dist$probability(lower_stat,params)
    subheader = bquote(P(.(dist$variable) <= .(lower_stat))+P(.(dist$variable) >= .(upper_stat)) == .(signif(prob, digits=3)))
  }
  else{
    stop("Section not specified. Please choose either lower, bounded, tails, or upper.")
  }
  
  if(length(stat)==1){
    axis(1,at=stat[[1]],labels=bquote(eta[.(stat[[1]])]), line=.69)
  }
  else{
    axis(1,at=stat[[1]],labels=bquote(eta[.(stat[[1]])]), line=.69)
    axis(1,at=stat[[2]],labels=bquote(eta[.(stat[[2]])]), line=.69)
  }
  mtext(subheader,3)
  title(sub = paste("\u03BC = ", mean,", \u03C3\u00B2 = ", var))
}
