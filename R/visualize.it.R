#' Visualize's Processing Function
#' 
#' Acts as a director of traffic and first line of error handling regarding
#' submitted visualization requests. This function should only be used by
#' advanced users.
#' 
#' 
#' @param dist a string that should be contain a supported probability
#' distributions name in R. Supported continuous distributions: `"beta"`,
#' `"chisq"`, `"exp"`, `"gamma"`, `"norm"`, and
#' `"unif"`. Supported discrete distributions: `"binom"`,
#' `"geom"`, `"hyper"`, `"nbinom"`, and `"pois"`.
#' @param stat a statistic to obtain the probability from. When using the
#' "bounded" condition, you must supply the parameter as `stat =
#' c(lower_bound, upper_bound)`. Otherwise, a simple `stat =
#' desired_point` will suffice.
#' @param params A list that must contain the necessary parameters for each
#' distribution. For example, `params = list(mu = 1, sd = 1)` would be for
#' a normal distribution with mean 1 and standard deviation 1. If you are not
#' aware of the parameters for the distribution, consider using the
#' `visualize.dist` functions listed under the "See Also" section.
#' @param section Select how you want the statistic(s) evaluated via
#' `section=` either `"lower"`,`"bounded"`, `"upper"`,
#' or`"tails"`.
#' @param strict Determines whether the probability will be generated as a
#' strict (<, >) or equal to (<=, >=) inequality. `strict=` requires
#' either values = 0 or =FALSE for strict OR values =1 or =TRUE for equal to.
#' For bounded condition use: `strict=c(0,1)` or
#' `strict=c(FALSE,TRUE)`.
#' @return Returns a plot of the distribution according to the conditions
#' supplied.
#' @author James Balamuta
#' @export
#' @seealso [visualize.beta()], [visualize.chisq()],
#' [visualize.exp()], [visualize.gamma()],
#' [visualize.norm()], [visualize.unif()],
#' [visualize.binom()], [visualize.geom()],
#' [visualize.hyper()], [visualize.nbinom()],
#' [visualize.pois()].
#' @references http://cran.r-project.org/web/views/Distributions.html
#' @keywords visualize
#' @examples
#' 
#' # Defaults to lower tail evaluation
#' visualize.it(dist = 'norm', stat = 1, list(mu = 3 , sd = 2), section = "lower")
#' 
#' # Set to evaluate the upper tail.
#' visualize.it(dist = 'norm', stat = 1, list(mu=3,sd=2),section="upper")
#' 
#' # Set to shade inbetween a bounded region.
#' visualize.it(dist = 'norm', stat = c(-1,1), list(mu=0,sd=1), section="bounded")
#' 
#' # Gamma distribution evaluated at upper tail.
#' visualize.it(dist = 'gamma', stat = 2, params = list(alpha=2,beta=1),section="upper")
#' 
#' # Binomial distribution evaluated at lower tail.
#' visualize.it('binom', stat = 2, params = list(n=4,p=.5))
#' 
#' 
visualize.it <-
function(dist='norm', stat = c(0,1), params = list(mu = 0, sd = 1), section = "lower", strict = c(0,1)) {
  dist = visualize.distributions[[casefold(dist)]]
  if(is.null(dist)) stop("Distribution not found.\n")
  if(length(params) != dist$params) stop("Invalid amount of parameters provided.\n")
    
  if(length(stat)>1 & (section != "bounded" & section != "tails")){ 
    stop(paste('Supplied stat length > 1 and section="',section,'" requires one statistic. Please resubmit with stat=your_test_statistic.\n'))
  }
  else if(length(stat)<2 & (section == "bounded" | section == "tails")){ 
    stop(paste('Supplied stat length < 2 and section="',section,'" requires two statistics. Please resubmit with stat=c(lower,upper).\n'))
  }

  #distribution specific graphing call.
  if(dist$type == "continuous"){ visualize.continuous(dist, stat, params, section)}
  else{     
    
    #Ensures array is inbounds according to conditions
    inequality = if(strict[[1]] == 0) {"equal to"} else {"strict"}
    if(length(strict)<2 & (section == "bounded" | section == "tails")){ 
      strict = c(strict[[1]],strict[[1]]) 
      cat(paste("Supplied strict length < 2, setting inequalities to ", inequality, " inequality.\n"))
    }
    
    visualize.discrete(dist, stat, params, section, strict)
  } 
}
