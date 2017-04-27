visualize.it <-
function(dist='norm', stat = c(0,1), params = list(mu = 0, sd = 1), section = "lower") {
  dist = visualize.distributions[[casefold(dist)]]
  if(is.null(dist)) stop("Distribution not found")
  if(length(params) != dist$params) stop("Invalid amount of parameters provided.")
  if(dist$type == "continuous") visualize.continuous(dist, stat, params, section)
  else visualize.discrete(dist, stat, params, section)
}
