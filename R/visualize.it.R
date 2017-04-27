visualize.it <-
function(dist='norm', stat = c(0,1), params = list(mu = 0, sd = 1), section = "lower", strict = c(0,1)) {
  dist = visualize.distributions[[casefold(dist)]]
  if(is.null(dist)) stop("Distribution not found.")
  if(length(params) != dist$params) stop("Invalid amount of parameters provided.")
    
  if(length(stat)>1 & (section != "bounded" & section != "tails")){ 
    section = "bounded"; cat("Supplied stat length > 1, reverting to bounded.")
  }
  else if(length(stat)<2 & (section == "bounded" | section == "tails")){ 
    section = "lower"; cat("Supplied stat length < 2, reverting to lower.")
  }
  
  #distribution specific graphing call.
  if(dist$type == "continuous"){ visualize.continuous(dist, stat, params, section)}
  else{     
    
    #Ensures array is inbounds according to conditions
    inequality = if(strict[[1]] == 0) {"equal to"} else {"strict"}
    if(length(strict)<2 & (section == "bounded" | section == "tails")){ 
      strict = c(strict[[1]],strict[[1]]) 
      cat(paste("Supplied strict length < 2, setting inequalities to ", inequality, " inequality."))
    }
    
    visualize.discrete(dist, stat, params, section, strict)
  } 
}
