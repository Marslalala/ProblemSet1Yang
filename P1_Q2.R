###Problem2


## Part a

#  Establish the function
isPerfectPower <- function(n, p){
  #input: n - an integer, the number to check
  #input: p - an integer, the specified power
  
  # Define the root 
  if (p < 2){
    return('p must be an integer greater than 1')
  }
  if (n < 0 && p %% 2 == 0) {
    return(list(isPerfect = FALSE, root = paste(n, 'is not a perfect power of', p)))
  } else if (n < 0) {
    r <- -((-n)^(1 / p))
  } else if (n == 0){
    return(list(isPerfect = TRUE, root = 0))
  } else {
    r <- n^(1 / p)
  }
  tol <- 1e-10    # Set a tolerance to deal with floating power double numeric precision
  
  if(n %% 1 != 0 | p %% 1 != 0){  # Check if n and p are integers
    return(list(isPerfect = FALSE, wrongArgument = 'Please enter integers for arguments'))
  }
  
  if (-tol < n %% r && n %% r < tol){  # Set the checking conditions and check
    return(list(isPerfect = TRUE, root = r))
  } else {
    return(list(isPerfect = FALSE, root = paste(n, 'is not a perfect power of', p)))
  }
  
}


## Part b

#  Establish the function
findRootPower <- function(n){
  #input: n - an integer, the number to check
  
  if(n == 0){
    return(paste(n, '=', 0, '^', 2))
  }
  
  high_p <- 70
  r_vec <- c()
  p_vec <- c()
  for (i in 2:high_p){    # Start from 2 since when power = 1, n itself is a perfect power
    if(isPerfectPower(n, i)$isPerfect){
      r_vec <- append(r_vec, isPerfectPower(n, i)$root)
      p_vec <- append(p_vec, i)
      
    }
  }
  
  if(is.null(p_vec)){
    return(paste(n, 'is not a perfect power'))
    
  } else {
    r <- max(r_vec)
    p <- min(p_vec)
    return(paste(n, '=', r, '^', p))
  }
  
}

#  Test cases
findRootPower(27)               #Perfect power; root = 3, power = 3
findRootPower(13060694016)
findRootPower(7776)
findRootPower(170859375)
findRootPower(58247422)
findRootPower(94143178827)      #Perfect power; root = 3, power = 23