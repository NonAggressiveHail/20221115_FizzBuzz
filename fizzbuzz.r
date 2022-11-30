#Script to do FIzzBuzz!
#Author: Jake Hudson
#Date  : 15 Nov 2022

#wholenumber function
iswholenumber <- function(x) {
 
 return(x == round(x)) 
  
}

iswholenumber(3.3)

#fizzbuzz with a loop
fizzbuzz <- function(min, max, fizz, buzz){
  
  #define output vector
  output <- seq(min, max)
  
  #loop through
  for(i in output) {
    
    tmp <- ""
    
    if(iswholenumber(i/fizz)){
      
      tmp <- paste0(tmp, "fizz")
      
    }
    
    if(iswholenumber(i/buzz)){
      
      tmp <- paste0(tmp, "buzz")
      
    }
    
    if(nchar(tmp) == 0){
      
      val <- i
      
    } else {
      
      val <- tmp
      
    }
    
    output[i] <- val
    
  }
  
  return(output)
    
  }
  
}

#fizzbuzz without a loop!
fizzbuzz2 <- function(min, max, fizz, buzz){
  
  #define output vector
  output <- rep("", min, max)
  
  output <- seq(min, max)
  
  output[iswholenumber(output/fizz)] <- paste0(output, fizz)
  
  #loop through
  for(i in output) {
    
    tmp <- ""
    
    if(iswholenumber(i/fizz)){
      
      tmp <- paste0(tmp, "fizz")
      
    }
    
    if(iswholenumber(i/buzz)){
      
      tmp <- paste0(tmp, "buzz")
      
    }
    
    if(nchar(tmp) == 0){
      
      val <- i
      
    } else {
      
      val <- tmp
      
    }
    
    output[i] <- val
    
  }
  
  return(output)
  
}

comparisons <- list(fizz = 3, buzz = 5, bizz = 7)

#fizzbuzz with multiple options
fizzbuzz3 <- function(min, max, ...){
  
  comparisons <- list(...)
  
  #define output vector
  output <- seq(min, max)
  
  #loop through output vector
  for(i in output){
    
    tmp = ""
    
    #loop through all comparisons
    for(pair in 1:length(comparisons)){
      
      #get word to print
      word <- names(comparisons[pair])
      
      #get value to divide by
      tst  <- comparisons[[pair]]
      
      if(iswholenumber(i/tst)){
        
        tmp <- paste0(tmp, word)
        
      }
    }
    
    if(nchar(tmp) == 0){
      
      val <- i
      
    } else {
      
      val <- tmp
      
    }
    
    output[i] <- val
    
  }
  
  return(output)
  
}

