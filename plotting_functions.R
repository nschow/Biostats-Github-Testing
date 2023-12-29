##################################################################
#'@title make_breaks
#'@description THIS IS AN UPDATED version of the make_breaks function
#'The function takes in a raw maximum y value and returns a clean y
#'value divisible by five or 10 and clean breaks for the yaxis to fit specifications for
#'the Pinney by BPMG report. Developed by HLB and NS. 
#'@param y Maximum y value from data multiplied by buffer (i.e. 1.1)
#'@return list of clean breaks including maximum Y value that can be specified in
#'y_scale continuous. 

make_breaks<- function(y, print=FALSE){
  
  x <- floor(log(y, base=10)) - 1
  mult <- 1/(10^x)
  
  max <- y*mult
  
  if(print == TRUE){
    print(paste0("max: ", max))
  }
  
  rem_5 <- max%%5
  diff_5 <- 5 - rem_5
  max_5 <- max + diff_5
  
  if(print == TRUE){
    print(paste0("max5: ", max_5))
  }
  scale_fact <-  1
  
  #first check special cases:
  if (max_5 == 100){
    num_breaks <- 5
  }else if(max_5 %in% c(90, 75, 45)){
    num_breaks <- 3
  }else if(max_5%%10 == 0){
    num_breaks <- max_5/10
  }else{
    
    if (max > 50){
      max <- ceiling(max/2)
      
      scale_fact <-  2
    }
    
    if(print == TRUE){
      print(paste0("max: ", max))
    }
    
    rem_5 <- max%%5
    diff_5 <- 5 - rem_5
    max_5 <- max + diff_5
    
    if(print == TRUE){
      print(paste0("max_5: ", max_5))
    }
    
    if(max_5%%10 == 0){
      num_breaks <- max_5/10
    }else {
      num_breaks <- max_5 / 5
    }
    
    if(print == TRUE){
      print(paste0("num_breaks: ", num_breaks))
    }
  }
  
  ymax <- (max_5/(mult))*scale_fact
  
  #may need to add in an option for ten breaks again if we find something 
  
  break_size <- ymax/num_breaks
  ybreaks <- seq(0, ymax, by=break_size)
  
  return(ybreaks)
  
}

