##################################################################
#'@title make_breaks
#'@description The function takes in a raw maximum y value and returns a clean y
#'value divisible by five and clean breaks for the yaxis to fit specifications for
#'the Pinney by BPMG report. Developed by HLB and NS. 
#'@param y Maximum y value from data multiplied by buffer (i.e. 1.1)
#'@return list of clean breaks including maximum Y value that can be specified in
#'y_scale continuous. 
#'

#we want increments that round to 0.05

make_cracks<- function(y){
  
  
  if(y<10000000){
    if(y<0.001){ mult <- 100000 
    }else if(y<0.01){
      mult <- 10000
    } else if(y<0.1){
      mult <- 1000
    } else if(y<1){
      mult <- 100
    } else if(y<10){
      mult <- 10 
    } else if(y<100){
      mult <- 1 
    } else if(y<1000){
      mult <- 0.1 
    } else if(y<10000){
      mult <- 0.01 
    } else if(y<100000){
      mult <- 0.001 
    } else if(y<1000000){
      mult <- 0.0001 }
    else if (y<10000000){
      mult <- 0.00001 }
  }else {
    x <- floor(log(y, base=10)) - 1
    mult <- 1/(10^x)
  }
  
  max <- y * mult
  print(paste0("max: ", max))
  
  scale_fact <- 1
  
  #if (max > 50){
    #max <- ceiling(max/2)
    
    #scale_fact <-  2
  #}
  
  
  print(paste0("max: ", max))
  
  rem_5 <- max%%5
  diff_5 <- 5 - rem_5
  max_5 <- max + diff_5
  
  print(paste0("max_5: ", max_5))
  
  num_breaks <- max_5 / 5
  print(paste0("num_breaks: ", num_breaks))
  
  #additional code to check number of breaks->want to reduce to less than 7
  #if number of breaks is greater than 7 check if divisible by 2 or 3 and divide
  #else add five to max and then divide;
  
  break_min <- function(){
    print("Running break_min")
    if(num_breaks%%3 == 0){
      num_breaks <- num_breaks/3
      print(paste0("New breaks: ", num_breaks))
      return(c(TRUE, )
    }else if (num_breaks%%2 == 0){
      num_breaks <- num_breaks/2
      print(paste0("New breaks: ", num_breaks))
      return(TRUE)
    }else{
      return(FALSE)
    }}


  
  minimized <- FALSE
  print(minimized)
  
  if(minimized == FALSE){
    print("YOOO")
  }
  
  while(minimized == FALSE){
    
    if(num_breaks > 7){
      minimized <- break_min()
      print(minimized)
      
      if (minimized == FALSE) {
        max_5 <- max_5 + 5
        print(paste0("new max: ", max_5))
        num_breaks <- max_new /5
      }
      
    }else{minimized <- TRUE}
  }
    
  print(paste0("final breaks: ", num_breaks))
  
  ymax <- (max_5/(mult))*scale_fact
 
  
  #may need to add in an option for ten breaks again if we find something 
  
  break_size <- ymax/num_breaks
  ybreaks <- seq(0, ymax, by=break_size)
  
  return(ybreaks)
  
}

make_cracks(0.0069)
make_cracks(0.0072)
make_cracks(0.63)
make_cracks(0.81)
make_cracks(0.85)
make_cracks(0.9)

make_cracks(0.41)

make_cracks_v1<- function(y){
  
  
  if(y<10000000){
    if(y<0.001){ mult <- 100000 
    }else if(y<0.01){
      mult <- 10000
    } else if(y<0.1){
      mult <- 1000
    } else if(y<1){
      mult <- 100
    } else if(y<10){
      mult <- 10 
    } else if(y<100){
      mult <- 1 
    } else if(y<1000){
      mult <- 0.1 
    } else if(y<10000){
      mult <- 0.01 
    } else if(y<100000){
      mult <- 0.001 
    } else if(y<1000000){
      mult <- 0.0001 }
    else if (y<10000000){
      mult <- 0.00001 }
  }else {
    x <- floor(log(y, base=10)) - 1
    mult <- 1/(10^x)
  }
  
  max <- y * mult
  print(paste0("max: ", max))
  
  scale_fact <- 1
  
  if (max > 50){
  max <- ceiling(max/2)
  
   scale_fact <-  2
  }
  
  
  print(paste0("max: ", max))
  
  rem_5 <- max%%5
  diff_5 <- 5 - rem_5
  max_5 <- max + diff_5
  
  print(paste0("max_5: ", max_5))
  
  num_breaks <- max_5 / 5
  
  
  print(paste0("num_breaks: ", num_breaks))
  
  ymax <- (max_5/(mult))*scale_fact
  
  
  #may need to add in an option for ten breaks again if we find something 
  
  break_size <- ymax/num_breaks
  ybreaks <- seq(0, ymax, by=break_size)
  
  return(ybreaks)
  
}
y <- 0.09
x <- floor(log(y, base=10)) - 1
mult <- 1/(10^x)
mult

make_cracks_v2<- function(y){
  
  x <- floor(log(y, base=10)) - 1
  mult <- 1/(10^x)
  
  max <- y*mult
  print(paste0("max: ", max))

  rem_5 <- max%%5
  diff_5 <- 5 - rem_5
  max_5 <- max + diff_5
  
  print(paste0("max5: ", max_5))
  
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
    
    print(paste0("max: ", max))
  
    rem_5 <- max%%5
    diff_5 <- 5 - rem_5
    max_5 <- max + diff_5
  
    print(paste0("max_5: ", max_5))
    
    if(max_5%%10 == 0){
      num_breaks <- max_5/10
    }else {
      num_breaks <- max_5 / 5
    }
    
    print(paste0("num_breaks: ", num_breaks))
    
  }

    ymax <- (max_5/(mult))*scale_fact
  
  #may need to add in an option for ten breaks again if we find something 
  
  break_size <- ymax/num_breaks
  ybreaks <- seq(0, ymax, by=break_size)
  
  return(ybreaks)
  
}

make_cracks_v2(0.092)
make_cracks_v2(0.073)
make_cracks_v2(0.098)
make_cracks_v2(0.32)
make_cracks_v2(0.043)
make_cracks_v2(39)

make_cracks_v3<- function(y){
  
  
  if(y<10000000){
    if(y<0.001){ mult <- 100000 
    }else if(y<0.01){
      mult <- 10000
    } else if(y<0.1){
      mult <- 1000
    } else if(y<1){
      mult <- 100
    } else if(y<10){
      mult <- 10 
    } else if(y<100){
      mult <- 1 
    } else if(y<1000){
      mult <- 0.1 
    } else if(y<10000){
      mult <- 0.01 
    } else if(y<100000){
      mult <- 0.001 
    } else if(y<1000000){
      mult <- 0.0001 }
    else if (y<10000000){
      mult <- 0.00001 }
  }else {
    x <- floor(log(y, base=10)) - 1
    mult <- 1/(10^x)
  }
  
  max <- y * mult
  print(paste0("max: ", max))
  
  scale_fact <- 1
  
  #if (max > 50){
  #max <- ceiling(max/2)
  
 # scale_fact <-  2
  #}
  
  
  print(paste0("max: ", max))
  
  rem_5 <- max%%5
  diff_5 <- 5 - rem_5
  max_5 <- max + diff_5
  
  print(paste0("max_5: ", max_5))
  
  num_breaks <- max_5 / 5
  
  
  print(paste0("num_breaks: ", num_breaks))
  
  ymax <- (max_5/(mult))*scale_fact
  
  
  #may need to add in an option for ten breaks again if we find something 
  
  break_size <- ymax/num_breaks
  ybreaks <- seq(0, ymax, by=break_size)
  
  return(ybreaks)
  
}

make_cracks_v2(93)
make_cracks_v2(0.092)

make_breaks_setmod<- function(y){
  
  
  if(y<10000000){
    if(y<0.001){ mult <- 100000 
    }else if(y<0.01){
      mult <- 10000
    } else if(y<0.1){
      mult <- 1000
    } else if(y<1){
      mult <- 100
    } else if(y<10){
      mult <- 10 
    } else if(y<100){
      mult <- 1 
    } else if(y<1000){
      mult <- 0.1 
    } else if(y<10000){
      mult <- 0.01 
    } else if(y<100000){
      mult <- 0.001 
    } else if(y<1000000){
      mult <- 0.0001 }
    else if (y<10000000){
      mult <- 0.00001 }
  }else {
    x <- floor(log(y, base=10)) - 1
    mult <- 1/(10^x)
  }
  
  print(paste0("mult:" , mult))
  
  max <- y * mult
  print(paste0("max: ", max))

  
  rem_5 <- max%%5
  diff_5 <- 5 - rem_5
  max_5 <- max + diff_5
  
  rem_4 <- max%%4
  diff_4 <- 4 - rem_4
  max_4 <- max + diff_4
  
  rem_3 <- max%%3
  diff_3 <- 3 - rem_3
  max_3 <- max + diff_3
  
  
  print(paste0("max_5: ", max_5))
  print(paste0("max_4: ", max_4))
  print(paste0("max_3: ", max_3))
  
  #compare distances to find smallest
  
  if (diff_5 <= diff_4 & diff_5 <= diff_3){
    mod <- 5
  }else if(diff_4 <= diff_5 & diff_4 <=diff_3){
    mod <- 4
  }else if (diff_3 <= diff_5 & diff_3 <= diff_4){
    mod <- 3
  }
  
  #hard code 0.003 to have modulus of 3
  if(y == 0.003){
    mod <- 3
  }
  
  print(paste0("selected mod: ", mod))
  
  if (max%%mod == 0) {
    y_max_scale <- max
  }else{
    rem <-  max%%mod
    dist <- mod - rem
    y_max_scale <- max + dist}
  
  #print(y_max_scale)

  
  ymax <- y_max_scale/(mult)
  print(paste0("calc ymax: ", ymax))
  
  num_breaks <- mod
  print(num_breaks)
  
  
  break_size <- ymax/num_breaks
  ybreaks <- seq(0, ymax, by=break_size)
  
  return(ybreaks)
  
}

make_breaks_setmod(250)

make_breaks<- function(y){
  
  
  if(y<10000000){
    if(y<0.001){ mult <- 100000 
    }else if(y<0.01){
      mult <- 10000
    } else if(y<0.1){
      mult <- 1000
    } else if(y<1){
      mult <- 100
    } else if(y<10){
      mult <- 10 
    } else if(y<100){
      mult <- 1 
    } else if(y<1000){
      mult <- 0.1 
    } else if(y<10000){
      mult <- 0.01 
    } else if(y<100000){
      mult <- 0.001 
    } else if(y<1000000){
      mult <- 0.0001 }
    else if (y<10000000){
      mult <- 0.00001 }
  }else {
    x <- floor(log(y, base=10)) - 1
    mult <- 1/(10^x)
  }
  
  max <- y * mult
  #print(max)
  
  diff_5 <- max%%5
  diff_4 <- max%%4
  diff_3 <- max%%3
  
  print(diff_5)
  print(diff_4)
  print(diff_3)
  
  if (diff_5 <= diff_4 & diff_5 <= diff_3){
    mod <- 5
  }else if(diff_4 <= diff_5 & diff_4 <=diff_3){
    mod <- 4
  }else if (diff_3 <= diff_5 & diff_3 <= diff_4){
    mod <- 3
  }
  
  print(mod)
  
  if (max%%mod == 0) {
    y_max_scale <- max
  }else{
    rem <-  max%%mod
    dist <- mod - rem
    y_max_scale <- max + dist}
  
  print()
  #these numbers don't split nicely, round up
  
  if(y_max_scale == 55 | y_max_scale == 65 | y_max_scale == 85 | y_max_scale == 95){
    y_max_scale = y_max_scale + 5
  }
  
  #print(y_max_scale)
  
  ymax <- y_max_scale/(mult)
  #print(ymax)
  
  if (y_max_scale%%4 ==0){
    num_breaks <- 4
  }else if(y_max_scale%%7 ==0){
    num_breaks <- 7} else if(y_max_scale%%6 ==0){
      num_breaks <- 6}else if(y_max_scale%%8 ==0){
        num_breaks <- 8}else if(y_max_scale%%9 ==0){
          num_breaks <- 9}else if(y_max_scale%%9 ==0){
            num_breaks <- 9}else if(y_max_scale%%3 ==0){
              num_breaks <- 3}else{
                num_breaks <- 5}
  if(y_max_scale == 45){
    num_breaks <- 3
  }
  
  #may need to add in an option for ten breaks again if we find something 
  
  break_size <- ymax/num_breaks
  ybreaks <- seq(0, ymax, by=break_size)
  
  return(ybreaks)
  
}

make_breaks_easy<- function(y){
  
  
  if(y<10000000){
    if(y<0.001){ mult <- 100000 
    }else if(y<0.01){
      mult <- 10000
    } else if(y<0.1){
      mult <- 1000
    } else if(y<1){
      mult <- 100
    } else if(y<10){
      mult <- 10 
    } else if(y<100){
      mult <- 1 
    } else if(y<1000){
      mult <- 0.1 
    } else if(y<10000){
      mult <- 0.01 
    } else if(y<100000){
      mult <- 0.001 
    } else if(y<1000000){
      mult <- 0.0001 }
    else if (y<10000000){
      mult <- 0.00001 }
  }else {
    x <- floor(log(y, base=10)) - 1
    mult <- 1/(10^x)
  }
  
  max <- y * mult
  #print(max)
  
  if (max%%5 == 0) {
    y_max_scale <- max
  }else{
    mod <-  max%%5
    dist <- 5 - mod
    y_max_scale <- max + dist}
  
  
  #may need to add in an option for ten breaks again if we find something 
  
  break_size <- y_max_scale/5
  ybreaks <- seq(0, y_max_scale, by=break_size-1)
  
  return(ybreaks)
  
}

make_breaks_easy(27)
make_breaks_easy(0.0069)
