################
## Extremely crude and only to be used as a backup, but it works. Makes Bo great again. 
## Changed: 24/11/2019
################

modelSimilarByInteraction <- function(model, models){

if(identical(model, models[[1]])){
  number <- sample(5:6, 1)
  
  similarModel <- models[[number]]
}

else if(identical(model, models[[2]])){
  number <- 5
  
  similarModel <- models[[number]]
}

else if(identical(model, models[[3]])){
  number <- 6
  
  similarModel <- models[[number]]
}

else if(identical(model, models[[4]])){
  number <- sample(7:9, 1)
  
  similarModel <- models[[number]]
}

else if(identical(model, models[[5]])){
  number <- sample(c(2,10,11), 1)
  
  similarModel <- models[[number]]
}

else if(identical(model, models[[6]])){
  number <- sample(c(3,10,12), 1)
  
  similarModel <- models[[number]]
}

else if(identical(model, models[[7]])){
  number <- sample(c(4,10,11), 1)
  
  similarModel <- models[[number]]
}

else if(identical(model, models[[8]])){
  number <- sample(c(4,10,12), 1)
  
  similarModel <- models[[number]]
}

else if(identical(model, models[[9]])){
  number <- sample(c(4,11,12), 1)
  
  similarModel <- models[[number]]
}

else if(identical(model, models[[10]])){
  number <- sample(c(7,8,13), 1)
  
  similarModel <- models[[number]]
}

else if(identical(model, models[[11]])){
  number <- sample(c(7,9,13), 1)
  
  similarModel <- models[[number]]
}

else if(identical(model, models[[12]])){
  number <- sample(c(8,9,13), 1)
  
  similarModel <- models[[number]]
}

else if(identical(model, models[[13]])){
  number <- sample(c(10, 11, 12, 14), 1)
  
  similarModel <- models[[number]]
}

else if(identical(model, models[[14]])){
  number <- 13
  
  similarModel <- models[[number]]
}
  
return(similarModel)
  
}
