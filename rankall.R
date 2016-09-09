rankall <- function(outcome, num = "best") {
  retlist <- list(0)
  hospital <- c("hospital"=2)
  deseases <- c("heart attack"=11, "heart failure"=17, "pneumonia"=23)
  ## Read data
  data <- read.csv("outcome-of-care-measures.csv", stringsAsFactors=FALSE, na.strings="Not Available" )
  
  ## Check state and outcome are valid
  if (outcome %in% names(deseases) == FALSE) {
    error(paste(c('Error in best("', state, '", "', outcome, '") : invalid outcome'), sep=""))
  }
  
  ## Return hospital name in that state with lowest 30-day death
  data <- data[, c(hospital[1], states[1], deseases[[outcome]])]  # get specific columns, select only rows of specified state
  names(data) <- c(names(hospital), names(states), "outcome")
  data <- na.omit(data)
  
  data <- data[order(data$outcome,data$hospital),]
  
  data <- split(data, data$state)
  
  get_item <- function(x, ...) {
    if (num == "best") {
      num <- 1
    }
    else if (num == "worst") {
      num <- dim(x)[1]
    }
    
    l = list(x[num,1])
    names(l) = x[num,2]
    l
  }

  ret <- lapply(data, get_item, num)
  
  data.frame(hospital=unlist(ret), state=names(ret), row.names=names(ret))
}