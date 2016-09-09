best <- function(state, outcome) {
  states <- c("state"=7)
  hospital_name <- c("hospital_name"=2)
  deseases <- c("heart attack"=11, "heart failure"=17, "pneumonia"=23)
  ## Read data
  data <- read.csv("outcome-of-care-measures.csv", stringsAsFactors=FALSE, na.strings="Not Available" )
  
  ## Check state and outcome are valid
  if (state %in% unique(data$State) == FALSE) {
    error(paste(c('Error in best("', state, '", "', outcome, '") : invalid state'), sep=""))
  }
  
  if (outcome %in% names(deseases) == FALSE) {
    error(paste(c('Error in best("', state, '", "', outcome, '") : invalid outcome'), sep=""))
  }
  
  ## Return hospital name in that state with lowest 30-day death
  data <- data[data$State==state, c(hospital_name[1], states[1], deseases[[outcome]])]  # get specific columns, select only rows of specified state
  names(data) <- c(names(hospital_name), names(states), "outcome")
  data <- na.omit(data)
  #data <- split(data, data$state)
  data <- data[order(data$outcome,data$hospital_name),]
  data[1,1]
}