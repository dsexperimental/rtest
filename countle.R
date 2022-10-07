target <- 666
numbers <- c(25,50,75,100,6,1)
ops <- factor(c("+","-","*","/"))
stepStates <- factor(c("in-process","success","failed")


## This function solves the equation given two numbers and an operation
solveEquation <- function(n1,n2,op) {
  if(op == "+") {
    return(n1 + n2)
  }
  else if(op == "-") {
    return(n1-n2)
  }
  else if(op == "*") {
    return(n1*n2)
  }
  else if(op == "/") {
    return n1/n2
  }
}

## this function iterates the state list for a given set of indices and
## an operation
updateList <- doIndices(i1,i2,op,stateList) {
  if(i1 < 1)||(i2 > length(attemp$numbers)) error("Bad index!")
  if(i2 < 1)||(i2 > length(attempt$numbers)) error("Bad index!")
  result <- soveEquation(attempt$numbers[i1],attempt$numbers[12],op)
  
  # update to numbers
  indices = seq_along(numbers)
  newNumbers <- c(numbers[indices != i1 & indices != i2],result)
  
  # update to status
  if(result == target) {
    newStatus = "succes"
  }
  else if(floor(result) != result) {
    newStatus = "failed"
  }
  else if(length(newNumbers == 0)) {
    newStatus = "failed"
  }
  else {
    newStatus = "in-process"
  }
  
  #return new state list
  list(numbers=newNumbers,status=newStatus)
    
}
  


