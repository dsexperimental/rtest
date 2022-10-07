##==================================
## Countle Solver (countle.org)
##==================================

## get input
repeat {
  input = readline("What are the 6 numbers? ")
  numbers = as.numeric(unlist(strsplit(input,"[ ,]+",fixed=FALSE)))
  input = readline("what is the target number? ")
  target = as.numeric(input)
  print(sprintf("numbers: %g %g %g %g %g %g; target: %g",numbers[1],numbers[2],numbers[3],numbers[4],numbers[5],numbers[6],target))
  input = readline("Is this correct? (y or n) ")
  if(input == "y") {
    break
  }
  else if(input != "n") {
    print("That wasn't y or n. Now you need to start over.")
  }
}
                     
#sample inputs
#target <- 666
#numbers <- c(25,50,75,100,6,1)

#some constants
ops <- factor(c("+","-","*","/"))
stepStates <- factor(c("in-process","success","failed"))


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
    return(n1/n2)
  }
  else {
    error("invalid operation!")
  }
}

## this makes a string from an equation
equationToString <- function(i1,i2,op,result) {
  sprintf("%.0f %s %.0f = %.0f",i1,op,i2,result)
}

## this function updates the state list for a given set of indices and
## an operation
updateList <- function(i1,i2,op,stateList) {
  if( i1 < 1 || i2 > length(stateList$numbers) ) error("Bad index!")
  if( i2 < 1 || i2 > length(stateList$numbers) ) error("Bad index!")
  result <- solveEquation(stateList$numbers[i1],stateList$numbers[i2],op)
  
  #update the sequence report
  newSequence <- c(stateList$sequence,equationToString(stateList$numbers[i1],stateList$numbers[i2],op,result))
  
  # update to numbers
  indices <- seq_along(stateList$numbers)
  newNumbers <- c(stateList$numbers[indices != i1 & indices != i2],result)
  
  # update to status
  if(result == target) {
    newStatus <- "success"
  }
  else if(floor(result) != result) {
    newStatus <- "failed"
  }
  else if(result < 0) {
    newStatus <- "failed"
  }
  else if(length(newNumbers) <= 1) {
    newStatus <- "failed"
  }
  else {
    newStatus <- "in-process"
  }
  
  #return new state list
  list(sequence=newSequence,numbers=newNumbers,status=newStatus)
    
}
  
## this function does an iteration of the puzzle
## trying all combinations of numbers and operations
doIteration <- function(stateList) {
  #print(stateList)
  #dummy <- readline("Hit enter to continue")
  
  ## do the next iteration
  for(i1 in seq_along(stateList$numbers)) {
    for(i2 in seq_along(stateList$numbers)) {
      if(i2 == i1) {
        next
      }
      
      for(op in ops) {
        ## do one iteration on the state list
        newStateList = updateList(i1,i2,op,stateList)
        
        ##if we are not finished, iterate into this list
        if(newStateList$status == "in-process") {
          newStateList = doIteration(newStateList)
        }
        
        if(newStateList$status == "success") {
          return(newStateList)
        }
      }
    }
  }
  return(failedState)
}

## this constructs a state list
packStateList <- function(sequence,numbers,status) {
  list(sequence = sequence,numbers = numbers, status = status)
}

## initialize some values
initialState <- packStateList(vector(mode="character"),numbers,status="in-process")
failedState <- packStateList(c("--no data--"),c(0),status="failed")

## solve
result = doIteration(initialState)
print(result)
