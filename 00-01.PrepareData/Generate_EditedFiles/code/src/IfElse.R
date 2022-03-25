IfElse <- function(test, yes, no){
  
  result <- rep(NA, length(test))
  result[test] <- yes[test]
  result[!test] <- no[!test]
  return(result)
}
