##################################
# Estiation_VarCov
#################################
# INPUT:
# OUTPUT:

Estimation_VarCov <- function(Return,method="EWMA"){
  Return <- as.matrix(Return) # convert the dataframe to matrix for speed boost
  if(method=="EWMA"){
    lambda <- 0.97
    Sigma <- Return[1,] %*% t(Return[1,]) # initialization
    for(t in dim(Return)[1]){
      Sigma <- lambda * Return[t,] %*% t(Return[t,]) + (1-lambda) * Sigma
    }
  }
  return(Sigma)
}# end Estimation_VarCov