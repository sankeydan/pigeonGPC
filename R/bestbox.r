bestbox = function ( b,var){
  # var = d.solo2$birdmass
  lamda=b$x
  lik=b$y
  bc = cbind(lamda, lik)
  head(bc[order(-lik),])
  cb =bc[order(-lik),]
  cb = ifelse ( cb[1,1]== 0, cb[2,1],cb[1,1])
  trans = var^cb
  return(list( trans, cb))
}
