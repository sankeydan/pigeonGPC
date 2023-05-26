#' Get pairwise leadership score
#'
#' @export

# leadRship


leadRship= function( p1,
                     p2,
                     window = 25,
                     cor.min = 0.9,
                     hz = 5,
                     plot=F,
                     method = c( "colMeans" , "each.time.step"),
                     ac = 10,
                     auto.cor.test = F,
                     auto.test.length = NULL){

  # Objects

  # p1 = p1
  # p2 = p2
  # window = 25
  # cor.min = 0.9
  # hz = 5
  # plot = T
  # method = c( colMeans")
  # ac = 10
  # auto.cor.test = F
  # auto.test.length = NULL

  # stop if not
  if ( length(dim(p1)) != 2 ){
    stop( "p1 and p2 must be matrics with x and y columns")
  }

  # More objects
  len = nrow(p1)
  wn = ceiling(window/2)
  st = wn
  en = len - wn

  # Movement per timestep , dt
  dt1 = matrix(NA, len - 1, 2)
  dt2 = matrix(NA, len - 1, 2)
  dt1[,1] = p1[2:len,1] - p1[1:(len-1),1]
  dt1[,2] = p1[2:len,2] - p1[1:(len-1),2]
  dt2[,1] = p2[2:len,1] - p2[1:(len-1),1]
  dt2[,2] = p2[2:len,2] - p2[1:(len-1),2]

  # Normalise
  norm1 = matrix(NA, len-1,2)
  norm2 = matrix(NA, len-1,2)
  norm1[,1] = apply(cbind(dt1[,1], dt1[,2]), 1, function ( x) { x[1] / sqrt(x[1]^2 + x[2]^2)})
  norm1[,2] = apply(cbind(dt1[,2], dt1[,1]), 1, function ( x) { x[1] / sqrt(x[1]^2 + x[2]^2)})
  norm2[,1] = apply(cbind(dt2[,1], dt2[,2]), 1, function ( x) { x[1] / sqrt(x[1]^2 + x[2]^2)})
  norm2[,2] = apply(cbind(dt2[,2], dt2[,1]), 1, function ( x) { x[1] / sqrt(x[1]^2 + x[2]^2)})


  # pairwise correlation
  cors = matrix(NA,len-1, window)
  for ( j in 1:window){
    for ( i in st:en){
      cors[i,j] =  norm1[i,] %*% norm2[(j-wn+i),]
    }
  }

  # leader/folower

  if ( method == "colMeans"){
    coraverage = colMeans( cors[st:en,],na.rm = T)
    if(max( coraverage)< cor.min){
      leadr = NA
    } else {
      l.f = which.max(coraverage)
      leadr = -(wn - l.f) / hz
    }
  }
  if ( method == "each.time.step"){
    l.f = apply( cors[st:en,], 1, function(x) {

      if (! any(is.na(x))){
        if( max(x)[1] > cor.min ){
          round(mean(which (x == max(x))))
        } else {
          NA
        }} else {
          NA
        }
    })
    lead = c( rep(NA, wn-1) ,
              ifelse ( l.f < wn, 1, ifelse( l.f > wn, -1, 0)),  # this is the key part. If below window, then leading
              rep(NA, wn-1))

    ## Autocorrelation test
    if ( auto.cor.test){
      auto.testR = matrix(NA, (len - auto.test.length), auto.test.length)
      for ( j in 1:auto.test.length){
        for ( i in st:(en-auto.test.length)){
          auto.testR[i,j] = lead[i] == lead[i+j]
        }
      }
      at = apply( auto.testR[st:(nrow(auto.testR)-wn),],2, function(x){
        mean(as.numeric(x))})
    }

    # remove duplicates
    for ( i in 1:length(lead)){
      if ( !is.na(lead[i])){
        if( i + ac > length(lead)){
          ac2 = length(lead)-i
        } else {
          ac2 = ac
        }
        for ( j in 1:ac2){
          if ( !is.na(lead[i+j]) ){
            if( lead[i] == lead[i+j]){
              lead[i+j] = NA
            }
          }
        }
      }
    }
    leadr = sum(lead,na.rm = T)
  }

  # return objects
  if (auto.cor.test){
    return( list(at, leadr))
  } else {
    return( leadr)
  }

  #plot
  if(plot){
    plot( coraverage)
  }

}
