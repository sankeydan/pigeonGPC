fb_lr_funct = function( data.f, insta.flock.size , size , minflight.prop){
  #size = 3
  dims = dim(data.f)
  whi = which(insta.flock.size == size)
  vars= c("pidge" ,"fbmed","fbsd","lrmed","lrsd", "insta.size")
  d = data.f[whi,,]
  if( length(dim(d))>2){
    for( j in 1:dims[3]){
      #j=1
      if( length(which(!is.na(d[,1,j])))/ dims[1] < minflight.prop){
        d[,,j] = NA
      }
    }
    if ( length( whi)/dims[1] > minflight.prop){
      fblr = fb_lr_d2c(d, plot = F)
      fb = fblr[[1]]
      lr = fblr[[2]]

      # individual
      d2 = cbind   (apply( fb, 2, function(x){median(x,na.rm=T)}),
                    apply( fb, 2, function(x){sd    (x,na.rm=T)}),
                    apply( lr, 2, function(x){median(x,na.rm=T)}),
                    apply( lr, 2, function(x){sd    (x,na.rm=T)}))
      d2 = cbind(dimnames(data.f)[[3]], d2,size)
      dimnames(d2) = NULL
      d2 = as.data.frame(d2)
      names(d2) = vars

      # group
      fb = apply(fb,1,function(x){ifelse( length(na.omit(x)>1),diff(range(na.omit(x))),NA)})
      lr = apply(lr,1,function(x){ifelse( length(na.omit(x)>1),diff(range(na.omit(x))),NA)})
      d3 = c(median(fb,na.rm=T),
             median(lr,na.rm=T))
      return(list( d2, d3))
    } else {
      d2 = matrix(NA,dim(data.f)[[
        3]],length(vars),dimnames = list(NULL,vars))
      d2[,1] = dimnames(data.f)[[3]]
      d2 = as.data.frame(d2)
      return(list(d2, c(NA,NA)))
    }
  } else {
    d2 = matrix(NA,dim(data.f)[[
      3]],length(vars),dimnames = list(NULL,vars))
    d2[,1] = dimnames(data.f)[[3]]
    d2 = as.data.frame(d2)
    return(list(d2, c(NA,NA)))
  }
}
