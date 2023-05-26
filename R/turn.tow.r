turn.tow = function( fochead, otherhead,cent = T){


  pairheads = cbind( fochead, otherhead)
  di.turn = t(apply( pairheads,1,function(x){
    #x = pairheads[2044,]
    # x[2] = pi-0.0001
    # x[1] = -pi+0.001
    di = diff(x[1:2])
    di = ifelse ( di < -pi, di+(2*pi),di)
    di = ifelse ( di > pi, di-(2*pi),di)
    turn.toward = ifelse ( sign(di) == sign(x[3]),abs(x[3]),-abs(x[3]))
    c(di,turn.toward)
  }))
  di.mean = apply( di.turn , 2, function( x){mean(x, na.rm = T)})
  perc. =di.mean[2]/ di.mean[1]*100

  if( cent ){
    names =  c("centhead", "turntowardcent", "perc.turn.cent")
  } else {
    names =  c("nnhead"  , "turntowardnn"  , "perc.turn.nn")
  }
  return( matrix( c(di.mean,perc.),1,dimnames = list(NULL,names)))
}
