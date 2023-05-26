lead.big  = function( files,
                      gfolder,
                      names,
                      dates,
                      fis.dist = 50,
                      prop.flight.lead = 0.5,
                      cor.min = 0.9,
                      message = NULL,
                      plot.cor = F,
                      g){
  # OBJECTS

  # files = files2
  # gfolder = fold
  # names = names2
  # dates = dates
  # fis.dist = 50
  # prop.flight.lead = prop.f
  # g = i
  # plot.cor = T


  ## LEADERSHIP ##


  #build empty array
  arn = na.omit(names)
  lead.arr = array( NA, c( length(arn),
                           length(arn),
                           length(files),
                           3),
                    dimnames = list(arn,
                                    arn,
                                    dates,
                                    c("colmean","taumean","taumed")))

  # loop per flight
  for ( i in 1:length(files)){
    # l=2
    # i=2
    load(file.path( gfolder , files[i]))
    names = dimnames(data)[[3]]
    for ( j in 1:dim(data)[[3]]){
      for  ( k in 1:dim(data)[[3]]){
        if ( j > k){
          # j=4
          # k=1
          p1 = data[,,j]
          p2 = data[,,k]
          dis = na.omit(get_dist(p1[,"lon"],p1[,"lat"],p2[,"lon"],p2[,"lat"],method = "distance"))
          p1 = p1[which(dis < fis.dist),]
          p2 = p2[which(dis < fis.dist),]
          if( length( which(dis<fis.dist) ) <
              length( na.omit(dis))*prop.flight.lead |  length(dis) ==0){
            lead = NA
          } else{
            lead =leadRship(p1,p2,plot.cor = plot.cor, ac =10, cor.min = cor.min)
          }
          lead.arr[names[j],names[k],i,] =  unlist(lead)
          lead.arr[names[k],names[j],i,] = -unlist(lead)
          # print(j)
        }
      }
    }
    print( paste(  "done with flight", i , "/" , length(files)))
  }


  # remove birds which did not finish the trial
  if ( g ==1){
    missing = which( arn %in% c("59", "55", "81"))
    if( length(missing)>0){
      lead.arr = lead.arr[-missing,-missing,,]
      names = dimnames(lead.arr)[[1]]
    }
  }
  if( g == 4){
    missing = which( names %in% c("96"))
    if ( length(missing)>0){
      lead.arr = lead.arr[-missing,-missing,,]
      names = dimnames(lead.arr)[[1]]
    }
  }


  ## DESCRIPTIVES ##

  #objects
  names = dimnames(lead.arr)[[1]]

  #num.flights.included
  {table.lead = matrix( 0 , dim(lead.arr)[3], length(names),
                        dimnames = list(NULL,names))
    for ( i in 1:dim(lead.arr)[3]){
      for ( j in names){
        table.lead[i,j] = ifelse ( any(!is.na(lead.arr[,j,i,1])), 1,0)
      }
    }
    table.lead = rbind( table.lead , colSums(table.lead))
  }

  ## STATISTICS ##

  # mean the pairwise leadership scores
  lead = apply( lead.arr[,,,1] , 1:2 , function( x) {mean(x,na.rm = T)})
  diag(lead) = 0

  #remove negative numbers
  lead.w.out.neg = lead
  lead.w.out.neg[lead < 0] = 0
  lead.w.out.neg

  # rank order
  name.rank = names(sort(rowMeans(lead.w.out.neg,na.rm = T)))
  lead.order = lead[rev(name.rank),rev(name.rank)]

  # rowsums (w) and colsums (l) NAgy et al
  w = rowSums(lead.order,na.rm = T)
  l = colSums(lead.order,na.rm = T)
  l2 = rep(NA, ncol(lead))
  w2 = rep(NA, ncol(lead))
  for ( i in 1:ncol(lead)){
    #i=1
    lo = lead.order[,i]
    nao= na.omit(lo)

    l2[i]=crossprod(nao,l[!is.na(lo)])
    w2[i]=crossprod(nao,w[!is.na(lo)])
  }

  # normDS
  DS = w+w2-l-l2
  NormDS = (DS/ncol(lead) ) + ((ncol(lead)-1)/2)
  NormDS = NormDS[order(as.numeric(names(NormDS)))]
  leadscore = sort(NormDS)

  # stats
  test = compete::ttri_test(lead)





  # RETURN

  return( list ( lead.arr, table.lead, test, leadscore))


}
