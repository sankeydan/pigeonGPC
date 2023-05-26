lead.big.cent  = function( files,
                           gfolder,
                           nfolder,
                           ffolder,
                           names,
                           dates,
                           window = 25,
                           fis.dist = 50,
                           prop.flight.lead = 0.05,
                           cor.min = 0.9,
                           message = NULL,
                           method = "cent",
                           g){
  # OBJECTS

  # files = files2
  # gfolder = fold
  # nfolder = fold.n
  # ffolder = fold.f
  # names = names2
  # cor.min = 0.9
  # dates = dates
  # fis.dist = 50
  # prop.flight.lead = prop.flight.lead
  # method = "cent"
  # g = i


  ## LEADERSHIP ##


  #build empty array
  arn = na.omit(names)
  lead.arr = array( NA, c( length(arn),
                           length(files)),
                    dimnames = list(arn,
                                    dates
                    ))

  # loop per flight
  for ( i in 1:length(files)){
    # i=6

    # data
    load(file.path( gfolder , files[i]))
    names = dimnames(data)[[3]]

    # metadata
    if( method == "nei"){
      load(file.path( nfolder , files[i]))
      nn.mat = data.list[[3]]
      nn.dist= data.list[[2]]
      all.dist=data.list[[1]]
    }
    if( method == "cent"){
      load(file.path( ffolder , files[i]))
      fisdata   = data.list[[1]]
    }
    for ( j in 1:dim(data)[[3]]){
      # j=1
      # k=1

      # will run loop - CAUTION
      if( any(!is.na( data[,"lon",j]))){

      # focal data
      p1 = data[,c("lon","lat"),j]
      flight.len = length(na.omit(p1[,1]))

      # neighbour
      if (method == "nei"){
        nn = nn.mat[,j]
        foo = cbind( data[,"lon",], nn)
        nn.lon = apply( foo,1,function(x){
          #x=foo[14,]
          x[x[length(x)]]
        })
        foo = cbind( data[,"lat",], nn)
        nn.lat = apply( foo,1,function(x){
          #x=foo[14,]
          x[x[length(x)]]
        })
        p2 = as.matrix( cbind( lon = nn.lon,
                          lat = nn.lat))
      }

      # centroid
      if( method == "cent"){
        p2 = t(apply( fisdata[,c("lon","lat"),-j], 1,function(x){
          #x=data[1,c("lon","lat"),-j]
          rowMeans( x, na.rm = T)
        }))
      }


      # cut away when distance is below fission distance
      dis = get_dist(p1[,"lon"],p1[,"lat"],p2[,"lon"],p2[,"lat"],method = "distance")
      p1 = p1[which(dis < fis.dist),]
      p2 = p2[which(dis < fis.dist),]

      nrow = ifelse( length( nrow(p1)) == 0, 0, nrow(p1))
      if( nrow < (flight.len * prop.flight.lead) | nrow < ((window *2)+1) ){
        lead = NA
      } else{
        lead =leadRship(p1[,1:2],p2[,1:2],plot = T, ac =10, cor.min = cor.min )
      }
      lead.arr[names[j],i] =  unlist(lead)[1]

      # print(j)
      }
    }
    print( paste(  "done with flight", i , "/" , length(files)))
  }


  # remove birds which did not finish the trial
  if ( g ==1){
    missing = which( arn %in% c("59", "55", "81"))
    if( length(missing)>0){
      lead.arr = lead.arr[-missing,,,]
      names = dimnames(lead.arr)[[1]]
    }
  }
  if( g == 4){
    missing = which( names %in% c("96"))
    if ( length(missing)>0){
      lead.arr = lead.arr[-missing,,,]
      names = dimnames(lead.arr)[[1]]
    }
  }






  # RETURN

  return( lead.arr)


}
