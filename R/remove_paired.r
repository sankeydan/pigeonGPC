
remove.paired = function(

  files = NULL,
  fis.dist = 50,
  proportion.crit = 0.05,
  hz = 5){

  #objects

  # files = paste0(files.diff, ".rda")
  # fis.dist = 50
  # proportion.crit = 0.05
  # hz = 5
  # aborted = F
  # istart = 1

  # libraries
  library(stringr)
  library(data.table)
  library(massManip)

  # functions
  trim.fract = function( df, hz = 5 ){
    first.val = table(df$time)[1]
    if(first.val!= hz){
      df = df[(first.val+1):nrow(df),]
    }
    len.df = df$time [nrow(df)] - df$time [1]
    last.val = table(df$time)[len.df + 1]
    if(last.val != hz){
      df = df[1:(nrow(df)-last.val),]
    }
    return(df)
  }

  # files
  if ( is.null( files)){
    files = list.files( file.path (PROJHOME , "Output" ,"GPS")) # only solo flight files
  }

  # Which flight?
  flight.nums = str_split_fixed(files, "\\.", 8)[,2]


  # list of rejected flights, and overlap
  lorf = list()
  over.list = list()

  # LOOP each flight
  for (i in unique(flight.nums)){
    #i=6

    # files
    files.i = files[flight.nums == i]

    # LOOP each bird
    for ( j in 1:length(files.i)){
      #j = 4

      # load file
      load( file.path(PROJHOME , "Output" , "GPS" , files.i[j])) # load the data

      # convert time into seconds
      foo = apply(str_split_fixed( data$UTC.TIME, ":", 3),2, as.numeric)
      foo2 = cbind((foo[,1]* 3600), (foo[,2]*60), (foo[,3]))
      time = apply(foo2 , 1, sum)

      # make into a dataframe
      assign( paste0("p",j) , data.frame(lon = data$LONGITUDE, lat = data$LATITUDE , time))
    }

    # OVERLAP
    overlap = matrix(NA,  length(files.i) , length(files.i)) # we want to see where the times of the bird's flights overlap so we can measure the distance between indiv iduals at these times and hence determine whether they were flying alone or not

    for ( j in 1:length(files.i)){ # for each pigeon
      for ( k in 1:length(files.i)){ # and each nieghbour
        # k = 10
        # j = 15

        if ( j != k){ # if focal doesn't equal neighbour

          # get dataframes
          x = get(paste0("p", j)) # for easier manipulation, put focal individual's data into object x
          y = get(paste0("p", k)) # and neighbour into y

          # trim fractions of seconds off end of dataframes
          x2 = trim.fract(x)
          y2 = trim.fract(y)

          # Add rows of NA to make dataframes same size
          maxmin = range( c( x$time, y$time ),na.rm = T)
          y3 = rbind ( matrix( NA, nrow = (min(y2[,3],na.rm=T) - maxmin[1] ) *hz,3),
                       as.matrix(y2),
                       matrix ( NA,nrow = (maxmin[2] - max(y2[,3],na.rm=T) ) *hz,3))
          x3 = rbind ( matrix( NA, nrow = (min(x2[,3],na.rm=T) - maxmin[1] ) *hz,3),
                       as.matrix(x2),
                       matrix ( NA,nrow = (maxmin[2] - max(x2[,3],na.rm=T) ) *hz,3))
          minrow = min(c(nrow(x3),nrow(y3)))
          x = as.data.frame(x3[1:minrow,])
          y = as.data.frame(y3[1:minrow,])

          # Calculate distance and proportion of time paired (overlap)
          dis.to.neighbour = get_dist(x$lon, x$lat, y$lon, y$lat, method = "distance") # use get_dist functon to derive distance between neighbours
          overlap[j,k] = length(which( dis.to.neighbour <fis.dist))/nrow(x) # and the proportion of their flight which overlapped?
        }
      }
    }


    rows = floor( which(as.vector(t(overlap) > proportion.crit)) / nrow(overlap) ) + 1 # # which rows are the values
    # of overlap abover the proportion of overlap deemed critical for the file to be rejected as a solo flight?

    lorf[[i]] = files.i[unique(rows)] # which files had unsatisfactory levels of overlap?
    over.list[[i]] = overlap

    print( paste(i ))
  }

  return( list ( lorf , over.list ))
}
