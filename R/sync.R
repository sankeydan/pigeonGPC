sync = function(lead.arr){
  ## SYNCHRONISATION ##
  synch.mat = matrix( NA, length(files), length(names), dimnames = list( NULL, names))
  for ( i in 1:length(files)){
    #i=1
    temp.mat = lead.arr[,,i]
    synch.mat[i,] = rowSums(abs(temp.mat),na.rm = T)

  }
}
