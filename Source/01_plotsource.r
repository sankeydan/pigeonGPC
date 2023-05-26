d.grpmas$medspeedingroup = as.numeric(as.character(d.grpmas$medspeedingroup))
d.grpmas$medairspeedingroup = as.numeric(as.character(d.grpmas$medairspeedingroup))
d.grpmas$prop.heavyatstart = round(as.numeric(as.character(
  d.grpmas$prop.heavyatstart)),2)


d.solo$ff[d.solo$ff > 10] = NA
