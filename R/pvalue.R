pvalue = function(pval = NULL,model = NULL, use_tukey = F ){
  # model = model
  # use_tukey = T
  # dimnamez = dimnames(table)

  # functions
  sig. = function(pval){
    return( ifelse( pval < 0.001, "***",
                    ifelse( pval < 0.01,  "**" ,
                            ifelse( pval < 0.05,  "*", "NS"))))
  }

  # Tukey?
  if ( use_tukey){
  tukey = emmeans::emmeans(model, list(pairwise ~ indvar), adjust = "tukey")
  tuk = tukey$`pairwise differences of indvar`
  sum = summary(tuk)
  pval = sum[,"p.value"]
  sig = sig.(pval)
  tab = cbind(sum,sig)
  return( tab)
  } else{
    return(sig.(pval))
  }
}
