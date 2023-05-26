stat.funct.group = function(df,box.or.log = "box",depvar =NULL ,indvar =NULL, cond = NULL, experiment = NULL,rou = 4 ){
  # df = dg
  # depvar="medspeed"
  # indvar= indvar1
  # cond = NULL
  # box.or.log = "log"
  # experiment = exp1
  # rou = 4


  # experiment
  if ( experiment != "all"){
    df = df[df$experiment == experiment,]
  }


  # which vars
  nam = names(df)
  df$depvar = df[,which( nam %in% depvar)]
  df$indvar = df[,which( nam %in% indvar)]

  #### STATS

  ##1. With condition

  if ( !is.null(cond)){

    # condition
    df$cond   = df[,which( nam %in% cond  )]

    # remove NAs
    df = df[,c("depvar", "indvar", "cond", "insta.size", "flight.order", "date", "group", "tort","cross.wind", "support.wind")]
    df = df[,apply(df,2,function(x){any(!is.na(x))})]
    df$depvar[df$depvar == 0] = NA
    df = df[complete.cases(df),]

    if( !is.null(df$group)){

      if ( box.or.log == "box"){
        b = boxcox(depvar ~indvar + cond + insta.size  + tort + cross.wind + support.wind, data = df ,plotit = F)
        bb = bestbox(b, df$depvar)
        df$depvar = bb[[1]]
        mod= lme( depvar ~  indvar+ cond + insta.size  + tort + cross.wind + support.wind , random = list(date=~1,group=~1), data = df, na.action = na.omit)
        trans = as.character(bb[[2]])
      } else {
        mod= lme( log(depvar) ~ indvar + cond + insta.size   + tort + cross.wind + support.wind , random = list(date=~1,group=~1), data = df, na.action = na.omit)
        trans = "log"
      }
    } else {
      if ( box.or.log == "box"){
        b = boxcox(depvar ~indvar + cond + insta.size  + tort + cross.wind + support.wind, data = df ,plotit = F)
        bb = bestbox(b, df$depvar)
        df$depvar = bb[[1]]
        mod= lme( depvar ~  indvar+ cond + insta.size  + tort + cross.wind + support.wind , random = list(date=~1), data = df, na.action = na.omit)
        trans = as.character(bb[[2]])
      } else {
        mod= lme( log(depvar) ~ indvar + cond + insta.size   + tort + cross.wind + support.wind , random = list(date=~1), data = df, na.action = na.omit)
        trans = "log"
      }
    }
    # output
    sum = summary(mod)
    tab = sum$tTable
    for ( j in 1:ncol(tab)){
      tab[,j] = round(tab[,j],rou)
    }
    predictors = dimnames(tab)[[1]]
    colnams = c("depvar","indvar", "cond", "trans" ,"predictors",dimnames(tab)[[2]])
    dimnames(tab) = NULL
    tab = as.data.frame(cbind(depvar,indvar , cond, trans, predictors, tab))
    names(tab) = colnams
    return(list(mod,tab))
  } else {

    # 2. Without condtion

    if( experiment == "all"){

      if( indvar != "flight.order"){
        # remove NAs
        df = df[,c("depvar", "indvar", "insta.size", "flight.order", "date", "experiment", "tort","cross.wind", "support.wind")]
        df = df[,apply(df,2,function(x){any(!is.na(x))})]
        df$depvar[df$depvar == 0] = NA
        df = df[complete.cases(df),]
      } else {
        # remove NAs
        df = df[,c("depvar", "indvar", "insta.size", "date", "experiment", "tort","cross.wind", "support.wind")]
        df = df[,apply(df,2,function(x){any(!is.na(x))})]
        df$depvar[df$depvar == 0] = NA
        df = df[complete.cases(df),]
      }

      # stats
      if ( indvar == "homo_hetero" | indvar == "homogenous_condition" | indvar == "flight.order"){

        if ( indvar == "homo_hetero" | indvar == "homogenous_condition"){
          if ( box.or.log == "box"){
            b = boxcox(depvar ~indvar + insta.size  + tort + cross.wind + support.wind, data = df ,plotit = F)
            bb = bestbox(b, df$depvar)
            df$depvar = bb[[1]]
            mod= lme( depvar ~  indvar + insta.size  + tort + cross.wind + support.wind , random = list(date=~1), data = df, na.action = na.omit)
            trans = as.character(bb[[2]])
          } else {
            mod= lme( log(depvar) ~ indvar + insta.size   + tort + cross.wind + support.wind , random = list(date=~1), data = df, na.action = na.omit)
            trans = "log"
          }
        }
        if ( indvar == "flight.order"){
          if ( box.or.log == "box"){
            b = boxcox(depvar ~indvar + insta.size + tort + cross.wind + support.wind, data = df ,plotit = F)
            bb = bestbox(b, df$depvar)
            df$depvar = bb[[1]]
            mod= lme( depvar ~  indvar + insta.size + tort + cross.wind + support.wind , random = list(date=~1,experiment=~1), data = df, na.action = na.omit)
            trans = as.character(bb[[2]])
          } else {
            mod= lme( log(depvar) ~ indvar + insta.size +  tort + cross.wind + support.wind , random = list(date=~1,experiment=~1), data = df, na.action = na.omit)
            trans = "log"
          }
        }

      } else {
        if ( indvar != "insta.size.num"){
          if ( indvar != "insta.size"){
            if ( box.or.log == "box"){
              b = boxcox(depvar ~indvar + insta.size  + tort + cross.wind + support.wind, data = df ,plotit = F)
              bb = bestbox(b, df$depvar)
              df$depvar = bb[[1]]
              mod= lme( depvar ~  indvar + insta.size  + tort + cross.wind + support.wind , random = list(date=~1,experiment=~1), data = df, na.action = na.omit)
              trans = as.character(bb[[2]])
            } else {
              mod= lme( log(depvar) ~ indvar + insta.size   + tort + cross.wind + support.wind , random = list(date=~1,experiment=~1), data = df, na.action = na.omit)
              trans = "log"
            }
          }
        }
      }

      if ( indvar == "insta.size" | indvar == "insta.size.num"){

        if ( box.or.log == "box"){
          b = boxcox(depvar ~indvar  + tort + cross.wind + support.wind, data = df ,plotit = F)
          bb = bestbox(b, df$depvar)
          df$depvar = bb[[1]]
          mod= lme( depvar ~  indvar  + tort + cross.wind + support.wind , random = list(date=~1), data = df, na.action = na.omit)
          trans = as.character(bb[[2]])
        } else {
          mod= lme( log(depvar) ~ indvar  + tort + cross.wind + support.wind , random = list(date=~1), data = df, na.action = na.omit)
          trans = "log"
        }

      } else {
        if ( indvar!= "flight.order"){
          if ( box.or.log == "box"){
            b = boxcox(depvar ~indvar + insta.size  + tort + cross.wind + support.wind, data = df ,plotit = F)
            bb = bestbox(b, df$depvar)
            df$depvar = bb[[1]]
            mod= lme( depvar ~  indvar + insta.size  + tort + cross.wind + support.wind , random = list(date=~1,experiment=~1), data = df, na.action = na.omit)
            trans = as.character(bb[[2]])
          } else {
            mod= lme( log(depvar) ~ indvar + insta.size   + tort + cross.wind + support.wind , random = list(date=~1,experiment=~1), data = df, na.action = na.omit)
            trans = "log"
          }
        }
      }



      # output
      sum = summary(mod)
      tab = sum$tTable
      for ( j in 1:ncol(tab)){
        tab[,j] = round(tab[,j],rou)
      }
      predictors = dimnames(tab)[[1]]
      colnams = c("depvar","indvar", "trans" ,"predictors",dimnames(tab)[[2]])
      dimnames(tab) = NULL
      tab = as.data.frame(cbind(depvar,indvar , trans, predictors, tab))
      names(tab) = colnams
      return(list(mod,tab))
    } else{

      if ( indvar != "flight.order"){
        # remove NAs
        df = df[,c("depvar", "indvar", "insta.size", "flight.order", "date", "group", "tort","cross.wind", "support.wind")]
        df = df[,apply(df,2,function(x){any(!is.na(x))})]
        df$depvar[df$depvar == 0] = NA
        df = df[complete.cases(df),]



        # stats
        if ( "group" %in% names(df)){
          if ( box.or.log == "box"){
            b = boxcox(depvar ~indvar + insta.size  + tort + cross.wind + support.wind, data = df ,plotit = F)
            bb = bestbox(b, df$depvar)
            df$depvar = bb[[1]]
            mod= lme( depvar ~  indvar + insta.size  + tort + cross.wind + support.wind , random = list(date=~1,group=~1), data = df, na.action = na.omit)
            trans = as.character(bb[[2]])
          } else {
            mod= lme( log(depvar) ~ indvar + insta.size   + tort + cross.wind + support.wind , random = list(date=~1,group=~1), data = df, na.action = na.omit)
            trans = "log"
          }
        } else {
          if ( box.or.log == "box"){
            b = boxcox(depvar ~indvar + insta.size  + tort + cross.wind + support.wind, data = df ,plotit = F)
            bb = bestbox(b, df$depvar)
            df$depvar = bb[[1]]
            mod= lme( depvar ~  indvar + insta.size  + tort + cross.wind + support.wind , random = list(date=~1), data = df, na.action = na.omit)
            trans = as.character(bb[[2]])
          } else {
            mod= lme( log(depvar) ~ indvar + insta.size   + tort + cross.wind + support.wind , random = list(date=~1), data = df, na.action = na.omit)
            trans = "log"
          }
        }
      } else {
        # remove NAs
        df = df[,c("depvar", "indvar", "insta.size", "date", "group", "tort","cross.wind", "support.wind")]
        df = df[,apply(df,2,function(x){any(!is.na(x))})]
        df$depvar[df$depvar == 0] = NA
         df = df[complete.cases(df),]


        # stats
        if ( "group" %in% names(df)){
          if ( box.or.log == "box"){
            b = boxcox(depvar ~indvar + insta.size  + tort + cross.wind + support.wind, data = df ,plotit = F)
            bb = bestbox(b, df$depvar)
            df$depvar = bb[[1]]
            mod= lme( depvar ~  indvar + insta.size  + tort + cross.wind + support.wind , random = list(date=~1,group=~1), data = df, na.action = na.omit)
            trans = as.character(bb[[2]])
          } else {
            mod= lme( log(depvar) ~ indvar + insta.size   + tort + cross.wind + support.wind , random = list(date=~1,group=~1), data = df, na.action = na.omit)
            trans = "log"
          }
        } else {
          if ( box.or.log == "box"){
            b = boxcox(depvar ~indvar + insta.size  + tort + cross.wind + support.wind, data = df ,plotit = F)
            bb = bestbox(b, df$depvar)
            df$depvar = bb[[1]]
            mod= lme( depvar ~  indvar + insta.size  + tort + cross.wind + support.wind , random = list(date=~1), data = df, na.action = na.omit)
            trans = as.character(bb[[2]])
          } else {
            mod= lme( log(depvar) ~ indvar + insta.size   + tort + cross.wind + support.wind , random = list(date=~1), data = df, na.action = na.omit)
            trans = "log"
          }
        }
      }
      # output
      sum = summary(mod)
      tab = sum$tTable
      for ( j in 1:ncol(tab)){
        tab[,j] = round(tab[,j],rou)
      }
      predictors = dimnames(tab)[[1]]
      colnams = c("depvar","indvar", "trans" ,"predictors",dimnames(tab)[[2]])
      dimnames(tab) = NULL
      tab = as.data.frame(cbind(depvar,indvar , trans, predictors, tab))
      names(tab) = colnams
      return(list(mod,tab))
    }
  }
}
