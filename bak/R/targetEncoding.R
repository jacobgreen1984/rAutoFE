#' @title targetEncoding_fit
#' @description 
#' @param 
#' @return 
#' @examples 
#' require(data.table)
#' require(h2o)
#' h2o.init()
#' data <- fread("data/churn.csv")
#' data <- dt_convert2fac(data=data, columns=c("Churn.","State","Area.Code","Int.l.Plan","VMail.Plan"))
#' splits <- dt_splitFrame(data=data, ratio = c(0.5, 0.2), seed=1234)
#' train <- splits[[1]]
#' valid <- splits[[2]]
#' test  <- splits[[3]]
#' x = c("State","Area.Code")
#' y = "Churn."
#' fit <- targetEncoding_fit(data = train, x = x, y = y)
#' saveRDS(fit,"fit.Rda")
#' @export
targetEncoding_fit <- function(data, x, y){
  if(class(data)[[1]]=="H2OFrame"){
    h2o.target_encode_create(data = data, x = x, y = y)
  }else if(class(data)[[1]]=="data.table"){
    fit_list <- list()      
    for(i in x){
      setkeyv(data, i)
      fit_list[[i]] <- data[ ,list(numerator=sum(as.numeric(get(y))-1,na.rm=T), denominator=.N), by=i]
    }
    return(fit_list)
  }
}


#' @title targetEncoding_transform
#' @description 
#' @param 
#' @return 
#' @examples 
#' fit <- readRDS("fit.Rda")
#' train <- targetEncoding_transform(data = train, x = x, y = y, fit = fit)
#' valid <- targetEncoding_transform(data = valid, x = x, y = y, fit = fit)
#' test  <- targetEncoding_transform(data = test, x = x, y = y, fit = fit)
#' @export
targetEncoding_transform <- function(data, x, y, fit){
  if(class(data)[[1]]=="H2OFrame"){
    h2o.target_encode_apply(
      data = data, 
      x = x, 
      y = y, 
      target_encode_map = fit, 
      holdout_type = "None", 
      blended_avg = FALSE, 
      noise_level = 0,
      seed = 1234
    )
  }else if(class(data)[[1]]=="data.table"){
    for(i in x){
      x_map <- fit[[i]]
      x_map[,paste0("TargetEncode_",i):=numerator/denominator]
      x_map[,':='(numerator=NULL, denominator=NULL)]
      setkeyv(x_map, i)
      setkeyv(data, i)
      data <- x_map[data]
    }
    return(data)
  }
}

