#' @title frequencyEncoding_fit
#' @description 
#' @param 
#' @return 
#' @examples 
#' source("INTERNAL/auto_fe/R/user_functions.R")
#' library(data.table)
#' path = c("./INTERNAL/auto_fe/data")
#' data <- readRDS(file.path(path,"churn.Rda"))
#' data <- dt_convert2fac(data=data, columns=c("Churn.","State","Area.Code","Int.l.Plan","VMail.Plan"))
#' splits <- dt_splitFrame(data=data, ratio = c(0.5, 0.2), seed=1234)
#' train <- splits[[1]]
#' valid <- splits[[2]]
#' test  <- splits[[3]]
#' x = c("State","Area.Code")
#' y = "Churn."
#' fit <- frequencyEncoding_fit(data = train, x = x)
#' saveRDS(fit,"./bak/fit.Rda")
#' @export
frequencyEncoding_fit <- function(data, x){
  fit_list <- list()      
  for(i in x){
    setkeyv(data, i)
    fit_list[[i]] <- data[, .(frequencyEncode_=.N), by=i]
    colnames(fit_list[[i]])[2] <- paste0(colnames(fit_list[[i]])[2], i)
  }
  return(fit_list)
}


#' @title frequencyEncoding_transform
#' @description 
#' @param 
#' @return 
#' @examples 
#' fit <- readRDS("./bak/fit.Rda")
#' train <- frequencyEncoding_transform(data = train, x = x, fit = fit)
#' valid <- frequencyEncoding_transform(data = valid, x = x, fit = fit)
#' test  <- frequencyEncoding_transform(data = test, x = x, fit = fit)
#' @export
frequencyEncoding_transform <- function(data, x=x, fit=fit){
  for(i in x){
    x_map <- fit[[i]]
    setkeyv(x_map, i)
    setkeyv(data, i)
    data <- x_map[data]
  }
  return(data)
}


























