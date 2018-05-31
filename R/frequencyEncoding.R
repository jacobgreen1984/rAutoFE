#' @title frequencyEncoding_fit
#' @description
#' @param
#' @return
#' @examples
#' library(rAutoFE)
#' library(data.table)
#' data(churn, package = "rAutoFE")
#' data.table::setDT(churn)
#' splits <- rAutoFE::dt_splitFrame(dt=churn, ratio = c(0.5, 0.2), seed=1234)
#' train <- splits[[1]]
#' valid <- splits[[2]]
#' test  <- splits[[3]]
#' x = c("State","Area.Code")
#' y = "Churn."
#' fit <- rAutoFE::frequencyEncoding_fit(dt = train, x = x)
#' @export
frequencyEncoding_fit <- function(dt, x){
  setDT(dt)
  fit_list <- list()
  for(i in x){
    setkeyv(dt, i)
    fit_list[[i]] <- dt[, list(frequencyEncode_=.N), by=i]
    colnames(fit_list[[i]])[2] <- paste0(colnames(fit_list[[i]])[2], i)
  }
  return(fit_list)
}


#' @title frequencyEncoding_transform
#' @description
#' @param
#' @return
#' @examples
#' library(rAutoFE)
#' library(data.table)
#' data(churn, package = "rAutoFE")
#' data.table::setDT(churn)
#' splits <- rAutoFE::dt_splitFrame(dt=churn, ratio = c(0.5, 0.2), seed=1234)
#' train <- splits[[1]]
#' valid <- splits[[2]]
#' test  <- splits[[3]]
#' x = c("State","Area.Code")
#' y = "Churn."
#' fit <- rAutoFE::frequencyEncoding_fit(dt = train, x = x)
#' saveRDS(fit,"fit.rds")
#' rm(fit)
#' fit <- readRDS("fit.rds")
#' train <- rAutoFE::frequencyEncoding_transform(dt = train, x = x, fit = fit)
#' valid <- rAutoFE::frequencyEncoding_transform(dt = valid, x = x, fit = fit)
#' test  <- rAutoFE::frequencyEncoding_transform(dt = test, x = x, fit = fit)
#' @export
frequencyEncoding_transform <- function(dt, x=x, fit=fit){
  for(i in x){
    x_map <- fit[[i]]
    setkeyv(x_map, i)
    setkeyv(dt, i)
    dt <- x_map[dt]
  }
  return(dt)
}


