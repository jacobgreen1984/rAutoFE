#' @title dt_convert2fac
#' @description convert all selected features to factor
#' @param dt data.table object
#' @param columns character vector
#' @export
dt_convert2fac <- function(dt, columns){
  change_columns <- columns
  dt[,(change_columns):=lapply(.SD, as.factor), .SDcols=change_columns]
  return(dt)
}


#' @title dt_splitFrame
#' @description data split using data.table
#' @param dt data.table object
#' @param ratio numeric vector
#' @param seed integer
#' @export
dt_splitFrame <- function(dt, ratio, seed){
  set.seed(seed)
  train_index <- sample(nrow(dt), as.integer(nrow(dt)*ratio[1]))
  train <- dt[train_index, ]
  valid_test <- dt[-train_index, ]
  valid_index <- sample(nrow(valid_test), as.integer(nrow(train)/ratio[1]*ratio[2]))
  valid <- valid_test[valid_index, ]
  test <- valid_test[-valid_index, ]
  rm(valid_test)
  return(list(train, valid, test))
}


