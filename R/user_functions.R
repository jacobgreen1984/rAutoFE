#' @title dt_convert2fac
#' @description
#' @param
#' @return
#' @examples
#' @export
dt_convert2fac <- function(dt, columns){
  change_columns <- columns
  dt[,(change_columns):=lapply(.SD, as.factor), .SDcols=change_columns]
  return(dt)
}


#' @title dt_splitFrame
#' @description
#' @param
#' @return
#' @examples
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


