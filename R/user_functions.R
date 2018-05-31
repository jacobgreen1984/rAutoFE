#' @title dt_convert2fac
#' @description 
#' @param 
#' @return 
#' @examples 
#' @export
dt_convert2fac <- function(data, columns){
  change_columns <- columns
  data[,(change_columns):=lapply(.SD, as.factor), .SDcols=change_columns]
  return(data)
}


#' @title dt_splitFrame
#' @description 
#' @param 
#' @return 
#' @examples 
#' @export
dt_splitFrame <- function(data, ratio, seed){
  set.seed(seed)
  train_index <- sample(nrow(data), as.integer(nrow(data)*ratio[1]))
  train <- data[train_index, ]
  valid_test <- data[-train_index, ]
  valid_index <- sample(nrow(valid_test), as.integer(nrow(train)/ratio[1]*ratio[2]))
  valid <- valid_test[valid_index, ]
  test <- valid_test[-valid_index, ]
  rm(valid_test)
  return(list(train, valid, test))
}

