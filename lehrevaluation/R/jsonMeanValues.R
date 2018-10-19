
#' receives a SPSS file with the evaluation data and writes a file with the mean values for each variable of each course
#' @importFrom  foreign read.spss
#' @import jsonlite
#' @import dplyr
#' @import tidyr
#'
#' @param spss_file the absolute path to a SPSS file with .sav extension
#' @param path_to_json a path with the name to be given to the new file ending with the .json extension
#'
#' @return None
#' @export
#'
#' @examples
#'    \dontrun{
#'    Calling jsonMeanValues on Windows operational system:
#'    jsonMeanValues("C:\\Folder\\WS1718.sav", "C:\\Folder\\eval.json")}
#'

jsonMeanValues <- function(spss_file, path_to_json){

  #transforms spss data into a dataset
  evaluation <- read.spss(spss_file, to.data.frame=TRUE)

  eval_num <- extended_mean_df(evaluation)
  all_mean_values <- summarising_num(eval_num)

  #writes json document with indentation and unbox vector of length 1
  write_json(all_mean_values, path_to_json, pretty = TRUE, auto_unbox = TRUE)

}


#creates a data frame with the numeric values of all observations for each variable
extended_mean_df <- function(ws){

  #selects numeric vectors
  ws1718_num2 <- select(ws, verst:mot)

  #selects factors
  ws1718_num3 <- select(ws, gurt:regelm)

  #selects course name
  id_course_name <- cbind(id = c(1:nrow(ws)), select(ws, vlidstr))

  #renames level names from factor variables
  levels(ws1718_num3$gurt) <- c(1,2,3,4,5)
  levels(ws1718_num3$stoff) <- c(1,2,3,4,5)
  levels(ws1718_num3$note) <- c(1,2,3,4,5)
  levels(ws1718_num3$regelm) <- c(1,2,3,4)

  #converts factors to numeric
  ws1718_num4 <- sapply(ws1718_num3, function(x){
    x <- as.numeric(levels(x))[x]
    x
  })

  #merges variables gurt:regelm and verst:mot in the same
  ext_df <- cbind(id_course_name, ws1718_num4, ws1718_num2)
  ext_df
}


#creates a data frame with the mean value of each numeric variable in the dataset
summarising_num <- function(df_num){

  n_df_num <- summarise_all(group_by(df_num, vlidstr), mean, na.rm = TRUE)
  n_df_num <- n_df_num[,-2]
  n_df_num[[1]] <- trimws(n_df_num[[1]])

  #Replaces the name of the column vlidstr for "vlid"
  colnames(n_df_num)[1]<- "vlid"

  n_df_num
}
