library(plyr) #wegen ddply
library(dplyr)
library(tidyr)
library(foreign)
library(jsonlite)

ws1718 <- read.spss("C:\\Users\\Tai\\Uni\\SoSe18\\Medienerstellung\\Data\\WS1718.sav", to.data.frame=TRUE)


ws1718_fac <- cbind(id = c(1:4139), select(ws1718, vlid:mot4))
ws1718_num <- cbind(id = c(1:4139), select(ws1718, vlid, verst:mot))

my_summs <- summarising_num(ws1718_num)
#my_facs <- summarising_fac(ws1718_fac)
my_inverse_df <- creating_inverted_df(my_summs)

#Erstellt eine Data Frame mit dem Mittelwert von der Gesamtevaluation für jeden Kurs
summarising_num <- function(df_num){
  
  n_df_num <- summarise_all(group_by(df_num, vlid), mean, na.rm = TRUE) 
  n_df_num <- n_df_num[,-2]
  n_df_num
}

#inverts the summarised data frame: rows turn into columns
creating_inverted_df <- function(my_summs){
  
course_names <- my_summs[,1]
course_names <- course_names[[1]]
invert_df <- data.frame(t(my_summs))
rownames(invert_df) <- NULL
colnames(invert_df) <- course_names
invert_df <- invert_df[-1,]

categories <- names(my_summs)
row_categories <- c(categories[-1])

invert_df <- cbind(categorie = row_categories, invert_df)

  for(i in 2:ncol(invert_df)){
    invert_df[,i] <- as.numeric(as.character(invert_df[,i]))
  }

invert_df
}

#only_verst <- ordering_var_num(my_summs)
#Ordert jede Variablespalte nach größte Werten und gibt kreiert eine Liste mit dem Inhalt

#ordering_var_num <- function(summ_num){
#  
#  course <- select(summ_num, vlid, verst)
#  course <- arrange(course, desc(verst))
#  course
#}

data_temp <- data.frame(type = c("object", "animal", "human"), characteristic = I(list(list(breath = FALSE, color = "blue", age = 3), list(breath = FALSE, color = "red", age = 3), list(breath = FALSE, color = "green", age = 3))))
write_json(data_temp, "C:\\Users\\Tai\\Uni\\SoSe18\\Medienerstellung\\Data\\fourth_js.json", pretty = TRUE, auto_unbox = TRUE)

#summarising_fac <- function(df_fac){

  #n_df_fac <- summarise_all(group_by(df_fac, vlid), , na.rm = TRUE)
  #n_df_fac
#}

#writing a function that captures the "häufigkeit" from a factor
#factors are not ordered!!!
levels_of_verst1 <- list_of_fact(ws1718_fac$verst1)

list_of_fact <- function(a_row){
  my_list <- levels(a_row)
  my_list
}