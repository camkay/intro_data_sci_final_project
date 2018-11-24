###############################
###.rcol
###############################

.rcol <- function(column = NULL, data = df) {
  grep(column, colnames(data))
}