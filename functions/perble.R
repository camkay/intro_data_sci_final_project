###############################
###perble function
###############################

.perble <- function(column = NULL){
  temp <- table(column)
  percent_table <- rbind(temp, temp / sum(as.vector(temp))*100)
  rownames(percent_table) <- c("Count", "Percent")
  return(percent_table)
}
