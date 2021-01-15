## conditional mutation
## works like mutate(), but only performs the operation on a subset of the dataset, returns the whole dataset
##first arguement subsets the data, second argument mutates that subset
## example: prism2 <- prism2 %>% mutate_cond(Plot %in% MVI_PPI$Plot, Plot_type = "RAPID")


mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
  condition <- eval(substitute(condition), .data, envir)
  .data[condition , ] <- .data[condition, ] %>% mutate(...,  na.rm = TRUE)
  .data
}
