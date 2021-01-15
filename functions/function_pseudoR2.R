#source: https://bookdown.org/egarpor/PM-UC3M/glm-deviance.html


r2Log <- function(model) {
  
  summaryLog <- summary(model)
  1 - summaryLog$deviance / summaryLog$null.deviance
  
}