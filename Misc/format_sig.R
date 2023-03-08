format_sig <- function(x, k = 3){
  x <- round(x, digits = k)
  ifelse(x<=.01, paste0(x,'***'),
         ifelse(x<=.05, paste0(x,'**'),
                ifelse(x<=.1, paste0(x,'*'), x)))
}