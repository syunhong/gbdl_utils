# ------------------------------------------------------------------------------
# process_pop()
# ------------------------------------------------------------------------------
process_pop <- function(df) {
  
  n <- ncol(df)
  for (i in 5:n) {
    df[,i] <- as.numeric(as.character(df[,i]))
    df[,i] <- round(df[,i], 0)
  }
  
  d <- as.character(df[,1])
  df[,1] <- paste(substr(d, 1, 4), substr(d, 5, 6), substr(d, 7, 8), sep = "-")
  
  colnames(df) <- c("p_date", "p_time", "adm_code", "oa_code", "pop", 
                    "m00", "m10", "m15", "m20", "m25", "m30", "m35", "m40", 
                    "m45", "m50", "m55", "m60", "m65", "m70", "f00", "f10", 
                    "f15", "f20", "f25", "f30", "f35", "f40", "f45", "f50", 
                    "f55", "f60", "f65", "f70")

  return(df)
}