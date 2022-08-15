# ******************************************************************************
#
# db_conn()
#
# ******************************************************************************
db_conn <- function(param, user_name, passwd) {
  
  require(RPostgreSQL)
  require(getPass)
  
  if (missing(user_name)) {
    user_name <- readline("Enter your username: ")
    Sys.sleep(3)
  }
  
  if (missing(passwd))
    passwd <- getPass(msg = "Enter your password")

  con <- dbConnect(dbDriver("PostgreSQL"), 
                   dbname = param$db, port = param$port, host = param$host, 
                   user = user_name, password = passwd)
  return(con)
}