# ******************************************************************************
#
# find_latlong()
#
# ******************************************************************************
find_latlong <- function(addr, id, secret, silent = TRUE, progressBar = TRUE,
                         save_xml = TRUE, xml_dir) {
  require(httr)
  require(xml2)
  
  # No. addresses to process in the current function call
  n <- length(addr)
  
  startTime <- Sys.time()
  if (save_xml & missing(xml_dir))
    xml_dir <- getwd()
  
  if (!silent) {
    cat("*** GBDL Geocoding Wrapper Function ***\n\n")
    cat("Geocoding API:", "Naver Maps\n")
    cat("Execution date:", as.character(startTime), "\n")
    cat("Input addresses:", n, "\n")
    
    if (save_xml) {
      cat("save_xml: Enabled\n")
      cat("xml_dir:", xml_dir, "\n")
    } else {
      cat("save_xml: Disabled\n")
    }
    
    if (progressBar)
      cat("progressBar: Enabled\n\n\n")
    else
      cat("progressBar: Disabled\n\n\n")
  }
  
  res <- data.frame(id = 1:n, input_addr = addr, 
                    road_addr = NA, jibun_addr = NA, en_addr = NA, 
                    x = NA, y = NA, dist = NA)
  err <- numeric()
  
  # If 'progressBar' is TRUE, initialise it.
  if (progressBar)
    pb <- txtProgressBar(min = 0, max = n, style = 3)
  
  # Process each input address
  for (i in 1:n) {
    
    if (progressBar)
      setTxtProgressBar(pb, i)
    
    tmp <- GET(url = url, query = list(query = addr[i]),
               add_headers("X-NCP-APIGW-API-KEY-ID" = id,
                           "X-NCP-APIGW-API-KEY" = secret))
    tmp <- content(tmp, as = "parse", encoding = "UTF-8")
    
    # If an XML document is returned from API (i.e., successful):
    if (any(is(tmp) %in% "xml_document")) {
      
      if (save_xml)
        write_xml(tmp, paste0(xml_dir, "/id-", i, ".xml"))
      
      tmp_processed <- as_list(tmp)$addressResponse$addresses
      
      x <- as.numeric(unlist(tmp_processed$x))
      y <- as.numeric(unlist(tmp_processed$y))
      road_addr <- unlist(tmp_processed$roadAddress)
      jibun_addr <- unlist(tmp_processed$jibunAddress)
      en_addr <- unlist(tmp_processed$englishAddress)
      dist <- as.numeric(unlist(tmp_processed$distance))
    }
    
    # If something went wrong:
    else {
      
      if (save_xml)
        save(tmp, file = paste0(xml_dir, "/id-", i, ".RData"))
      
      x <- NA; y <- NA
      road_addr <- NA
      jibun_addr <- NA
      en_addr <- NA
      dist <- NA
      err <- append(err, i)
    }
    
    # Append the result from API to the output data frame
    tryCatch({ 
      res[i,3:8] <- c(road_addr, jibun_addr, en_addr, x, y, dist)
    }, error = function(e) err <- append(err, i))
  }
  
  err <- unique(err)
  endTime <- Sys.time()
  
  if (!silent) {
    cat("\n\nGeocoding completed!\n\n")
    cat("Completion date:", as.character(endTime), "\n")
    cat("Elapsed time:", round(endTime - startTime, 2), "sec\n")
    cat("Input addresses:", n, "\n")
    cat("No. failed:", length(err), "\n")
    cat("Error rate: ", round(length(err)/n * 100, 2), "%\n", sep = "")
  }
  
  return(list(result = res, error = err, start = startTime, end = endTime))
}
