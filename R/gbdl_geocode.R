# ------------------------------------------------------------------------------
#
# gbdl_geocode()
#
# ------------------------------------------------------------------------------
gbdl_geocode <- function(addr, method = "naver", client_id, client_secret, 
                         save_xml = TRUE, xml_dir, out_dir, 
                         autosave_freq = 100, showProgress = TRUE) {
  require(httr)
  require(xml2)
  
  n <- length(addr)
  
  if (missing(xml_dir))
    xml_dir <- getwd()
  if (missing(out_dir))
    out_dir <- getwd()
  if (missing(autosave_freq))
    autosave_freq <- n + 1
  
  res <- data.frame(id = 1:n, input_addr = addr, 
                    road_addr = NA, jibun_addr = NA, en_addr = NA, 
                    x = NA, y = NA, dist = NA)
  
  if (showProgress)
    pb <- txtProgressBar(min = 0, max = n, style = 3)
  
  for (i in 1:n) {
    if (showProgress)
      setTxtProgressBar(pb, i)
    
    tmp <- GET(url = url, query = list(query = addr[i]),
               add_headers("X-NCP-APIGW-API-KEY-ID" = client_id,
                           "X-NCP-APIGW-API-KEY" = client_secret))
    
    # If an error occurs:
    tryCatch({ 
      Sys.sleep(time = 1) 
    }, error = function(e) cat("\nUnable to geocode at", i, "\n"))
    
    tmp <- content(tmp, as = "parse", encoding = "UTF-8")
    if (save_xml)
      write_xml(tmp, paste0(xml_dir, "/id-", i, ".xml"))
    
    tmp_processed <- as_list(tmp)$addressResponse$address
    
    x <- as.numeric(unlist(tmp_processed$x))
    y <- as.numeric(unlist(tmp_processed$y))
    road_addr <- unlist(tmp_processed$roadAddress)
    jibun_addr <- unlist(tmp_processed$jibunAddress)
    en_addr <- unlist(tmp_processed$englishAddress)
    dist <- as.numeric(unlist(tmp_processed$distance))
    
    res[i,3:8] <- c(road_addr, jibun_addr, en_addr, x, y, dist)
    
    if ((i %% autosave_freq) == 0)
      save(res, file = paste0(out_dir, "/res-", i, ".RData"))
  }
  
  return(res)
}
