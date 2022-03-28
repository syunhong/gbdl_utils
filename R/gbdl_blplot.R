# ------------------------------------------------------------------------------
# gbdl_blplot()
# ------------------------------------------------------------------------------
gbdl_blplot <- function(y_bar, y_line, ylim, lines.args = list(type = "b"), 
                        ...) {

  if (missing(ylim))
    ylim <- range(y_bar) * c(0.8, 1.2)
    
  opar <- par()
  par(mar = rep(4, 4))
  
  xval <- (1:length(y_line)) * 1.2 - 0.5
  xlim <- c(0, length(y_bar) + 2.1)
  
  barplot(y_bar, width = 1, space = 0.2, xpd = FALSE, xlim = xlim, ylim = ylim, 
          ...)
  axis(1, labels = FALSE, at = xlim)
  
  yax <- axTicks(4)
  steps <- (max(y_line) - min(y_line)) / length(yax)
  axis(4, at = yax, 
       labels = round(min(y_line) + steps * (0:(length(yax)-1)), 2))


  y_rescaled <- y_line - min(y_line)
  y_rescaled <- y_rescaled / max(y_rescaled)
  y_rescaled <- y_rescaled * (max(y_bar) - min(y_bar)) + min(y_bar)
  
  lines.args <- list(x = xval, y = y_rescaled, lines.args)
  do.call("lines", lines.args)
  
  par(mar = opar$mar)
  
  invisible(TRUE)
}
