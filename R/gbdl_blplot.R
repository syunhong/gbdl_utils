# ------------------------------------------------------------------------------
# gbdl_biplot()
# ------------------------------------------------------------------------------
gbdl_blplot <- function(y_bar, y_line, wgts, 
                        lines.args = list(type = "b"), ...) {
  
  if (missing(wgts)) {
    if (max(y_line) > max(y_bar))
      wgts <- max(y_line) / max(y_bar)
    else
      wgts <- max(y_bar) / max(y_line)
  }
  
  opar <- par()
  par(mar = rep(4, 4))
  
  xval <- (1:length(y_line)) * 1.2 - 0.5
  xlim <- c(0, length(y_bar) + 2.1)
  
  barplot(y_bar, width = 1, space = 0.2, xpd = FALSE, xlim = xlim, ...)
  axis(1, labels = FALSE, at = xlim)
  
  yax <- axTicks(4)
  intrvl <- (max(y_line) - min(y_line)) / length(yax)
  y_labels <- min(y_line) + intrvl * (0:(length(yax)-1))
  axis(4, at = yax, labels = round(y_labels, 2))
  
  lines.args <- list(x = xval, y = y_line * wgts, lines.args)
  do.call("lines", lines.args)
  
  par(mar = opar$mar)
  
  invisible(TRUE)
}
