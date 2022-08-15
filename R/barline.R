# ******************************************************************************
#
# barline()
#
# ******************************************************************************
barline <- function(y_bar, y_line, ylim, lines.args = list(type = "b"), 
                    margins = c(5.1, 4.1, 4.1, 2.1), ylab_right, 
                    padj = 1, ...) {

  if (missing(ylim))
    ylim <- range(y_bar) * c(0.8, 1.2)
  if (missing(ylab_right))
    ylab_right <- ""
  
  opar <- par()
  par(mar = margins)
  
  xval <- (1:length(y_line)) * 1.2 - 0.5
  
  if (is.matrix(y_bar))
    xlim <- c(0, ncol(y_bar) + 2.1)
  else
    xlim <- c(0, length(y_bar) + 2.1)
  
  barplot(y_bar, width = 1, space = 0.2, xpd = FALSE, xlim = xlim, ylim = ylim, 
          ...)
  axis(1, labels = FALSE, at = xlim)
  
  y_rescaled <- y_line - min(y_line)
  y_rescaled <- y_rescaled / max(y_rescaled)
  
  if (is.matrix(y_bar)) {
    y_max <- max(colSums(y_bar))
    y_min <- min(colSums(y_bar))
  } else {
    y_max <- max(y_bar)
    y_min <- min(y_bar)
  }
  
  y_rescaled <- y_rescaled * (y_max - y_min) + y_min
  
  lines.args <- list(x = xval, y = y_rescaled, lines.args)
  do.call("lines", lines.args)
  
  # yax <- axTicks(4)
  # steps <- (max(y_line) - min(y_line)) / (length(yax) - 1)
  # axis(4, at = yax, 
  #     labels = round(min(y_line) + steps * (0:(length(yax)-1)), 2))
  alpha <- (max(y_line) - min(y_line)) / (y_max - y_min)
  beta <- min(y_line) - (y_min * alpha)
  axis(4, at = yax, labels = round(axTicks(2) * alpha + beta, 2))
  
  mtext(ylab_right, side = 4, padj = padj)
  
  par(mar = opar$mar)
  
  invisible(TRUE)
}
