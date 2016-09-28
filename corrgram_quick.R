library(corrgram)

panel.cor2 <- function (x, y, corr = NULL, col.regions, cor.method, digits = 2, 
                        cex.cor, ...) 
{
  if (is.null(corr)) {
    if (sum(complete.cases(x, y)) < 2) {
      warning("Need at least 2 complete cases for cor()")
      return()
    }
    else {
      corr <- cor(x, y, use = "pair", method = cor.method)
    }
  }
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  ncol <- 14
  pal <- col.regions(ncol)
  col.ind <- as.numeric(cut(corr, breaks = seq(from = -1, to = 1, 
                                               length = ncol + 1), include.lowest = TRUE))
  col.text <- ifelse(col.ind %in% c(1,2, 13, 14), 'grey', 'black')
  corr <- formatC(corr, digits = digits, format = "f")
  
  usr <- par("usr")
  rect(usr[1], usr[3], usr[2], usr[4], col = pal[col.ind], 
       border = NA)
  box(col = "lightgray")
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  
  r <- formatC(corr, digits = 2, format = "f")
  cex.cor <- .8/strwidth("-X.xx")
  text(0.5, 0.5, corr, cex = cex.cor, col = col.text)
}


x1 <- rnorm(100)
x2 <- rnorm(100)
x3 <- x1 + runif(100)
x4 <- 0.5 * x2 + runif(100) - x3 * 2
x5 <- rnorm(100) + x4

X <- data.frame(x1, x2, x3, x4, x5)

cor_X <- cor(X)

corrgram(cor_X, upper.panel = NULL,
         lower.panel = panel.cor2)

  replace_with_space <- function(x) gsub(x, pattern = ".", replacement = '\n', fixed = T)
