## 
## panel functions:
##
## gpanel.fit incorporating gpanel.band
## gpanel.text
##
## designed to work easily with 'layer' and 'glayer' in latticeExtra
## Added: 2014 09 18
## Author: GM
##

# gpanel.fit ####
#   Adds error bands. Uses two additional arguments: 
#   'lower' and 'upper' for the lower and upper values of the bands
#   There are, provisionally, two versions:
#   panel.band which is minimal. To get group colors, specify col = col.symbol
#   and panel.band2 based on panel.smoother in latticeExtra that does a lot
#   more work with colors, etc. 
#   We'll see which approach works best.
#
#FUTURE: gpanel.model 
# gpanel.model( x, y, model, se = T)
# will combine gpanel.fit and gpanel.band
#
gpanel.fit <-
  function(x, y, fit, lower, upper,
           subscripts, ..., type, group.number, alpha, col, col.line, col.symbol, border = F, font, fontface)
  {
    if( !missing(fit)) {
      if( missing(col) ) col <- 'blue'
      if( !missing(group.number)) {
        col <- col.line
      }
      if( !missing(subscripts) ) {
        fit <- fit[subscripts]
      }
      dd <- data.frame( x=x, fit = fit)
      dd <- dd[order(dd$x),]
      panel.lines( dd$x, dd$fit, ..., col = col, type = 'l')      
    }
    if( !missing(lower)){
      if( missing(alpha) || alpha == 1) alpha <- .3
      if( missing(col) ) col <- 'blue'
      if( !missing(group.number)) {
        col <- col.symbol
      }
      if( !missing(subscripts) ) {
        upper <- upper[subscripts]
        lower <- lower[subscripts]
      }
      dd <- data.frame( x=x, lower = lower, upper = upper)
      dd <- dd[order(dd$x),]
      panel.polygon( c(dd$x, rev(dd$x)),c(dd$upper, rev(dd$lower)), 
                     border = border, col = col, alpha = alpha,...)
    }
      #  panel.polygon(c(dd$x, rev(dd$x)), c(dd$upper, rev(dd$lower)), col = col, alpha = alpha, ...)
  }
#' Panel functions for predicted values and SE bands %% ~~function to do ... ~~
#' 
#' Panel functions for predicted values and SE bands using 'layer' and 'glayer'
#' in the package latticeExtra
#' 
#' With 'layer' and 'glayer' in 'latticeExtra', these functions can be used to
#' easily generate fitted values and confidence or prediction bands that have a
#' reasonable appearance whether a plot uses 'groups' or not. %% ~~ If
#' necessary, more details than the description above ~~
#' 
#' @aliases gpanel.band gpanel.fit gpanel.labels fillin
#' @param data data frame to be used to add additional values of numeric
#' variable
#' @param form formula evaluated in data. The first term defines the variable
#' with values to be filled in and the remaining terms define the variables to
#' be used for grouping determining the minima and maxima within which values
#' are added. %% ~~Describe \code{x} here~~
#' @param n the number of values to be added between the global mininum and
#' maximum. Values falling outside conditional minima and maxima are culled.
#' Default 200.
#' @param xpd expansion factor to add points beyond minima and maxima. Default
#' 1.0.
#' @param y %% ~~Describe \code{y} here~~
#' @param fit fitted values of a model, generally passed through 'layer' from a
#' call to 'xyplot': e.g. \code{xyplot( y ~ x, data, groups = g, fit =
#' data$yhat, lower = with(data, yhat - 2*se), upper = with(data, yhat + 2*se),
#' subscripts = T)} %% ~~Describe \code{lower} here~~
#' @param lower %% ~~Describe \code{lower} here~~
#' @param upper %% ~~Describe \code{upper} here~~
#' @param subscripts %% ~~Describe \code{subscripts} here~~
#' @param \dots %% ~~Describe \code{\dots} here~~
#' @param col %% ~~Describe \code{col} here~~
#' @param group.number %% ~~Describe \code{group.number} here~~
#' @param alpha to deal with the problem that 'trellis.par.get' will pass a
#' value of alpha = 1, 'panel.band' will set alpha = .3 if alpha = 1. To get a
#' nearly opaque band, use alpha = .99. %% ~~Describe \code{alpha} here~~
#' @param col.symbol is used to control color when using 'groups' %% ~~Describe
#' \code{col.symbol} here~~
#' @param border default = FALSE for panel.band. %% ~~Describe \code{border}
#' here~~
#' @param font %% ~~Describe \code{font} here~~
#' @param fontface %% ~~Describe \code{fontface} here~~
#' @return The 'panel.bands', 'panel.fit', and 'panel.labels' functions are
#' invoked for their graphical effect.
#' @author Georges Monette <georges@@yorku.ca> %% ~~who you are~~
#' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#' @references %% ~put references to the literature/web site here ~
#' @keywords ~kwd1 ~kwd2
#' @examples
#'   library(spida)
#'   library(latticeExtra)
#'   library(car)
#'   
#'   fit <- lm(prestige ~ (income + I(income^2)) * type, Prestige, 
#'       na.action = na.exclude)
#'   pred <- cbind( Prestige, predict(fit, newdata = Prestige, se = TRUE))
#'   head(pred)
#'   (p <- xyplot( prestige ~ income , pred,  groups = type,
#'                 subscripts = T,
#'                 fit = pred$fit,
#'                 lower = with(pred, fit - 2*se.fit), 
#'                 upper = with(pred, fit + 2*se.fit)))
#'   p + glayer(gpanel.band(...))
#'   p + glayer(gpanel.band(...)) + glayer(gpanel.fit(...))
#'   
#'   # Another example 
#'   # to illustrate 'fillin'
#'   
#'   fit <- lm( income ~ 
#'         (education+I(education^2)+I(education^3) +I(education^4))* type, 
#'         Prestige, na.action = na.exclude)    # overfitting!
#'   # adding extra values of predictor to get smooth line
#'   Prestige$occupation <- rownames(Prestige)
#'   z <- fillin(Prestige, ~education + type,xpd = 1.1)
#'   
#'   dim(z)  
#'   dim(Prestige)
#'   z <- cbind(z, predict(fit, newdata = z, se = TRUE))
#'   head(z)
#'   gd(3,cex=2,lwd=2, alpha = .7)
#'   (p <-  xyplot( income ~ education, z, groups = type,
#'                  subscripts = T,
#'                  fit = z$fit,
#'                  lower = z$fit - z$se,
#'                  upper = z$fit + z$se,
#'                  auto.key = list(space='right', lines= T)))
#'   p + glayer( gpanel.fit(...))
#'   p + glayer( gpanel.fit(...)) + glayer(gpanel.band(...))  
#'   p + glayer( gpanel.band(...,  alpha = .1))
#'   gd(3,lty=1,lwd=2)
#'   p + glayer( gpanel.band(...,alpha = .1)) + glayer(gpanel.fit(...))
#'   
#'   # with panels and no groups:
#'   
#'   (p <-  xyplot( income ~ education| type, z,
#'                  subscripts = T,
#'                  fit = z$fit,
#'                  lower = z$fit - z$se,
#'                  upper = z$fit + z$se,
#'                  auto.key = list(space='right', lines= T)))
#'   p + layer( gpanel.fit(...)) + layer( gpanel.band(...))
#'   gd(basecol = 'tomato4')
#'   p + layer( gpanel.band(...,  col = 'grey10')) + layer(gpanel.fit(...))
#'   p + layer( gpanel.band(...)) + layer(gpanel.fit(...))
#'   
#'   # With panels and groups
#'   
#'   z <- Prestige 
#'   z$gender <- with(z, cut( women, c(-1,15,50,101),labels = c("Male","Mixed","Female")))
#'   tab(z, ~ gender + type)
#'   z <- fillin( z, ~ education + type + gender, xpd = 1.1)
#'   fit <- lm( income ~ (education+I(education^2)+I(education^3) )* type * gender, 
#'              z, na.action = na.exclude)    # overfitting!
#'   summary(fit)
#'   z <- cbind( z, predict(fit, newdata = z, se = TRUE))
#'   head(z)
#'   (p <-  xyplot( income ~ education| gender, z, groups = type, 
#'                  subscripts = T,
#'                  fit = z$fit,
#'                  lower = z$fit - z$se,
#'                  upper = z$fit + z$se,
#'                  layout = c(1,3),
#'                  auto.key = list(space='right', lines= T, cex = 1.5)))
#'   
#'   p + glayer( gpanel.band(...)) + glayer(gpanel.fit(...))
#'   # trellis.focus()
#'   # panel.identify(labels= z$occupation)
#'   # trellis.unfocus()
#'   
#'   z$type2 <- with( z, reorder(type,education, mean, na.rm=T))
#'   gd(3)
#'   (p <-  xyplot( income ~ education| type2, z, groups = gender, 
#'                  subscripts = T,
#'                  fit = z$fit,
#'                  lower = z$fit - z$se,
#'                  upper = z$fit + z$se,
#'                  layout = c(1,3),
#'                  par.strip.text = list(cex = 2),  
#'                  auto.key = list(space='right', lines= T, cex = 1.5)))
#'   
#'   p + glayer( gpanel.fit(...))
#'   p + glayer( gpanel.band(...))
#'   p + glayer( gpanel.band(...)) + glayer(gpanel.fit(...))
#'   # trellis.focus()
#'   # panel.identify(labels= z$occupation)
#'   # trellis.unfocus()
#'   
#'   # With panels^2
#'   # need to remove 'col = col.line'
#'   
#'   z <- Prestige 
#'   z$occ <- rownames(Prestige)
#'   z$gender <- with(z, cut( women, c(-1,15,50,101),labels = c("Male","Mixed","Female")))
#'   z$type2 <- with( z, reorder(type,education, mean, na.rm=T))
#'   tab(z, ~ gender + type2)
#'   z <- fillin( z, ~ education + type + gender, xpd = 1.1)
#'   fit <- lm( income ~ (education+I(education^2)+I(education^3) )* type * gender, 
#'              z, na.action = na.exclude)    # overfitting!
#'   summary(fit)
#'   z <- cbind( z, predict(fit, newdata = z, se = TRUE))
#'   head(z)
#'   (p <-  xyplot( income ~ education| gender*type, z, 
#'                  subscripts = T,
#'                  fit = z$fit,
#'                  labels = z$occ,
#'                  lower = z$fit - z$se,
#'                  upper = z$fit + z$se,
#'                  auto.key = list(space='right', lines= T, cex = 1.5)))
#'   
#'   p + layer( gpanel.fit(...)) + layer( gpanel.band(...))
#'   p + layer( gpanel.band(..., col = 'black', alpha = .1)) + layer(gpanel.fit(...))  +
#'     layer(gpanel.text(...))
#'   
#' 
#' 
gpanel.band <- gpanel.fit
gpanel.labels <-
  function (x, y, labels , subscripts, ...) 
  {
    # NOTE: Also include names of anything you DON'T want passed.
    # this is an experiment in writing a function that can be
    # called via layer or glayer without further complications
    # e.g. xyplot(......,labels = rownames(data)) + layer( gpanel.labels(...))
    # or  xyplot(....., labels = rownames(data), subscripts = T) +
    # glayer(gpanel.labels(...))
    # for selected lables, use
    # trellis.focus()
    # panel.identify(labels = rownames(data),rot=-15,col = col.symbol, etc.)  
    #
    # Note that subscripting is taken care of
    # trellis.unfocus()
    
    if(!missing(subscripts)) {
      labels <- labels[subscripts]
    }
    panel.text(x, y, labels, ...)
  }


#' @export
fillin <- function(data, form, n = 200, xpd = 1.0) {
  # data: data frame with values of x to fill in within ranges of x within
  #       levels of g
  # form: formula idenfying variable x to fill in and grouping variables, g1, g2, etc.
  #       of form ~ x + g1 + g2 (the variable to fill in comes first)
  # n:    number of values to fill in across the entire range
  # xpd:  proportional range expansion
  
  sel.mf <- model.frame(form, data, na.action = na.pass)
  ret <- sel.mf
  xmina <- min(sel.mf[[1]], na.rm = T)
  xmaxa <- max(sel.mf[[1]], na.rm = T)
  xmida <- (xmaxa+xmina)/2
  xrana <- (xmaxa-xmina)/2
  xmina <- xmida - xpd*xrana
  xmaxa <- xmida + xpd*xrana
  if (ncol(sel.mf) ==1 ) {
    ret$xmin <- min(sel.mf[[1]],na.rm=T)
    ret$xmax <- max(sel.mf[[1]],na.rm=T)
  } else {
    grps <- lapply( sel.mf[2:ncol(sel.mf)], function(x)
      as.numeric(as.factor(x)))
    grps <- factor( do.call(paste,grps))
    ret$xmin <- capply( sel.mf[[1]], grps, min, na.rm=T)
    ret$xmax <- capply( sel.mf[[1]], grps, max, na.rm=T)
  }
  summ <- up(ret, formula(ret)[-2])  # has every unique combination of g's
  added.vals <- summ[ rep(1:nrow(summ), n),]
  added.vals[[names(ret)[1]]] <- rep( seq(xmina, xmaxa, length.out = n), each = nrow(summ))
  x <- added.vals[[names(ret)[1]]]
  x.min <- added.vals[['xmin']]
  x.max <- added.vals[['xmax']]
  x.mid <- (x.min + x.max)/2
  x.ran <- (x.max - x.min)/2
  ok <- (x <= (x.mid + xpd*x.ran)) & (x >= (x.mid - xpd*x.ran))
  merge( data, added.vals[ok,], all = T)  
}

# zd <- data.frame( x = 1:10, 
#                   g = rep(c('a','b'), each = 5), 
#                   y = 1:10,
#                   g2 = LETTERS[c(1,1,1,2,2,2,3,3,3,3)])
# zd
# (ff <- fillin(zd, ~ x + g, n = 10, xpd = 1.2))
# tab(ff, ~ g)
# barchart
# ?barchart
# library(latticeExtra)
# zz <- fillin(zd, ~ x + g+g2, n =100)
# xyplot( x ~ g|g2 ,zd, cex = 2) +
#   xyplot( x ~ g|g2, fillin(zd, ~ x + g+g2, n =100),col='red')
# 
# as.data.frame(tab(zd, ~g))




