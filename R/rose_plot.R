#' rose
#' 声势浩大说
#' Log-Exponential density
#'
#' Compute the density or log-density for a Log-Exponential (LogExp)
#' distribution
#'
#' @param x vector of quantiles
#' @param y vector of rates
#' @param text wenben

rose_plot <- function(text="plot by xxx") {
  library(tidyverse)
  library(plotly)
  x<- seq(0, 24) /24
  t <- seq(0, 575, by = 0.5) / 575*20 *pi + 4 *pi
  grid <- expand.grid(x = x, t = t)
  x <- matrix(grid$x, ncol = 25, byrow = TRUE)
  t <- matrix(grid$t, ncol = 25, byrow = TRUE)
  p<- (pi/2)*exp(-t/(8*pi))
  change <- sin(15 * t) /150
  u<-1-(1-(3.6*t)%%(2*pi) /pi)^4/2+change
  y <- 2*(x^2- x)^2* sin(p)
  r<- u*(x*sin(p) +y *cos(p))
  xx=r*cos(t)
  yy=r*sin(t)
  zz=u*(x*cos(p)-y*sin(p))#花的平面参数，不晓得哪位大神计算的
  fig <- plot_ly(showscale = FALSE,
                 showlegend = FALSE)%>%
    add_surface(x = ~xx, y = ~yy, z = ~zz,color = ~zz,
                colors = 'Reds',opacity = 0.5)%>%
    add_trace(plot,x=rep(0,4),y=rep(0,4),z=seq(-0.5,0,length=4),
              mode='lines',
              line = list(color = 'green', width = 8))%>%
    add_text(x=0,y=0,z=1,text=text)
  fig
}

