# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'


gup_plot <- function(id="002468.sz",day=30) {
  library(tidyverse)
  library(pedquant)
  library(plotly)
  stock<-md_stock(id,from=today()-day,
                  to=today(),
                  source="163",
                  adjust="dividend")
  mydata<-stock[[1]]%>%as_tibble() %>%
    mutate(date=ymd(date),xx=c(1:length(date)))
  xdat<-mydata[seq(1,nrow(mydata),30),c('xx',"date")]
  p1<-ggplot()+geom_line(data=mydata,aes(x=xx,y=close))+
    theme_classic()
  ggplotly(p1)
}

