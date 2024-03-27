# Hello, world!
#
# This is an example function named 'gup_plot'
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


gup_plot <- function(id="002468.sz",day=30,size=2) {
  # id="002468.sz";day=30;size=2
  library(tidyverse)
  library(pedquant)
  library(plotly)
  library(zoo)
  stock<-md_stock(id,from=today()-day-5,
                  to=today(),
                  source="163",
                  adjust="dividend")
  mydata<-stock[[1]]%>%as_tibble() %>%
    mutate(mean_5day=zoo::rollmean(amount,k = 5, align = "right", fill = NA)/
             zoo::rollmean(volume, k = 5,align = "right", fill = NA)/100)
  p1<-ggplot(data=mydata,aes(x=date,y=close))+
    geom_line()+
    geom_point(aes(col=volume),size=size,show.legend = F)+
    geom_line(aes(y=mean_5day),col='red',alpha=0.6)+
    scale_color_gradient2(mid="green",high = 'red')+
    theme_dark()
  ggplotly(p1)
}

