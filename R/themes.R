# flrtheme.R - 
# ggplotFL/R/flrtheme.R

# Copyright 2003-2007 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# $Id:  $

# mytheme {{{
mytheme <- function(theme=theme_bw, base_size = 12, base_family="", ...)
  modifyList(do.call(theme, list(base_size=base_size, base_family=base_family)),
    list(...))
# }}}

# theme_flr {{{
theme_flr<-function (size           =10,        font=NA,            face='plain',
                     backgroundColor='white',   panelColor='white',
                     axisColor      ='#999999', gridColor ='white', textColor='#999999'){

    theme_text = function(...) ggplot2::theme_text(family=font,face=face,colour=textColor,size=size, ...)

    opts(axis.text.x       =theme_text(vjust=1, hjust=0.5),
         axis.text.y       =theme_text(hjust=1, vjust=0.5),
         axis.title.x      =theme_text(),
         axis.title.y      =theme_text(angle=90),
         axis.line         =theme_blank(),
         axis.ticks        =theme_segment(colour=axisColor, size=0.25),
         panel.border      =theme_rect(colour=axisColor),
         legend.background =theme_blank(),
         legend.key        =theme_blank(),
         legend.key.size   =unit(1.5, 'lines'),
         legend.text       =theme_text(hjust=0),
         legend.title      =theme_text(hjust=0),
         panel.background  =theme_rect(fill=panelColor, colour=NA),
         plot.background   =theme_rect(fill=backgroundColor, colour=NA),
         panel.grid.major  =theme_line(colour=gridColor, size=0.33),
         panel.grid.minor  =theme_blank(),
         strip.background  =theme_rect(fill=NA, colour=NA),
         strip.text.x      =theme_text(hjust=0),
         strip.text.y      =theme_text(angle=-90),
         plot.title        =theme_text(hjust=0),
         plot.margin       =unit(c(0.1, 0.1, 0.1, 0.1), 'lines'))}

 # }}}

# theme_min {{{
theme_min <- function (size=10, font=NA, face='plain',
  backgroundColor='white', panelColor='white', axisColor='#999999',
  gridColor='white', textColor='black') {

    opts(axis.text.x = theme_text(vjust=1, hjust=0.5, colour=axisColor,
           family=font, face=face, size=size),
         axis.text.y = theme_text(hjust=1, vjust=0.5, colour=axisColor,
           family=font, face=face, size=size),
         axis.title.x = theme_text(colour=textColor, family=font, face=face,
           size=size),
         axis.title.y = theme_text(angle=90, colour=textColor, family=font,
           face=face, size=size),
         axis.line = theme_blank(),
       axis.ticks = theme_segment(colour=axisColor, size=0.25),
       panel.border = theme_rect(                      colour=axisColor),
       legend.background = theme_blank(),
       legend.key = theme_blank(),
       legend.key.size = unit(1.5, 'lines'),
       legend.text = theme_text(hjust=0, colour=textColor, family=font,
         face=face, size=size),
       legend.title = theme_text(hjust=0, colour=textColor, family=font,
         face=face, size=size),
       panel.background = theme_rect(fill=panelColor, colour=NA),
       plot.background = theme_rect(fill=backgroundColor, colour=NA),
       panel.grid.major = theme_line(colour=gridColor, size=0.33),
       panel.grid.minor = theme_blank(),
       strip.background = theme_rect(fill=NA, colour=NA),
       strip.text.x = theme_text(hjust=0, colour=textColor, family=font,
         face=face, size=size),
       strip.text.y = theme_text(angle=-90, colour=textColor, family=font,
         face=face, size=size),
       plot.title = theme_text(hjust=0, colour=textColor, family=font,
         face=face, size=size),
       plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), 'lines'))} 
# }}}
