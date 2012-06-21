######## Plot stuff #############################################################################################
theme_ms = function (base_size=16,axis.text=1.0,font=NA,face='plain', 
	                   backgroundColor='white',   panelColor='white', 
                     axisColor      ='#999999', gridColor= 'white', textColor='black',
                     ...) {

      #theme_set(theme_bw(base_size=base_size))
    
      #res=theme_get()
      
      (res=opts(panel.grid.major = theme_blank(),
                           panel.grid.minor = theme_blank(),
                           plot.background  = theme_rect(fill="white",colour="white"),
                           panel.background = theme_rect(fill="white",colour="white"),
                           plot.margin      = unit(rep(0.2, 4), "lines"),
                           axis.ticks.margin= unit(0.1, "lines"),
                           axis.text.x       = theme_text(hjust=1, vjust=0.5,   colour=axisColor, family=font, face=face, size=base_size*axis.text),
                           axis.text.y       = theme_text(hjust=1, vjust=0.5,   colour=axisColor, family=font, face=face, size=base_size*axis.text),
                           axis.title.x      = theme_text(                      colour=textColor, family=font, face=face, size=base_size),
                           axis.title.y      = theme_text(angle=90,             colour=textColor, family=font, face=face, size=base_size),
                           axis.line         = theme_blank(),
                           axis.ticks        = theme_segment(                   colour=axisColor,                         size=0.25),
                           panel.border      = theme_rect(                      colour=axisColor),
                           legend.background = theme_blank(),
                           legend.key        = theme_blank(),
                           legend.key.size   = unit(1.5, 'lines'),
                           legend.text       = theme_text(hjust=0,              colour=textColor, family=font, face=face, size=base_size*1),
                           legend.title      = theme_text(hjust=0,              colour=textColor, family=font, face=face, size=base_size*1),
                           legend.position   ="bottom",
                           legend.direction  ="horizontal",
                           panel.background  = theme_rect(fill=panelColor,      colour=NA),
                           plot.background   = theme_rect(fill=backgroundColor, colour=NA),
                           panel.grid.major  = theme_line(                      colour=gridColor,                         size=0.33),
                           panel.grid.minor  = theme_blank(),
                           strip.background  = theme_rect(fill=NA,              colour=NA),
                           strip.text.x      = theme_text(                      colour=textColor, family=font, face=face, size=base_size*1.5),
                           strip.text.y      = theme_text(angle=-90,            colour=textColor, family=font, face=face, size=base_size*1.5),
                           plot.title        = theme_text(hjust=0,              colour=textColor, family=font, face=face, size=base_size),
                           plot.margin       = unit(c(0.1, 0.1, 0.1, 0.1), 'lines')))
                        
        modifyList(res, list(...))}

theme_invisible <- function(base_size = 12) {
  structure(list(
    axis.line =         theme_blank(),
    axis.text.x =       theme_text(colour = NA,size = base_size * 0.8 , lineheight = 0.9, vjust = 1),
    axis.text.y =       theme_text(colour = NA,size = base_size * 0.8, lineheight = 0.9, hjust = 1),
    axis.ticks =        theme_segment(colour = NA, size = 0.2),
    axis.title.x =      theme_text(colour = NA,size = base_size, vjust = 1),
    axis.title.y =      theme_text(colour = NA,size = base_size, angle = 90, vjust = 0.5),
    axis.ticks.length = unit(0.3, "lines"),
    axis.ticks.margin = unit(0.5, "lines"),
 
    legend.background = theme_rect(colour=NA), 
    legend.key =        theme_rect(colour = NA, ),
    legend.key.size =   unit(1.2, "lines"),
    legend.text =       theme_text(colour = NA,size = base_size * 0.8),
    legend.title =      theme_text(colour = NA,size = base_size * 0.8, face = "bold", hjust = 0),
    legend.position =   "right",
 
    panel.background =  theme_rect(fill = NA, colour = NA), 
    panel.border =      theme_rect(fill = NA, colour=NA), 
    panel.grid.major =  theme_line(colour = NA, size = 0.2),
    panel.grid.minor =  theme_line(colour = NA, size = 0.5),
    panel.margin =      unit(0.25, "lines"),
 
    strip.background =  theme_rect(fill = NA, colour = NA), 
    strip.text.x =      theme_text(colour = NA,size = base_size * 0.8),
    strip.text.y =      theme_text(colour = NA,size = base_size * 0.8, angle = -90),
 
    plot.background =   theme_rect(colour = NA),
    plot.title =        theme_text(colour = NA,size = base_size * 1.2),
    plot.margin =       unit(c(1, 1, 0.5, 0.5), "lines")
  ), class = "options")
}

theme_complete_bw <- function(base_size = 12) {
  structure(list(
    axis.line =         theme_blank(),
    axis.text.x =       theme_text(size = base_size * 0.8 , lineheight = 0.9, colour = "black", vjust = 1),
    axis.text.y =       theme_text(size = base_size * 0.8, lineheight = 0.9, colour = "black", hjust = 1),
    axis.ticks =        theme_segment(colour = "black"),
    axis.title.x =      theme_text(size = base_size, vjust = 0.5),
    axis.title.y =      theme_text(size = base_size, angle = 90, vjust = 0.5),
    axis.ticks.length = unit(0.15, "cm"),
    axis.ticks.margin = unit(0.1, "cm"),
 
    legend.background = theme_rect(colour=NA), 
    legend.key =        theme_rect(fill = NA, colour = "black", size = 0.25),
    legend.key.size =   unit(1.2, "lines"),
    legend.text =       theme_text(size = base_size * 0.8),
    legend.title =      theme_text(size = base_size * 0.8, face = "bold", hjust = 0),
    legend.position =   "right",
 
    panel.background =  theme_rect(fill = NA, colour = "black", size = 0.25), 
    panel.border =      theme_blank(),
    panel.grid.major =  theme_line(colour = "black", size = 0.05),
    panel.grid.minor =  theme_line(colour = "black", size = 0.05),
    panel.margin =      unit(0.25, "lines"),
 
    strip.background =  theme_rect(fill = NA, colour = NA), 
    strip.text.x =      theme_text(colour = "black", size = base_size * 0.8),
    strip.text.y =      theme_text(colour = "black", size = base_size * 0.8, angle = -90),
 
    plot.background =   theme_rect(colour = NA, fill = "white"),
    plot.title =        theme_text(size = base_size * 1.2),
    plot.margin =       unit(c(1, 1, 0.5, 0.5), "lines")
  ), class = "options")
}
