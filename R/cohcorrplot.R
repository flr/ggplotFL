# cohcorrplot.R - DESC
# /cohcorrplot.R

# Copyright European Union, 2019
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.


# cohcorrplot
setGeneric("cohcorrplot", function(x, ...)
  standardGeneric("cohcorrplot"))

# FLCohort

#' @examples
#' cohcorrplot(FLCohort(stock.n(ple4)))


# {{{

setMethod("cohcorrplot", signature(x="FLCohort"),
  function(x) {

  # DATA.FRAME
  flc <- as.data.frame(x)

  # RESHAPE

  flc.wide <-  reshape(flc, direction="wide", timevar=names(flc)[1],
  idvar=names(flc)[2:6])
  
  names(flc.wide) <-  gsub("data.","", names(flc.wide))

  #

  pL <- vector("list", length=max(flc$age)^2)

  cMat <- matrix(NA, ncol = max(flc$age), nrow = max(flc$age))

  c2 <- numeric(max(flc$age)^2)

  za <- 1

  for (coh in min(flc$age):(max(flc$age))) {
    # 
  	if (coh > 1) {

		  for (corZ in (max(flc$age)-inc):1) {
  			d1 <- flc.wide[,c(ac(coh),ac(coh - corZ))]
	  		names(d1) <- c("x","y")
		  	c1 <- round(with(na.omit(d1),cor(x,y)),2)
			  c2[za] <- c1
  			pL[[za]] <- ggplot(data.frame(x = 1, y = 1, text = ac(c1)), aes(.data$x,.data$y)) +
	  		geom_text(aes(label = .data$text), size=10) +
		  		theme(axis.title=element_blank(),
        			axis.text=element_blank(),
        			axis.ticks=element_blank(),
        			panel.grid.major=element_blank(),
    				panel.grid.minor=element_blank())
			  za <- za + 1
		  }
	  }

  	pL[[za]] <- ggplot(data.frame(x = 1, y = 1, text = ac(coh)), aes(.data$x,.data$y)) +
	    geom_text(aes(label = .data$text), size=16) +
		  theme(axis.title=element_blank(),
        		axis.text=element_blank(),
        		axis.ticks=element_blank(),
        		panel.grid.major=element_blank(),
    			panel.grid.minor=element_blank())
  	c2[za] <- 1
	  za <- za + 1

	  if (coh < max(flc$age)) {
		
      for (inc in 1:(max(flc$age)-coh)) {
  			d1 <- flc.wide[,c(ac(coh),ac(coh + inc))]
	  		names(d1) <- c("x","y")
		  	cMat[coh, (coh+inc)] <- with(na.omit(d1),cor(x,y))
			  c2[za] <- with(na.omit(d1),cor(x,y))

        d1$za <- za
        pL[[za]] <- ggplot(data = na.omit(d1), aes(x=.data$x, y=.data$y)) +
		  		scale_fill_gradient2(low="blue", mid = "white", high="red",
					limits=c(-1,1), guide = FALSE) +
			  	geom_rect(aes(fill = c2[za]),xmin = -Inf,xmax = Inf,
	               ymin = -Inf,ymax = Inf,alpha = 0.8) +
				geom_point() +
				geom_smooth(method = "lm", fullrange = TRUE, col = 1) +
				theme(axis.title=element_blank(),
	        		axis.text=element_blank(),
	        		axis.ticks=element_blank())

			za <- za + 1
		}
	}
}

margin = theme(plot.margin = unit(c(0.01,0.01,0.01,0.01), "cm"))
pL <- lapply(pL, "+", margin)

do.call("grid.arrange", c(pL, ncol =(max(flc$age))))
}
) # }}}
