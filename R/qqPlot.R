#### Plots ##########################################################################################
# Quantile-comparison plots (J. Fox)

# last modified 30 September 2009 by J. Fox
# November 2009 by S. Weisberg -- changed to use showLabels for point identification
# 14 April 2010: set id.n = 0. J. Fox
# 1 June 2010: set reps=100 in qqPlot.lm. J. Fox
# 28 June 2010: fixed labeling bug S. Weisberg
# 11 March 2011: moved up ... argument. J. Fox

qqPlot.default <- function(x, distribution="norm", ..., ylab=deparse(substitute(x)),
    xlab=paste(distribution, "quantiles"), main=NULL, las=par("las"),
		envelope=.95,  
		col=palette()[1], col.lines=palette()[2], lwd=2, pch=1, cex=par("cex"), 
		line=c("quartiles", "robust", "none"), 
		labels = if(!is.null(names(x))) names(x) else seq(along=x),
		id.method = "y", 
		id.n = if(id.method[1]=="identify") Inf else 0,
		id.cex=1, id.col=palette()[1], grid=TRUE, noShow=TRUE)
{
res=list()

	line <- match.arg(line)
	good <- !is.na(x)
	ord <- order(x[good])
	ord.x <- x[good][ord]
	ord.lab <- labels[good][ord]
	q.function <- eval(parse(text=paste("q", distribution, sep="")))
	d.function <- eval(parse(text=paste("d", distribution, sep="")))
	n <- length(ord.x)
	P <- ppoints(n)
	z <- q.function(P, ...)
	if (!noShow) plot(z, ord.x, type="n", xlab=xlab, ylab=ylab, main=main, las=las)
	if(grid & !noShow){
		grid(lty=1, equilogs=FALSE)
		box()}
	if (!noShow) points(z, ord.x, col=col, pch=pch, cex=cex)
	if (line == "quartiles" || line == "none"){
		Q.x <- quantile(ord.x, c(.25,.75))
		Q.z <- q.function(c(.25,.75), ...)
		b <- (Q.x[2] - Q.x[1])/(Q.z[2] - Q.z[1])
		a <- Q.x[1] - b*Q.z[1]
		if (!noShow) abline(a, b, col=col.lines, lwd=lwd)
		res$a=a
		res$b=b
	}
	if (line=="robust") {
		coef <- coef(rlm(ord.x ~ z))
		a <- coef[1]
		b <- coef[2]
		if (!noShow) abline(a, b)
		res$a=a
		res$b=b
	}
	conf <- if (envelope == FALSE) .95 else envelope
	zz <- qnorm(1 - (1 - conf)/2)
	SE <- (b/d.function(z, ...))*sqrt(P*(1 - P)/n)
	fit.value <- a + b*z
	upper <- fit.value + zz*SE
	lower <- fit.value - zz*SE
	res$z=z
        res$lower=lower
        res$upper=upper
	if (envelope != FALSE & !noShow) {
		lines(z, upper, lty=2, lwd=lwd, col=col.lines)
		lines(z, lower, lty=2, lwd=lwd, col=col.lines)
	}
	if (!noShow) showLabels(z, ord.x, labels=ord.lab,
		  	id.method = id.method, id.n = id.n, id.cex=id.cex, id.col=id.col)

invisible(res)}
