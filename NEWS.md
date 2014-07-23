# CHANGES IN ggplotFL VERSION 2.6

## NEW FEATURES
- New package vignette
- plot() method for FLQuants, reused by plot(FLStock)
- plot() method for FLQuant
- plot(FLStocks) now uses geom_ribbon for objects with multiple iters

## USER-VISIBLE CHANGES 
- Default ribbons for plot(FLStocks) with iters have no borders
- Ribbons for 90% intervals in plot for FLQuant and FQuants now have a dotted line added.
- plot(FLSR) has been improved: axis labels, use of plotmath and regression line on residuals AR plot.

## BUG FIXES
- Call to dcast had wrong argument name for value.var

## UTILITIES

## DOCUMENTATION
	- Added mention to plot(FLStocks) to vignette

## DEPRECATED & DEFUNCT
