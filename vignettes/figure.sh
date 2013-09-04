#!/bin/bash

# figure chunck names to modify as figure*
export FIGS=flstock

# figure chunck names to modify as figure

# FIND lines for FIGS
export FIGSLN=`grep -n "figure\/$FIGS.pdf" ggplotFL.tex | awk -F: '{print $1}'`

# GET section of file up to $FIGSLN and line with marginfigure
export LINE=`head -n $FIGSLN ggplotFL.tex| tac | grep -n "marginfigure" | head -n+1 | awk -F: '{print $1}'`

export LINEA=`expr $FIGSLN - $LINE + 1`

# CHANGE marginfigure to figure*

sed -i "$LINEA s/marginfigure/figure*/" ggplotFL.tex

# SAME DOWN
export LINE=`tail -n +$FIGSLN ggplotFL.tex | grep -n "marginfigure" | head -n+1 | awk -F: '{print $1}'`

export LINEA=`expr $FIGSLN + $LINE - 1`

sed -i "$LINEA s/marginfigure/figure*/" ggplotFL.tex
