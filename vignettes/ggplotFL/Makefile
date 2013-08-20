FILE=ggplotFL
TEMPLATE=tufte-handout.tex

all: knitr tex pdf
	mv $(FILE).pdf ../

knitr: $(FILE).Rmd
	R -e "library(knitr); knit('$(FILE).Rmd')"

tex: $(FILE).md
	pandoc -o $(FILE).tex --template=$(TEMPLATE) $(FILE).md
# TODO: search w/ awk for figure, match names in VAR, then substitute
	sed -i 's/figure}.*/marginfigure}/' $(FILE).tex

pdf: $(FILE).tex
	pdflatex $(FILE).tex

clean:
	rm -f $(FILE).md $(FILE).log $(FILE).tex $(FILE).aux $(FILE).out

cleanall: clean
	rm -f $(FILE).pdf
	rm -f figure/*
	rm -f cache/*
