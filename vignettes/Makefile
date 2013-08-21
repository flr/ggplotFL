TEMPLATE=tufte-handout.template
PDFS= ggplotFL.pdf
R_HOME=/usr/local


all: $(PDFS)

clean:
	rm -rf *.tex *.bbl *.blg *.aux *.out *.log *.spl *.md figure/ cache/

%.pdf: %.tex
	pdflatex $*.tex
#	$(R_HOME)/bin/Rscript -e "library(tools); texi2pdf('$*.Rmd')"

%.tex: %.md
	pandoc -o $*.tex --template=$(TEMPLATE) $*.md
	sed -i 's/figure}.*/marginfigure}/' $*.tex

%.md: %.Rmd
	$(R_HOME)/bin/Rscript -e "library(knitr); knit('$*.Rmd')"
