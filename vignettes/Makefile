TEMPLATE=tufte-handout.template
PDFS= ggplotFL.pdf
R_HOME=/usr/local


all: $(PDFS)

clean:
	rm -rf *.tex *.bbl *.blg *.aux *.out *.log *.spl *.md figure/ cache/

%.pdf: %.tex
	pdflatex $*.tex

%.tex: %.md
	pandoc -o $*.tex --template=$(TEMPLATE) $*.md
	# HACK: Turn figure{} output by pandoc md2tex to marginfigure
	sed -i 's/figure}.*/marginfigure}/' $*.tex

%.md: %.Rmd
	$(R_HOME)/bin/Rscript -e "library(knitr)" \
		-e "knit('$*.Rmd')"

