TEMPLATE=tufte-handout.template
PDFS:=$(patsubst %.Rmd,%.pdf,$(wildcard *.Rmd))
R_HOME=/usr/local

all: $(PDFS)

clean:
	rm -rf *.tex *.bbl *.blg *.aux *.out *.log *.spl *.md figure/ cache/

cleanall: clean
	rm $(PDFS)

%.pdf: %.tex
	pdflatex $*.tex

%.tex: %.md
	pandoc -o $*.tex --template=$(TEMPLATE) $*.md
	sed -i 's/figure}.*/marginfigure}/' $*.tex

%.md: %.Rmd
	$(R_HOME)/bin/Rscript -e "library(knitr)" \
		-e "knit('$*.Rmd')"
