TEMPLATE=tufte-handout.template
PDFS=ggplotFL.pdf
R_HOME=/usr/local
FIGS=flstock

all: $(PDFS)

clean:
	rm -rf *.tex *.bbl *.blg *.aux *.out *.log *.spl *.md figure/ cache/

%.pdf: %.tex
	pdflatex $*.tex

%.tex: %.md
	pandoc -o $*.tex --template=$(TEMPLATE) $*.md
	sed -i 's/figure}.*/marginfigure}/' $*.tex

%.md: %.Rmd
	$(R_HOME)/bin/Rscript -e "library(knitr)" \
		-e "knit('$*.Rmd')"

test:
	if [ $${FIGSLN} -gt 1 ] ; then \
		FIGSLN=`grep -n "figure\/$(FIGS).pdf" $*.tex | awk -F: '{print $$1}'`; \
		sed -i '$(shell expr $$(FIGSLN) - 2)s/figure}.*/figure*}/' $*.tex; \
		sed -i '$(shell expr $$(FIGSLN) + 3)s/figure}.*/figure*}/' $*.tex; \
	fi
	LN=$(shell grep -n "figure\/$(FIGS).pdf" $*.tex | awk -F: '{print $$1}');\
		PRE=`expr $$LN - 2`; \
		$(shell sed '$$PREs/figu/FIGU/' $*,tex); \
		echo $$PRE
