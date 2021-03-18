
all: CARNIVAL.html CARNIVAL.pdf

CARNIVAL.html:
	R --slave -e "library(rmarkdown); rmarkdown::render('CARNIVAL.Rmd', output_format = 'html_vignette')"

CARNIVAL.pdf:
	R --slave -e "library(rmarkdown); rmarkdown::render('CARNIVAL.Rmd', output_format='pdf_document')"

clean:
	rm -rf CARNIVAL.html CARNIVAL.pdf example*/
