
all: CARNIVAL.html

CARNIVAL.html:
	R --slave -e "library(rmarkdown); rmarkdown::render('CARNIVAL.Rmd', output_format = 'html_vignette')"

clean:
	rm -rf CARNIVAL.html example*/
