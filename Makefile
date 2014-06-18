all: move rmd2md

vignette:
	cd inst/vign;\
	Rscript --vanilla -e 'library(knitr); knit("RRT_vignette.Rmd")'

move:
	cp inst/vign/RRT_vignette.md vignettes

pandoc:
	cd vignettes;\
	pandoc -H margins.sty RRT_vignette.md -o RRT_vignette.pdf --highlight-style=tango;\
	pandoc -H margins.sty RRT_vignette.md -o RRT_vignette.html --highlight-style=tango

rmd2md:
	cd vignettes;\
	cp RRT_vignette.md RRT_vignette.Rmd
