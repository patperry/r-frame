RSCRIPT= Rscript --vanilla
BUILT_VIGNETTES= \
	vignettes/dplyr.Rmd \
	vignettes/index.Rmd

all: README.md $(BUILT_VIGNETTES)

NEWS: NEWS.md
	sed -e 's/^### //g; s/`//g' $< > $@

README.md: README.Rmd
	$(RSCRIPT) -e 'devtools::load_all(); knitr::knit("README.Rmd")'

vignettes/%.Rmd: vignettes/%.Rmd.in
	$(RSCRIPT) -e 'devtools::load_all(); setwd("vignettes"); knitr::knit(basename("$<"), basename("$@"))'

check:
	$(RSCRIPT) -e 'devtools::test(".")'

clean:
	$(RSCRIPT) -e 'devtools::clean_dll(".")' && rm -f README.md

cov:
	$(RSCRIPT) -e 'covr::package_coverage(line_exclusions = "R/deprecated.R")' 

dist: $(BUILT_VIGNETTES) NEWS README.md
	mkdir -p dist && cd dist && R CMD build ..

distclean: clean
	rm -rf $(BUILT_VIGNETTES)

doc: $(BUILT_VIGNETTES) NEWS README.md

install:
	$(RSCRIPT) -e 'devtools::install()'

site:
	$(RSCRIPT) -e 'pkgdown::build_site()'

.PHONY: all check clean cov dist distclean doc install site
