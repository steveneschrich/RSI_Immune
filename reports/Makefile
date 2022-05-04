all: cohort

%.pdf: %.Rmd
  Rscript -e "rmarkdown::render($<)"
  
cohort: Extract_Cohort/Extract_Cohort.pdf

