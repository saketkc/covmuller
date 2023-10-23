Rscript -e "pkgdown::init_site()"
ls vignettes/*.Rmd | xargs -n1 basename | cut -f 1 -d '.' | parallel -j 48 "Rscript -e 'pkgdown::build_article(\"{}\")'" || true
Rscript -e "pkgdown::build_news()" || true
Rscript -e "pkgdown::build_home()" || true
