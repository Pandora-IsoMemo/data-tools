FROM inwt/r-shiny:4.3.2

RUN echo "options(repos = c(getOption('repos'), PANDORA = 'https://Pandora-IsoMemo.github.io/drat/'))" >> /usr/local/lib/R/etc/Rprofile.site

ADD . .

RUN installPackage

CMD ["Rscript", "-e", "library(shiny); DataTools::startApplication(3838, '0.0.0.0')"]
