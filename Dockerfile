FROM ghcr.io/pandora-isomemo/base-image:latest

ADD . .

RUN installPackage

CMD ["Rscript", "-e", "library(shiny); IsoAppTools::startApplication(3838), '0.0.0.0'"]
