FROM ghcr.io/pandora-isomemo/base-image:latest

ADD . .

RUN installPackage

CMD ["Rscript", "-e", "library(shiny); DataTools::startApplication(3838, '0.0.0.0')"]
