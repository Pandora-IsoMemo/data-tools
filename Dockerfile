FROM ghcr.io/pandora-isomemo/base-image:latest

ADD . .

# Install ollamar from GitHub
RUN Rscript -e "remotes::install_github('hauselin/ollama-r')"

RUN installPackage

CMD ["Rscript", "-e", "library(shiny); DataTools::startApplication(3838), '0.0.0.0'"]
