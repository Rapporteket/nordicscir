FROM rapporteket/base-r:main

LABEL maintainer="Arnfinn Hykkerud Steindal <arnfinn.hykkerud.steindal@helse-nord.no>"
LABEL no.rapporteket.cd.enable="true"

WORKDIR /app/R

COPY *.tar.gz .

RUN R -e "remotes::install_local(list.files(pattern = \"*.tar.gz\"))" \
    && rm ./*.tar.gz \
    && R -e "remotes::install_github(\"Rapporteket/rapbase\", ref = \"main\")"

EXPOSE 3838

CMD ["R", "-e", "options(shiny.port = 3838, shiny.host = \"0.0.0.0\"); nordicscir::kjor_NSapper(register='nordicscir')"]
