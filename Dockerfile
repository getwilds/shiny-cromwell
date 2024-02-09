FROM fredhutch/r-shiny-server-base:4.3.2

RUN apt-get update -y && apt-get install -y libssh-dev

RUN R -q -e 'install.packages("pak", repos="https://packagemanager.posit.co/cran/__linux__/jammy/latest")'

RUN R -q -e 'options(repos = c(POSIT = "https://packagemanager.posit.co/cran/__linux__/jammy/latest"))' \
	-e 'pak::pak(c("ellipsis", "shiny",  "shinyFeedback", "shinyWidgets", "shinydashboard", "shinydashboardPlus", "ssh", "paws", "remotes", "markdown", "lubridate", "jsonlite", "cookies", "dplyr", "RSQLite", "DBI", "data.table", "DT", "glue", "httr", "purrr", "RColorBrewer", "rlang", "shinyBS", "shinyjs", "tidyverse", "uuid", "base64enc"))'

RUN R -q -e "remotes::install_github('getwilds/rcromwell@v3.2.0')"

RUN R -q -e "remotes::install_github('getwilds/proofr@v0.2')"


ADD check.R /tmp/

RUN R -f /tmp/check.R --args ellipsis shiny shinyWidgets shinydashboard shinydashboardPlus ssh paws remotes markdown lubridate jsonlite rcromwell DT tidyverse RColorBrewer glue shinyBS shinyjs shinyFeedback rmarkdown proofr httr cookies dplyr RSQLite DBI purrr data.table rlang uuid base64enc

RUN rm -rf /srv/shiny-server/
ADD app/. /srv/shiny-server/
EXPOSE 3838
