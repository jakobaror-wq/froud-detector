# שימוש בבסיס R עם Shiny מובנה
FROM rocker/shiny:latest

# התקנת ספריות מערכת נדרשות
RUN apt-get update && apt-get install -y \
    libcurl4-gnutls-dev \
    libssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

# התקנת חבילות R
RUN R -e "install.packages(c('bslib', 'thematic', 'plotly', 'DT', 'dplyr', 'rvest', 'httr', 'jsonlite', 'class', 'rpart', 'stringr'), repos='https://cran.rstudio.com/')"

# העתקת קבצי האפליקציה לתוך המכולה
COPY . /srv/shiny-server/

# חשיפת הפורט של Shiny
EXPOSE 7860

# הרצת האפליקציה על הפורט ש-Hugging Face דורש
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/', host = '0.0.0.0', port = 7860)"]
