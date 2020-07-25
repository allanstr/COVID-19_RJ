# Painel COVID-19 RIO

O painel COVID-19 RIO é uma ferramenta desenvolvida por pesquisadores do LEGOS/UERJ que tem como intuito auxiliar na disseminação de informações e no combate à pandemia no Município do Rio de Janeiro. Nesee repositório encontram-se todos os arquivos utilizados no projeto, o qual foi hospedado em https://legos-uerj.shinyapps.io/Covid-19_Rio/.

O painel utiliza os dados de Covid-19 disponibilizados pela [prefeitura](https://experience.arcgis.com/experience/38efc69787a346959c931568bd9e2cc4) e as informações de renda média e população dos arquivos do [data.rio](http://www.data.rio/). Os dados provenientes do data.rio foram tratados previamente e convertidos em um arquivo .csv.

Todos os dados são tratados a partir da linguagem R e o painel é criado com os pacotes Shiny e Shinydashboard. Outras técnicas contidas no script são: criação de mapas com o pacote leaflet, manipulação de dados com o pacote tidyverse, manipulação de strings com o stringr, técnicas de visualização de dados com ggplot2 e plotly e aplicação do algoritmo k-means (um tipo de aprendizado de máquina não supervisionado).
