# Packages ----------------------------------------------------------------

if(!require(shiny)){
  install.packages("shiny", dep = T)
  library("shiny")
}

if(!require(shinydashboard)){
  install.packages("shinydashboard", dep = T)
  library("shinydashboard")
}

if(!require(stringr)){
  install.packages("stringr", dep = T)
  library("stringr")
}

if(!require(stringi)){
  install.packages("stringi", dep = T)
  library("stringi")
}

if(!require(magrittr)){
  install.packages("magrittr", dep = T)
  library("magrittr")
}

if(!require(tidyverse)){
  install.packages("tidyverse", dep = T)
  library("tidyverse")
}

if(!require(leaflet)){
  install.packages("leaflet", dep = T)
  library("leaflet")
}

if(!require(RColorBrewer)){
  install.packages("RColorBrewer", dep = T)
  library("RColorBrewer")
}

if(!require(sf)){
  install.packages("sf", dep = T)
  library("sf")
}

if(!require(htmlwidgets)){
  install.packages("htmlwidgets", dep = T)
  library("htmlwidgets")
}

if(!require(plotly)){
  install.packages("plotly", dep = T)
  library("plotly")
}

if(!require(ggplot2)){
  install.packages("ggplot2", dep = T)
  library("ggplot2")
}

if(!require(forecast)){
  install.packages("forecast", dep = T)
  library("forecast")
}

if(!require(DT)){
  install.packages("DT", dep = T)
  library("DT")
}

if(!require(factoextra)){
  install.packages("factoextra", dep = T)
  library("factoextra")
}

if(!require(ggforce)){
  install.packages("ggforce", dep = T)
  library("ggforce")
}

# Loading data ------------------------------------------------------------

## Input dos dados e transformação dos tipos de variáveis

###### Casos de COVID-19 ######

dados_rio <- read.delim("https://pcrj.maps.arcgis.com/sharing/rest/content/items/754cc0698129404ba8bfb053cbdbd158/data",
                        header = F, skip = 1, sep = ";", na.strings = c("Missing","","#N/D", "NA", "N/D"),
                        stringsAsFactors = F, fileEncoding = "latin1")

names(dados_rio) <- c("classific","dt_notific","dt_sintomas","residencia",
                      "ap_resid","sexo","faixa_etaria","evolucao","dt_obito",
                      "raca_cor","dt_atualizacao")

dados_rio$evolucao[dados_rio$evolucao == "óbito"] <- "obito"

dados_rio %<>%
  mutate(dt_notific = as.Date(as.character(dt_notific), format = "%d/%m/%Y", origin = "1900-01-01"),
         dt_sintomas = as.Date(as.character(dt_sintomas), format = "%d/%m/%Y", origin = "1900-01-01"),
         dt_obito = as.Date(dt_obito, format = "%d/%m/%Y", origin = "1900-01-01"),
         classific = as.factor(classific),
         residencia = as.factor(residencia),
         sexo = as.factor(sexo),
         faixa_etaria = as.factor(faixa_etaria),
         evolucao = as.factor(evolucao),
         raca_cor = as.factor(raca_cor)
         ) %>%
  filter(dt_sintomas >= as.Date("01/01/2020", format = "%d/%m/%Y"))

dados_rio$residencia <- recode(dados_rio$residencia, 
                           "FREGUESIA-ILHA" = "FREGUESIA (ILHA)",
                           "FREGUESIA-JPA" = "FREGUESIA (JACAREPAGUA)")


###### População e renda dos bairros ###### 

dados_bairros_rio <- read.csv2("data/dados_bairros_rio.csv") %>%
  select(bairro,rendimento_nominal_medio_inclui_sem_rendimento,populacao) %>%
  rename("residencia" = "bairro",
         "renda_media" = "rendimento_nominal_medio_inclui_sem_rendimento")


###### Dados espaciais ###### 

mapa_bairros_rio <- st_read("data/Limite_de_Bairros.shp")

mapa_bairros_rio %<>%
  mutate(NOME = toupper(stri_trans_general(NOME, "Latin-ASCII")))

mapa_bairros_rio$NOME <- recode(mapa_bairros_rio$NOME , 
                                "OSVALDO CRUZ" = "OSWALDO CRUZ",
                                "CAVALCANTI" = "CAVALCANTE",
                                "RICARDO DE ALBUQUERQUE" = "RICARDO ALBUQUERQUE")


## Agregando dados bairro-dia + letalidade e casos e obitos por 100mil hab

obitos_bairro <- dados_rio %>%
  group_by(residencia, dt_obito) %>%
  summarise(obitos_novos = n()) %>%
  ungroup() %>%
  mutate(id = paste0(residencia,"_",dt_obito)) %>%
  select(id,obitos_novos)

dados_rio_casos <- dados_rio %>%
  group_by(residencia,dt_sintomas) %>%
  summarise(casos_novos = n()) %>%
  group_by(residencia) %>%
  complete(dt_sintomas = seq.Date(min(dados_rio$dt_sintomas, na.rm=T), 
                                  max(dados_rio$dt_sintomas, na.rm=T), 
                                  by="1 day"),
           fill = list(casos_novos = 0, obitos_novos = 0)) %>%
  ungroup() %>%
  mutate(id = paste0(residencia,"_",dt_sintomas)) %>%
  left_join(x=.,y=obitos_bairro, by="id") %>%
  mutate(obitos_novos = ifelse(is.na(obitos_novos),0,obitos_novos)) %>%
  rename("dia" = dt_sintomas) %>%
  select(residencia, dia, casos_novos, obitos_novos) %>%
  group_by(residencia) %>%
  mutate(casos = cumsum(casos_novos),
         obitos = cumsum(obitos_novos)) %>%
  left_join(dados_bairros_rio, by = "residencia") %>%
  mutate(casos_100m = casos*100000/populacao,
         obitos_100m = obitos*100000/populacao,
         letalidade = ifelse(casos==0,0,obitos/casos))

rm(obitos_bairro)


## Agregando dados para o Rio (soma dos bairros)

x <- dados_rio_casos %>%
  group_by(dia) %>%
  summarise(residencia = as.factor("RIO DE JANEIRO"),
            casos_novos = sum(casos_novos),
            obitos_novos = sum(obitos_novos),
            casos = sum(casos),
            obitos = sum(obitos),
            renda_media = NA,
            populacao = sum(populacao, na.rm=T)) %>%
  mutate(casos_100m = round(casos*100000/populacao,2),
         obitos_100m = round(obitos*100000/populacao,2),
         letalidade = ifelse(casos == 0,0,round(obitos/casos,2))) %>%
  select(residencia, dia, casos_novos, obitos_novos, casos, obitos, renda_media,
         populacao, casos_100m, obitos_100m, letalidade)

dados_rio_casos <- rbind.data.frame(x,dados_rio_casos)

rm(x)

dados_rio_casos %<>%
  mutate("casos_mm7" = forecast::ma(casos, 7),
         "obitos_mm7" = forecast::ma(obitos, 7),
         "casos_novos_mm7" = forecast::ma(casos_novos, 7),
         "obitos_novos_mm7" = forecast::ma(obitos_novos, 7),
         "casos_100m_mm7" = forecast::ma(casos_100m, 7),
         "obitos_100m_mm7" = forecast::ma(obitos_100m, 7))

localidades <- levels(dados_rio_casos$residencia)
names(localidades) <- levels(dados_rio_casos$residencia)


## Inserindo semanas epidemiologicas

semanas_epidem <- data.frame("dia" = seq.Date(from = as.Date("29/12/2019	", format = "%d/%m/%Y"),
                                              to = as.Date("02/01/2021	", format = "%d/%m/%Y"),
                                              by = "1 day"),
                             "semana_epidem" = rep(1:53, each = 7))

dados_rio_casos %<>%
  left_join(semanas_epidem, by="dia")


## Dataframe para clustering

dados_clusters <- dados_rio_casos %>%
  filter(residencia != "RIO DE JANEIRO",
         residencia != "FORA DO MUNICÍPIO",
         residencia != "INDEFINIDO") %>%
  group_by(residencia, semana_epidem) %>%
  summarise(casos_novos = sum(casos_novos),
            obitos_novos = sum(obitos_novos),
            populacao = mean(populacao),
            renda_media = mean(renda_media)) %>%
  mutate(casos = cumsum(casos_novos),
         obitos = cumsum(obitos_novos),
         letalidade = ifelse(casos == 0,0,round(obitos/casos,2)),
         casos_100m = casos*100000/populacao,
         obitos_100m = obitos*100000/populacao)


## Funções para gerar mapa

map_base_generate <- function(data, lng, lat, zoom){
  map <- leaflet(data) %>%
    addTiles() %>%
    setView(lng=lng, lat=lat, zoom = zoom)
  map
}

map_add_characteristics <- function(map, data, color=c("Reds","Blues","Greens","Oranges","Greys","Purples"),col){
  
  mypalette <- colorBin(palette=color, 
                        domain=data[[col]], 
                        na.color="transparent",
                        bins = c(0,10^(1:str_length(max(data[[col]], na.rm = T)))))
  
  texto <- paste0("<strong>Local: </strong>",
                  data$residencia, 
                  "<br><strong>Casos: </strong>", 
                  data$casos,
                  "<br><strong>Óbitos: </strong>",
                  data$obitos,
                  "<br><strong>Letalidade: </strong>",
                  paste0(round(data$letalidade*100,2),"%")
  ) %>%
    lapply(htmltools::HTML)
  
  
  leafletProxy(map, data = data) %>%
    addPolygons(stroke = TRUE, 
                fillOpacity = 0.7, 
                smoothFactor = 0.5,
                weight = 1,
                fillColor = ~mypalette(data[[col]]),
                color = "black",
                highlightOptions = highlightOptions(color = "white",
                                                    weight = 5,
                                                    bringToFront = TRUE),
                label = texto,
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "13px",
                  direction = "auto")
    ) %>%
    clearControls() %>%
    addLegend(position = "bottomleft", pal = mypalette, 
              values = ~data[[col]], opacity = 0.8, title = str_to_title(col))
  
}


# Shiny app ---------------------------------------------------------------

ui <- dashboardPage(
  dashboardHeader(title = "COVID-19 RJ",
                  dropdownMenu(type = "messages",
                               messageItem("Allan Strougo",
                                           message = "astrougo@producao.uerj"),
                               messageItem("LEGOS",
                                           message = "legos@eng.uerj.br")
                  ),
                  dropdownMenu(type = "notifications",
                               icon = icon("linkedin"),
                               notificationItem("Allan Strougo",
                                                href = "https://www.linkedin.com/in/allan-strougo-977927114/",
                                                icon = icon("linkedin")),
                               notificationItem("LEGOS",
                                                href = "https://www.linkedin.com/company/legos-uerj/",
                                                icon = icon("linkedin"))
                  )
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "maptab", icon = icon("poll")),
      menuItem("Séries temporais", tabName = "timeseriestab", icon = icon("chart-line")),
      menuItem("Clustering", tabName = "clustering", icon = icon("project-diagram")),
      menuItem("Database", tabName = "database", icon = icon("table")),
      menuItem("LEGOS", tabName = "LEGOS", icon = icon("info"))
    ),
    dateInput(inputId = "data_filter", label = "Data",
              value = max(dados_rio_casos$dia),
              min = min(dados_rio_casos$dia), 
              max = max(dados_rio_casos$dia),
              format = "yyyy-mm-dd"),
    br(), br(), br(), br(), br(),
    fluidRow(
      align = "center",
      h4(" Desenvolvido por"),
      img(src="legos.png", height = "50", width = "180")
    ),
    br(),
    fluidRow(
      column(
        width = 11,
        offset = 1,
        align = "left",
        p("Allan Dominguez Strougo"),
        p("Daniel Bouzon Nagem Assad")
      )
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "LEGOS",
              h3("Este painel é uma contribuição do LEGOS para a disseminação de informações
                  sobre o COVID-19 no Rio de Janeiro"),
              br(),
              fluidRow(
                column(
                  width = 9,
                  box(width = 12,
                      status = "primary",
                      solidHeader = TRUE,
                      title = "Sobre o LEGOS",
                      p("O Laboratório de Engenharia e Gestão em Saúde (LEGOS/UERJ) é uma Unidade de 
                        Desenvolvimento Tecnológico (UDT/InovaUERJ) vinculada ao Departamento de 
                        Engenharia de Produção da UERJ. Motivado pelos desafios complexos do sistema 
                        de saúde, o LEGOS tem promovido uma maior aproximação entre alunos, pesquisadores 
                        e docentes da Engenharia e de áreas correlatas com as Unidades de Saúde, 
                        reconhecendo as mesmas como objeto de projeto e gestão relevante."),
                      br(),
                      p("Através de atuação na tríade ensino, pesquisa e extensão, o LEGOS visa o 
                        desenvolvimento de conhecimento sobre as Unidades de Saúde que subsidie a 
                        formulação de soluções de projeto e gestão que promovam melhorias nas funções 
                        assistenciais (fim), administrativas e de formação (meio) para a entrega de 
                        saúde de qualidade à população no âmbito do SUS e do sistema suplementar."),
                      br(),
                      p("A equipe que integra o LEGOS conta com experiência teórica e prática em aplicações 
                        de Engenharia e Gestão em Unidades de Saúde públicas e privadas. Destacam-se: 
                        HUPE/UERJ, PPC/UERJ, RioSaúde."),
                      br(),
                      p("O LEGOS conduz as suas ações de forma multidisciplinar, contando com interações 
                        com unidades de referência na UERJ, a saber: FEN, FCM, IMS, ESDI, FAF; e externamente 
                        com parcerias acadêmicas com a Engenharia de Produção da UFRJ e a Engenharia da PUC-Rio.")
                  )
                ),
                column(
                  width = 3,
                  align = "center",
                  box(width = 12,
                      align = "left",
                      status = "primary",
                      solidHeader = TRUE,
                      title = "Nossas redes",
                      span(tagList(icon("internet-explorer"), a("Website", href="http://www.legos.uerj.br/"))), br(),
                      span(tagList(icon("linkedin"), a("Linkedin", href = "https://www.linkedin.com/company/legos-uerj/"))), br(),
                      span(tagList(icon("facebook-square"), a("Facebook", href = "https://www.facebook.com/legosaude"))), br(),
                      span(tagList(icon("camera"), a("Instagram", href = "https://www.instagram.com/legos.uerj/?hl=pt-br"))), br(),
                      span(tagList(icon("youtube"), a("Youtube", href = "https://www.youtube.com/channel/UCjz1kp0k17IktuTf6UnyaVg"))), br(),
                      span(tagList(icon("envelope"), "legos@eng.uerj.br"))
                  ),
                  br(), br(), br(), br(), br(),
                  img(src = "legos2.png", height = "150", width = "150", align = "center")
                )
              )
      ),
      tabItem(tabName = "maptab",
              fluidRow(
                box(title = "Atualizado em", textOutput("lastupdate"), width = 3),
                valueBoxOutput(outputId = "totalcases", width = 3),
                valueBoxOutput(outputId = "totaldeaths", width = 3),
                valueBoxOutput(outputId = "lethality", width = 3)
              ),
              fluidRow(
                column(width = 9,
                       box(title = "Mapa COVID-19 - Rio de Janeiro",
                           leafletOutput("map", height = "340px", width = "100%"),
                           width = 12)
                ),
                column(width = 3,
                       selectInput(inputId = "map_color", label = "Cor do mapa",
                                   choices = c("Vermelho"="Reds",
                                               "Azul"="Blues",
                                               "Verde"="Greens",
                                               "Laranja"="Oranges",
                                               "Cinza"="Greys",
                                               "Roxo"="Purples"),
                                   selected = "Vermelho"),
                       selectInput(inputId = "data_fill_map", label = "Casos/óbitos",
                                   choices = c("Casos" = "casos",
                                               "Óbitos" = "obitos"),
                                   selected = "Casos")
                )
              )    
      ),
      tabItem(tabName = "timeseriestab",
              fluidRow(
                column(width = 3,
                       selectInput(inputId = "residencia_filter", label = "Bairro/Município",
                                   choices = localidades,
                                   selected = "RIO DE JANEIRO"),
                       selectInput(inputId = "ts_y_filter", label = "Dados acumulados/novos",
                                  choices = c("Dados acumulados" = "-acumulados",
                                              "Dados diários" = "_novos-diários",
                                              "Dados acumulados relativos" = "_100m-acumulados por 100 mil habitantes",
                                              "Dados acumulados (média móvel - 7 dias)" = "_mm7-acumulados (média móvel 7 dias)",
                                              "Dados diários (média móvel - 7dias)" = "_novos_mm7-diários (média móvel 7 dias)",
                                              "Dados acumulados relativos (média móvel - 7 dias)" = "_100m_mm7-acumulados por 100 mil habitantes (média móvel 7 dias)"),
                                  selected = "Dados acumulados"),
                       downloadButton('downloadplot1', 'Download plot')
                ),
                column(width = 9,
                       box(title = "Casos e óbitos de COVID-19",
                           width = 12,
                           plotlyOutput(outputId = "ts_plotly1"))
                )
              )
      ),
      tabItem(tabName = "clustering",
              fluidRow(
                column(width = 4, align="center",
                       selectInput(inputId = "cluster_variable",
                                   label = "Análise de interesse",
                                   choices = c("Renda x Casos relativos" = "casos_100m",
                                               "Renda x letalidade" = "letalidade"),
                                   selected = "Renda x Casos relativos"),
                       sliderInput(inputId = "semana_epidem_slider", label = "Semana epidemiológica",
                                   min = min(dados_clusters$semana_epidem),
                                   max = max(dados_clusters$semana_epidem),
                                   value = max(dados_clusters$semana_epidem),
                                   step = 1),
                       sliderInput(inputId = "k_clusters", label = "Número de clusters",
                                   min = 2,
                                   max = floor(length(localidades)/10),
                                   value = 2,
                                   step = 1),
                       box(plotOutput("optimal_k_clusters", height = "167px"), 
                           width = 12,
                           height = "187"),
                       downloadButton('downloadplot2', 'Download plot')
                ),
                column(width = 8,
                       box(title = "Clusterização com k-means",
                           plotOutput(outputId = "cluster_plot"),
                           width = 12)
                )
              )

      ),
      tabItem(tabName = "database",
              h3("Dados de COVID para o município do Rio de Janeiro e seus bairros"),
              DT::dataTableOutput("dados_tabela"),
              br(),
              downloadButton('downloadtable', 'Download csv'),
              br(),
              h4("Fonte dos dados"),
              a("COVID-19, ",href="https://experience.arcgis.com/experience/38efc69787a346959c931568bd9e2cc4"),
              a("População, ",href="http://www.data.rio/datasets/5deef82befff41c6810865c8e7200879"),
              a("Renda",href="http://www.data.rio/datasets/rendimento-nominal-m%C3%A9dio-e-mediano-de-pessoas-de-10-anos-ou-mais-de-idade-segundo-as-%C3%A1reas-de-planejamento-ap-regi%C3%B5es-administrativas-ra-e-bairros-no-munic%C3%ADpio-do-rio-de-janeiro-2010")
      )
    )
  )
)

server <- function(input, output) {
  
  dados_casos_filtered <- reactive({
    dados_rio_casos %>% 
      filter(dia <= input$data_filter)
  })

  ## Mapa ------------------------------------------------------------------------------------
  df_map <- reactive({
    x <- dados_casos_filtered() %>%
        filter(dia == max(dia)) %>%
        right_join(x=.,y=mapa_bairros_rio, by = c("residencia" = "NOME")) %>%
        st_as_sf(x=.)
    x <- st_transform(x,"+init=epsg:4326")
    x <- st_as_sf(x)
    x
  })
  
  output$map <- renderLeaflet({
    map_base_generate(data = df_map(),lng=-43.4049496,lat=-22.9533120,zoom = 10)
  })
  
  observe({
    map_add_characteristics(map = "map",data = df_map(),input$map_color,input$data_fill_map)
  })

  ## Valueboxs ---------------------------------------------------------------------------------
  output$lastupdate <- renderText({
    as.character(dados_rio$dt_atualizacao[1])
  })
  
  casos <- reactive({sum(dados_casos_filtered()$casos_novos)})
  
  output$totalcases <- renderValueBox({
    valueBox(casos(), "Casos acumulados")
  })
  
  obitos <- reactive({sum(dados_casos_filtered()$obitos_novos)})
  
  output$totaldeaths<- renderValueBox({
    valueBox(obitos(), "Óbitos acumulados")
  })
  
  output$lethality<- renderValueBox({
    valueBox(paste0(round(obitos()*100/casos(),2),"%"), "Letalidade")
  })
  
  ## Time series ----------------------------------------------------------------
  fig_ts <- reactive({
    
    x <- strsplit(input$ts_y_filter,"-")[[1]]
    var <- x[1]
    axislab <- x[2]
    
    plot_ly(dados_rio_casos %>% filter(as.character(residencia) == input$residencia_filter),
            x = ~dia,
            y = ~.data[[paste0("casos",var)]],
            type = "scatter",
            mode = "markers+lines",
            name = paste0("Casos ",axislab)) %>%
      add_trace(y = ~.data[[paste0("obitos",var)]], name = paste0("Óbitos ",axislab)) %>%
      layout(xaxis = list(range = c(min(dados_rio_casos$dia)-10,
                                    max(dados_rio_casos$dia)+10),
                          title = "", zeroline = T, showline = F, showgrid = T),
             yaxis = list(title = "", zeroline = T, showline = F, showgrid = T),
             hovermode = "x unified",
             title = input$residencia_filter,
             legend = list(orientation = "h"))
  })
  
  output$ts_plotly1 <- renderPlotly({
    fig_ts()
  })
  
  output$downloadplot1<- downloadHandler(
    filename = function() { 
      paste("plot", Sys.Date(), ".html", sep="")
    },
    content = function(file) {
      htmlwidgets::saveWidget(fig_ts(), file)
    })


  ## Clustering -------------------------------------------------------------------------------
  
  dados_clusters2 <- reactive({
    dados_clusters %>%
      filter(semana_epidem == as.numeric(input$semana_epidem_slider)) %>%
      as.data.frame() %>%
      select(residencia, renda_media, input$cluster_variable)
  })
  
  dados_clusters2_scaled <- reactive({
    x <- dados_clusters2()
    
    x %<>% 
      mutate(renda_media = scale(renda_media)) %>%
      select(-residencia)
    
    if(var(x[,2])!=0){
      x[,2] <- scale(x[,2])
    }
    
    row.names(x) <- as.character(dados_clusters2()$residencia)
    
    x
  })
  
  output$optimal_k_clusters <- renderPlot({
    
    fviz_nbclust(dados_clusters2_scaled(), kmeans, method = "silhouette")
    
  })
  
  plot_clusters <- reactive({
    
    n <- as.numeric(input$k_clusters)
    
    set.seed(123)
    
    resultado <- kmeans(dados_clusters2_scaled(), n, iter.max = 50, nstart = 10)
    
    centroides_renda <- resultado$centers[,1]*attr(dados_clusters2_scaled()[,1],"scaled:scale") + 
      attr(dados_clusters2_scaled()[,1],"scaled:center")
   
    if(var(dados_clusters2()[,3])==0){
      centroides_var <- rep(0,n)
    } else {
      centroides_var <- resultado$centers[,2]*attr(dados_clusters2_scaled()[,2],"scaled:scale") + 
        attr(dados_clusters2_scaled()[,2],"scaled:center")
    }

    centroides <- data.frame(var = centroides_var, 
                             renda_media = centroides_renda,
                             Grupo = as.factor(1:n))
    
    names(centroides)[1] <- input$cluster_variable
    
    dados <- cbind(Bairro = names(resultado$cluster), 
                   dados_clusters2(), 
                   Grupo = as.factor(resultado$cluster))
    
    if(input$cluster_variable == "letalidade"){
      titlelab <- "Renda média vs letalidade"
      xlab <- "Letalidade (óbitos/casos)"
    } else {
      titlelab <- "Renda média vs casos acumulados relativos"
      xlab <- "Casos por 100 mil habitantes"
    }
    
    dados %>%
      ggplot(aes(x = .data[[input$cluster_variable]], y = renda_media, shape = Grupo)) +
      geom_point(aes(color = Grupo)) +
      geom_label(aes(label = Bairro, fill = Grupo), size = 3) +
      geom_mark_ellipse(expand = 0, aes(fill=Grupo)) +
      geom_point(data = centroides, show.legend = F) +
      labs(title = titlelab,
           x = xlab,
           y = "Renda média (R$)") +
      xlim(0,max(dados_clusters[,input$cluster_variable])) +
      theme_bw()
  })
  
  output$cluster_plot <- renderPlot({
    plot_clusters()
  })
  
  output$downloadplot2<- downloadHandler(
    filename = function() { 
      paste("plot2", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      ggsave(filename = file, plot = plot_clusters(), width = 9, height = 6)
    })
  
  ## Tabela -----------------------------------------------------------------------------------
  
  output$dados_tabela = DT::renderDataTable({
    DT::datatable(dados_rio_casos, 
                  options = list(lengthMenu = c(5, 10, 20, 40), pageLength = 5, 
                                 scrollX=TRUE, scrollCollapse=TRUE))
  })
  
  output$downloadtable<- downloadHandler(
    filename = function() { 
      paste("database", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv2(dados_rio_casos, file)
    })

}

shinyApp(ui = ui, server = server)




