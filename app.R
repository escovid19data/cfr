# CFR Edades

# Load libraries
library(shiny)
library(tidyverse)
library(plotly)

# setttings: captions
# Cambia el pie del gráfico pero conserva la fuente de los datos
caption_provincia <- "Gráfico: @numeroteca (lab.montera34.com/covid19) | Datos: Escovid19data (github.com/montera34/escovid19data)"
caption_provincia2 <- "Gráfico: @numeroteca (lab.montera34.com/covid19) | Datos: Ministerio Sanidad ISCIII vía Escovid19data (github.com/montera34/escovid19data)"
updated <- ""
# period <- "Para CCAA uniprov. casos es la suma de PCR+ y TestAc+ desde 2020.04.15"
today <- Sys.Date() 
period <- paste0("(Actualizado: ",today,")")

# data_cases_sp_provinces <- read.delim("https://github.com/montera34/escovid19data/blob/master/data/output/covid19-ccaa-spain_consolidated.csv?raw=true",sep = ",") 
ciii <- readRDS(file = "data/sciii.rds") # %>% filter( province=="Bizkaia") # %>% head(2)

# load data
print("process edades data")
print("end process edades data ------------------------")
# ciii <- read.delim("/home/numeroteca/sites/covid19/data/original/spain/isciii_casos_hosp_uci_def_sexo_edad_provres.csv",sep = ",") %>%
#   # filter( provincia_iso =="SS" ) %>%
#   mutate (
#     date = as.Date(as.character(fecha)),
#     provincia_iso = ifelse ( is.na(provincia_iso), "NA", as.character(provincia_iso) )
#   ) %>%
#   mutate(
#     province =  "",
#     province = ifelse( provincia_iso == "C", "Coruña, A", province ),
#     province = ifelse( provincia_iso =="VI", "Araba/Álava", province ),
#     province = ifelse( provincia_iso =="AB", "Albacete", province ),
#     province = ifelse( provincia_iso =="A", "Alicante/Alacant", province ),
#     province = ifelse( provincia_iso =="AL", "Almería", province ),
#     province = ifelse( provincia_iso =="O", "Asturias", province ),
#     province = ifelse( provincia_iso =="AV", "Ávila", province ),
#     province = ifelse( provincia_iso =="BA", "Badajoz", province ),
#     province = ifelse( provincia_iso =="PM", "Balears, Illes", province ),
#     province = ifelse( provincia_iso =="B", "Barcelona", province ),
#     province = ifelse( provincia_iso =="BI", "Bizkaia", province ),
#     province = ifelse( provincia_iso =="BU", "Burgos", province ),
#     province = ifelse( provincia_iso =="CC", "Cáceres", province ),
#     province = ifelse( provincia_iso =="CA", "Cádiz", province ),
#     province = ifelse( provincia_iso =="S", "Cantabria", province ),
#     province = ifelse( provincia_iso =="CS", "Castellón/Castelló", province ),
#     province = ifelse( provincia_iso =="CR", "Ciudad Real", province ),
#     province = ifelse( provincia_iso =="CO", "Córdoba", province ),
#     province = ifelse( provincia_iso =="CU", "Cuenca", province ),
#     province = ifelse( provincia_iso =="SS", "Gipuzkoa", province ),
#     province = ifelse( provincia_iso =="GI", "Girona", province ),
#     province = ifelse( provincia_iso =="GR", "Granada", province ),
#     province = ifelse( provincia_iso =="GU", "Guadalajara", province ),
#     province = ifelse( provincia_iso =="H", "Huelva", province ),
#     province = ifelse( provincia_iso =="HU", "Huesca", province ),
#     province = ifelse( provincia_iso =="J", "Jaén", province ),
#     province = ifelse( provincia_iso =="LO", "Rioja, La", province ),
#     province = ifelse( provincia_iso =="GC", "Palmas, Las", province ),
#     province = ifelse( provincia_iso =="LE", "León", province ),
#     province = ifelse( provincia_iso =="L", "Lleida", province ),
#     province = ifelse( provincia_iso =="LU", "Lugo", province ),
#     province = ifelse( provincia_iso =="M", "Madrid", province ),
#     province = ifelse( provincia_iso =="MA", "Málaga", province ),
#     province = ifelse( provincia_iso =="MU", "Murcia", province ),
#     province = ifelse( provincia_iso =="OR", "Ourense", province ),
#     province = ifelse( provincia_iso =="P", "Palencia", province ),
#     province = ifelse( provincia_iso =="PO", "Pontevedra", province ),
#     province = ifelse( provincia_iso =="SA", "Salamanca", province ),
#     province = ifelse( provincia_iso =="TF", "Santa Cruz de Tenerife", province ),
#     province = ifelse( provincia_iso =="SE", "Sevilla", province ),
#     province = ifelse( provincia_iso =="SG", "Segovia", province ),
#     province = ifelse( provincia_iso =="SO", "Soria", province ),
#     province = ifelse( provincia_iso =="T", "Tarragona", province ),
#     province = ifelse( provincia_iso =="TE", "Teruel", province ),
#     province = ifelse( provincia_iso =="TO", "Toledo", province ),
#     province = ifelse( provincia_iso =="V", "Valencia/València", province ),
#     province = ifelse( provincia_iso =="VA", "Valladolid", province ),
#     province = ifelse( provincia_iso =="ZA", "Zamora", province ),
#     province = ifelse( provincia_iso =="Z", "Zaragoza", province ),
#     province = ifelse( provincia_iso =="CE", "Ceuta", province ),
#     province = ifelse( provincia_iso =="ML", "Melilla", province ),
#     province = ifelse( provincia_iso =="NA", "Navarra", province )
#     # province = ifelse( provincia_iso == "NC", "Navarra", province ), no sé qué es NC
#   ) %>% filter ( ! provincia_iso == "NC" ) %>% arrange(date) %>% group_by(province,date,grupo_edad) %>%
#   summarise(
#     num_casos = sum(replace_na(num_casos,0)),
#     num_hosp = sum(num_hosp ),
#     num_uci = sum(num_uci),
#     num_def = sum( replace_na(num_def,0))
#   ) %>% ungroup() %>% group_by(province,grupo_edad) %>% mutate (
#     num_casos_cum = cumsum(num_casos),
#     num_hosp_cum = cumsum(num_hosp ),
#     num_uci_cum = cumsum(num_uci),
#     num_def_cum = cumsum(num_def),
#     num_casos14 = num_casos_cum - lag(num_casos_cum,14),
#     # num_hosp = sum(num_hosp ),
#     # num_uci = sum(num_uci),
#     num_def14 = num_def_cum - lag(num_def_cum,14),
# 
#     province = as.factor(province),
#     grupo_edad = as.character(grupo_edad)
#   )

# print("--------------finished loading data -------------")
# 
# saveRDS(ciii, file = "/home/numeroteca/sites/covid19/apps/casos-muertes-edades/data/sciii.rds")
# print("load ISCIII")
# ciii <- readRDS(file = "data/sciii.rds") # %>% filter( province=="Sevilla")

# Define UI ----
ui <- navbarPage(
  id = "escovid19",
  title = "Calculando CFR", 
  selected = "isciii",
  #header=singleton(tags$head(includeScript("google-analytics.js"))),#href = "//cultureofinsight.com", target = "blank"),)),
  inverse = F,
  collapsible = TRUE,
  
  tags$head(
    # Note the wrapping of the string in HTML()
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Robot+Condensed&display=swap');
      body {
        background-color: #f0f0f0;
        font-family: 'Roboto Condensed', sans-serif;
      }
      h2 {
        font-size: 20px;
      }
      .control-label, p {
        font-size: 12px !important;
      }
      .form-group {
          margin-bottom: 6px;
      }
      .shiny-input-container {
        color: #474747;
      }"))
  ),
  
  tabPanel(
    navid = "isciii",
    id = "isciii",
    value = "isciii",
    title = "Por edades",
    titlePanel("Fallecidos y casos detectados por edades"),
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
      # Sidebar panel for inputs ----
      sidebarPanel(
        width = 2,
        # p("prueba"),
        actionButton("go2", "Aplica filtros",style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
        numericInput (inputId = "coef2",
                      label = "Coeficiente (CFR)",
                      min = 1, max = 20, value = 20, step = 0.1
        ),
        sliderInput(inputId = "decalaje2",
                    label = "Decalaje (días)",
                    min = 0, max = 30, value = 14
        ),
        # Input: select age
        selectInput(inputId= "edadesselect",
                    label ="Edades",
                    choices = c("0-9",   "10-19", "20-29", "30-39", "40-49", "50-59",
                                "60-69", "70-79", "80+",   "NC"),
                    selected = "80+"
                    ),
        selectInput(inputId= "laprovincia",
                    label ="Selecciona provincia",
                    # choices = c("Bizkaia","Gipuzkoa","Sevilla"),
                    choices = levels(ciii$province),
                    selected = "Bizkaia"
        ),
        p("Los casos y fallecidos en las series de datos de ISCIII infravaloran las últimas fechas. En algunas CCAA hay discrepancias fuertes:"),
        tags$a(href="https://lab.montera34.com/covid19/otros.html#fallecidosprovcompara", "ver análisis en Escovid19data"),
        p(),
        tags$a(href="https://twitter.com/numeroteca", "Gráficos de @numeroteca."),
        p(),
        tags$a(href="https://cnecovid.isciii.es/covid19/resources/casos_hosp_uci_def_sexo_edad_provres.csv", "Datos de RENAVE-ISCIII")
        
      ),
      mainPanel(
        width = 10,
        p("En este primer gráfico se muestras dos líneas: la de fallecidos y la de casos detectados, esta última modificada por un factor que se puede cambiar en el panel de la izquierda"),
        plotOutput("thePlot1", height = 400),
        p("Este 2º gráfico es el cociente entre fallecidos y casos detectados: ojo el decalaje modifica el cálculo.
          El decalaje es cuánto se mueven los días hacia delante."),
        plotOutput("thePlot3", height = 400),
        h2("Provincias"),
        p(),
        plotOutput("thePlot2", height = 600),
        p(),
        plotOutput("thePlot4", height = 600),
        
        h3("Descarga los datos recalculados en formato tabla"),
        tableOutput("thetableedades"),
      )
    )
  )
)



# Define server logic to plot various variables against mpg ----
server <- function(input, output) {
  
  decalaje2 <- eventReactive(input$go2, {
    input$decalaje2
  })
  coef2 <- eventReactive(input$go2, {
    input$coef2
  })

  # select edades
  edadesselect <- eventReactive(input$go2, {
    input$edadesselect
  })
  
  laprovincia <- eventReactive(input$go2, {
    input$laprovincia
  })
  
  # Data edades -----
  dataedades <- eventReactive(input$go2, {
    # print("end print edades head data ------------------------")
    # print("load ISCIII")
    # print("tail--------------")
    # print( tail(isciii))
    ciii
  })
  
  # Table edades ----------
  output$thetableedades <- renderTable({
    print("table edades")
    fortable <- dataedades() %>%  mutate (
      date = as.character( as.Date(date,  origin = lubridate::origin) )
    ) %>% filter( 
      # province =="Bizkaia" & 
      province == laprovincia() & 
      grupo_edad == as.character(edadesselect())
        # grupo_edad == "80+"
      ) %>%
      select(
        date,province,grupo_edad,
        num_casos,num_casos_cum,num_casos14,
        num_def,num_def_cum,num_def14
      ) %>% arrange(date) %>% group_by(province, grupo_edad) %>% mutate(
        CFR = num_def14 / lag(num_casos14,decalaje2() ) * 100
      )
    print("print edades")
    print(dataedades() %>% tail(1))
    print("end tables edades")
    fortable
  })
  
  
  # Plot 1 ------
  output$thePlot1 <- renderPlot({
    
    the_plot <- dataedades() %>% as.data.frame() %>% filter ( date > as.Date("2020-07-01")) %>%
      filter (
        # grupo_edad == as.character(edadesselect())
        province == laprovincia() &
        grupo_edad ==  as.character(edadesselect())
        # grupo_edad == input$edadesselect
      ) %>%
      ggplot() +
      geom_line(aes(date, num_def14, group=grupo_edad),size=1, color="black") +
      geom_line(aes(date + decalaje2(), num_casos14/100 * coef2(),  group=grupo_edad),size=1,color="#BB0088" ) + # , color = grupo_edad / 100 * 1.1,
      # # geom_line(aes(date + decalaje(), casos14dias / 100 * coef(), group=ccaa),size=1, color ="#BB0088" ) +
      expand_limits(y = 0) +
      facet_wrap(~province, scales = "free_y") +
      scale_y_continuous(
        # limits = c(0, 1300 ),
        labels=function(x) format(round(x, digits = 1), big.mark = ".", scientific = FALSE)
        ) +
      scale_x_date( date_breaks = "1 month",
                    date_labels = "%m/%y",
                    # limits = c( as.Date("2020-07-25"), max(data_cases_sp_provinces$date) )
                    # limits=c( min(data_cases_sp_provinces[!is.na(data_cases_sp_provinces$deceased),]$date), max(data_cases_sp_provinces$date)+1),
                    # limits=c( as.Date("2020-03-01"), max(data_cases_sp_provinces$date)+51)
                    expand = c(0,0)
      ) +
      theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
      theme(
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.ticks.x = element_line(color = "#888888"),
        legend.position = "top",
        axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9)
      )  +
      labs(title = paste0("Muertes en últimos 14 días (negro) y Casos detectados / ", 100 , " en últimos 14 días * ",
                          coef2(), " (rosa). Franja edad: ", edadesselect() ,updated),
           subtitle = paste0("COVID-19 en España por CCAA (escala lineal). Decalaje de ",  decalaje2(), " días: los casos se desplazan",
                             decalaje2(), " días. ",period),
           y = "fallecidos vs casos",
           x = "2020-2021",
           caption = caption_provincia2)
    the_plot
  })
  
  # Plot 2 facet ------
  output$thePlot2 <- renderPlot({
    
    the_plot <- dataedades() %>% as.data.frame() %>% filter ( date > as.Date("2020-07-01")) %>% 
      filter (
        # grupo_edad == as.character(edadesselect())
        # province == laprovincia() &
          grupo_edad ==  as.character(edadesselect())
        # grupo_edad == input$edadesselect
      ) %>%
      ggplot() +
      geom_line(aes(date, num_def14, group=grupo_edad),size=1.3, color="black") +
      geom_line(aes(date + decalaje2(), num_casos14/100 * coef2(),  group=grupo_edad),size=0.9,color="#BB0088" ) + # , color = grupo_edad / 100 * 1.1,
      # # geom_line(aes(date + decalaje(), casos14dias / 100 * coef(), group=ccaa),size=1, color ="#BB0088" ) +
      expand_limits(y = 0) +
      facet_wrap(~province, scales = "free_y") +
      scale_y_continuous(
        # limits = c(0, 1300 ),
        labels=function(x) format(round(x, digits = 1), big.mark = ".", scientific = FALSE)
      ) +
      scale_x_date( date_breaks = "2 month",
                    date_labels = "%m",
                    # limits = c( as.Date("2020-07-25"), max(data_cases_sp_provinces$date) )
                    # limits=c( min(data_cases_sp_provinces[!is.na(data_cases_sp_provinces$deceased),]$date), max(data_cases_sp_provinces$date)+1),
                    # limits=c( as.Date("2020-03-01"), max(data_cases_sp_provinces$date)+51)
                    expand = c(0,0)
      ) +
      theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
      theme(
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.ticks.x = element_line(color = "#888888"),
        legend.position = "top",
        axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9)
      )  +
      labs(title = paste0("Muertes en últimos 14 días (negro) y Casos detectados / ", 100 , " en últimos 14 días * ",
                          coef2(), " (rosa). Franja edad: ", edadesselect() ,updated),
           subtitle = paste0("COVID-19 en España por CCAA (escala lineal). Decalaje de ",  decalaje2(), " días: los casos se desplazan",
                             decalaje2(), " días. ",period),
           y = "fallecidos vs casos",
           x = "2020-2021",
           caption = caption_provincia2)
    the_plot
  })
  
  # Plot 3 ------
  output$thePlot3 <- renderPlot({
    
    the_plot <- dataedades() %>% as.data.frame() %>% filter ( date > as.Date("2020-07-01")) %>%
      filter (
        # grupo_edad == as.character(edadesselect())
        province == laprovincia() &
          grupo_edad ==  as.character(edadesselect())
        # grupo_edad == input$edadesselect
      ) %>%
      ggplot() +
      geom_line(aes(date, num_def14 / lag(num_casos14,decalaje2() ) * 100 , group=grupo_edad),size=1, color="black") +
      # geom_line(aes(date + decalaje2(), num_casos14/100 * coef2(),  group=grupo_edad),size=1,color="#BB0088" ) + # , color = grupo_edad / 100 * 1.1,
      # # geom_line(aes(date + decalaje(), casos14dias / 100 * coef(), group=ccaa),size=1, color ="#BB0088" ) +
      expand_limits(y = 0) +
      facet_wrap(~province, scales = "free_y") +
      scale_y_continuous(
        # limits = c(0, 1300 ),
        labels=function(x) format(round(x, digits = 1), big.mark = ".", scientific = FALSE)
      ) +
      scale_x_date( date_breaks = "1 month",
                    date_labels = "%m/%y",
                    # limits = c( as.Date("2020-07-25"), max(data_cases_sp_provinces$date) )
                    # limits=c( min(data_cases_sp_provinces[!is.na(data_cases_sp_provinces$deceased),]$date), max(data_cases_sp_provinces$date)+1),
                    # limits=c( as.Date("2020-03-01"), max(data_cases_sp_provinces$date)+51)
                    expand = c(0,0)
      ) +
      theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
      theme(
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.ticks.x = element_line(color = "#888888"),
        legend.position = "top",
        axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9)
      )  +
      labs(title = paste0("CFR: Muertes en últimos 14 días (negro) / Casos detectados * 100 en últimos 14 días",
                          " . Franja edad: ", edadesselect() ,updated),
           subtitle = paste0("COVID-19 en España por CCAA (escala lineal). Decalaje de ",  decalaje2(), " días: los casos se desplazan",
                             decalaje2(), " días. ",period),
           y = "fallecidos / casos * 100",
           x = "2020-2021",
           caption = caption_provincia2)
    the_plot
  })
  
  
  # Plot 4 ------
  output$thePlot4 <- renderPlot({
    
    newdata <- dataedades() %>% as.data.frame() %>% filter ( date > as.Date("2020-07-01")) %>% mutate(
      cfr = num_def14 / lag(num_casos14,decalaje2() ) * 100,
      cfr = replace_na(cfr,0)
    )
   
    maxy <- max(newdata$cfr)
    print("maxy 1")
    print(maxy)
    maxy <-  ifelse(maxy > 40, 40, maxy)
    print("maxy 2")
     print(maxy)
    
    the_plot <- dataedades() %>% as.data.frame() %>% filter ( date > as.Date("2020-07-01")) %>%
      filter (
          grupo_edad ==  as.character(edadesselect())
      ) %>%
      ggplot() +
      geom_line(aes(date, num_def14 / lag(num_casos14,decalaje2() ) * 100 , group=grupo_edad),size=1, color="black") +
      # geom_line(aes(date + decalaje2(), num_casos14/100 * coef2(),  group=grupo_edad),size=1,color="#BB0088" ) + # , color = grupo_edad / 100 * 1.1,
      # # geom_line(aes(date + decalaje(), casos14dias / 100 * coef(), group=ccaa),size=1, color ="#BB0088" ) +
      expand_limits(y = 0) +
      facet_wrap(~province) + # scales = "free_y"
      scale_y_continuous(
        limits = c(0, maxy ),
        labels=function(x) format(round(x, digits = 1), big.mark = ".", scientific = FALSE)
      ) +
      scale_x_date( date_breaks = "2 month",
                    date_labels = "%m",
                    # limits = c( as.Date("2020-07-25"), max(data_cases_sp_provinces$date) )
                    # limits=c( min(data_cases_sp_provinces[!is.na(data_cases_sp_provinces$deceased),]$date), max(data_cases_sp_provinces$date)+1),
                    # limits=c( as.Date("2020-03-01"), max(data_cases_sp_provinces$date)+51)
                    expand = c(0,0)
      ) +
      theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
      theme(
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.ticks.x = element_line(color = "#888888"),
        legend.position = "top",
        axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9)
      )  +
      labs(title = paste0("CFR: Muertes en últimos 14 días (negro) / Casos detectados * 100 en últimos 14 días",
                          " . Franja edad: ", edadesselect() ,updated),
           subtitle = paste0("COVID-19 en España por CCAA (escala lineal). Decalaje de ",  decalaje2(), " días: los casos se desplazan",
                             decalaje2(), " días. ",period),
           y = "fallecidos / casos * 100",
           x = "2020-2021",
           caption = caption_provincia2)
    the_plot
  })
}

shinyApp(ui, server)