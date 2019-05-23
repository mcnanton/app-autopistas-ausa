
library(dplyr)
library(lubridate)
library(stringr)
#cargo datos de trÃ¡nsito vehicular 2018
transito_2018<-read.csv("flujo-vehicular-2018.csv",fileEncoding = 'UTF-8')
#Aprolijo las fechas, las paso a date con lubridate
transito_2018$fecha<-ymd(transito_2018$fecha)

#Adjudico el nombre de la autopista a las estaciones de peaje
transito_2018$estacion <- plyr::revalue(transito_2018$estacion, c("ALBERDI"="AU 25 DE MAYO", "AVELLANEDA"="AU PERITO MORENO","DELLEPIANE LINIERS"="AU DELLEPIANE", "DELLEPIANE LINIERSLEPIANE CENTRO"="AU DELLEPIANE", "ILLIA"="AU ILLIA", "SARMIENTO"="AU ILLIA", "RETIRO"="AU ILLIA", "SALGUERO"="AU ILLIA"))

#Calculo trÃ¡nsito por dia y por hora en cada autopista
transito_por_dia_autopista <- transito_2018 %>% group_by(estacion, dia) %>% summarise(trafico=sum(cantidad_pasos))
transito_por_hora_autopista <- transito_2018 %>% group_by(estacion, hora_inicio) %>% summarise(trafico=sum(cantidad_pasos))

#Dropeo variables que no voy a utilizar, agrupo los datos de transito por fecha, hora y estacion de modo que sume la cantidad de pases
transito_2018<-transito_2018%>%select(-forma_pago, -observacion, -periodo, -mes, -hora_fin, -dia_fecha)%>% rename(hora=hora_inicio, autopista=estacion)%>% group_by(fecha, autopista) %>% summarise(trafico=sum(cantidad_pasos)) 

#Levanto dataset de intervenciones de seguridad vial
intervenciones_2018<-read.csv("intervenciones-de-seguridad-vial.csv",fileEncoding = 'UTF-8')
#dropeo variable que no voy a utilizar y filtro por aÃ±o 2018
intervenciones_2018<-intervenciones_2018%>%select(-pk)%>%filter(as.integer(intervenciones_2018$periodo) >= 201801 & intervenciones_2018$periodo <= 201812) %>% select(-periodo)
#La variable fecha tiene valores mal procesados, esto viene del CSV base. Para esto creo dos variables y las combino para tener una variable fecha unificada.
intervenciones_2018$fecha<-as.character(intervenciones_2018$fecha)
ydm <- ydm(intervenciones_2018$fecha) 
ymd <- ymd(intervenciones_2018$fecha) 
ydm[is.na(ydm)] <- ymd[is.na(ydm)]  
intervenciones_2018$fecha <- ydm
#Corrijo discrepancias en la manera de nombrar las autopistas/semiautopistas
intervenciones_2018$autopista<-plyr::revalue(intervenciones_2018$autopista, c("AU PERTIO MORENO" = "AU PERITO MORENO", "AV LUGONES" = "AV. LUGONES"))
#Agrego dia de semana (L-V) a intervenciones
intervenciones_2018$dia<-as.factor(str_to_title(weekdays(intervenciones_2018$fecha)))
autopistas_finales <- c("AU 25 DE MAYO", "AU FRONDIZI", "AU ILLIA", "AU DELLEPIANE", "AU PERITO MORENO", "AU CAMPORA")
intervenciones_2018<- filter(intervenciones_2018, autopista %in% autopistas_finales)

#Obtengo dataset Autopistas x HS x Peligrosidad
autopistas_hs_peligrosidad <- intervenciones_2018 %>% group_by(autopista, hora) %>%tally()
autopistas_hs_peligrosidad<- autopistas_hs_peligrosidad %>% rename(accidentes=n) %>% arrange(autopista, desc(accidentes))
#Asigno Indice de peligrosidad
autopistas_hs_peligrosidad <- transform(autopistas_hs_peligrosidad, indice_peligrosidad = ave(as.character(autopista),as.character(autopista), FUN = seq_along))

#Calculo porcentaje que representa cada hs del total de accidentes
autopistas_hs_peligrosidad <- left_join(autopistas_hs_peligrosidad %>% group_by(autopista) %>% summarize(total_accidentes=sum(accidentes)), autopistas_hs_peligrosidad)

autopistas_hs_peligrosidad$porcentaje <- autopistas_hs_peligrosidad$accidentes*100/autopistas_hs_peligrosidad$total_accidentes

#Obtengo dataset Autopistas x dia x Peligrosidad
autopistas_dia_peligrosidad <-intervenciones_2018 %>% group_by(autopista, dia) %>% summarise(accidentes=sum(n())) %>% arrange(autopista, desc(accidentes)) %>% mutate(indice_peligrosidad=rep_len(1:7, length.out = 7))

autopistas_dia_peligrosidad$dia <- ordered(autopistas_dia_peligrosidad$dia, levels=c("Domingo", "Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado"))
##
#Elimino niveles no utilizados del factor autopista de autopistas_dia y autopistas_hs
autopistas_dia_peligrosidad$autopista <- droplevels(autopistas_dia_peligrosidad$autopista)
autopistas_hs_peligrosidad$autopista <- droplevels(autopistas_hs_peligrosidad$autopista)

#Porcentaje de accidentes del ano por autopista
accidentes_autopista_2018 <- intervenciones_2018 %>% filter(autopista %in% autopistas_finales) %>% group_by(autopista) %>% summarise(total_accidentes=sum(n())) %>% mutate(total_2018=sum(total_accidentes)) %>%  mutate(porcentaje_accidentes_ano=total_accidentes*100/total_2018)
accidentes_autopista_2018$autopista <- as.factor(accidentes_autopista_2018$autopista)

#TrÃ¡fico anual por autopista
transito_2018_autopista  <-  transito_2018 %>% group_by(autopista) %>% summarise(trafico_anual=sum(trafico))
#Uno trÃ¡fico anual por autopista a la tabla de accidentes por autopista
accidentes_autopista_2018 <- accidentes_autopista_2018 %>% left_join(transito_2018_autopista, by="autopista")
accidentes_autopista_2018$autopista <- as.factor(accidentes_autopista_2018$autopista)

#Porcentaje de accidentes respecto al trÃ¡fico anual de la autopista
accidentes_autopista_2018 <- accidentes_autopista_2018 %>% mutate(porcentaje_accidentes_trafico=total_accidentes*100/trafico_anual)
accidentes_autopista_2018 <- accidentes_autopista_2018 %>% arrange(desc(porcentaje_accidentes_trafico))

#Asigno orden de pelogrosidad a las autopistas que cuentan con datos de trÃ¡fico
accidentes_autopista_2018$indice_peligrosidad <- seq_len(6)
accidentes_autopista_2018$indice_peligrosidad[is.na(accidentes_autopista_2018$porcentaje_accidentes_trafico)]<-NA
 
accidentes_autopista_2018 <- as_tibble(accidentes_autopista_2018)
autopistas_dia_peligrosidad <- as_tibble(autopistas_dia_peligrosidad)
autopistas_hs_peligrosidad <- as_tibble(autopistas_hs_peligrosidad)


#App
library(shiny)
require(shinydashboard)
library(ggplot2)
#Header
header <- dashboardHeader(title = "Mi viaje")  

#Sidebar
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Autopista de viaje",icon=icon("road", lib='glyphicon'),selectInput(inputId = "seleccion_autopista",
                                                                                 label = "Autopista:",
                                                                                 choices = accidentes_autopista_2018$autopista)),
    
    menuItem("Dia",icon=icon("picture", lib='glyphicon'),selectInput(inputId = "seleccion_dia",
                                                                     label = "Dia de la semana:",
                                                                     choices = autopistas_dia_peligrosidad$dia,
                                                                     selected = TRUE)),
    
    menuItem("Hora de viaje",icon=icon("time", lib='glyphicon'),sliderInput(inputId = "seleccion_hora",
                                                                            label = "Hora de viaje:",
                                                                            min = 0,
                                                                            max = 23,
                                                                            value = 30)),
    
    menuItem("C0digo de esta App", icon = icon("bullhorn",lib='glyphicon'), 
             href = "https://github.com/bynans/app-autopistas-ausa")
    
    
  )
)


frow1 <- fluidRow(
  valueBoxOutput("value1")
  ,valueBoxOutput("value2")
  ,valueBoxOutput("value5")
)

frow2 <- fluidRow(
  
  box(
    title = "Incidentes por dia"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("accidentesDia", height = "300px")
  )
  
  ,box(
    title = "Incidentes por hora"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("accidentesHora", height = "300px")
  ) 
  
)

frow3 <- fluidRow(
  valueBoxOutput("value3"),
  valueBoxOutput("value4"),
  valueBoxOutput('value6')
)

body <- dashboardBody(frow1, frow2, frow3)
ui <- dashboardPage(title = 'APP: Mi viaje', header, sidebar, body, skin='yellow')

# Server  
server <- function(input, output) { 
  
  #Variables para cajitas
  accidentes_seleccion_2018 <- reactive({accidentes_autopista_2018 %>% filter(autopista==input$seleccion_autopista) %>% select(total_accidentes)})
  peligrosidad_seleccion_autopista <- reactive({accidentes_autopista_2018 %>% filter(autopista==input$seleccion_autopista) %>% select(indice_peligrosidad)})
  porcentaje_accidentes_2018 <- reactive({accidentes_autopista_2018 %>% filter(autopista==input$seleccion_autopista) %>% select(porcentaje_accidentes_ano)})
  porcentaje_acc_hora <- reactive({autopistas_hs_peligrosidad %>% filter(autopista==input$seleccion_autopista) %>% filter (hora==input$seleccion_hora) %>% select(porcentaje)})
  accidentes_seleccion_dia <- reactive({autopistas_dia_peligrosidad %>% filter(autopista==input$seleccion_autopista) %>% filter (dia==input$seleccion_dia) %>% select(accidentes)})
    peligrosidad_seleccion_dia <- reactive({autopistas_dia_peligrosidad %>% filter(autopista==input$seleccion_autopista) %>% filter(dia==input$seleccion_dia) %>% select(indice_peligrosidad)})
  
  
  #Cajitas
  output$value1 <- renderValueBox({
    valueBox(
      formatC(accidentes_seleccion_2018(), format="d", big.mark=',')
      ,paste('Incidentes 2018 en:', input$seleccion_autopista)
      ,icon = icon("stats",lib='glyphicon')
      ,color = "yellow")
  })
  output$value3 <- renderValueBox({
    valueBox(
      formatC(accidentes_seleccion_dia(), format="d", big.mark=',')
      ,paste('Incidentes el dia', input$seleccion_dia)
      ,icon = icon("stats",lib='glyphicon')
      ,color = "yellow")
  })
  output$value2 <- renderValueBox({
    valueBox(
      formatC(peligrosidad_seleccion_autopista(), format="d", big.mark=','),
      paste('Orden de peligrosidad de',input$seleccion_autopista)
      ,icon = icon("search",lib='glyphicon')
      ,color = "red")
  })
  output$value4 <- renderValueBox({
    valueBox(
      formatC(peligrosidad_seleccion_dia(), format="d", big.mark=',')
      ,paste('Orden de peligrosidad del dia',input$seleccion_dia, "en la autopista", input$seleccion_autopista)
      ,icon = icon("search",lib='glyphicon')
      ,color = "red")
  })
  output$value5 <- renderValueBox({
    valueBox(
      formatC(porcentaje_accidentes_2018(), format="d", big.mark=',')
      ,paste('% de los incidentes 2018')
      ,icon = icon("menu-hamburger",lib='glyphicon')
      ,color = "yellow")
  })
  
  #GrÃ¡ficos
  
  output$accidentesDia <- renderPlot({ autopistas_dia_peligrosidad %>% filter(autopista==input$seleccion_autopista) %>% 
      ggplot(aes(x=dia, y=accidentes)) + 
      geom_bar(position = "dodge", stat = "identity") + ylab("Accidentes") + 
      xlab("dia") + theme(legend.position="bottom" 
                          ,plot.title = element_text(size=15, face="bold")) + 
      ggtitle("Incidentes por dia")
  })
  
  
  output$accidentesHora <- renderPlot({autopistas_hs_peligrosidad %>% filter(autopista==input$seleccion_autopista) %>% 
      ggplot(aes(x=hora, y=accidentes)) + 
      geom_bar(position = "dodge", stat = "identity") + ylab("Accidentes") + 
      xlab("Hora") + theme(legend.position="bottom" 
                           ,plot.title = element_text(size=15, face="bold")) + 
      ggtitle("Incidentes por hora en la autopista")
  })
  
  output$value6 <- renderValueBox({
    valueBox(
      formatC(porcentaje_acc_hora(), format="d", big.mark=',')
      ,paste('% de incidentes a las', input$seleccion_hora, ' hs.respecto al total 2018 en la autopista')
      ,icon = icon("menu-hamburger",lib='glyphicon')
      ,color = "yellow")
  })
}

shinyApp(ui, server)