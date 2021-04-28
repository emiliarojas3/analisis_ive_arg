library(shiny)
library(shinythemes)
library(tidyverse)
library(plyr)
library(ggparliament)
library(ggthemes)
library(plotly)
library(readr)
library(dplyr)
library(ggplot2)
library(geofacet)
library(data.table)
library(highcharter)
library(scales)
library(paletteer)
library(gganimate)
library(png)
library(writexl)
library(sf)
library(ggmap)
library(leaflet)
library(RColorBrewer)


resultados_ive <- read.csv("resultados_ive.csv", sep = ",", encoding = "latin1") # Dataset general
prov_geo <- st_read("prov_geo.shp") # Geometrias de Provincias
resultados_ive_geo <- prov_geo %>% left_join(resultados_ive) # Unimos geometrias con dataset general para la solapa "Por provincia"
rm(prov_geo) # Quitamos archivo de geometrias


############################## UI ##############################################

ui <- fluidPage(
    theme = shinytheme("flatly"),
    titlePanel(
      title=h2('Análisis de resultados de la votación del proyecto IVE en el Poder Legislativo argentino', align="center")),
    tabsetPanel(
      
        tabPanel('Por voto',
                 sidebarLayout(
                     sidebarPanel(
                         selectInput(inputId = "camara1",
                                     label = h5("Selección de Cámara"),
                                     choices = unique(resultados_ive$Camara),
                                     selected = unique(resultados_ive$Camara)[1],
                                     multiple = FALSE),
                         selectInput(inputId = "anio1",
                                     label = h5("Selección de Año"),
                                     choices = unique(resultados_ive$Anio),
                                     selected = unique(resultados_ive$Anio)[1],
                                     multiple = FALSE),
                         br(),
                     helpText("Presionando el siguiente botón, podrás descargar el dataset utilizado."),
                     downloadButton("download_data1"),
                     br(),
                     br(),
                     br(),
                     helpText("Fuentes: Cámara de Diputados y de Senadores de la Nación Argentina")),
                     mainPanel(
                         plotlyOutput('voto'),
                         br(),
                         p("En 2020, en Argentina, se aprobó la", strong("Ley de Interrupción Voluntaria del Embarazo."),
                           " Esta Ley ya había pasado por el Congreso en 2018 con 129 votos afirmativos en la cámara baja, 
                           y había sido rechazada en el Senado con 38 votos negativos. Para el 2020, la tendencia se 
                           revirtió ampliamente ya que el proyecto obtuvo media sanción en Diputadxs con 131 votos afirmativos 
                           y logró aprobarse con 38 votos positivos en la cámara alta. Bajo esta investigación nos proponemos 
                           realizar una radiografía de la conformación de las cámaras del Poder Legislativo Nacional 
                           en relación a las posiciones sobre la IVE, para comprender mejor el paso hacia su sanción.")
                     )
                 )
        ),
        tabPanel('Por orientación política',
                 sidebarLayout(
                     sidebarPanel(
                         selectInput(inputId = "camara2",
                                     label = h5("Selección de Cámara"),
                                     choices = unique(resultados_ive$Camara),
                                     selected = unique(resultados_ive$Camara)[1],
                                     multiple = FALSE),
                         selectInput(inputId = "anio2",
                                     label = h5("Selección de Año"),
                                     choices = unique(resultados_ive$Anio),
                                     selected = unique(resultados_ive$Anio)[1],
                                     multiple = FALSE),
                         br(),
                         helpText("Presionando el siguiente botón, podrás descargar el dataset utilizado."),
                         downloadButton("download_data2"),
                         br(),
                         br(),
                         br(),
                         helpText("Fuentes: Cámara de Diputados y de Senadores de la Nación Argentina")),
                     mainPanel(
                       highchartOutput('comp_orien_pol'),
                         br(),
                       highchartOutput('orien_pol'),
                       br(),
                         p("En cuanto a la orientación política, las votaciones de ambos años para las dos cámaras 
                           reflejan tendencias mayoritarias a favor del proyecto en el kirchnerismo y partidos aliados,
                           mientras que en fuerzas como Cambiemos la mayor parte de sus legisladorxs han votado en 
                           contra del mismo. Precisamente, en 2018, el 60.75% de lxs diputadxs de Cambiemos votó en contra 
                           del proyecto mientras el 85.94% de los representantes del Kirchnerismo y aliados manifestaron su 
                           posición afirmativa. De manera similar, esta situación se replicó en 2020 cuando un 60.34% de 
                           lxs legisladorxs de Cambiemos emitieron su voto negativo y el 70.34% de lxs miembrxs del 
                           Kirchnerismo, junto con el PJ, apoyaron el proyecto. En el caso del Senado, el 68% de lxs 
                           representantes de Cambiemos votaron en contra de la iniciativa mientras que el 88.89% de votos 
                           kirchneristas fueron positivos. En 2020, la escena presentó algunos cambios donde el 57.14% de 
                           los votos de Cambiemos fueron negativos y el 60.98% de los votos kirchneristas fueron afirmativos.")
                     )
                 )
        ),
        tabPanel('Por provincia',
                 sidebarLayout(
                     sidebarPanel(
                         selectInput(inputId = "camara3",
                                     label = h5("Selección de Cámara"),
                                     choices = unique(resultados_ive_geo$Camara),
                                     selected = unique(resultados_ive_geo$Camara)[1],
                                     multiple = FALSE),
                         selectInput(inputId = "anio3",
                                     label = h5("Selección de Año"),
                                     choices = unique(resultados_ive_geo$Anio),
                                     selected = unique(resultados_ive_geo$Anio)[1],
                                     multiple = FALSE),
                         br(),
                         helpText("Presionando el siguiente botón, podrás descargar el dataset utilizado."),
                         downloadButton("download_data3"),
                         br(),
                         br(),
                         br(),
                         helpText("Fuentes: Cámara de Diputados y de Senadores de la Nación Argentina")),
                     mainPanel( 
                         leafletOutput('provincia'),
                         br(),
                         p("De acuerdo con los gráficos de provincia, en ambas cámaras (diputadxs y senadorxs), y años 
                           (2018 y 2020), se observa una tendencia de las provincias del norte a rechazar el proyecto de ley, 
                           mientras que las del sur tienden a aprobarlo. En cuanto a las provincias del centro, 
                           estas tienden a mantener su posición. De hecho, podemos encontrar que las 
                           posiciones negativas superaron más del 50% de los votos emitidos en provincias como Salta, Jujuy,
                           Tucumán, Catamarca y Santiago del Estero. Por otro lado, territorios como Santa Cruz, Río Negro, 
                           Neuquen y Tierra del Fuego manifestaron una posición mayoritaria afirmativa. En el Centro, 
                           provincias como Buenos Aires, La Pampa y Entre Ríos mantuvieron su postura afirmativa en la mayoría
                           de sus votos mientras que Córdoba y Santa Fe pasaron del rechazo a la aprobación en más del 50% de 
                           sus votos.")
                     )
                 )
        ),
        tabPanel('Por género',
                 sidebarLayout(
                     sidebarPanel(
                         selectInput(inputId = "camara4",
                                     label = h5("Selección de Cámara"),
                                     choices = unique(resultados_ive$Camara),
                                     selected = unique(resultados_ive$Camara)[1],
                                     multiple = FALSE),
                         selectInput(inputId = "anio4",
                                     label = h5("Selección de Año"),
                                     choices = unique(resultados_ive$Anio),
                                     selected = unique(resultados_ive$Anio)[1],
                                     multiple = FALSE),
                         br(),
                         helpText("Presionando el siguiente botón, podrás descargar el dataset utilizado."),
                         downloadButton("download_data4"),
                         br(),
                         br(),
                         br(),
                         helpText("Fuentes: Cámara de Diputados y de Senadores de la Nación Argentina")),
                     mainPanel(
                       highchartOutput('comp_genero'),
                         br(),
                       highchartOutput('genero'),
                         br(),
                         p("En relación a la votación según género, se observa, en primer lugar, una mayoría de hombres en 
                           la composición de ambas cámaras y en ambos períodos legislativos. Los resultados de las votaciones 
                           muestran un incremento de adhesión de las mujeres legisladoras en la comparación de los períodos. 
                           En el caso de Diputadxs, el incremento es de 6.14%, mientras que en Senadorxs es de aproximadamente
                           21.19%. En el caso de los legisladores hombres se mantiene la proporción de la adhesión en ambos
                           períodos: el apoyo se reduce 2.96% en Diputadxs y se incrementa un 2.7% en Senadorxs. 
                           Aquí se puede evaluar cómo la Ley de Paridad de Género en Ámbitos de Representación Política del 2017 
                           ha contribuido a la aprobación de iniciativas legislativas vinculadas a los derechos de las mujeres.")
                     )
                 )
        ),
        tabPanel('Por rango de edad',
                 sidebarLayout(
                     sidebarPanel(
                         selectInput(inputId = "camara5",
                                     label = h5("Selección de Cámara"),
                                     choices = unique(resultados_ive$Camara),
                                     selected = unique(resultados_ive$Camara)[1],
                                     multiple = FALSE),
                         selectInput(inputId = "anio5",
                                     label = h5("Selección de Año"),
                                     choices = unique(resultados_ive$Anio),
                                     selected = unique(resultados_ive$Anio)[1],
                                     multiple = FALSE),
                         br(),
                         helpText("Presionando el siguiente botón, podrás descargar el dataset utilizado."),
                         downloadButton("download_data5"),
                         br(),
                         br(),
                         br(),
                         helpText("Fuentes: Cámara de Diputados y de Senadores de la Nación Argentina")),
                     mainPanel( 
                       highchartOutput('comp_edad'),
                       br(),
                       p("Aclaración: En algunos casos no se han encontrado información sobre la edad del diputadx, con lo cual se le asignó la categoria 'SIN DATOS' (S/D)."),
                       highchartOutput('edad'),
                       br(),
                       p("Teniendo en cuenta los gráficos adquiridos, se aprecia una postura mayoritaria en contra del
                         proyecto IVE en grupos de edades mayores. En ambas cámaras y períodos se puede ver una 
                         predominancia del voto afirmativo para el grupo joven. En este sentido, lxs diputadxs cuya edad
                         ronda entre los rangos de 46 a 75 años, manifestaron una posición negativa superior al 50% en 2018.
                         Para 2020, la situación se muestra alterada por posiciones más dividas en los rangos 56-65 años
                         con un 47.76% de votos negativos contra el mismo porcentaje para votos afirmativos.
                         En el caso de la Cámara Alta, los rangos etarios mencionados mantienen al menos la mitad de
                         sus votos en contra del proyecto pero el escenario se modifica cuando los grupos de 46-55 y 
                         66-75 movilizan la mayoría de sus votos hacia la sanción del proyecto. Por último, el grupo
                         joven de 25 a 45 años refuerza su postura en favor de la iniciativa con un aumento del 
                         13.19% y 11.2% respectivamente en la cámara baja y un incremento del 33.33% y 45.71% a pesar de
                         que no mantengan una posición dominante en la composición del Senado.")
                     )
                 )
        ),
        tabPanel('Equipo',
                 mainPanel(
                   h2(strong("Equipo:")),
                   br(),
                   tags$h4("Mariano Mezzadra: ",
                           tags$a(href="https://twitter.com/Mezzadra_?s=20",
                                  "Twitter")),
                   br(),
                   h4("Luisina Picariello: ",
                      tags$a(href="https://twitter.com/LuPicariello?s=20",
                             "Twitter")),
                   br(),
                   h4("Alejandro Pocoroba: ",
                      tags$a(href="https://twitter.com/AlejPocorob?s=20",
                             "Twitter")),
                   br(),
                   h4("María Emilia Rojas: ",
                      tags$a(href="https://www.linkedin.com/in/mariaemiliarojas3/",
                             "LinkedIn")),
                   br(),
                   h4("Carmen Tello: ",
                      tags$a(href="https://www.linkedin.com/in/carmen-tello/",
                             "LinkedIn")),
                   br(),
                   h2(strong("Curso:")),
                   tags$a(href="https://eant.tech/escuela-de-ciencias-de-datos/programas/social-data-analytics",
                          h4("Social Data Analytics - EANT")),
                textOutput('equipo')
                 )
        )
    )
)

############################## SERVER ##########################################

server <- function(input, output) {
  
  filt_voto <- reactive ({
    filt_voto = resultados_ive[resultados_ive$Camara==input$camara1 & resultados_ive$Anio==input$anio1, ]
    filt_voto
  })   
  
  output$voto <- renderPlotly({
    
    votocam <- filt_voto() %>% 
      ggplot(aes(x, y, fill = Voto, text = paste0("Diputadx: ", Nombre_legislador, "\n", "Provincia: ", Provincia, "\n", "Orientación: ", Orientacion , "\n", "Voto: ", Voto))) +
      geom_parliament_seats(size = 4) +
      geom_parliament_bar(party = Voto, label = T) + 
      draw_majoritythreshold(n = 36, label = F, type = "semicircle", linesize = 0.5) + 
      scale_fill_manual(values = resultados_ive$colour,limits=resultados_ive$Voto) +
      labs(title = "Votación del proyecto IVE según tipo de voto", 
           x = "",
           y = "",
           colour = NULL) +
      theme_minimal() +
      theme(panel.grid = element_blank(), 
            axis.text = element_blank(),
            legend.text = element_text(size = 10),
            legend.position = "bottom"
      )
    
    votoplotly <- ggplotly(votocam, tooltip = "text") %>% 
    layout(
      legend = list(x=0.99,y=0.0, orientation = "h"))
    
    votoplotly
  })
  
  output$download_data1 <- downloadHandler(
    filename = "resultados_ive.xlsx",
    content = function(file) {
      filt_votacion <- filt_voto ()
      write_xlsx(filt_votacion[,2:10], file)
    }
  )
  
    df_filt_orienpol <- reactive ({
        df_filt_orienpol = resultados_ive[resultados_ive$Camara==input$camara2 & resultados_ive$Anio==input$anio2, ]
        df_filt_orienpol
    }) 
    
    output$comp_orien_pol <- renderHighchart({
      
      comp_orienpolcam <- df_filt_orienpol() %>%
        dplyr::count(Orientacion) %>% 
        mutate(perc1 = round(n/sum(n) *100, 2)) %>% 
        arrange(Orientacion) %>%
        hchart( 
          "bar",
          hcaes(x = Orientacion, y = n, p=perc1, color = Orientacion)
          
        ) %>%  
        hc_title(text = "Composición de la Cámara según Orientación Política") %>%
        hc_yAxis(
          title = list(text = "Cantidad de Legisladorxs"),
          labels = list(format = "{value}"), max = 120)  %>%
        hc_xAxis(
          title = list(text = "Orientación Política"))
      
      
      comp_orienpolcam
      
    })
    
    output$orien_pol <- renderHighchart({
      
      orienpolcam <- df_filt_orienpol() %>%
        dplyr::count(Orientacion, Voto, sort = F)%>% 
        rename(Cantidad = n) %>% 
        group_by(Orientacion) %>% 
        mutate(Porcentaje = (round(Cantidad/sum(Cantidad) * 100, 2))) %>% 
        transform(Voto = reorder(Voto, -Cantidad)) %>%
        arrange(Orientacion) %>%
        mutate(Voto = factor(Voto, levels=c("NEGATIVO", "AFIRMATIVO", "ABSTENCION", "AUSENTE"))) %>%
        hchart( 
          "bar",
          hcaes(
            x = Orientacion,"Voto", 
            y = Porcentaje,
            group=Voto), 
          color = c("#EA7B77", "#8DB994", "#cac795", "#3f3f3f"),
          stacking = "percent"
        ) %>% 
        hc_plotOptions(
          Voto = list(
            colorByPoint = TRUE
          )) %>%
        
        hc_title(text = "Votación del proyecto IVE según Orientación Política") %>%
        
        hc_yAxis(
          title = list(text = "Porcentaje de Votos"),
          labels = list(format = "{value}%"), max = 100)  %>%
        hc_xAxis(
          title = list(text = "Orientación Política"))
      
      orienpolcam
      
    })
    
    output$download_data2 <- downloadHandler(
      filename = "resultados_ive.xlsx",
      content = function(file) {
        filt_orienpol <- df_filt_orienpol ()
        write_xlsx(filt_orienpol[,2:10], file)
      }
    )
    
    df_filt_prov <- reactive ({
      df_filt_prov = resultados_ive_geo[resultados_ive_geo$Camara==input$camara3 & resultados_ive_geo$Anio==input$anio3, ]
      df_filt_prov
    }) 
    
    output$provincia <- renderLeaflet({
      
      mapa_provincias <- df_filt_prov() %>%
        select(Provincia, Voto) %>% 
        dplyr::count(Provincia, Voto, sort = F)%>% 
        rename(Cantidad = n) %>% 
        group_by (Provincia) %>% 
        mutate(Porcentaje = (round(Cantidad/sum(Cantidad) * 100, 2)),
               Porcentaje_Texto = paste0(Porcentaje, "%")) %>% 
        mutate(Max = max(Porcentaje)) %>% 
        filter(Porcentaje == Max) %>% 
        mutate(Porcentaje = ifelse(Voto == "NEGATIVO", Porcentaje*-1, Porcentaje))
      
      mypalette1d <- colorNumeric(c("#EA7B77", "#CAC795", "#8DB994"), domain=mapa_provincias$Porcentaje, na.color="transparent")
      mypalette1d(c(1,6,9))
      
      mytext1d <- paste(
        "Provincia: ", mapa_provincias$Provincia,"<br/>", 
        "Voto Mayoritario: ", mapa_provincias$Voto, "<br/>",
        "Cantidad: ", mapa_provincias$Cantidad, "<br/>",
        "Porcentaje sobre el total: ", mapa_provincias$Porcentaje_Texto, "<br/>") %>%
        lapply(htmltools::HTML)
      
      mapa_prov <- leaflet(mapa_provincias) %>% 
        addTiles()  %>% 
        setView(lat=-39, lng=-60, zoom=3.49) %>%
        addPolygons( 
          fillColor = ~mypalette1d(Porcentaje), 
          stroke=TRUE, 
          fillOpacity = 0.9, 
          color="white", 
          weight=0.4,
          label=mytext1d,
          labelOptions = labelOptions( 
            style = list("font-weight" = "normal", padding = "3px 8px"), 
            textsize = "13px", 
            direction = "auto")) %>%
        addLegend("bottomright", 
                  colors =c("#8DB994","#EA7B77"),
                  labels= c("NEGATIVO", "AFIRMATIVO"),
                  title= "Voto",
                  opacity = 0.9)
      mapa_prov
      
    })
    
    output$download_data3 <- downloadHandler(
      filename = "resultados_ive.xlsx",
      content = function(file) {
        filt_prov <- df_filt_prov ()
        write_xlsx(filt_prov[,c(1,3,4,5,6,7,8,9,10)], file)
      }
    )
    
    df_filt_gen <- reactive ({
      df_filt_gen = resultados_ive[resultados_ive$Camara==input$camara4 & resultados_ive$Anio==input$anio4, ]
      df_filt_gen
    })
    
    output$comp_genero <- renderHighchart({
      
      comp_generocam <- df_filt_gen() %>%
        dplyr::count(Genero,name="Cantidad") %>% 
        arrange(Genero) %>% 
        mutate(Porcentaje = paste0(round(Cantidad/sum(Cantidad) * 100, 2), "%")) %>% 
        
        hchart( 
          "column",
          hcaes(x = Genero, y = Cantidad, p=Porcentaje, color = Genero)
          
        ) %>%  
        hc_title(text = "Composición de la Cámara según Genero") %>%
        hc_yAxis(
          title = list(text = "Cantidad de Legisladorxs"),
          labels = list(format = "{value}"), max = 160)  %>%
        hc_xAxis(
          title = list(text = "Genero"))
      
      comp_generocam
      
    })
    
    output$genero <- renderHighchart({
    
        generocam <- df_filt_gen() %>% 
          dplyr::count(Genero, Voto, sort = F)%>% 
          rename(Cantidad = n) %>% 
          group_by(Genero) %>% 
          mutate(Porcentaje = (round(Cantidad/sum(Cantidad) * 100, 2))) %>% 
          transform(Voto = reorder(Voto, -Cantidad)) %>%
          arrange(Genero) %>%
          mutate(Voto = factor(Voto, levels=c("NEGATIVO", "AFIRMATIVO", "ABSTENCION", "AUSENTE"))) %>%  
          
          hchart( 
            "column",
            hcaes(
              x = Genero, 
              y = Porcentaje,
              group=Voto), 
            color = c("#EA7B77", "#8DB994", "#cac795", "#3f3f3f"),
            stacking = "percent"
          ) %>% 
          hc_plotOptions(
            Voto = list(
              colorByPoint = TRUE
            )) %>%
          
          hc_title(text = "Votación del proyecto IVE según Genero") %>%
          
          hc_yAxis(
            title = list(text = "Porcentaje de Votos"),
            labels = list(format = "{value}%"), max = 100)  %>%
          hc_xAxis(
            title = list(text = "Genero"))
        
        generocam
    
      })
    
    output$download_data4 <- downloadHandler(
      filename = "resultados_ive.xlsx",
      content = function(file) {
        filt_gen <- df_filt_gen ()
        write_xlsx(filt_gen[,2:10], file)
      }
    )
    
    df_filt_edad <- reactive ({
      df_filt_edad = resultados_ive[resultados_ive$Camara==input$camara5 & resultados_ive$Anio==input$anio5, ]
      df_filt_edad
    }) 
    
    output$comp_edad <- renderHighchart({
      
      comp_edadcam <- df_filt_edad() %>% 
        dplyr::count(Rango_Edad) %>% 
        mutate(perc1 = round(n/sum(n) *100, 2)) %>% 
        arrange(Rango_Edad) %>%
        hchart( 
          "bar",
          hcaes(x = Rango_Edad, y = n, p=perc1, color = Rango_Edad)
          
        ) %>%  
        hc_title(text = "Composición de la Cámara según rango de edad") %>%
        hc_yAxis(
          title = list(text = "Cantidad de Legisladorxs"),
          labels = list(format = "{value}"), max = 80)  %>%
        hc_xAxis(
          title = list(text = "Rango de edad"))
      
      comp_edadcam
      
    })
    
    output$edad <- renderHighchart({

    edadcam <- df_filt_edad() %>% 
      dplyr::count(Rango_Edad, Voto, sort = F)%>% 
      rename(Cantidad = n) %>% 
      group_by(Rango_Edad) %>% 
      mutate(Porcentaje = (round(Cantidad/sum(Cantidad) * 100, 2))) %>% 
      transform(Voto = reorder(Voto, -Cantidad)) %>%
      arrange(Rango_Edad) %>%
      mutate(Voto = factor(Voto, levels=c("NEGATIVO", "AFIRMATIVO", "ABSTENCION", "AUSENTE"))) %>%
      
      hchart( 
        "bar",
        hcaes(
          x = Rango_Edad, 
          y = Porcentaje, 
          group=Voto), 
        color = c("#EA7B77", "#8DB994", "#cac795", "#3f3f3f"),
        stacking = "percent"
      ) %>% 
      hc_plotOptions(
        Voto = list(
          colorByPoint = TRUE
        )) %>%
      
      hc_title(text = "Votación del proyecto IVE según rango de edad") %>%
      
      hc_yAxis(
        title = list(text = "Porcentaje de Votos"),
        labels = list(format = "{value}%"), max = 100)  %>%
      hc_xAxis(
        title = list(text = "Rango de edad"))
    
    edadcam
    
    })
    
    output$download_data5 <- downloadHandler(
      filename = "resultados_ive.xlsx",
      content = function(file) {
        filt_edad <- df_filt_edad ()
        write_xlsx(filt_edad[,2:10], file)
      }
    )
    
    output$equipo <- renderText({ 
     " "
      })
    
    }    

############################# RUN #############################################

shinyApp(ui = ui, server = server)
