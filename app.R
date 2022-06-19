

library(shiny)
library(tidyverse)
library(sf)

dane <- read_csv("dane.csv")
dane_woj <- dane[is.na(dane$Liczba_miast),1]
dane_type <-  dane[!(is.na(dane$Liczba_miast)),1]
dane_dane <- dane[!(is.na(dane$Liczba_miast)),2]
dane_dane2 <- dane[!(is.na(dane$Liczba_miast)),3]
dane <- 
  tibble(woj = rep(dane_woj$Zmienna_1, each = 4),
         wielkosc = dane_type$Zmienna_1, 
         liczba = dane_dane$Liczba_miast,
         ludnosc = dane_dane2$Ludnosc)
woj <- st_read("mapa/gadm40_POL_1.shp")
total_dane <- left_join(woj, dane, by = c("NAME_1" = "woj"))
rm(dane_dane, dane_type, dane_woj, dane_dane2, dane, woj)


generate_map <- function(data, variable, type) {
  date[date$wielkosc == type,]
  ggplot(data=data) +
    geom_sf(aes(fill = variable)) +
    xlab("Długość geograficzna")+ylab("Szerokość geograficzna") +
    xlab("") + 
    ylab("") 

}

ui <- fluidPage(
  titlePanel("Miasta w liczbach"
  ),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "variable",
                  label = "Variable",
                  choices =  names(total_dane)[13:14]),
      selectInput(inputId = "size",
                  label = "Size",
                  choices = unique(total_dane$wielkosc)),
      numericInput(inputId = "year", 
                   label = "Rok", 
                   value = 2020, min = 2010, max = 2020),
      downloadButton("download", "Download .csv")
      ),
  mainPanel(
    fluidRow(
      plotOutput("my_map")
      )
    )
  )
)
  

server <- function(input, output, session) {
  
  title <- reactive(input$variable)
  subtitle <- reactive(input$size)
  year_ <- reactive(input$year)
  data <- reactive(
    st_drop_geometry(total_dane) %>% filter(wielkosc == input$size) %>%
    select("NAME_1", .data[[input$variable]])
  )
  
  output$my_map <- renderPlot({
    total_dane %>% filter(wielkosc == input$size) %>%
      ggplot() +
      geom_sf(aes(fill = .data[[input$variable]] )) +
      xlab("Długość geograficzna")+ylab("Szerokość geograficzna") +
      xlab("") + 
      ylab("")  +
      ggtitle(title(), 
              subtitle = paste("W roku ",year_())) +
      labs(fill=subtitle())  +
      scale_fill_gradient(low = "#ffffcc", high = "#006837") +
      geom_sf_text(mapping=aes(label=NAME_1),size=3) +
      geom_sf_text(mapping=aes(label=.data[[input$variable]]),size=3,nudge_y = -0.2)  +
      theme(axis.ticks = element_blank(),
            axis.text = element_blank(),
            rect = element_blank(), # usuwa siatkę
            legend.position = c(0.1,0.15),legend.title.align=0.5, legend.text.align = 1 )
}, res = 96)

  output$download <- downloadHandler(
    filename = function() {
      paste0("test", ".csv")
    },
    content = function(file) {
      write_csv(data(), file)
    }
  )
  
  
}

shinyApp(ui, server)






