library(shiny)
library(dplyr)
library(tidyr)
library(stringr)
library(sf)
library(ggplot2)


load("woj_id.RData")
woj_id <- woj_id %>% dplyr::arrange(name)
woj_id$name <- str_to_lower(woj_id$name )
woj <- st_read("mapa/gadm40_POL_1.shp")
woj <- woj %>% dplyr::arrange(NAME_1)
woj$NAME_1 <- str_to_lower(woj$NAME_1 )


get_data <- function(x) {
  my_file <- paste(paste("test_", x, sep = ""), ".csv", sep = "")
  read.csv(my_file)
}
id_data <- tibble(id = c("a", "b"),
                       variable = c("Ilość", "Małżeństwa"))



ui <- fluidPage(
  titlePanel("Miasta w liczbach"
  ),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "main_variable",
                  label = "Cecha",
                  choices =  c("Ilość", "Małżeństwa") ),
      selectInput(inputId = "size",
                  label = "Wielkość",
                  choices = c("Ogółem", "poniżej 20000", "20000 - 99999", "100000 i więcej")),
      numericInput(inputId = "year", 
                   label = "Rok", 
                   value = 2020, min = 1999, max = 2020),
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
  
  data_ready <- reactive({
    not_ready <- get_data(id_data$id[id_data$variable == input$main_variable])
    not_ready %>% filter(year == input$year) %>% select(3:18) %>%
      pivot_longer(names_to = "id", values_to = "variable", cols = everything()) %>%
      mutate(id = str_remove(string = .$id, pattern = "val_")) %>%
      left_join(woj_id, by = "id")
  })
  
  data_to_map <- reactive(left_join(woj, data_ready(), by = c("NAME_1" = "name")))
  year_var <- reactive(input$year)
  main_var <- reactive(input$main_variable)
  
  
  output$my_map <- renderPlot({
    data_to_map() %>%
      ggplot() +
      geom_sf(aes(fill = variable )) +
      xlab("Długość geograficzna")+ylab("Szerokość geograficzna") +
      xlab("") + 
      ylab("") +
      scale_fill_gradient(low = "#ffffcc", high = "#006837") +
      geom_sf_text(mapping=aes(label=NAME_1),size=3) +
      geom_sf_text(mapping=aes(label= variable),size=3,nudge_y = -0.2)  +
      ggtitle(main_var(),
              subtitle = paste("W roku ", year_var(), sep = "")) +
      labs(fill=main_var()) + 
      theme(axis.ticks = element_blank(),
            axis.text = element_blank(),
            rect = element_blank(), # usuwa siatkę
            legend.position = c(0.1,0.15),legend.title.align=0.5, legend.text.align = 1 )
  }, res = 120)
  
  
  output$download <- downloadHandler(
    filename = function() {
      paste0("test", ".csv")
    },
    content = function(file) {
      write.csv(data_ready(), file)
    }
  )
  
  
}

shinyApp(ui, server)






