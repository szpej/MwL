
library(shiny)
library(dplyr)
library(tidyr)
library(stringr)
library(sf)
library(ggplot2)



load("data/woj_id.RData")
woj_id <- woj_id %>% dplyr::arrange(name)
woj_id$name <- str_to_lower(woj_id$name )

woj <- st_read("mapa/gadm40_POL_1.shp")
woj$NAME_1 <- str_to_lower(woj$NAME_1 )

# here should be bdl function
get_data <- function(x) {
  my_file <- paste(paste("data/test_", x, sep = ""), ".csv", sep = "")
  read.csv(my_file)
}
id_data <- tibble(id = c("a", "b"),
                       variable = c("Ilość", "Małżeństwa"))



# UI ----------------------------------------------------------------------


ui <- fluidPage(
  tabsetPanel(
      tabPanel("Pobranie danych",
               sidebarPanel(
                 selectInput(inputId = "main_variable",
                             label = "Cecha",
                             choices =  c("Ilość", "Małżeństwa") ),
                 selectInput(inputId = "size",
                             label = "Wielkość",
                             choices = c("Ogółem", "poniżej 20000", "20000 - 99999", "100000 i więcej")),
                 selectInput(inputId = "year", 
                             label = "Rok", 
                             choices = NULL),
                 checkboxInput("compare_id", "Czy chcesz porównać z innym rokiem?", value = FALSE),
                 selectInput(inputId = "year_compare", 
                             label = "Porównaj z rokiem:",
                             choices = NULL),
                 actionButton("confirm", "Pobierz", class = "btn-block"),
                 downloadButton("download", "Download .csv")
          )
          ),
      tabPanel("Mapa",
               plotOutput("my_map"),
               selectInput(inputId = "color", 
                           label = "Kolory",
                           choices = c("1", "2", "3", "4", "5"))
               ),
      tabPanel("Tabelka",
               mainPanel(
                 tableOutput("data")
               )),
      tabPanel("Porównanie",

                 plotOutput("map_comparison"),
                 selectInput(inputId = "color_comp", 
                             label = "Kolory",
                             choices = c("1", "2", "3", "4", "5"))
               )
      ))



# Server ------------------------------------------------------------------

  
server <- function(input, output, session) {
  
  year_var <- reactive(as.numeric(input$year))
  main_var <- eventReactive(input$confirm, input$main_variable)
  year_comp <- reactive(as.numeric(input$year_compare))
  state <- reactive(input$compare_id)
  
  
  color <- reactive(
    switch(input$color,
           "1" = c("#f7fcb9", "#31a354"),
           "2" = c("#e5f5e0", "#31a354"),
           "3" = c("#edf8b1", "#2c7fb8"),
           "4" = c("#e0ecf4", "#ffeda0"),
           "5" = c("#f7fcb9", "#f03b20")
           ))
  color_2 <- reactive(
    switch(input$color_comp,
           "1" = c("#f7fcb9", "#31a354"),
           "2" = c("#e5f5e0", "#31a354"),
           "3" = c("#edf8b1", "#2c7fb8"),
           "4" = c("#e0ecf4", "#ffeda0"),
           "5" = c("#f7fcb9", "#f03b20")
    ))
  
  data_ready <- eventReactive(input$confirm,{
    get_data(id_data$id[id_data$variable == input$main_variable])
  })
  
  observeEvent(input$compare_id, {
    if(input$compare_id == TRUE) {
      choices <- unique(data_ready()$year)
    } else {
      choices <- ""}
    freezeReactiveValue(input, "year_compare")
    updateSelectInput(inputId = "year_compare", choices = choices)
  }) 
  
  observeEvent(main_var(), {
    choices <- unique(data_ready()$year)
    freezeReactiveValue(input, "year")
    updateSelectInput(inputId = "year", choices = choices) 
  })
  
  
  data_to_table <- reactive({
    data_ready() %>%  
      filter(year == year_var()) %>% select(3:18) %>%
      pivot_longer(names_to = "id", values_to = "variable", cols = everything()) %>%
      mutate(id = str_remove(string = .$id, pattern = "val_")) %>%
      left_join(woj_id, by = "id")
  })
  
  data_to_map <- reactive({
    data_to_table() %>% left_join(woj, ., by = c("NAME_1" = "name"))
  })
  
  
  
  date_to_compare <- reactive({
    if(state() == TRUE) {
      data_ready() %>%  
        filter(year == year_comp()) %>% select(3:18) %>%
        pivot_longer(names_to = "id", values_to = "variable", cols = everything()) %>%
        mutate(id = str_remove(string = .$id, pattern = "val_")) %>%
        left_join(woj_id, by = "id")
  }})
  
  date_comp_to_map <- reactive({
    if(input$compare_id == TRUE) {
      date_to_compare() %>% 
        mutate(variable = round((data_to_table()$variable - variable)/variable,4)*100) %>%
        left_join(woj, ., by = c("NAME_1" = "name"))
    } else {
      NULL}
  })
    
  

  
  
  
  output$my_map <-
    renderPlot({
      data_to_map() %>%
        ggplot() +
        geom_sf(aes(fill = variable )) +
      #  xlab("Długość geograficzna")+ylab("Szerokość geograficzna") +
        xlab("") + 
        ylab("") +
        scale_fill_gradient(low = color()[1], high = color()[2]) +
        geom_sf_text(mapping=aes(label=NAME_1),size=5) +
        geom_sf_text(mapping=aes(label= variable),size=5,nudge_y = -0.2)  +
        ggtitle(main_var(),
                subtitle = paste("W roku ", year_var(), sep = "")) +
        labs(fill=main_var()) + 
        theme(axis.ticks = element_blank(),
              axis.text = element_blank(),
              rect = element_blank(), # remove grid
              legend.position = c(0.1,0.15),legend.title.align=0.5, legend.text.align = 1 )
    }, res = 80, height = 1000)
  
  
  output$data <- renderTable(data_to_table())
  
  output$map_comparison <-
    renderPlot({
      if(state()== TRUE) {
        date_comp_to_map() %>%
        ggplot() +
        geom_sf(aes(fill = variable )) +
     #   xlab("Długość geograficzna")+ylab("Szerokość geograficzna") +
        xlab("") + 
        ylab("") +
        scale_fill_gradient(low = color_2()[1], high = color_2()[2]) +
        geom_sf_text(mapping=aes(label=NAME_1),size=5) +
        geom_sf_text(mapping=aes(label= variable),size=5,nudge_y = -0.2)  +
        ggtitle(main_var(),
                subtitle = paste("Rok ", year_var(), " z rokiem ", year_comp(), " jako bazowy" ,sep = "")) +
        guides(fill=guide_legend(title="Procenty")) + 
        theme(axis.ticks = element_blank(),
              axis.text = element_blank(),
              rect = element_blank(), # remove grid
              legend.position = c(0.1,0.15),legend.title.align=0.5, legend.text.align = 1 )
    }
      }, res = 80, height = 1000)
    

  
  output$download <- downloadHandler(
    filename = function() {
      paste0("test", ".csv")
    },
    content = function(file) {
      write.csv(data_to_table(), file)
    }
  )
  
  
}

shinyApp(ui, server)





