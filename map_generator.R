library(shiny)
library(purrr)
library(dplyr)
library(stringr)
library(sf)
library(tmap)

# todo jeżeli brak jakiegoklwiek zmiennej w  filtrze to generuje całość!!!

# 


woj <- st_read("mapa/gadm40_POL_1.shp")
# pow <- st_read("mapa/gadm40_POL_2.shp")
get_data <- function(x) {
  read.csv(file = x)
}


make_ui <- function(x, var) {
  if (is.integer(x)) {
    rng <- range(x, na.rm = TRUE)
    sliderInput(var, var, min = rng[1], max = rng[2], value = rng, step = 1, sep = "")
  } else if (is.numeric(x)) {
    rng <- range(x, na.rm = TRUE)
    sliderInput(var, var, min = rng[1], max = rng[2], value = rng)
  } else if (is.factor(x)) {
    levs <- levels(x)
    selectInput(var, var, choices = levs, selected = levs, multiple = TRUE)
  } else if (is.character(x)) {
   levs <- unique(x)
   selectInput(var, var, choices = levs, selected = levs, multiple = TRUE)
  } else if (lubridate::is.Date(x)) {
    x <- as.Date(x)
    rng <- range(x, na.rm = TRUE)
    dateRangeInput(var, var, min = rng[1], max = rng[2], value = rng)
  } else {
    NULL
  }
}

filter_var <- function(x, val) {
  if (is.numeric(x)) {
    !is.na(x) & x >= val[1] & x <= val[2]
  } else if (is.factor(x)) {
    x %in% val
  } else if (is.character(x)) {
    x %in% val
  } else {
    TRUE
  }
}




# UI ----------------------------------------------------------------------


ui <- fluidPage(
  tabsetPanel(
    tabPanel("Przygotowanie danych",
             sidebarPanel(
               #selectInput(inputId = "data",
                #           label = "Dane",
                 #          choices =  c("a", "b")),
               textInput(inputId = "data", placeholder = "Proszę wpisać ściężkę do pliku", value = "", label = "Dane"),
               actionButton("loading", "Pobierz dane", class = "btn-block"),
               selectInput(inputId = "main_variable", 
                           label = "Zmienna, która ma być naniesiona na mapę", 
                           choices = NULL,
                           multiple = FALSE),
               selectInput(inputId = "join_variable", 
                           label = "Proszę podać zmienną identyfikującą powiaty (teryt)", 
                           choices = NULL,
                           multiple = FALSE),
               #checkboxInput("main_filter", "Chcę odfiltrować główną zmienną?", value = FALSE),
               #conditionalPanel(
                # condition = "input.main_filter == true",
                 #selectInput("smoothMethod", "",
                  #           list("lm", "glm", "gam", "loess", "rlm"))
               #),
               selectInput("qualitative", "Jakie filtry ilościowe", choices = NULL, multiple = TRUE),
               selectInput("quantitative", "Jakie filtry dyskretne", choices = NULL, multiple = TRUE),
               uiOutput("num_filter"),
               uiOutput("char_filter"),
               checkboxInput("compare_id", "Chcę wygenerować mapę porównawczą?", value = FALSE),
               actionButton("confirm", "Generuj mapę", class = "btn-block")
             )
    ),
    tabPanel("Mapa",
             tmapOutput("my_map"),
             verbatimTextOutput("text"),
             tableOutput("static"), # do testu
             selectInput(inputId = "color", 
                         label = "Kolory",
                         choices = c("1", "2", "3", "4", "5", "6", "7", "8"))
    )
    )
  )


# Server ------------------------------------------------------------------


server <- function(input, output, session) {
  
  
  data_ready <- eventReactive(input$loading,{
    get_data(input$data)
  })
  
  name_col <- eventReactive(input$loading,{ # do zastanowienia na co ma reagować, może na observenet data_ready()
    colnames(data_ready())
  })
  
  observeEvent(input$loading, {
    choices <- name_col()[map_lgl(name_col(), ~is.numeric(data_ready()[[.x]]))]
    freezeReactiveValue(input, "main_variable")
    updateSelectInput(inputId = "main_variable", choices = choices) 
  })
  
  observeEvent(input$loading, {
    choices <- name_col()[map_lgl(name_col(), ~is.numeric(data_ready()[[.x]]))]
    freezeReactiveValue(input, "join_variable")
    updateSelectInput(inputId = "join_variable", choices = choices) 
  })
  
  
  observeEvent(input$loading, {
    choices <- name_col()[map_lgl(name_col(), ~is.numeric(data_ready()[[.x]]))]
    freezeReactiveValue(input, "qualitative")
    updateSelectInput(inputId = "qualitative", choices = choices) 
  })
  
  observeEvent(input$loading, {
    choices <- name_col()[map_lgl(name_col(), ~is.character(data_ready()[[.x]]))]
    freezeReactiveValue(input, "quantitative")
    updateSelectInput(inputId = "quantitative", choices = choices) 
  })
  

  qual <- reactive(input$qualitative)
  quant <- reactive(input$quantitative)
  teryt <- reactive(input$join_variable)
  path <- reactive(input$data)
  map_variable <- reactive(input$main_variable)
  state <- reactive(input$compare_id)
  
  output$num_filter <- renderUI(
    map(qual(), ~ make_ui(data_ready()[[.x]], .x))
  )
  output$char_filter <- renderUI(
    map(quant(), ~ make_ui(data_ready()[[.x]], .x))
  )
  
  selected <- reactive({
    qual_var <- map(qual(), ~ filter_var(data_ready()[[.x]], input[[.x]]))
    quant_var <- map(quant(), ~ filter_var(data_ready()[[.x]], input[[.x]]))
    reduce(c(qual_var, quant_var), `&`)
  })

  color <- reactive(
    switch(input$color,
           "1" = "white",
           "2" = "gray"/"grey",
           "3" = "natural",
           "4" = "bw",
           "5" = "classic",
           "6" = "cobalt",
           "7" = "albatross",
           "8" = "beaver"
    ))
  
  razem <- eventReactive(input$confirm, {
    col_number <- which(name_col() == teryt())
    to_text <- c(rep("NULL", col_number-1), "character", rep("NULL", length(name_col())-col_number))
    to_replace <- read.csv("/home/pawel/Nauka_R/MwL/data/test_c.csv", 
                            colClasses = to_text)
    for(i in seq(to_replace[[teryt()]])) {
      to_replace[i,teryt()] <- 
        ifelse(str_count(to_replace[i,teryt()]) == 1, 
               str_c("0",to_replace[i,teryt()]), 
               to_replace[i,teryt()])
    }
    data_to_map <- data_ready()
    colnames(data_to_map)[which(colnames(data_to_map) == teryt())] <- "CC_1"
    data_to_map[["CC_1"]] <- to_replace[[teryt()]]
    data_to_map <- data_to_map[selected(), ]
    data_to_map <- left_join(data_to_map, woj, by ="CC_1")
    data_to_map <- st_as_sf(data_to_map)
  #  if(state() == FALSE) {
   #   data_to_map <-  tm_shape(razem()) + tm_basemap(server = NULL) + tmap_style(color()) +
    #    tm_polygons(map_variable(), id = "NAME_1") + tm_text(map_variable())
      
    #} else {
     # tm1 <-   tm_shape(razem()) + tm_basemap(server = NULL) + tmap_style(color()) +
      #  tm_polygons(map_variable(), id = "NAME_1") + tm_text(map_variable()) 
      #tm2 <- tm_shape(razem()) + tm_polygons(map_variable())
    #data_to_map <- tmap_arrange(tm1, tm2)
    #}
    data_to_map
  })
  
    
  
  
  #output$static <- renderTable(head(razem(), 12)) # do testu
  output$my_map <- renderTmap({
    tm_shape(razem()) + tm_basemap(server = NULL) + tmap_style(color()) +
      tm_polygons(map_variable(), id = "NAME_1") + tm_text(map_variable())
    })
  
  output$text <- renderText(names(state())) # do testu
  
  
  
  
}

shinyApp(ui, server)






