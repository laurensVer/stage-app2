library(shiny)
library(shinythemes)
library(ggplot2)
library(plotly)
library(dplyr)
library(shinycssloaders)

ui <- fluidPage(theme = shinytheme("united"), #themeSelector(),
                navbarPage("Shiny app cell analyse",
                           tabPanel("Input files",
                                    titlePanel("Bestanden Inladen"),
                                    sidebarLayout(
                                      sidebarPanel(
                                        fileInput("values_file", "Upload values bestand (.txt)"),
                                        fileInput("cells_file", "Upload cellen bestand (.txt)"),
                                        br(),
                                        uiOutput("values_info"),
                                        uiOutput("cells_info")
                                      ),
                                      mainPanel(
                                        tableOutput("values_table"),
                                        tableOutput("cells_table")
                                      )
                                    )
                           ),
                           tabPanel("ALL", # TabPanel voor ALL resultaten
                                    titlePanel("ALL Results"),
                                    mainPanel(
                                      withSpinner(tableOutput("all_results"))  # Met spinner
                                    )  
                           ),
                           tabPanel("Scale + check",
                                    titlePanel("Control"),
                                    sidebarLayout(
                                      sidebarPanel(
                                        numericInput("scale_input", "Enter scale:", 
                                                      value = 1, min = 0, step = 0.001),
                                        actionButton("submit_scale", "Submit")
                                      ),
                                      mainPanel(
                                        textOutput("dimension_info"),
                                        textOutput("areapx_info")
                                      )
                                    )
                           ),
                           tabPanel("graph1",
                                    titlePanel("Cell id"),
                                    fluidRow(
                                      column(width = 8,
                                             plotlyOutput("all_plot"),
                                             downloadButton("foo", "Download Graph 1")
                                      )
                                    )
                           ),
                           tabPanel("graph2",
                                    titlePanel("Cell id"),
                                    fluidRow(
                                      column(width = 8,
                                             plotlyOutput("stoma_plot"),
                                             downloadButton("stima", "Download Graph 2"),
                                             downloadButton("download_circ_data", "Download circulation data"),
                                             downloadButton("download_test", "Download test")
                                      )
                                    )
                           ),
                           tabPanel("Corrected graph", 
                                    titlePanel("Correction of ID"),
                                    sidebarLayout(
                                      sidebarPanel(
                                        textInput("correction_input", "IDs en nieuwe typen (bv. 1:Stom,2:PC):"),
                                        actionButton("apply_correction", "Correctie toepassen")
                                      ),
                                      mainPanel(
                                        plotlyOutput("corrected_plot"),
                                        downloadButton("download_correction", "Download here the correction plot"),
                                        downloadButton("download_correction_data","Download here the data of the correction")
                                      )
                                    )
                           ),
                           tabPanel("Area plot", 
                                    titlePanel("Area plot"),
                                    fluidRow(
                                      column(width = 8,
                                             plotlyOutput("areaplot")
                                             
                                      )
                                    )
                           )
                )
)


# Define server logic required to draw a histogram
# Server
server <- function(input, output, session) {
  
  # Lees waardenbestand in
  values <- reactive({
    req(input$values_file)
    read.delim(input$values_file$datapath)
  })
  
  # Lees cellenbestand in
  cells <- reactive({
    req(input$cells_file)
    Cells <- read.delim(input$cells_file$datapath, row.names = 1)
    Cells[, 2:ncol(Cells)] # Pas aan indien nodig
  })
  
  # Bereken coördinaten van de cellen
  cell_coordinates <- reactive({
    req(cells())
    Xcoord <- c()
    Ycoord <- c()
    ids <- c()
    
    for (i in 1:ncol(cells())) {
      for (j in 1:nrow(cells())) {
        if (cells()[j, i] > 0) {
          Xcoord <- c(Xcoord, i)
          Ycoord <- c(Ycoord, j)
          ids <- c(ids, cells()[j, i])
        }
      }
    }
    
    maxY <- max(Ycoord)
    InvY <- maxY - Ycoord
    All <- data.frame(Xcoord, InvY, ids)
    # Tijdelijke print-statements
    print(dim(All))
    print(head(All))
    
    All
  })
  
  # Toon waardentabel
  output$values_table <- renderTable({
    req(values())
    values()
  })
  
  # Toon cellentabel
  #output$cells_table <- renderTable({
  # req(cells())
  #cells()
  #})
  
  # Print dimensies van geüploade bestanden
  output$values_info <- renderUI({
    if (!is.null(input$values_file)) {
      values_dim <- dim(values())
      HTML(paste("Dimensions of values file: ", values_dim[1], " rows and ", values_dim[2], " columns"))
    }
  })
  
  output$cells_info <- renderUI({
    if (!is.null(input$cells_file)) {
      cells_dim <- dim(cells())
      HTML(paste("Dimensions of position file: ", cells_dim[1], " rows and ", cells_dim[2], " columns"))
    }
  })
  
  # Toon tabel met de resultaten, de ALL results
  output$all_results <- renderTable({
    req(cell_coordinates())
    if (is.null(cell_coordinates())) {
      return("Resultaten worden geladen...")
    } else {
      head(cell_coordinates(), 250)  # Toon alleen de eerste 20 rijen
    }
  })
  # Plot voor alle resultaten
  output$all_plot <- renderPlotly({
    req(cell_coordinates())
    plot_data <- ggplot(cell_coordinates(), aes(x = Xcoord, y = InvY, colour = ids, text = ids)) +
      geom_point(size = 0.1) + theme_minimal() 
    plotly_build(plot_data)
  })
  output$scale_value <- renderText({
    paste("scale value:", input$scale_input)
  })
  
  # berekening areapx
  areapx <- reactive({
    req(values())
    values_df <- values()
    colnames(values_df) <- c("cellid", "area", "CoMX", "CoMY", "Peri")
    areapx <- values_df$area * input$scale_input^2
    round(areapx, digits = 0)
  })
  # Reactieve expressie om de andere bewerkingen uit te voeren
  circ <- reactive({
    req(values())
    values_df <- values()  # Ophalen van de waarden
    colnames(values_df) <- c("cellid", "area", "CoMX", "CoMY", "Peri")  # Hernoemen van kolommen
    areapx_val <- areapx()  # Ophalen van de berekende areapx
    values_df$areapx <- areapx_val  # Toevoegen van de berekende areapx aan de dataset
    values_df$Circ <- (4 * 3.14 * values_df$area / (values_df$Peri^2))  # Berekenen van Circ
    values_df$Type <- ifelse(values_df$Circ > 0.83 & values_df$Circ < 1.3 & values_df$area > 40, "Stom", "PC")  # Bepalen van het type
    values_df
    # Toevoegen van kolommen Type en sorteren van de dataset
    type_list <- rep(values_df$Type, areapx())
    area_list <- rep(values_df$area,areapx())
    sorted_All <- cell_coordinates()[order(cell_coordinates()$ids),]
    sorted_All$Type <- type_list
    sorted_All$area <- area_list
    list(values_df = values_df, sorted_All = sorted_All)  # Return de aangepaste dataset en de gesorteerde dataset
  })
  # Output to display pixel info
  output$dimension_info <- renderText({
    req(cell_coordinates())
    paste("Dimensions of All: ", dim(cell_coordinates())[1], " rows ", dim(cell_coordinates())[2], " columns")
  })
  
  output$areapx_info <- renderText({
    req(cell_coordinates())
    paste("Sum of areapx: ", sum(areapx()))
  })
  # submit button
  observeEvent(input$submit_scale, {
    print(input$scale_input)
  })
  
  # Plot for graph2
  output$stoma_plot <- renderPlotly({
    circ_result <- circ() # Access circ reactive expression result
    plot_data <- ggplot(circ_result$sorted_All, aes(x = Xcoord, y = InvY, colour = Type, text = ids)) +
      geom_point(size = 0.1) +
      scale_color_manual(values = c("Stom" = "chocolate3", "PC" = "aquamarine4")) +
      theme(panel.background = element_rect(fill = "black")) +
      theme(panel.grid = element_blank(), legend.position = "none")
    plotly_build(plot_data)
  })
  
  # correctie op graph2
  # Declare corrected_data_df outside the observeEvent block, initialized as NULL
  corrected_data <- reactive({
    req(input$apply_correction, input$correction_input)  # Controleer of er invoer is
    
    # Split de invoer op komma's om een lijst te maken van ID:Type paren
    correction_list <- strsplit(input$correction_input, ",")[[1]]
    
    # Haal de huidige data op
    circ_data <- circ()
    
    # Haal de huidige gecorrigeerde data op
    current_corrected_data <- circ_data$sorted_All
    
    # Maak een kopie van de gecorrigeerde data om mee te werken
    corrected_data_df <- current_corrected_data
    
    # Loop door elk paar ID:Type en pas de correctie toe
    for (pair in correction_list) {
      id_type <- strsplit(pair, ":")[[1]]
      id <- as.numeric(id_type[1])
      type <- id_type[2]
      corrected_data_df$Type[corrected_data_df$ids == id] <- type
    }
    
    # Return de gecorrigeerde data
    corrected_data_df
  })
  # Update de plot met de bijgewerkte data
    output$corrected_plot <- renderPlotly({
      corr_result <- corrected_data()
        plot_data <- ggplot(corr_result, aes(x = Xcoord, y = InvY, colour = Type)) +
        geom_point(size = 0.3) +
        scale_color_manual(values = c("Stom" = "chocolate3", "PC" = "aquamarine4")) +
        theme(panel.background = element_rect(fill = "black")) +
        theme(panel.grid = element_blank(), legend.position = "none")
      
      p <- ggplotly(plot_data)
      p <- style(p, hoverinfo = "none")
      p
    })
  # last plot
    output$areaplot <- renderPlotly({
      circ_result <- circ()
      plot_data <- ggplot(circ_result$sorted_All, aes(x = Xcoord, y = InvY, colour = area, alpha = Type)) +
        geom_point(size = 0.3) +
        scale_color_gradient2(low = "orangered3", midpoint = 500, high = "aquamarine4", limits = c(0, 2200)) +
        scale_alpha_manual(values = c("Stom" = 0, "PC" = 1)) +
        theme(panel.background = element_rect(fill = "black")) +
        theme(panel.grid = element_blank(), legend.position = "left")
      plotly_build(plot_data)
    })
      
  
  ##############################################################################
  ### Download section ###
  ##############################################################################
  # download the circulation with type data
  output$download_circ_data <- downloadHandler(
    filename = function() {
      paste("circulation_data", ".csv", sep = "")
    },
    content = function(file) {
      circ_data <- circ()$values_df
      write.csv(circ_data, file, row.names = FALSE)
    }
  )
  #############################################################################
  # download the corrected graph data
  output$download_correction_data <- downloadHandler(
      filename = function() {
        paste("correction_data", ".csv", sep = "")
      },
      content = function(file) {
        corr_data <- corrected_data()
        write.csv(corr_data, file, row.names = FALSE)
      }
    )  
    
  ############################################################################## 
  # Download function graph1
  plotInput = function(){
    ggplot(cell_coordinates(), aes(x = Xcoord, y = InvY, colour = ids)) +
      geom_point(size = 0.1) +
      theme_minimal()
  }
  output$foo = downloadHandler(
    filename = 'graph1.png',
    content = function(file){
      device <- function(...,width,height){
        grDevices::png(...,width = width, height = height, res = 300, units = "in")
      }
      ggsave(file, plot = plotInput(), device = device)
    })
  ##############################################################################
  # Download function graph2
  plotInput = function(){
    circ_result <- circ()
    ggplot(circ_result$sorted_All, aes(x = Xcoord, y = InvY, colour = Type)) +
      geom_point(size = 0.3) +
      scale_color_manual(values = c("Stom" = "chocolate3", "PC" = "aquamarine4")) +
      theme(panel.background = element_rect(fill = "black")) +
      theme(panel.grid = element_blank(), legend.position = "none")
  }
  output$stima = downloadHandler(
    filename = 'graph2.png',
    content = function(file){
      device <- function(...,width,height){
        grDevices::png(...,width = width, height = height, res = 300, units = "in")
      }
      ggsave(file, plot = plotInput(), device = device)
    })
  ##############################################################################
  # Download corrected graph
  #plotInput = function(){
    #corrected_data <- corrected_data_df()
    #ggplot(corrected_data, aes(x = Xcoord, y = InvY, colour = Type)) +
      #geom_point(size = 0.3) +
      #scale_color_manual(values = c("Stom" = "chocolate3", "PC" = "aquamarine4")) +
      #theme(panel.background = element_rect(fill = "black")) +
      #theme(panel.grid = element_blank(), legend.position = "none")
  #}
  #output$download_correction = downloadHandler(
    #filename = 'correction plot.png',
    #content = function(file){
     # device <- function(...,width,height){
      #  grDevices::png(...,width = width, height = height, res = 300, units = "in")
      #}
      #ggsave(file, plot = plotInput(), device = device)
    #})
}
# Run the application 
shinyApp(ui = ui, server = server)