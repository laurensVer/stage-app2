library(shinydashboard)
library(shiny)
library(shinythemes)
library(ggplot2)
library(plotly)
library(dplyr)
library(shinycssloaders)
library(orca)
library(scales)
library(DT)
library(grid)

ui <- dashboardPage(
  dashboardHeader(title = "LeafXtrack",
                  tags$li(
                    class = "dropdown",
                    tags$img(
                      src = "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcS7EkIMJPaaQDDwRMbnh5Nj_s7YMCen5AVIVuICTARbww&s",
                      height = "40px",
                      style = "margin-top: 5px; margin-right: 10px;"
                    )
                  )
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Input files", tabName = "input_files", icon = icon("upload")),
      menuItem("Epidermal cell analysis", tabName = "epidermal", icon = icon("microscope"),
               menuSubItem("Check dataframe", tabName = "all_results"),
               menuSubItem("Check cell separation", tabName = "graph1"),
               menuSubItem("Cell type graph", tabName = "graph2"),
               menuSubItem("Corrected graph", tabName = "corrected_data"),
               menuSubItem("Feature plots", tabName = "area_plot")
      ),
      menuItem("Cell tracking", tabName = "cell tracking", icon = icon("clock"),
               menuSubItem("Upload Files", tabName = "upload_files"),
               menuSubItem("Calculations", tabName = "calc"),
               menuSubItem("Plots stomata numbers", tabName = "plot"),
               menuSubItem("Indicata anchor points", tabName = "cell_number"),
               menuSubItem("Verfiy anchor points", tabName = "rotated"),
               menuSubItem("Plots stomata tracking", tabName = "test"),
               menuSubItem("Plots pavement cell tracking", tabName = "PC")
      ),
      menuItem("Return to Landing Page", tabName = "landing", icon = icon("arrow-left"), 
               style = "color: #333333; background-color: #FFFFFF; border-color: #DDDDDD;",  # Stijl voor de knop
               actionButton("returnToLanding", "Return to Landing Page"))  # Knop om terug te gaan naar landingspagina
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML('
        body {
          background-color: #ffffff !important;
        }
        .wrapper {
          background-color: #ffffff !important;
        }
        .main-header .logo {
          background-color:#222d32 !important;
          color: #ffffff !important;
        }
        .main-header .navbar {
          background-color: #222d32 !important;
        }
        .main-sidebar, .left-side {
          background-color: #222d32 !important;
        }
        .content-wrapper, .right-side {
          background-color: #ffffff !important;
        }
        .main-footer {
          background-color: #ffffff !important;
          border-top: 1px solid #d2d6de;
          color: #444;
        }
        .skin-blue .sidebar-menu > li.active > a {
          background-color: #222d32;
          color: #ffffff;
        }
        .skin-blue .sidebar-menu > li:hover > a {
          background-color: #222d32;
          color: #ffffff;
        }
        .sidebar-menu {
          background-color: #222d32;
          color: #ffffff;
        }
        .nav-tabs-custom > .nav-tabs > li.active {
          border-top-color: #ffffff;
        }
      '))
    ),
    tabItems(
      tabItem(tabName = "dashboard",
              tags$style(
                HTML(
                  "
            .dashboard-background {
              position: absolute;
              top: 0;
              left: 0;
              width: 100%;
              height: 100%;
              background-image: url('Achtergrond.jpg');
              background-size: cover;
              background-position: center;
              background-repeat: no-repeat;
            }
            .dashboard-overlay {
              position: absolute;
              top: 0;
              left: 0;
              position: center;
              width: 100%;
              height: 100%;
              background-color: rgba(0, 0, 0, 0.5); /* Semi-transparent black overlay */
            }
            .dashboard-content {
              position: relative;
              z-index: 1; /* Ensure content appears above the background */
              padding: 20px;
              color: #ffffff; /* Text color */
              top: 50px; /* Verplaats de tekst 50 pixels naar beneden */
              left: 250px; /* Verplaats de tekst 50 pixels naar rechts
            }
            "
                )
              ),
              div(
                class = "dashboard-background",
                div(class = "dashboard-overlay"),  # Semi-transparent overlay
                div(
                  class = "dashboard-content",
                  fluidRow(
                    column(
                      width = 8,
                      h2("Background Information"),
                      p("Here is some general background information about the topic of your app. You can place text here to inform users about the purpose and context of the app."),
                      hr(),  # Horizontal line for visual separation
                      h3("Key Features"),
                      tags$ul(
                        tags$li("First feature"),
                        tags$li("Second feature"),
                        tags$li("Third feature")
                      ),
                      hr(),  # Horizontal line
                      h3("More Information"),
                      p("For more detailed information, you can add links to external sources, documentation, or other relevant information.")
                    )
                  )
                )
              )
      ),
      tabItem(tabName = "input_files",
              h1("Upload here your files generated by fiji macro"),
              fluidRow(
                sidebarLayout(
                  sidebarPanel(
                    fileInput("values_file", "Upload values.txt file"),
                    fileInput("cells_file", "Upload position.txt file"),
                    textInput("filename_prefix", "File name:"),
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
              #div(
              #  class = "sun",
              #  img(src = "https://media3.giphy.com/media/v1.Y2lkPTc5MGI3NjExdXl5cG0wd2RnYWR1bzh2cjVicnRjdHE1MzgwZ3piazVjYmZjOXU3YSZlcD12MV9pbnRlcm5hbF9naWZfYnlfaWQmY3Q9Zw/l41Yk0Ovlxko5oYco/giphy.webp"),
              #  style = "width: 70px; height: 100px;"
              #)
      ),
      tabItem(tabName = "all_results",
              h1("A overview of the coordinates"),
              mainPanel(
                withSpinner(tableOutput("all_results"))  # Met spinner
              )  
      ),
      tabItem(tabName = "graph1",
              fluidRow(
                column(width = 8,
                       plotOutput("all_plot", width = "70%", height = "450px"),
                       downloadButton("foo", "Download Graph 1")
                )
              )
      ),
      tabItem(tabName = "graph2",
              h2("Visualization of the pavement cells and stomata"),
              fluidRow(
                column(width = 8,
                       plotlyOutput("stoma_plot", width = "70%", height = "450px"),
                       downloadButton("stima", "Download Graph")
                )
              )
      ),
      tabItem(tabName = "corrected_data",
              h1("Corrected cells in a new plot"),
              p("Even if no id has to be changed, it is important to overwrite one id, otherwise the area plot will not be plotted."),
              sidebarLayout(
                sidebarPanel(
                  textInput("correction_input", "IDs and new types (bv. 1:Stom,2:PC):"),
                  actionButton("apply_correction", "Apply correction ")
                ),
                mainPanel(
                  plotlyOutput("corrected_plot", width = "60%", height = "450px"),
                  downloadButton("corrected", "Download here the corrected graph"),
                  downloadButton("download_correction_data","Download updated Values file"),
                  downloadButton("download_correction_circulation_data", "Download updated Position matrix")
                )
              )
      ),
      tabItem(tabName = "area_plot",
              h1("Feature plot"),
              p("Please adjust the maximum limit to ensure all cells are within the range (grey cells are out of range)."),
              p("If you don't see a plot, see corrected graph."),
              fluidRow(
                column(width = 8,
                       plotOutput("areaplot", width = "55%", height = "450px"),
                       downloadButton("areaPlot", "Download here the area plot")
                ),
                column(width = 6,
                       sliderInput("slider_input", label = "Maximum limit", min = 0, max = 50000, value = 5000, step = 1000, width = "80%")
                       
                )
                
              )
      ),
      tabItem(tabName = "upload_files",
              h1("Upload corrected Values files for cell tracking"),
              p("If you uploaded the wrong file, you can replace it by uploading the correct file via browse. Then you have to press submit button so the data for the next steps is correct."),
              sidebarLayout(
                sidebarPanel(
                  fileInput("values_D1_file", "Upload the D1 file"),
                  fileInput("values_D2_file", "Upload the D2 file"),
                  fileInput("values_D3_file", "Upload the D3 file"),
                  fileInput("values_D4_file", "Upload the D4 file"),
                  fileInput("values_D5_file", "Upload the D5 file"),
                  div(
                    style = "margin-top: 20px;",
                    actionButton(
                      "refresh_btn", 
                      label = "Submit", 
                      icon = icon("check"), 
                      style = "color: #fff; background-color: #28a745; border-color: #28a745;",
                      title = "Submit"
                    )
                  )
                ),
                mainPanel(
                  tabsetPanel(
                    tabPanel("D1 Data", tableOutput("D1_table")),
                    tabPanel("D2 Data", tableOutput("D2_table")),
                    tabPanel("D3 Data", tableOutput("D3_table")),
                    tabPanel("D4 Data", tableOutput("D4_table")),
                    tabPanel("D5 Data", tableOutput("D5_table"))
                  )
                )
              )
      ),
      tabItem(tabName = "calc",
              h1("Calculation of stomatal index and stomatal density"),
              mainPanel(
                dataTableOutput("cell_param_table"),
                downloadButton("download_data", "Download Data")
              )
              
      ),
      tabItem(tabName = "plot",
              h1("Combined Plot"),
              fluidRow(
                column(6, plotOutput("plot1")),
                column(6, plotOutput("plot2"))
              ),
              fluidRow(
                column(6, plotOutput("plot3")),
                column(6, plotOutput("plot4"))
              )
      ),
      tabItem(tabName = "cell_number",
              h1("Indicata anchor points"),
              p("Upload here the circulation data of the corrected graph and provide cell number of a top, right and base anchor point of each drawing."),               sidebarPanel(
                fileInput("upload_data1", "Upload data 1"),
                fileInput("upload_data2", "Upload data 2"),
                fileInput("upload_data3", "Upload data 3"),
                fileInput("upload_data4", "Upload data 4"),
                fileInput("upload_data5", "Upload data 5"),
                div(
                  style = "margin-top: 20px;",
                  actionButton(
                    "submit", 
                    label = "Submit", 
                    icon = icon("check"), 
                    style = "color: #fff; background-color: #28a745; border-color: #28a745;",
                    title = "Submit"
                  )
                ),
                downloadButton("download_sorted_All_D1", "Download sorted data"),
                width = 3
              ),
              mainPanel(
                fluidRow(
                  column(6, 
                         plotOutput("plots1"),
                         uiOutput("plot1_inputs")
                  ),
                  column(6, 
                         plotOutput("plots2"),
                         uiOutput("plot2_inputs")
                  )
                ),
                fluidRow(
                  column(6, 
                         plotOutput("plots3"),
                         uiOutput("plot3_inputs")
                  ),
                  column(6, 
                         plotOutput("plots4"),
                         uiOutput("plot4_inputs")
                  )
                ),
                fluidRow(
                  column(6, 
                         plotOutput("plots5"),
                         uiOutput("plot5_inputs")
                  )
                )
              )
      ),
      tabItem(tabName = "rotated",
              h1("Verify anchor points (yellow)"),
              p("If necessary, adjust the cell numbers on the previous page."),
              mainPanel(
                div(style = "overflow-x: auto; white-space: nowrap; width: 150%;",
                    div(style = "float: left; width: 33%;", plotOutput("D1_plot")),
                    div(style = "float: left; width: 33%;", plotOutput("D2_plot")),
                    div(style = "float: left; width: 33%;", plotOutput("D3_plot")),
                    div(style = "float: left; width: 33%;", plotOutput("D4_plot")),
                    div(style = "float: left; width: 33%;", plotOutput("D5_plot"))
                ),
              )
      ),
      tabItem(tabName = "test",
              h1("Stomata tracking"),
              p("The upper row of graphs indicate new stomata (olive green) at each time point."),
              p("The lower row indicates corresponding stomata in the same color, new stomata appear in other colors."),
              mainPanel(
                div(style = "overflow-x: scroll; white-space: nowrap; width: 150%; display: grid; grid-template-columns: repeat(4, 1fr); grid-gap: 20px;",
                    div(style = "float: left; width: 120%;", plotOutput("BAM")),
                    div(style = "float: left; width: 120%;", plotOutput("BAM2")),
                    div(style = "float: left; width: 120%;", plotOutput("BAM3")),
                    div(style = "float: left; width: 120%;", plotOutput("BAM4")),
                ),
                div(style = "overflow-x: scroll; white-space: nowrap; width: 150%; display: grid; grid-template-columns: repeat(4, 1fr); grid-gap: 20px;",
                    div(style = "float: left; width: 120%;", plotOutput("BOEM1")),
                    div(style = "float: left; width: 120%;", plotOutput("BOEM2")),
                    div(style = "float: left; width: 120%;", plotOutput("BOEM3")),
                    div(style = "float: left; width: 120%;", plotOutput("BOEM4"))
                    
                  )
              )
      ),
      tabItem(tabName = "PC",
              mainPanel(
                div(
                  style = "overflow-x: scroll; white-space: nowrap; width: 150%; display: grid; grid-template-columns: repeat(4, 1fr); grid-gap: 20px;",
                    div(style = "width: 120%;", plotOutput("PC1")),
                    div(style = "width: 120%;", plotOutput("PC2")),
                    div(style = "width: 120%;", plotOutput("PC3")),
                    div(style = "width: 120%;", plotOutput("PC4")),
                ),
                div(
                  style = "overflow-x: scroll; white-space: nowrap; width: 150%; margin-top: 10px; display: grid; grid-template-columns: repeat(3, 1fr); grid-gap: 20px;",
                    div(style = "width: 90%; margin-top: 20px; margin-left: 65px;", plotOutput("PC1.1")),
                    div(style = "width: 90%; margin-top: 20px; margin-left: 65px;", plotOutput("PC1.2")),
                    div(style = "width: 90%; margin-top: 20px; margin-left: 65px;", plotOutput("PC1.3")),
                  )
              )
      )
    )
  )
)

server <- function(input, output, session) {
  set.seed(122)
  histdata <- rnorm(500)
  
  # Reactive value to track modal dialog status
  showModalDialog <- reactiveVal(FALSE)
  
  # Function to show modal dialog
  showModalFunction <- function() {
    showModal(modalDialog(
      title = "From leaf image to data",
      tags$style(HTML(".modal-header { background-color: #8ab5e0; } .modal-title { color: white; }")), # Donkerblauwe achtergrondkleur en witte tekst
      h1('From images to segmented cells'),
      p(HTML('Are your epidermal cells not segmented yet? Perform cell segmentation using
        LeafNet: <a href="https://leafnet.whu.edu.cn/">link</a>')),
      h1('From segmented cells to measurements'),
      p('Download here (download button) the LeafXtrack Fiji macro for cellular measurments'),
      footer = tagList(
        actionButton("dismiss", "Next", style = "color: white;", class = "btn-primary")
      )
    ))
  }
  # Observer for dismissing the modal dialog
  observeEvent(input$dismiss, {
    removeModal()  # Close the modal dialog
  })
  
  # Show modal dialog when the app starts
  observe({
    if (!showModalDialog()) {
      showModalFunction()
      showModalDialog(TRUE)
    }
  })
  # Observer for the button to return to the landing page
  observeEvent(input$returnToLanding, {
    showModalDialog(FALSE)  # Set modal dialog status to FALSE
    showModalFunction()  # Show modal dialog
  })
  
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
      return("Resuls are loading...")
    } else {
      head(cell_coordinates(), 250)  # Toon alleen de eerste 20 rijen
    }
  })
  # Plot voor alle resultaten
  cols<-rainbow(12)
  output$all_plot <- renderPlot({
    coords <- cell_coordinates()
    ggplot(coords, aes(x = Xcoord, y = InvY, color = factor(ids))) +
      geom_point(size = 0.6) +
      scale_color_discrete(guide = FALSE) +  # Verwijder de legende
      theme_minimal() +  # Verwijder titel en assen
      theme(axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            plot.title = element_blank(),
            plot.caption = element_blank())
    })
  
  # berekening areapx
  areapx <- reactive({
    req(values())
    req(cells())
    values_df <- values()
    colnames(values_df) <- c("cellid", "area", "CoMX", "CoMY", "Peri")
    scale <- (sum(cells()[,]>0)/round(sum(values_df$area))) ^(1/2)
    cat("Scale:", scale, "\n")  # Schrijf de waarde van scale naar de console
    areapx <- values_df$area * scale ^2
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
  
  # Plot for graph2
  output$stoma_plot <- renderPlotly({
    circ_result <- circ() # Access circ reactive expression result
    plot_data <- ggplot(circ_result$sorted_All, aes(x = Xcoord, y = InvY, colour = Type, text = ids)) +
      geom_point(size = 0.4) +
      scale_color_manual(values = c("Stom" = "chocolate3", "PC" = "aquamarine4")) +
      theme(panel.background = element_rect(fill = "black")) +
      theme(panel.grid = element_blank(), legend.position = "none") +
      theme_void()
    plotly_build(plot_data)
  })
  
  # correctie op graph2
  corrected_data <- reactive({
    req(input$apply_correction, input$correction_input)  # Controleer of er invoer is
    
    # Split de invoer op komma's om een lijst te maken van ID:Type paren
    correction_list <- strsplit(input$correction_input, ",")[[1]]
    
    # Haal de huidige data op
    req(circ())
    circ_data <- circ()
    
    # Haal de huidige gecorrigeerde data op
    current_corrected_data <- circ_data$sorted_All
    #current_corrected_data2 <- circ_data$values_df
    # Maak een kopie van de gecorrigeerde data om mee te werken
    corrected_data_df <- current_corrected_data
    #corrected_data_df2 <- current_corrected_data2
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
  
  # the corrected data 
  updated_data <- reactive({
    req(input$apply_correction, input$correction_input)  # Controleer of er invoer is
    
    # Split de invoer op komma's om een lijst te maken van ID:Type paren
    correction_list <- strsplit(input$correction_input, ",")[[1]]
    print(correction_list)
    # Haal de huidige data op
    circ_data <- circ()
    
    # Haal de huidige gecorrigeerde data op
    current_corrected_data2 <- circ_data$values_df
    
    # Maak een kopie van de gecorrigeerde data om mee te werken
    corrected_data_df2 <- current_corrected_data2
    
    # Loop door elk paar ID:Type en pas de correctie toe
    for (pair in correction_list) {
      id_type <- strsplit(pair, ":")[[1]]
      id <- as.numeric(id_type[1])
      type <- id_type[2]
      corrected_data_df2$Type[corrected_data_df2$cellid == id] <- type
    }
    
    # Return de gecorrigeerde data
    corrected_data_df2
    
  })
  # The corrected circulation data
  updated_circulation_data <- reactive({
    req(input$apply_correction, input$correction_input, circ())  # Check if there is input
    
    # Split the input by commas to create a list of ID:Type pairs
    correction_list <- strsplit(input$correction_input, ",")[[1]]
    
    # Make a copy of the corrected data to work with
    corrected_data_df <- circ()$sorted_All
    
    # Loop through each ID:Type pair and apply the correction
    for (pair in correction_list) {
      id_type <- strsplit(pair, ":")[[1]]
      id <- as.numeric(id_type[1])
      type <- id_type[2]
      corrected_data_df$Type[corrected_data_df$ids == id] <- type
    }
    
    # Return the corrected data
    corrected_data_df
  })
  
  # Update de plot met de bijgewerkte data
  output$corrected_plot <- renderPlotly({
    corr_result <- corrected_data()
    plot_data <- ggplot(corr_result, aes(x = Xcoord, y = InvY, colour = Type)) +
      geom_point(size = 0.3) +
      scale_color_manual(values = c("Stom" = "chocolate3", "PC" = "aquamarine4")) +
      theme(panel.background = element_rect(fill = "black")) +
      theme(panel.grid = element_blank(), legend.position = "none") +
      theme_void()
    
    p <- ggplotly(plot_data)
    p <- style(p, hoverinfo = "none")
    p
  })
  # last plot
  output$areaplot <- renderPlot({
    circ_result <- circ()
    
    # Haal de waarde op van de sliderinput
    max_limit <- input$slider_input
    
    # Pas de limieten van de kleurschaal aan op basis van de sliderinput
    color_limits <- c(0, max_limit)
    
    # Genereer de plot met ggplot
    area_result <- corrected_data()
    plot_data <- ggplot(area_result, aes(x = Xcoord, y = InvY, colour = area, alpha = Type)) +
      geom_point(size = 0.5) +
      scale_color_gradient2(low = "orangered3", midpoint = 500, high = "aquamarine4", limits = color_limits) +
      scale_alpha_manual(values = c("Stom" = 0, "PC" = 1)) +
      theme(panel.background = element_rect(fill = "black")) +
      theme(panel.grid = element_blank(), legend.position = "left") +
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title.x=element_blank(), axis.title.y=element_blank())
    print(plot_data)
  })
  ##############################################################################
  ### Download section ###
  ##############################################################################
  # download the corrected graph data
  output$download_correction_data <- downloadHandler(
    filename = function() {
      paste0(input$filename_prefix, "_correction_data.csv")  # Bestandsnaam instellen
    },
    content = function(file) {
      corr_data <- updated_data()  # Ophalen van de gecorrigeerde gegevens
      write.csv(corr_data, file, row.names = FALSE)  # CSV-bestand schrijven
    }
  )
  #############################################################################
  # download the corrected circulation data
  output$download_correction_circulation_data <- downloadHandler(
    filename = function() {
      paste0(input$filename_prefix, "_circulation_data.csv")  # Bestandsnaam instellen
    },
    content = function(file) {
      corr_data <- updated_circulation_data()  # Ophalen van de gecorrigeerde gegevens
      write.csv(corr_data, file, row.names = FALSE)  # CSV-bestand schrijven
    }
  )
  
  ############################################################################## 
  # Download function graph1
  plotInput1 = function(){
    ggplot(cell_coordinates(), aes(x = Xcoord, y = InvY, colour = ids)) +
      geom_point(size = 0.4) +
      theme_minimal()
  }
  output$foo = downloadHandler(
    filename = function() {
      paste(input$filename_prefix, "_graph1.png", sep = "")
    },
    content = function(file){
      device <- function(...,width,height){
        grDevices::png(...,width = width, height = height, res = 300, units = "in")
      }
      ggsave(file, plot = plotInput1(), device = device)
    })
  ##############################################################################
  # Download function graph2
  plotInput2 = function(){
    circ_result <- circ()
    ggplot(circ_result$sorted_All, aes(x = Xcoord, y = InvY, colour = Type)) +
      geom_point(size = 0.3) +
      scale_color_manual(values = c("Stom" = "chocolate3", "PC" = "aquamarine4")) +
      theme(panel.background = element_rect(fill = "black")) +
      theme(panel.grid = element_blank(), legend.position = "none")
  }
  output$stima = downloadHandler(
    filename = function() {
      paste(input$filename_prefix, "_graph2.png", sep = "")
    },
    content = function(file){
      device <- function(...,width,height){
        grDevices::png(...,width = width, height = height, res = 300, units = "in")
      }
      ggsave(file, plot = plotInput2(), device = device)
    })
  ##############################################################################
  # Download function corrected graph
  plotInputCorrected <- function() {
    corr_result <- corrected_data()  # Ophalen van de gecorrigeerde data
    plot_data <- ggplot(corr_result, aes(x = Xcoord, y = InvY, colour = Type)) +
      geom_point(size = 0.3) +
      scale_color_manual(values = c("Stom" = "chocolate3", "PC" = "aquamarine4")) +
      theme(panel.background = element_rect(fill = "black")) +
      theme(panel.grid = element_blank(), legend.position = "none")
    return(plot_data)
  }
  output$corrected = downloadHandler(
    filename = function() {
      paste(input$filename_prefix, "_corrected_graph.png", sep = "")
    },
    content = function(file){
      device <- function(...,width,height){
        grDevices::png(...,width = width, height = height, res = 300, units = "in")
      }
      ggsave(file, plot = plotInputCorrected(), device = device)
    })
  
  # Download functie voor de area plot
  plotInputArea <- function(color_limits) {  # Voeg color_limits toe als een argument
    area <- corrected_data()
    plot_data <- ggplot(area, aes(x = Xcoord, y = InvY, colour = area, alpha = Type)) +
      geom_point(size = 0.5) +
      scale_color_gradient2(low = "orangered3", midpoint = 500, high = "aquamarine4", limits = c(lowest_limit, max(color_limits))) +
      scale_alpha_manual(values = c("Stom" = 0, "PC" = 1)) +
      theme(panel.background = element_rect(fill = "black")) +
      theme(panel.grid = element_blank(), legend.position = "left")
    return(plot_data)  # Geef de plot terug
  }
  
  output$areaPlot = downloadHandler(
    filename = function() {
      paste(input$filename_prefix, "_area_plot.png", sep = "")
    },
    content = function(file){
      circ_result <- circ()
      
      # Haal de waarde op van de sliderinput
      max_limit <- input$slider_input
      
      # Pas de limieten van de kleurschaal aan op basis van de sliderinput
      color_limits <- c(0, max_limit)
      
      # Genereer de plot met behulp van plotInputArea-functie en geef color_limits door
      plot_data <- plotInputArea(color_limits)
      
      # Sla de plot op als afbeeldingsbestand
      ggsave(file, plot_data)
    })
  ####################################################
  ## cell tracking ##
  ####################################################
  ## input$file is a data frame and contains the details around the name, size and temp location of the files uploaded
  observe({
    if (!showModalDialog()) {
      showModalFunction()
      showModalDialog(TRUE)
    }
  })
  
  # Observer for the button to return to the landing page
  observeEvent(input$returnToLanding, {
    showModalDialog(FALSE)  # Set modal dialog status to FALSE
    showModalFunction()  # Show modal dialog
  })
  # Lees waardenbestanden in voor elk submenu-item
    values_D1 <- reactive({
      req(input$values_D1_file)
      read.csv(input$values_D1_file$datapath)
    })
    values_D2 <- reactive({
      req(input$values_D2_file)
      read.csv(input$values_D2_file$datapath)
    })
    values_D3 <- reactive({
      req(input$values_D3_file)
      read.csv(input$values_D3_file$datapath)
    })
    values_D4 <- reactive({
      req(input$values_D4_file)
      read.csv(input$values_D4_file$datapath)
    })
    values_D5 <- reactive({
      req(input$values_D5_file)
      read.csv(input$values_D5_file$datapath)
    })
    # Toon waardentabellen voor elk submenu-item
    output$D1_table <- renderTable({
      req(values_D1())
      values_D1()
    })
    output$D2_table <- renderTable({
      req(values_D2())
      values_D2()
    })
    output$D3_table <- renderTable({
      req(values_D3())
      values_D3()
    })
    output$D4_table <- renderTable({
      req(values_D4())
      values_D4()
    })
    output$D5_table <- renderTable({
      req(values_D5())
      values_D5()
    })
    observeEvent(input$refresh_btn, {
    ###########################################################
    ## calculations ##
    ###########################################################
    # Initialize the calculations list
    calculations_list <- reactiveVal(list())
    
    # Perform calculations when a file is uploaded
    observeEvent(input$values_D1_file, {
      if (!is.null(input$values_D1_file)) {
        values_D1 <- read.csv(input$values_D1_file$datapath)
        nrPC_D1 <- length(which(values_D1$Type == "PC" & values_D1$area > 25))
        nrSt_D1 <- length(which(values_D1$Type == "Stom"))
        SI_D1 <- nrSt_D1 / (nrPC_D1 + nrSt_D1)
        SD_D1 <- nrSt_D1 / sum(values_D1$area)
        calculations_list( c(calculations_list(), list(D1 = c(nrPC_D1 + nrSt_D1, nrPC_D1, nrSt_D1, SI_D1, SD_D1))) )
      }
    })
    
    # Perform calculations for D2 similarly
    observeEvent(input$values_D2_file, {
      if (!is.null(input$values_D2_file)) {
        values_D2 <- read.csv(input$values_D2_file$datapath)
        nrPC_D2 <- length(which(values_D2$Type == "PC" & values_D2$area > 25))
        nrSt_D2 <- length(which(values_D2$Type == "Stom"))
        SI_D2 <- nrSt_D2 / (nrPC_D2 + nrSt_D2)
        SD_D2 <- nrSt_D2 / sum(values_D2$area)
        calculations_list( c(calculations_list(), list(D2 = c(nrPC_D2 + nrSt_D2, nrPC_D2, nrSt_D2, SI_D2, SD_D2))) )
      }
    })
    
    # Perform calculations for D3 similarly
    observeEvent(input$values_D3_file, {
      if (!is.null(input$values_D3_file)) {
        values_D3 <- read.csv(input$values_D3_file$datapath)
        nrPC_D3 <- length(which(values_D3$Type == "PC" & values_D3$area > 25))
        nrSt_D3 <- length(which(values_D3$Type == "Stom"))
        SI_D3 <- nrSt_D3 / (nrPC_D3 + nrSt_D3)
        SD_D3 <- nrSt_D3 / sum(values_D3$area)
        calculations_list( c(calculations_list(), list(D3 = c(nrPC_D3 + nrSt_D3, nrPC_D3, nrSt_D3, SI_D3, SD_D3))) )
      }
    })
    
    # Perform calculations for D4 similarly
    observeEvent(input$values_D4_file, {
      if (!is.null(input$values_D4_file)) {
        values_D4 <- read.csv(input$values_D4_file$datapath)
        nrPC_D4 <- length(which(values_D4$Type == "PC" & values_D4$area > 25))
        nrSt_D4 <- length(which(values_D4$Type == "Stom"))
        SI_D4 <- nrSt_D4 / (nrPC_D4 + nrSt_D4)
        SD_D4 <- nrSt_D4 / sum(values_D4$area)
        calculations_list( c(calculations_list(), list(D4 = c(nrPC_D4 + nrSt_D4, nrPC_D4, nrSt_D4, SI_D4, SD_D4))) )
      }
    })
    
    # Perform calculations for D5 similarly
    observeEvent(input$values_D5_file, {
      if (!is.null(input$values_D5_file)) {
        values_D5 <- read.csv(input$values_D5_file$datapath)
        nrPC_D5 <- length(which(values_D5$Type == "PC" & values_D5$area > 25))
        nrSt_D5 <- length(which(values_D5$Type == "Stom"))
        SI_D5 <- nrSt_D5 / (nrPC_D5 + nrSt_D5)
        SD_D5 <- nrSt_D5 / sum(values_D5$area)
        calculations_list( c(calculations_list(), list(D5 = c(nrPC_D5 + nrSt_D5, nrPC_D5, nrSt_D5, SI_D5, SD_D5))) )
      }
    })
    
    # Combine all calculations into a matrix
    combined_calculations <- reactive({
      result <- matrix(NA, nrow = 5, ncol = length(calculations_list()))
      rownames(result) <- c("Total", "nr_PC", "nr_Stom", "SI", "SD")
      for (i in seq_along(calculations_list())) {
        result[, i] <- calculations_list()[[i]]
      }
      colnames(result) <- names(calculations_list())
      result
    })
    
    # Show the matrix in the UI
    output$cell_param_table <- renderDataTable({ combined_calculations() })
    
    # Download handler
    output$download_data <- downloadHandler(
      filename = function() { paste("cell_param_table.csv", sep = "") },
      content = function(file) { write.csv(combined_calculations(), file, row.names = TRUE) }
    )
    
    # Plot 1
    output$plot1 <- renderPlot({
      # Definieer de dagen
      dagen <- seq_along(combined_calculations()[1,]) 
      # Extraheren van de Total kolom uit de matrix
      total_cells <- combined_calculations()[1,]
      # Maak de data frame voor ggplot
      data <- data.frame(Days = dagen, Number_of_cells = total_cells)
      # Maak de plot met ggplot
      ggplot(data, aes(x = Days, y = Number_of_cells)) +
        geom_point() +
        geom_line() +
        labs(x = "Days", y = "Number of cells") +
        ggtitle("Total cells over time") +
        theme(plot.title = element_text(hjust = 0.5))  # Centreren van de titel
    })
    
    # Plot 2
    output$plot2 <- renderPlot({
      # Definieer de dagen
      dagen <- seq_along(combined_calculations()[1,]) 
      # Extraheren van de Total kolom uit de matrix
      total_cells <- combined_calculations()[2,]
      # Maak de data frame voor ggplot
      data <- data.frame(Days = dagen, Number_of_PCs = total_cells)
      # Maak de plot met ggplot
      ggplot(data, aes(x = Days, y = Number_of_PCs)) +
        geom_point() +
        geom_line() +
        ylim(10, 150) +  # Limieten instellen voor de y-as
        labs(x = "Days", y = "Number of PCs") +
        ggtitle("Number of PCs over time") +
        theme(plot.title = element_text(hjust = 0.5))  # Centreren van de titel
    })
    
    # Plot 3
    output$plot3 <- renderPlot({
      # Definieer de dagen
      dagen <- seq_along(combined_calculations()[1,]) 
      # Extraheren van de Total kolom uit de matrix
      total_cells <- combined_calculations()[3,]
      # Maak de data frame voor ggplot
      data <- data.frame(Days = dagen, Number_of_Stom = total_cells)
      # Maak de plot met ggplot
      ggplot(data, aes(x = Days, y = Number_of_Stom)) +
        geom_point() +
        geom_line() +
        labs(x = "Days", y = "Number of stomata") +
        ggtitle("Number of stomata over time") +
        theme(plot.title = element_text(hjust = 0.5))  # Centreren van de titel
    })
    
    # Plot 4
    output$plot4 <- renderPlot({
      # Definieer de dagen
      dagen <- seq_along(combined_calculations()[1,]) 
      # Extraheren van de Total kolom uit de matrix
      total_cells <- combined_calculations()[4,]
      # Maak de data frame voor ggplot
      data <- data.frame(Days = dagen, Number_of_SI = total_cells)
      # Maak de plot met ggplot
      ggplot(data, aes(x = Days, y = Number_of_SI)) +
        geom_point() +
        geom_line() +
        labs(x = "Days", y = "SI") +
        ggtitle("SI over time") +
        theme(plot.title = element_text(hjust = 0.5))  # Centreren van de titel
    })
  })
  #############################################################################
  # Function to load data and calculate centroids
  calculate_centroids <- function(data) {
    centroids <- data %>%
      group_by(ids) %>%
      summarize(CoMX = mean(Xcoord), CoMY = mean(InvY))
    data <- merge(data, centroids, by = 'ids')
    data
  }
  
  # Function to load data
  load_data <- function(input_file) {
    req(input_file)
    read.csv(input_file$datapath)
  }
  
  # Function to create plot
  plot_functions <- function(data, top_cell, right_cell, base_cell) {
    data <- calculate_centroids(data)
    ggplot(data, aes(x = Xcoord, y = InvY)) +
      geom_point(aes(colour = as.factor(ids))) +
      geom_text(aes(x = CoMX, y = CoMY, label = ids), colour = "white", size = 3) +
      theme_minimal() +
      theme(legend.position = "none") +
      labs(x = NULL, y = NULL)
  }
  
  # Reactive expressions for each dataset
  data1 <- reactive({ load_data(input$upload_data1) })
  data2 <- reactive({ load_data(input$upload_data2) })
  data3 <- reactive({ load_data(input$upload_data3) })
  data4 <- reactive({ load_data(input$upload_data4) })
  data5 <- reactive({ load_data(input$upload_data5) })
  
  # Render UI for cell input fields
  output$plot1_inputs <- renderUI({
    fluidRow(
      column(4, numericInput("top_cell1", "Top Cell:", value = 1, min = 1)),
      column(4, numericInput("right_cell1", "Right Cell:", value = 2, min = 1)),
      column(4, numericInput("base_cell1", "Base Cell:", value = 3, min = 1))
    )
  })
  
  output$plot2_inputs <- renderUI({
    fluidRow(
      column(4, numericInput("top_cell2", "Top Cell:", value = 1, min = 1)),
      column(4, numericInput("right_cell2", "Right Cell:", value = 2, min = 1)),
      column(4, numericInput("base_cell2", "Base Cell:", value = 3, min = 1))
    )
  })
  
  output$plot3_inputs <- renderUI({
    fluidRow(
      column(4, numericInput("top_cell3", "Top Cell:", value = 1, min = 1)),
      column(4, numericInput("right_cell3", "Right Cell:", value = 2, min = 1)),
      column(4, numericInput("base_cell3", "Base Cell:", value = 3, min = 1))
    )
  })
  
  output$plot4_inputs <- renderUI({
    fluidRow(
      column(4, numericInput("top_cell4", "Top Cell:", value = 1, min = 1)),
      column(4, numericInput("right_cell4", "Right Cell:", value = 2, min = 1)),
      column(4, numericInput("base_cell4", "Base Cell:", value = 3, min = 1))
    )
  })
  
  output$plot5_inputs <- renderUI({
    fluidRow(
      column(4, numericInput("top_cell5", "Top Cell:", value = 1, min = 1)),
      column(4, numericInput("right_cell5", "Right Cell:", value = 2, min = 1)),
      column(4, numericInput("base_cell5", "Base Cell:", value = 3, min = 1))
    )
  })
  
  # Render plots for each dataset
  output$plots1 <- renderPlot({
    plot_functions(data1(), input$top_cell1, input$right_cell1, input$base_cell1)
  })
  
  output$plots2 <- renderPlot({
    plot_functions(data2(), input$top_cell2, input$right_cell2, input$base_cell2)
  })
  
  output$plots3 <- renderPlot({
    plot_functions(data3(), input$top_cell3, input$right_cell3, input$base_cell3)
  })
  
  output$plots4 <- renderPlot({
    plot_functions(data4(), input$top_cell4, input$right_cell4, input$base_cell4)
  })
  
  output$plots5 <- renderPlot({
    plot_functions(data5(), input$top_cell5, input$right_cell5, input$base_cell5)
  })
  
  ###########################################################
  ## New Calculations and Plot for Rotated Tab ##
  ###########################################################
  # Observe the submit button to update the rotated tab plots
  observeEvent(input$submit, {
    lapply(1:5, function(i) {
      dataset <- get(paste0("data", i))()
      req(dataset)
      highlight_cells <- c(input[[paste0("top_cell", i)]], input[[paste0("right_cell", i)]], input[[paste0("base_cell", i)]])
      
      # Ensure highlight_cells are within bounds
      if (all(highlight_cells %in% dataset$ids)) {
        # Update "Type" column for all rows with specified IDs
        dataset$Type <- ifelse(dataset$ids %in% highlight_cells, "Highlight", dataset$Type)
        
        sorted_dataset <- dataset[order(dataset$ids), ]
        cols <- c("Stom" = "chocolate3", "PC" = "aquamarine4", "Highlight" = "yellow")
        
        output[[paste0("D", i, "_plot")]] <- renderPlot({
          ggplot(sorted_dataset, aes(x = Xcoord, y = InvY, colour = Type)) +
            geom_point(size = 0.1) +
            scale_color_manual(values = cols) +
            theme(panel.background = element_rect(fill = "gray27")) +
            theme(panel.grid = element_blank(), legend.position = "none") +
            theme(axis.text.x = element_blank(),
                  axis.ticks.x = element_blank(),
                  axis.text.y = element_blank(),
                  axis.ticks.y = element_blank(),
                  axis.title.x=element_blank(), axis.title.y=element_blank())
        })
      }
    })
  })
  #########################
  ## achtergrond rotatie op de values files
  #########################
  V_D1 <- reactive({ req(input$values_D1_file)
    read.csv(input$values_D1_file$datapath)
  })
  V_D2 <- reactive({ req(input$values_D2_file)
    read.csv(input$values_D2_file$datapath)
  })
  V_D3 <- reactive({ req(input$values_D3_file)
    read.csv(input$values_D3_file$datapath)
  })
  V_D4 <- reactive({ req(input$values_D4_file)
    read.csv(input$values_D4_file$datapath)
  })
  V_D5 <- reactive({req(input$values_D5_file)
    read.csv(input$values_D5_file$datapath)
  })
  
  data1_D1 <- reactive({ 
    load_data(input$upload_data1)
  })
  data2_D2 <- reactive({ req(input$upload_data2)
    read.csv(input$upload_data2$datapath)
  })
  data3_D3 <- reactive({ req(input$upload_data3)
    read.csv(input$upload_data3$datapath)
  })
  data4_D4 <- reactive({ req(input$upload_data4)
    read.csv(input$upload_data4$datapath)
  })
  data5_D5 <- reactive({ req(input$upload_data5)
    read.csv(input$upload_data5$datapath)
  })
  
  highlight_D1 <- reactive({
    c(input$top_cell1, input$right_cell1, input$base_cell1)
  })
  highlight_D2 <- reactive({
    c(input$top_cell2, input$right_cell2, input$base_cell2)
  })
  highlight_D3 <- reactive({
    c(input$top_cell3, input$right_cell3, input$base_cell3)
  })
  highlight_D4 <- reactive({
    c(input$top_cell4, input$right_cell4, input$base_cell4)
  })
  highlight_D5 <- reactive({
    c(input$top_cell5, input$right_cell5, input$base_cell5)
  })
  
  sorted_All_D1 <- reactive({
    req(V_D1(), highlight_D1(), data1_D1())
    
    Values_D1_temp <- V_D1()
    highlights <- highlight_D1()
    
    # Update the Type column to "Highlight" based on the highlight indices
    Values_D1_temp$Type[Values_D1_temp$cellid %in% highlights] <- "Highlight"
    
    type_list_D1 <- rep(Values_D1_temp$Type, Values_D1_temp$areapx)
    
    sorted_All_D1 <- data1_D1()[order(data1_D1()$ids), ]
    sorted_All_D1$Type <- type_list_D1
    
    sorted_All_D1
  })
  
  sorted_All_D2 <- reactive({
    req(V_D2(), highlight_D2(), data2_D2())
    
    # Fetch data and highlights
    Values_D2_temp <- V_D2()
    highlights <- highlight_D2()
    
    # Update the Type column to "Highlight" based on the highlight indices
    Values_D2_temp$Type[Values_D2_temp$cellid %in% highlights] <- "Highlight"
    
    # Create a list of types repeated by area pixels
    type_list_D2 <- rep(Values_D2_temp$Type, Values_D2_temp$areapx)
    
    # Sort data and update the Type column
    sorted_All_D2 <- data2_D2()[order(data2_D2()$ids), ]
    sorted_All_D2$Type <- type_list_D2
    
    sorted_All_D2
  })
  
  Col_id <- reactive({
    req(V_D1(), sorted_All_D1())
    
    Values_D1 <- V_D1()
    colnames(Values_D1) <- c("cellid_D1","area_D1","CoMX_D1","CoMY_D1","Peri_D1", "areapx_D1", "Circ_D1", "Type_D1")
    
    Color_id <- numeric(nrow(Values_D1))
    
    for (i in 1:nrow(Values_D1)) {
      if (Values_D1$Type_D1[i] == "PC" | Values_D1$Type_D1[i] == "Highlight") {
        Color_id[i] <- 1
      } else if (Values_D1$Type_D1[i] == "Stom" | Values_D1$Type_D1[i] == "NEW_Stom") {
        Color_id[i] <- Values_D1$cellid_D1[i]
      }
    }
    
    Values_D1$Color_id <- Color_id
    sorted_All_D1_data <- sorted_All_D1()
    Color_id_D1 <- rep(Values_D1$Color_id, Values_D1$areapx_D1)
    sorted_All_D1_data$Color_id_D1 <- Color_id_D1
    
    # Ensure Xcoord and InvY columns are present
    if (!"Xcoord" %in% colnames(sorted_All_D1_data) | !"InvY" %in% colnames(sorted_All_D1_data)) {
      stop("Xcoord or InvY columns not found in sorted_All_D1_data")
    }
    
    sorted_All_D1_data
  })
  
  test <- reactive({
    req(V_D1(), V_D2(), highlight_D1(), highlight_D2())
    Values_D1 <- V_D1()
    Values_D2 <- V_D2()
    colnames(Values_D1)<-c("cellid_D1","area_D1","CoMX_D1","CoMY_D1","Peri_D1", "areapx_D1", "Circ_D1", "Type_D1")
    colnames(Values_D2)<-c("cellid_D2","area_D2","CoMX_D2","CoMY_D2","Peri_D2", "areapx_D2", "Circ_D2", "Type_D2")
    
    diff_x <- Values_D2[highlight_D2()[1], "CoMX_D2"] - Values_D1[highlight_D1()[1], "CoMX_D1"]
    diff_y <- Values_D2[highlight_D2()[1], "CoMY_D2"] - Values_D1[highlight_D1()[1], "CoMY_D1"]
    Values_D2[, "cor_CoMX_D2"] <- Values_D2[, "CoMX_D2"] - diff_x
    Values_D2[, "cor_CoMY_D2"] <- Values_D2[, "CoMY_D2"] - diff_y
    
    side_a <- ((Values_D1[highlight_D1()[3], "CoMX_D1"] - Values_D2[highlight_D2()[3], "cor_CoMX_D2"])^2 + 
                 (Values_D1[highlight_D1()[3], "CoMY_D1"] - Values_D2[highlight_D2()[3], "cor_CoMY_D2"])^2)^(1/2)
    side_b <- ((Values_D1[highlight_D1()[1], "CoMX_D1"] - Values_D1[highlight_D1()[3], "CoMX_D1"])^2 + 
                 (Values_D1[highlight_D1()[1], "CoMY_D1"] - Values_D1[highlight_D1()[3], "CoMY_D1"])^2)^(1/2)
    side_c <- ((Values_D1[highlight_D1()[1], "CoMX_D1"] - Values_D2[highlight_D2()[3], "cor_CoMX_D2"])^2 + 
                 (Values_D1[highlight_D1()[1], "CoMY_D1"] - Values_D2[highlight_D2()[3], "cor_CoMY_D2"])^2)^(1/2)
    cos_alfa <- -(side_a^2 - side_b^2 - side_c^2) / (2 * side_b * side_c)
    alfa <- acos(cos_alfa)
    Values_D2[, "cor_CoMX_D2"] <- Values_D2[, "cor_CoMX_D2"] * cos(-alfa) - Values_D2[, "cor_CoMY_D2"] * sin(-alfa)
    Values_D2[, "cor_CoMY_D2"] <- Values_D2[, "cor_CoMX_D2"] * sin(alfa) + Values_D2[, "cor_CoMY_D2"] * cos(alfa)
    
    width_D1 <- Values_D1[highlight_D1()[2], "CoMX_D1"] - Values_D1[highlight_D1()[1], "CoMX_D1"]
    width_D2 <- Values_D2[highlight_D2()[2], "cor_CoMX_D2"] - Values_D2[highlight_D2()[1], "cor_CoMX_D2"]
    Values_D2[, "cor_CoMX_D2"] <- (Values_D2[, "cor_CoMX_D2"] * (width_D1 / width_D2)) + 
      (Values_D2[highlight_D2()[1], "cor_CoMX_D2"] * (width_D1 / width_D2))
    height_D1 <- Values_D1[highlight_D1()[3], "CoMY_D1"] - Values_D1[highlight_D1()[1], "CoMY_D1"]
    height_D2 <- Values_D2[highlight_D2()[3], "cor_CoMY_D2"] - Values_D2[highlight_D2()[1], "cor_CoMY_D2"]
    Values_D2[, "cor_CoMY_D2"] <- Values_D2[, "cor_CoMY_D2"] * (height_D1 / height_D2) + 
      (Values_D2[highlight_D2()[1], "cor_CoMY_D2"] * (height_D1 / height_D2))
    
    # Re-iterate the move only
    diff_x <- Values_D2[highlight_D2()[1], "cor_CoMX_D2"] - Values_D1[highlight_D1()[1], "CoMX_D1"]
    diff_y <- Values_D2[highlight_D2()[1], "cor_CoMY_D2"] - Values_D1[highlight_D1()[1], "CoMY_D1"]
    Values_D2[, "cor_CoMX_D2"] <- Values_D2[, "cor_CoMX_D2"] - diff_x
    Values_D2[, "cor_CoMY_D2"] <- Values_D2[, "cor_CoMY_D2"] - diff_y
    
    # Correction for distortion around the right highlight cell
    dist_to_right <- ((((Values_D2[, "cor_CoMX_D2"] - Values_D2[highlight_D2()[2], "cor_CoMX_D2"])^2) + 
                         ((Values_D2[, "cor_CoMY_D2"] - Values_D2[highlight_D2()[2], "cor_CoMY_D2"])^2))^(1/2))
    dist_to_right <- dist_to_right / max(dist_to_right) + 0.1 # +0.1 is to not have 0 values, important for no error when taking log
    
    distortion <- (Values_D2[highlight_D2()[2], "cor_CoMY_D2"] - Values_D1[highlight_D1()[2], "CoMY_D1"]) * 
      (-log10(dist_to_right))
    
    Values_D2[, "cor_CoMY_D2"] <- Values_D2[, "cor_CoMY_D2"] - distortion
    
    
    list(Values_D1 = Values_D1, Values_D2 = Values_D2)
  })
  
  stomata_test <- reactive({
    req(test()$Values_D1, test()$Values_D2, data2_D2())
    
    Values_D1 <- test()$Values_D1
    Values_D2 <- test()$Values_D2
    
    Stomata_D1 <- subset(Values_D1, Type_D1 == "Stom")
    Stomata_D2 <- subset(Values_D2, Type_D2 == "Stom")
    
    # Renaming columns for stomata data frames
    colnames(Stomata_D1) <- c("St_cellid_D1", "St_area_D1", "St_CoMX_D1", "St_CoMY_D1", "St_Peri_D1", "St_area_px_D1", "St_Circ_D1", "St_Type_D1")
    colnames(Stomata_D2) <- c("St_cellid_D2", "St_area_D2", "St_old_CoMX_D2", "St_old_CoMY_D2", "St_Peri_D2", "St_area_px_D2", "St_Circ_D2", "St_Type_D2", "St_CoMX_D2", "St_CoMY_D2")
    
    # Coordinates
    St_CoMX_D1_px <- Stomata_D1$St_CoMX_D1
    St_CoMY_D1_px <- Stomata_D1$St_CoMY_D1
    St_CoMX_D2_px <- Stomata_D2$St_CoMX_D2
    St_CoMY_D2_px <- Stomata_D2$St_CoMY_D2
    
    St_scores <- numeric()
    
    for (j in 1:length(Stomata_D2$St_area_D2)) {
      for (i in 1:length(Stomata_D1$St_area_D1)) {
        St_pc <- sqrt(((St_CoMX_D2_px[j] - St_CoMX_D1_px[i]) * 2)^2 + (St_CoMY_D2_px[j] - St_CoMY_D1_px[i])^2)
        St_scores <- c(St_scores, St_pc)
      }
    }
    
    St_scoretable <- matrix(St_scores, nrow = length(Stomata_D2$St_cellid_D2), ncol = length(Stomata_D1$St_cellid_D1), byrow = TRUE)
    St_bestmatch <- apply(St_scoretable, 1, which.min)
    
    for (i in 1:length(St_bestmatch)) {
      Stomata_D2$St_PrevID[i] <- Stomata_D1$St_cellid_D1[St_bestmatch[i]]
      Stomata_D2$St_score[i] <- min(St_scoretable[i, ])
    }
    
    PrevID_all_dbl_St <- unique(Stomata_D2[duplicated(Stomata_D2$St_PrevID), "St_PrevID"])
    
    for (i in 1:length(PrevID_all_dbl_St)) {
      PrevID_dbl_St <- PrevID_all_dbl_St[i]
      dbl_St_id <- Stomata_D2[Stomata_D2$St_PrevID == PrevID_dbl_St, "St_cellid_D2"]
      dbl_St_scores <- Stomata_D2[Stomata_D2$St_PrevID == PrevID_dbl_St, "St_score"]
      old_St <- which.min(dbl_St_scores)
      
      for (j in 1:length(dbl_St_id)) {
        if (j != old_St) {
          new_St <- dbl_St_id[j]
          Values_D2[Values_D2$cellid_D2 == new_St, "Type_D2"] <- "NEW_Stom"
        }
      }
    }
    
    D2_PrevID_st <- c()
    j <- 1
    for (i in 1:length(Values_D2$cellid_D2)) {
      if (Values_D2$Type_D2[i] == "PC" | Values_D2$Type_D2[i] == "Highlight") {
        D2_PrevID_st <- c(D2_PrevID_st, 1)
      }
      if (Values_D2$Type_D2[i] == "Stom" | Values_D2$Type_D2[i] == "NEW_Stom") {
        D2_PrevID_st <- c(D2_PrevID_st, Stomata_D2$St_PrevID[j])
        j <- j + 1
      }
    }
    
    Values_D2$D2_PrevID_st <- D2_PrevID_st
    Old_stomata_D2 <- subset(Values_D2[Values_D2$Type_D2 == "Stom", ])
    attach(Old_stomata_D2)
    D2_PrevID_st <- c()
    j <- 1
    for (i in 1:length(Values_D2$cellid_D2)) {
      if (Values_D2$Type_D2[i] == "PC" | Values_D2$Type_D2[i] == "Highlight") {
        D2_PrevID_st <- c(D2_PrevID_st, 1)
      }
      if (Values_D2$Type_D2[i] == "NEW_Stom") {
        D2_PrevID_st <- c(D2_PrevID_st, Values_D2$cellid_D2[i])
      }
      if (Values_D2$Type_D2[i] == "Stom") {
        D2_PrevID_st <- c(D2_PrevID_st, Old_stomata_D2$D2_PrevID_st[j])
        j <- j + 1
      }
    }
    
    Values_D2$D2_PrevID_st <- D2_PrevID_st
    OrigID_D2_st <- rep(Values_D2$D2_PrevID_st, Values_D2$areapx_D2)
    
    # Sorting and updating the final data
    sorted_All_D2 <- data2_D2()[order(data2_D2()$ids), ]
    type_list_D2 <- rep(Values_D2$Type_D2, Values_D2$areapx_D2)
    sorted_All_D2$Type_D2 <- type_list_D2
    sorted_All_D2$OrigID_st <- OrigID_D2_st
    
    # Return both Values_D2 and sorted_All_D2 in a list
    list(Values_D2 = Values_D2, sorted_All_D2 = sorted_All_D2)
  })
  
  Values_D2 <- reactive({
    req(stomata_test())
    
    stomata_result <- stomata_test()
    
    print("Resultaat van stomata_test:")
    print(str(stomata_result$Values_D2))
    
    values_D2 <- stomata_result$Values_D2
    
    print("BOEM BAM THIS IS VALUES-D2: ")
    print(str(values_D2))
    
    return(values_D2)
  })
  
  sorted_All_D2 <- reactive({
    req(stomata_test())
    
    stomata_result <- stomata_test()
    
    print("Resultaat van stomata_test (sorted_All_D2):")
    print(str(stomata_result$sorted_All_D2))
    
    sorted_all_d2 <- stomata_result$sorted_All_D2
    
    print("BOEM BAM THIS IS sorted_All_D2: ")
    print(str(sorted_all_d2))
    
    return(sorted_all_d2)
  })
  
  test2 <- reactive({
    req(V_D1(), V_D3(), highlight_D1(), highlight_D3(), sorted_All_D1())
    Values_D1 <- V_D1()
    Values_D3 <- V_D3()
    print("this is Values D1:")
    print(str(Values_D1))
    print("this is values d3:")
    print(str(Values_D3))
    colnames(Values_D1) <- c("cellid_D1","area_D1","CoMX_D1","CoMY_D1","Peri_D1", "areapx_D1", "Circ_D1", "Type_D1")
    colnames(Values_D3) <- c("cellid_D3","area_D3","CoMX_D3","CoMY_D3","Peri_D3", "areapx_D3", "Circ_D3", "Type_D3")
    
    diff_x <- Values_D3[highlight_D3()[1], "CoMX_D3"] - Values_D1[highlight_D1()[1], "CoMX_D1"]
    diff_y <- Values_D3[highlight_D3()[1], "CoMY_D3"] - Values_D1[highlight_D1()[1], "CoMY_D1"]
    Values_D3[, "cor_CoMX_D3"] <- Values_D3[, "CoMX_D3"] - diff_x
    Values_D3[, "cor_CoMY_D3"] <- Values_D3[, "CoMY_D3"] - diff_y
    
    side_a <- ((Values_D1[highlight_D1()[3], "CoMX_D1"] - Values_D3[highlight_D3()[3], "cor_CoMX_D3"])^2 + 
                 (Values_D1[highlight_D1()[3], "CoMY_D1"] - Values_D3[highlight_D3()[3], "cor_CoMY_D3"])^2)^(1/2)
    side_b <- ((Values_D1[highlight_D1()[1], "CoMX_D1"] - Values_D1[highlight_D1()[3], "CoMX_D1"])^2 + 
                 (Values_D1[highlight_D1()[1], "CoMY_D1"] - Values_D1[highlight_D1()[3], "CoMY_D1"])^2)^(1/2)
    side_c <- ((Values_D1[highlight_D1()[1], "CoMX_D1"] - Values_D3[highlight_D3()[3], "cor_CoMX_D3"])^2 + 
                 (Values_D1[highlight_D1()[1], "CoMY_D1"] - Values_D3[highlight_D3()[3], "cor_CoMY_D3"])^2)^(1/2)
    cos_alfa <- -(side_a^2 - side_b^2 - side_c^2) / (2 * side_b * side_c)
    alfa <- acos(cos_alfa)
    Values_D3[, "cor_CoMX_D3"] <- Values_D3[, "cor_CoMX_D3"] * cos(-alfa) - Values_D3[, "cor_CoMY_D3"] * sin(-alfa)
    Values_D3[, "cor_CoMY_D3"] <- Values_D3[, "cor_CoMX_D3"] * sin(alfa) + Values_D3[, "cor_CoMY_D3"] * cos(alfa)
    
    width_D1 <- Values_D1[highlight_D1()[2], "CoMX_D1"] - Values_D1[highlight_D1()[1], "CoMX_D1"]
    width_D3 <- Values_D3[highlight_D3()[2], "cor_CoMX_D3"] - Values_D3[highlight_D3()[1], "cor_CoMX_D3"]
    Values_D3[, "cor_CoMX_D3"] <- (Values_D3[, "cor_CoMX_D3"] * (width_D1 / width_D3)) + 
      (Values_D3[highlight_D3()[1], "cor_CoMX_D3"] * (width_D1 / width_D3))
    height_D1 <- Values_D1[highlight_D1()[3], "CoMY_D1"] - Values_D1[highlight_D1()[1], "CoMY_D1"]
    height_D3 <- Values_D3[highlight_D3()[3], "cor_CoMY_D3"] - Values_D3[highlight_D3()[1], "cor_CoMY_D3"]
    Values_D3[, "cor_CoMY_D3"] <- Values_D3[, "cor_CoMY_D3"] * (height_D1 / height_D3) + 
      (Values_D3[highlight_D3()[1], "cor_CoMY_D3"] * (height_D1 / height_D3))
    
    # Re-iterate the move only
    diff_x <- Values_D3[highlight_D3()[1], "cor_CoMX_D3"] - Values_D1[highlight_D1()[1], "CoMX_D1"]
    diff_y <- Values_D3[highlight_D3()[1], "cor_CoMY_D3"] - Values_D1[highlight_D1()[1], "CoMY_D1"]
    Values_D3[, "cor_CoMX_D3"] <- Values_D3[, "cor_CoMX_D3"] - diff_x
    Values_D3[, "cor_CoMY_D3"] <- Values_D3[, "cor_CoMY_D3"] - diff_y
    
    # Correction for distortion around the right highlight cell
    dist_to_right <- ((((Values_D3[, "cor_CoMX_D3"] - Values_D3[highlight_D3()[2], "cor_CoMX_D3"])^2) + 
                         ((Values_D3[, "cor_CoMY_D3"] - Values_D3[highlight_D3()[2], "cor_CoMY_D3"])^2))^(1/2))
    dist_to_right <- dist_to_right / max(dist_to_right) + 0.1 # +0.1 is to not have 0 values, important for no error when taking log
    
    distortion <- (Values_D3[highlight_D3()[2], "cor_CoMY_D3"] - Values_D1[highlight_D1()[2], "CoMY_D1"]) * 
      (-log10(dist_to_right))
    
    Values_D3[, "cor_CoMY_D3"] <- Values_D3[, "cor_CoMY_D3"] - distortion
    print("this is a other text:")
    print(str(Values_D3))
    list(Values_D1 = Values_D1, Values_D3 = Values_D3)
  })
  stomata_test2 <- reactive({
    req(Values_D2(), test2()$Values_D3, data3_D3())
    
    Values_D3 <- test2()$Values_D3
    Values_D2 <- Values_D2()
    
    print("this is Values D3 test: ")
    print(str(Values_D3))
    print("This is Values D2 test: ")
    print(str(Values_D2))
    
    # First, the stomata are extracted from the datasets (again only D1 and D2)
    Stomata_D2 <- subset(Values_D2[Values_D2$Type_D2 == "Stom" | Values_D2$Type_D2 == "NEW_Stom", ])
    Stomata_D3 <- subset(Values_D3[Values_D3$Type_D3 == "Stom", ])
    
    colnames(Stomata_D2) <- c("St_cellid_D2", "St_area_D2", "St_old_CoMX_D2", "St_old_CoMY_D2", "St_Peri_D2", "St_area_px_D2", "St_Circ_D2", "St_Type_D2", "St_CoMX_D2", "St_CoMY_D2")
    colnames(Stomata_D3) <- c("St_cellid_D3", "St_area_D3", "St_old_CoMX_D3", "St_old_CoMY_D3", "St_Peri_D3", "St_area_px_D3", "St_Circ_D3", "St_Type_D3", "St_CoMX_D3", "St_CoMY_D3")
    
    St_CoMX_D2_px <- Stomata_D2$St_CoMX_D2 # Hier voeg je de juiste variabele toe
    St_CoMY_D2_px <- Stomata_D2$St_CoMY_D2 # Hier voeg je de juiste variabele toe
    St_CoMX_D3_px <- Stomata_D3$St_CoMX_D3
    St_CoMY_D3_px <- Stomata_D3$St_CoMY_D3
    
    St_pos_ch <- c()
    St_pc <- c()
    for (j in 1:length(Stomata_D3$St_area_D3)) {
      for (i in 1:length(Stomata_D2$St_area_D2)) {
        St_pc <- ((((St_CoMX_D3_px[j] - St_CoMX_D2_px[i]) * 2)^2) + ((St_CoMY_D3_px[j] - St_CoMY_D2_px[i])^2))^(1/2)
        St_pos_ch <- c(St_pos_ch, St_pc)
        St_pc <- c()
      }
    }
    St_scores <- St_pos_ch
    length(St_scores)
    St_scoretable <- matrix(St_scores, nrow = length(Stomata_D3$St_cellid_D3), ncol = length(Stomata_D2$St_cellid_D2), byrow = TRUE)
    St_bestmatch <- apply(St_scoretable, 1, which.min)
    for (i in 1:length(St_bestmatch)) {
      Stomata_D3$D3_St_PrevID[i] <- Stomata_D2$St_cellid_D2[St_bestmatch[i]]
      Stomata_D3$D3_St_score[i] <- min(St_scoretable[i, ])
    }
    
    PrevID_all_dbl_St <- unique(Stomata_D3[duplicated(Stomata_D3$D3_St_PrevID), 11])
    length(PrevID_all_dbl_St) # Number of Stomata on D2 that gave rise to more than one Stomata on D3
    
    PrevID_dbl_St <- c()
    dbl_St_id <- c()
    dbl_St_scores <- c()
    old_St <- c()
    new_St <- c()
    for (i in 1:length(PrevID_all_dbl_St)) {
      PrevID_dbl_St <- PrevID_all_dbl_St[i]
      dbl_St_id <- Stomata_D3[Stomata_D3$D3_St_PrevID == PrevID_dbl_St, 1]
      dbl_St_scores <- Stomata_D3[Stomata_D3$D3_St_PrevID == PrevID_dbl_St, 12]
      print(dbl_St_scores)
      old_St <- match(min(dbl_St_scores), dbl_St_scores)
      for (j in 1:length(dbl_St_id)) {
        if (j != old_St) {
          new_St <- dbl_St_id[j]
          print(new_St)
          Values_D3[Values_D3$cellid_D3 == new_St, 8] <- "NEW_Stom"
        }
      }
    }
    table(Values_D3$Type_D3)
    
    D3_PrevID_st <- c()
    
    for (i in 1:length(Values_D3$cellid_D3)) {
      if (Values_D3$Type_D3[i] == "PC" | Values_D3$Type_D3[i] == "Highlight") {
        D3_PrevID_st <- c(D3_PrevID_st, 1)
      }
      if (Values_D3$Type_D3[i] == "NEW_Stom") {
        D3_PrevID_st <- c(D3_PrevID_st, Values_D3$cellid_D3[i])
      }
      if (Values_D3$Type_D3[i] == "Stom") {
        cellnr <- Values_D3$cellid_D3[i]
        if (is.na(Stomata_D3$D3_St_PrevID[Stomata_D3$St_cellid_D3 == cellnr])) {
          D3_PrevID_st <- c(D3_PrevID_st, NA)
        } else {
          D3_PrevID_st <- c(D3_PrevID_st, Values_D2$D2_PrevID_st[Stomata_D3$D3_St_PrevID[Stomata_D3$St_cellid_D3 == cellnr]])
        }
      }
    }
    
    # Vervang eventuele NA-waarden door 0 of een andere geschikte waarde, afhankelijk van je gegevens
    D3_PrevID_st[is.na(D3_PrevID_st)] <- 0
    
    Values_D3$D3_PrevID_st <- D3_PrevID_st
    attach(Values_D3)
    colnames(Values_D3)
    OrigID_D3_st <- rep(Values_D3$D3_PrevID_st, Values_D3$areapx_D3)
    length(OrigID_D3_st)
    
    type_list_D3 <- rep(Values_D3$Type_D3, Values_D3$areapx_D3)
    sorted_All_D3 <- data3_D3()[order(data3_D3()$ids), ]
    sorted_All_D3$Type <- type_list_D3
    sorted_All_D3$OrigID_st <- OrigID_D3_st
    
    list(Values_D3 = Values_D3, sorted_All_D3 = sorted_All_D3)
  })
  
  Values_D3 <- reactive({
    req(stomata_test2())
    
    stomata_result <- stomata_test2()
    
    print("Resultaat van stomata_test2:")
    print(str(stomata_result$Values_D3))
    
    values_D3 <- stomata_result$Values_D3
    
    print("BOEM BAM THIS IS VALUES-D3: ")
    print(str(values_D3))
    
    return(values_D3)
  })
  
  sorted_All_D3 <- reactive({
    req(stomata_test2())
    
    stomata_result <- stomata_test2()
    
    print("Resultaat van stomata_test (sorted_All_D3):")
    print(str(stomata_result$sorted_All_D3))
    
    sorted_all_d3 <- stomata_result$sorted_All_D3
    
    print("BOEM BAM THIS IS sorted_All_D3: ")
    print(str(sorted_all_d3))
    
    return(sorted_all_d3)
  })
  test3 <- reactive({
    req(V_D1(), V_D4(), highlight_D1(), highlight_D4())
    Values_D1 <- V_D1()
    Values_D4 <- V_D4()
    print("this is Values D1:")
    print(str(Values_D1))
    print("this is values D4:")
    print(str(Values_D4))
    colnames(Values_D1) <- c("cellid_D1","area_D1","CoMX_D1","CoMY_D1","Peri_D1", "areapx_D1", "Circ_D1", "Type_D1")
    colnames(Values_D4) <- c("cellid_D4","area_D4","CoMX_D4","CoMY_D4","Peri_D4", "areapx_D4", "Circ_D4", "Type_D4")
    
    diff_x <- Values_D4[highlight_D4()[1], "CoMX_D4"] - Values_D1[highlight_D1()[1], "CoMX_D1"]
    diff_y <- Values_D4[highlight_D4()[1], "CoMY_D4"] - Values_D1[highlight_D1()[1], "CoMY_D1"]
    Values_D4[, "cor_CoMX_D4"] <- Values_D4[, "CoMX_D4"] - diff_x
    Values_D4[, "cor_CoMY_D4"] <- Values_D4[, "CoMY_D4"] - diff_y
    
    side_a <- ((Values_D1[highlight_D1()[3], "CoMX_D1"] - Values_D4[highlight_D4()[3], "cor_CoMX_D4"])^2 + 
                 (Values_D1[highlight_D1()[3], "CoMY_D1"] - Values_D4[highlight_D4()[3], "cor_CoMY_D4"])^2)^(1/2)
    side_b <- ((Values_D1[highlight_D1()[1], "CoMX_D1"] - Values_D1[highlight_D1()[3], "CoMX_D1"])^2 + 
                 (Values_D1[highlight_D1()[1], "CoMY_D1"] - Values_D1[highlight_D1()[3], "CoMY_D1"])^2)^(1/2)
    side_c <- ((Values_D1[highlight_D1()[1], "CoMX_D1"] - Values_D4[highlight_D4()[3], "cor_CoMX_D4"])^2 + 
                 (Values_D1[highlight_D1()[1], "CoMY_D1"] - Values_D4[highlight_D4()[3], "cor_CoMY_D4"])^2)^(1/2)
    cos_alfa <- -(side_a^2 - side_b^2 - side_c^2) / (2 * side_b * side_c)
    alfa <- acos(cos_alfa)
    Values_D4[, "cor_CoMX_D4"] <- Values_D4[, "cor_CoMX_D4"] * cos(-alfa) - Values_D4[, "cor_CoMY_D4"] * sin(-alfa)
    Values_D4[, "cor_CoMY_D4"] <- Values_D4[, "cor_CoMX_D4"] * sin(alfa) + Values_D4[, "cor_CoMY_D4"] * cos(alfa)
    
    width_D1 <- Values_D1[highlight_D1()[2], "CoMX_D1"] - Values_D1[highlight_D1()[1], "CoMX_D1"]
    width_D4 <- Values_D4[highlight_D4()[2], "cor_CoMX_D4"] - Values_D4[highlight_D4()[1], "cor_CoMX_D4"]
    Values_D4[, "cor_CoMX_D4"] <- (Values_D4[, "cor_CoMX_D4"] * (width_D1 / width_D4)) + 
      (Values_D4[highlight_D4()[1], "cor_CoMX_D4"] * (width_D1 / width_D4))
    height_D1 <- Values_D1[highlight_D1()[3], "CoMY_D1"] - Values_D1[highlight_D1()[1], "CoMY_D1"]
    height_D4 <- Values_D4[highlight_D4()[3], "cor_CoMY_D4"] - Values_D4[highlight_D4()[1], "cor_CoMY_D4"]
    Values_D4[, "cor_CoMY_D4"] <- Values_D4[, "cor_CoMY_D4"] * (height_D1 / height_D4) + 
      (Values_D4[highlight_D4()[1], "cor_CoMY_D4"] * (height_D1 / height_D4))
    
    # Re-iterate the move only
    diff_x <- Values_D4[highlight_D4()[1], "cor_CoMX_D4"] - Values_D1[highlight_D1()[1], "CoMX_D1"]
    diff_y <- Values_D4[highlight_D4()[1], "cor_CoMY_D4"] - Values_D1[highlight_D1()[1], "CoMY_D1"]
    Values_D4[, "cor_CoMX_D4"] <- Values_D4[, "cor_CoMX_D4"] - diff_x
    Values_D4[, "cor_CoMY_D4"] <- Values_D4[, "cor_CoMY_D4"] - diff_y
    
    # Correction for distortion around the right highlight cell
    dist_to_right <- ((((Values_D4[, "cor_CoMX_D4"] - Values_D4[highlight_D4()[2], "cor_CoMX_D4"])^2) + 
                         ((Values_D4[, "cor_CoMY_D4"] - Values_D4[highlight_D4()[2], "cor_CoMY_D4"])^2))^(1/2))
    dist_to_right <- dist_to_right / max(dist_to_right) + 0.1 # +0.1 is to not have 0 values, important for no error when taking log
    
    distortion <- (Values_D4[highlight_D4()[2], "cor_CoMY_D4"] - Values_D1[highlight_D1()[2], "CoMY_D1"]) * 
      (-log10(dist_to_right))
    
    Values_D4[, "cor_CoMY_D4"] <- Values_D4[, "cor_CoMY_D4"] - distortion
    print("this is another text:")
    print(str(Values_D4))
    list(Values_D1 = Values_D1, Values_D4 = Values_D4)
  })
  
  stomata_test3 <- reactive({
    req(Values_D3(), test3()$Values_D4, data4_D4())
    
    Values_D4 <- test3()$Values_D4
    Values_D3 <- Values_D3()
    
    print("this is Values D4 test: ")
    print(str(Values_D4))
    print("This is Values D3 test: ")
    print(str(Values_D3))
    
    #First, the stomata are extracted from the datasets (again only D1 and D2)
    # Subset Values_D3 to include both "Stom" and "NEW_Stom"
    Stomata_D3 <- subset(Values_D3[Values_D3$Type_D3 == "Stom" | Values_D3$Type_D3 == "NEW_Stom",])
    # Update column names
    colnames(Stomata_D3) <- c("St_cellid_D3", "St_area_D3", "St_old_CoMX_D3", "St_old_CoMY_D3", "St_Peri_D3", "St_area_px_D3", "St_Circ_D3", "St_Type_D3", "St_CoMX_D3", "St_CoMY_D3")
    Stomata_D4<-subset(Values_D4[Values_D4$Type_D4=="Stom",])
    #colnames(Stomata_D2)<-c("St_cellid_D2","St_area_D2","St_old_CoMX_D2", "St_old_CoMY_D2","St_Peri_D2","St_area_px_D2", "St_Circ_D2", "St_Type_D2", "St_CoMX_D2","St_CoMY_D2")
    colnames(Stomata_D4)<-c("St_cellid_D4","St_area_D4","St_old_CoMX_D4", "St_old_CoMY_D4","St_Peri_D4","St_area_px_D4", "St_Circ_D4", "St_Type_D4", "St_CoMX_D4","St_CoMY_D4")
    #attach(Stomata_D2)
    St_CoMX_D3_px<-Stomata_D3$St_CoMX_D3#*scale_D2
    St_CoMY_D3_px<-Stomata_D3$St_CoMY_D3#*scale_D2
    St_CoMX_D4_px<-Stomata_D4$St_CoMX_D4#*scale_D3
    St_CoMY_D4_px<-Stomata_D4$St_CoMY_D4#*scale_D3
    
    St_pos_ch<-c()
    St_pc<-c()
    for (j in 1:length(Stomata_D4$St_area_D4)){
      for (i in 1:length(Stomata_D3$St_area_D3)){
        St_pc<-(((((St_CoMX_D4_px[j]-St_CoMX_D3_px[i])*2)^2)+((St_CoMY_D4_px[j]-St_CoMY_D3_px[i])^2))^(1/2))
        St_pos_ch<-c(St_pos_ch,St_pc)
        St_pc<-c()
      }
    }
    St_scores<-St_pos_ch
    length(St_scores)
    St_scoretable<-matrix(St_scores,nrow=length(Stomata_D4$St_cellid_D4), ncol=length(Stomata_D3$St_cellid_D3), byrow=TRUE)
    #View(St_scoretable)
    St_bestmatch<-apply(St_scoretable, 1, which.min)
    for (i in 1:length(St_bestmatch)){
      Stomata_D4$D4_St_PrevID[i]<-Stomata_D3$St_cellid_D3[St_bestmatch[i]]
      Stomata_D4$D4_St_score[i]<-min(St_scoretable[i,])
    }
    
    PrevID_all_dbl_St<-unique(Stomata_D4[duplicated(Stomata_D4$D4_St_PrevID),11])
    length(PrevID_all_dbl_St)#Number of Stomata on D2 that gave rise to more than one Stomata on D3
    
    PrevID_dbl_St<-c()
    dbl_St_id<-c()
    dbl_St_scores<-c()
    old_St<-c()
    new_St<-c()
    for (i in 1:length(PrevID_all_dbl_St)){
      PrevID_dbl_St<-PrevID_all_dbl_St[i]
      dbl_St_id<-Stomata_D4[Stomata_D4$D4_St_PrevID==PrevID_dbl_St,1]
      dbl_St_scores<-Stomata_D4[Stomata_D4$D4_St_PrevID==PrevID_dbl_St,12]
      print(dbl_St_scores)
      old_St<-match(min(dbl_St_scores),dbl_St_scores)
      for (j in 1:length(dbl_St_id)){
        if (j != old_St){
          new_St<-dbl_St_id[j]
          print(new_St)
          Values_D4[Values_D4$cellid_D4==new_St,8]<-"NEW_Stom"
        }
      }
    }
    table(Values_D4$Type_D4)
    
    D4_PrevID_st <- c()
    
    for (i in 1:length(Values_D4$cellid_D4)){
      if (Values_D4$Type_D4[i]=="PC" | Values_D4$Type_D4[i]=="Highlight"){
        D4_PrevID_st <- c(D4_PrevID_st, 1)
      }
      if (Values_D4$Type_D4[i]=="NEW_Stom"){
        D4_PrevID_st <- c(D4_PrevID_st, Values_D4$cellid_D4[i])
      }
      if (Values_D4$Type_D4[i]=="Stom"){
        cellnr <- Values_D4$cellid_D4[i]
        # Hieronder de aanpassing om de ontbrekende waarden te vervangen door NA
        if (is.na(Stomata_D4$D4_St_PrevID[Stomata_D4$St_cellid_D4==cellnr])) {
          D4_PrevID_st <- c(D4_PrevID_st, NA)
        } else {
          D4_PrevID_st <- c(D4_PrevID_st, Values_D3$D3_PrevID_st[Stomata_D4$D4_St_PrevID[Stomata_D4$St_cellid_D4==cellnr]])
        }
      }
    }
    
    # Vervang eventuele NA-waarden door 0 of een andere geschikte waarde, afhankelijk van je gegevens
    D4_PrevID_st[is.na(D4_PrevID_st)] <- 0
    
    Values_D4$D4_PrevID_st <- D4_PrevID_st
    attach(Values_D4)
    colnames(Values_D4)
    OrigID_D4_st<-rep(Values_D4$D4_PrevID_st,areapx_D4)
    length(OrigID_D4_st)
    
    type_list_D4 <- rep(Values_D4$Type_D4, Values_D4$areapx_D4)
    sorted_All_D4 <- data4_D4()[order(data4_D4()$ids), ]
    sorted_All_D4$Type <- type_list_D4
    sorted_All_D4$OrigID_st <- OrigID_D4_st
    # Return both Values_D2 and sorted_All_D2 in a list
    list(Values_D4 = Values_D4, sorted_All_D4 = sorted_All_D4)
  })
  
  Values_D4 <- reactive({
    req(stomata_test3())
    
    stomata_result <- stomata_test3()
    
    print("Resultaat van stomata_test3:")
    print(str(stomata_result$Values_D4))
    
    values_D4 <- stomata_result$Values_D4
    
    print("BOEM BAM THIS IS VALUES-D4: ")
    print(str(values_D4))
    
    return(values_D4)
  })
  
  sorted_All_D4 <- reactive({
    req(stomata_test3())
    
    stomata_result <- stomata_test3()
    
    print("Resultaat van stomata_test (sorted_All_D4):")
    print(str(stomata_result$sorted_All_D4))
    
    sorted_all_d4 <- stomata_result$sorted_All_D4
    
    print("BOEM BAM THIS IS sorted_All_D3: ")
    print(str(sorted_all_d4))
    
    return(sorted_all_d4)
  })
  
  cols <- c("Stom" = "chocolate3", "PC" = "aquamarine4", "NEW_Stom" = "darkolivegreen1", "Highlight" = "white")
  
  output$BAM <- renderPlot({
    test_result <- sorted_All_D1()
    plot <- ggplot(test_result, aes(x = Xcoord, y = InvY, colour = Type)) +
      geom_point(size = 0.5) +
      scale_color_manual(values = cols) +
      theme(panel.background = element_rect(fill = "gray27")) +
      theme(panel.grid = element_blank(), legend.position = "none") +
      ggtitle("Day 1") +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title.x=element_blank(), axis.title.y=element_blank())
    plot
  })
  output$BAM2 <- renderPlot({
    sorted_result <- sorted_All_D2()
    plot <- ggplot(sorted_result, aes(x = Xcoord, y = InvY, colour = Type_D2)) +
      geom_point(size = 0.5) +
      scale_color_manual(values = cols) +
      theme(panel.background = element_rect(fill = "gray27")) +
      theme(panel.grid = element_blank(), legend.position = "none") +
      ggtitle("Day 2") +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title.x=element_blank(), axis.title.y=element_blank())
    plot
  })
  output$BAM3 <- renderPlot({
    sorted_result <- sorted_All_D3()
    plot<-ggplot(sorted_result, aes(x = Xcoord, y = InvY, colour = Type)) +
      geom_point(size = 0.5) +
      scale_color_manual(values = cols)+
      theme(panel.background = element_rect(fill = "gray27")) +
      theme(panel.grid = element_blank(), legend.position = "none") +
      ggtitle("Day 3") +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title.x=element_blank(), axis.title.y=element_blank())
    plot
  })
  output$BAM4 <- renderPlot({
    sorted_result <- sorted_All_D4()
    plot<-ggplot(sorted_result, aes(x = Xcoord, y = InvY, colour = Type)) +
      geom_point(size = 0.5) +
      scale_color_manual(values = cols)+
      theme(panel.background = element_rect(fill = "gray27")) +
      theme(panel.grid = element_blank(), legend.position = "none") +
      ggtitle("Day 4") +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title.x=element_blank(), axis.title.y=element_blank())
    plot
  })
  
  alphas<-c("Stom"=1, "PC"=0.1, "NEW_Stom"=1, "Highlight"=0)
  colss<-c("1" =  "black","2" =  "antiquewhite2","3" =  "aquamarine","4" =  "aquamarine3","5" =  "azure1","6" =  "azure4","7" =  "bisque1","8" =  "bisque4","9" =  "blue","10" =  "blue3","11" =  "brown","12" =  "brown3","13" =  "burlywood1","14" =  "burlywood4","15" =  "cadetblue2","16" =  "chartreuse","17" =  "chartreuse3","18" =  "chocolate1","19" =  "chocolate4","20" =  "coral2","21" =  "cornflowerblue","22" =  "cornsilk2","23" =  "cyan","24" =  "cyan3","25" =  "darkcyan","26" =  "darkgoldenrod2","27" =  "darkgray","28" =  "darkkhaki","29" =  "darkolivegreen1","30" =  "darkolivegreen4","31" =  "darkorange2","32" =  "darkorchid","33" =  "darkorchid3","34" =  "darksalmon","35" =  "darkseagreen2","36" =  "darkslateblue","37" =  "darkslategray2","38" =  "darkslategrey","39" =  "deeppink","40" =  "deeppink3","41" =  "deepskyblue1","42" =  "deepskyblue4","43" =  "dodgerblue","44" =  "dodgerblue3","45" =  "firebrick1","46" =  "firebrick4","47" =  "gainsboro","48" =  "gold1","49" =  "gold4","50" =  "goldenrod2","51" =  "gray","52" =  "gray17","53" =  "gray35","54" =  "gray53","55" =  "gray71","56" =  "gray92","57" =  "green","58" =  "green3","59" =  "honeydew","60" =  "honeydew3","61" =  "hotpink1","62" =  "hotpink4","63" =  "indianred2","64" =  "ivory","65" =  "ivory3","66" =  "khaki1","67" =  "khaki4","68" =  "lavenderblush1","69" =  "lavenderblush4","70" =  "lemonchiffon1","71" =  "lemonchiffon4","72" =  "lightblue2","73" =  "lightcoral","74" =  "lightcyan2","75" =  "lightgoldenrod","76" =  "lightgoldenrod3","77" =  "lightgray","78" =  "lightpink","79" =  "lightpink3","80" =  "lightsalmon1","81" =  "lightsalmon4","82" =  "lightskyblue1","83" =  "lightskyblue4","84" =  "lightslategrey","85" =  "lightsteelblue2","86" =  "lightyellow","87" =  "lightyellow3","88" =  "linen","89" =  "magenta2","90" =  "maroon","91" =  "maroon3","92" =  "mediumblue","93" =  "mediumorchid2","94" =  "mediumpurple","95" =  "mediumpurple3","96" =  "mediumslateblue","97" =  "mediumvioletred","98" =  "mistyrose","99" =  "mistyrose3","100" =  "navajowhite","101" =  "navajowhite3","102" =  "navyblue","103" =  "olivedrab1","104" =  "olivedrab4","105" =  "orange2","106" =  "orangered","107" =  "orangered3","108" =  "orchid1","109" =  "orchid4","110" =  "palegreen1","111" =  "palegreen4","112" =  "paleturquoise2","113" =  "palevioletred","114" =  "palevioletred3","115" =  "peachpuff","116" =  "peachpuff3","117" =  "pink","118" =  "pink3","119" =  "plum1","120" =  "plum4","121" =  "purple1","122" =  "purple4","123" =  "red2","124" =  "rosybrown","125" =  "rosybrown3","126" =  "royalblue1","127" =  "royalblue4","128" =  "salmon1","129" =  "salmon4","130" =  "seagreen1","131" =  "seagreen4","132" =  "seashell2","133" =  "sienna","134" =  "sienna3","135" =  "skyblue1","136" =  "skyblue4","137" =  "slateblue2","138" =  "slategray","139" =  "slategray3","140" =  "snow","141" =  "snow3","142" =  "springgreen1","143" =  "springgreen4","144" =  "steelblue2","145" =  "tan","146" =  "tan3","147" =  "thistle1","148" =  "thistle4","149" =  "tomato2","150" =  "turquoise","151" =  "turquoise3","152" =  "violetred","153" =  "violetred3","154" =  "wheat1","155" =  "wheat4","156" =  "yellow1","157" =  "yellow4","158" =  "aliceblue","159" =  "antiquewhite2","160" =  "aquamarine","161" =  "aquamarine3","162" =  "azure1","163" =  "azure4","164" =  "bisque1","165" =  "bisque4","166" =  "blue","167" =  "blue3","168" =  "brown","169" =  "brown3","170" =  "burlywood1","171" =  "burlywood4","172" =  "cadetblue2","173" =  "chartreuse","174" =  "chartreuse3","175" =  "chocolate1","176" =  "chocolate4","177" =  "coral2","178" =  "cornflowerblue","179" =  "cornsilk2","180" =  "cyan","181" =  "cyan3","182" =  "darkcyan","183" =  "darkgoldenrod2","184" =  "darkgray","185" =  "darkkhaki","186" =  "darkolivegreen1","187" =  "darkolivegreen4","188" =  "darkorange2","189" =  "darkorchid","190" =  "darkorchid3","191" =  "darksalmon","192" =  "darkseagreen2","193" =  "darkslateblue",
           "194" =  "darkslategray2","195" =  "darkslategrey","196" =  "deeppink","197" =  "deeppink3","198" =  "deepskyblue1","199" =  "deepskyblue4","200" =  "dodgerblue","201" =  "dodgerblue3","202" =  "firebrick1","203" =  "firebrick4","204" =  "gainsboro","205" =  "gold1","206" =  "gold4","207" =  "goldenrod2","208" =  "gray","209" =  "gray17","210" =  "gray35","211" =  "gray53","212" =  "gray71","213" =  "gray92","214" =  "green","215" =  "green3","216" =  "honeydew","217" =  "honeydew3","218" =  "hotpink1","219" =  "hotpink4","220" =  "indianred2","221" =  "ivory","222" =  "ivory3","223" =  "khaki1","224" =  "khaki4","225" =  "lavenderblush1","226" =  "lavenderblush4","227" =  "lemonchiffon1","228" =  "lemonchiffon4","229" =  "lightblue2","230" =  "lightcoral","231" =  "lightcyan2","232" =  "lightgoldenrod","233" =  "lightgoldenrod3","234" =  "lightgray","235" =  "lightpink","236" =  "lightpink3","237" =  "lightsalmon1","238" =  "lightsalmon4","239" =  "lightskyblue1","240" =  "lightskyblue4","241" =  "lightslategrey","242" =  "lightsteelblue2","243" =  "lightyellow","244" =  "lightyellow3","245" =  "linen","246" =  "magenta2","247" =  "maroon","248" =  "maroon3","249" =  "mediumblue","250" =  "mediumorchid2","251" =  "mediumpurple","252" =  "mediumpurple3","253" =  "mediumslateblue","254" =  "mediumvioletred","255" =  "mistyrose","256" =  "mistyrose3","257" =  "navajowhite","258" =  "navajowhite3","259" =  "navyblue","260" =  "olivedrab1","261" =  "olivedrab4","262" =  "orange2","263" =  "orangered","264" =  "orangered3","265" =  "orchid1","266" =  "orchid4","267" =  "palegreen1","268" =  "palegreen4","269" =  "paleturquoise2","270" =  "palevioletred","271" =  "palevioletred3","272" =  "peachpuff","273" =  "peachpuff3","274" =  "pink","275" =  "pink3","276" =  "plum1","277" =  "plum4","278" =  "purple1","279" =  "purple4","280" =  "red2","281" =  "rosybrown","282" =  "rosybrown3","283" =  "royalblue1","284" =  "royalblue4","285" =  "salmon1","286" =  "salmon4","287" =  "seagreen1","288" =  "seagreen4","289" =  "seashell2","290" =  "sienna","291" =  "sienna3","292" =  "skyblue1","293" =  "skyblue4","294" =  "slateblue2","295" =  "slategray","296" =  "slategray3","297" =  "snow","298" =  "snow3","299" =  "springgreen1","300" =  "springgreen4","301" =  "steelblue2","302" =  "tan","303" =  "tan3","304" =  "thistle1","305" =  "thistle4","306" =  "tomato2","307" =  "turquoise","308" =  "turquoise3","309" =  "violetred","310" =  "violetred3","311" =  "wheat1","312" =  "wheat4","313" =  "yellow1","314" =  "yellow4","315" =  "aliceblue","316" =  "antiquewhite2","317" =  "aquamarine","318" =  "aquamarine3","319" =  "azure1","320" =  "azure4","321" =  "bisque1","322" =  "bisque4","323" =  "blue","324" =  "blue3","325" =  "brown","326" =  "brown3","327" =  "burlywood1","328" =  "burlywood4","329" =  "cadetblue2","330" =  "chartreuse","331" =  "chartreuse3","332" =  "chocolate1","333" =  "chocolate4","334" =  "coral2","335" =  "cornflowerblue","336" =  "cornsilk2","337" =  "cyan","338" =  "cyan3","339" =  "darkcyan","340" =  "darkgoldenrod2","341" =  "darkgray","342" =  "darkkhaki","343" =  "darkolivegreen1","344" =  "darkolivegreen4","345" =  "darkorange2","346" =  "darkorchid","347" =  "darkorchid3","348" =  "darksalmon","349" =  "darkseagreen2","350" =  "darkslateblue","351" =  "darkslategray2","352" =  "darkslategrey","353" =  "deeppink","354" =  "deeppink3","355" =  "deepskyblue1","356" =  "deepskyblue4","357" =  "dodgerblue","358" =  "dodgerblue3","359" =  "firebrick1","360" =  "firebrick4","361" =  "gainsboro","362" =  "gold1","363" =  "gold4","364" =  "goldenrod2","365" =  "gray","366" =  "gray17","367" =  "gray35","368" =  "gray53","369" =  "gray71","370" =  "gray92","371" =  "green","372" =  "green3","373" =  "honeydew","374" =  "honeydew3","375" =  "hotpink1","376" =  "hotpink4","377" =  "indianred2","378" =  "ivory","379" =  "ivory3","380" =  "khaki1","381" =  "khaki4","382" =  "lavenderblush1",
           "383" =  "lavenderblush4","384" =  "lemonchiffon1","385" =  "lemonchiffon4","386" =  "lightblue2","387" =  "lightcoral","388" =  "lightcyan2","389" =  "lightgoldenrod","390" =  "lightgoldenrod3","391" =  "lightgray","392" =  "lightpink","393" =  "lightpink3","394" =  "lightsalmon1","395" =  "lightsalmon4","396" =  "lightskyblue1","397" =  "lightskyblue4","398" =  "lightslategrey","399" =  "lightsteelblue2","400" =  "lightyellow","401" =  "lightyellow3","402" =  "linen","403" =  "magenta2","404" =  "maroon","405" =  "maroon3","406" =  "mediumblue","407" =  "mediumorchid2","408" =  "mediumpurple","409" =  "mediumpurple3","410" =  "mediumslateblue","411" =  "mediumvioletred","412" =  "mistyrose","413" =  "mistyrose3","414" =  "navajowhite","415" =  "navajowhite3","416" =  "navyblue","417" =  "olivedrab1","418" =  "olivedrab4","419" =  "orange2","420" =  "orangered","421" =  "orangered3","422" =  "orchid1","423" =  "orchid4","424" =  "palegreen1","425" =  "palegreen4","426" =  "paleturquoise2","427" =  "palevioletred","428" =  "palevioletred3","429" =  "peachpuff","430" =  "peachpuff3","431" =  "pink","432" =  "pink3","433" =  "plum1","434" =  "plum4","435" =  "purple1","436" =  "purple4","437" =  "red2","438" =  "rosybrown","439" =  "rosybrown3","440" =  "royalblue1","441" =  "royalblue4","442" =  "salmon1","443" =  "salmon4","444" =  "seagreen1","445" =  "seagreen4","446" =  "seashell2","447" =  "sienna","448" =  "sienna3","449" =  "skyblue1","450" =  "skyblue4","451" =  "slateblue2","452" =  "slategray","453" =  "slategray3","454" =  "snow","455" =  "snow3","456" =  "springgreen1","457" =  "springgreen4","458" =  "steelblue2","459" =  "tan","460" =  "tan3","461" =  "thistle1","462" =  "thistle4","463" =  "tomato2","464" =  "turquoise","465" =  "turquoise3","466" =  "violetred","467" =  "violetred3","468" =  "wheat1","469" =  "wheat4","470" =  "yellow1","471" =  "yellow4","472" =  "aliceblue","473" =  "antiquewhite2","474" =  "aquamarine","475" =  "aquamarine3","476" =  "azure1","477" =  "azure4","478" =  "bisque1","479" =  "bisque4","480" =  "blue","481" =  "blue3","482" =  "brown","483" =  "brown3","484" =  "burlywood1","485" =  "burlywood4","486" =  "cadetblue2","487" =  "chartreuse","488" =  "chartreuse3","489" =  "chocolate1","490" =  "chocolate4","491" =  "coral2","492" =  "cornflowerblue","493" =  "cornsilk2","494" =  "cyan","495" =  "cyan3","496" =  "darkcyan","497" =  "darkgoldenrod2","498" =  "darkgray","499" =  "darkkhaki","500" =  "darkolivegreen1","501" =  "darkolivegreen4","502" =  "darkorange2","503" =  "darkorchid","504" =  "darkorchid3","505" =  "darksalmon","506" =  "darkseagreen2","507" =  "darkslateblue","508" =  "darkslategray2","509" =  "darkslategrey","510" =  "deeppink","511" =  "deeppink3","512" =  "deepskyblue1","513" =  "deepskyblue4","514" =  "dodgerblue","515" =  "dodgerblue3","516" =  "firebrick1","517" =  "firebrick4","518" =  "gainsboro","519" =  "gold1","520" =  "gold4","521" =  "goldenrod2","522" =  "gray","523" =  "gray17","524" =  "gray35","525" =  "gray53","526" =  "gray71","527" =  "gray92","528" =  "green","529" =  "green3","530" =  "honeydew","531" =  "honeydew3","532" =  "hotpink1","533" =  "hotpink4","534" =  "indianred2","535" =  "ivory","536" =  "ivory3","537" =  "khaki1","538" =  "khaki4","539" =  "lavenderblush1","540" =  "lavenderblush4","541" =  "lemonchiffon1","542" =  "lemonchiffon4","543" =  "lightblue2","544" =  "lightcoral","545" =  "lightcyan2","546" =  "lightgoldenrod","547" =  "lightgoldenrod3","548" =  "lightgray","549" =  "lightpink","550" =  "lightpink3","551" =  "lightsalmon1","552" =  "lightsalmon4","553" =  "lightskyblue1","554" =  "lightskyblue4","555" =  "lightslategrey","556" =  "lightsteelblue2","557" =  "lightyellow","558" =  "lightyellow3","559" =  "linen","560" =  "magenta2","561" =  "maroon","562" =  "maroon3","563" =  "mediumblue","564" =  "mediumorchid2","565" =  "mediumpurple","566" =  "mediumpurple3","567" =  "mediumslateblue",
           "568" =  "mediumvioletred","569" =  "mistyrose","570" =  "mistyrose3","571" =  "navajowhite","572" =  "navajowhite3","573" =  "navyblue","574" =  "olivedrab1","575" =  "olivedrab4","576" =  "orange2","577" =  "orangered","578" =  "orangered3","579" =  "orchid1","580" =  "orchid4","581" =  "palegreen1","582" =  "palegreen4","583" =  "paleturquoise2","584" =  "palevioletred","585" =  "palevioletred3","586" =  "peachpuff","587" =  "peachpuff3","588" =  "pink","589" =  "pink3","590" =  "plum1","591" =  "plum4","592" =  "purple1","593" =  "purple4","594" =  "red2","595" =  "rosybrown","596" =  "rosybrown3","597" =  "royalblue1","598" =  "royalblue4","599" =  "salmon1","600" =  "salmon4","601" =  "seagreen1","602" =  "seagreen4","603" =  "seashell2","604" =  "sienna","605" =  "sienna3","606" =  "skyblue1","607" =  "skyblue4","608" =  "slateblue2","609" =  "slategray","610" =  "slategray3","611" =  "snow","612" =  "snow3","613" =  "springgreen1","614" =  "springgreen4","615" =  "steelblue2","616" =  "tan","617" =  "tan3","618" =  "thistle1","619" =  "thistle4","620" =  "tomato2","621" =  "turquoise","622" =  "turquoise3","623" =  "violetred","624" =  "violetred3","625" =  "wheat1","626" =  "wheat4","627" =  "yellow1","628" =  "yellow4","629" =  "aliceblue","630" =  "antiquewhite2","631" =  "aquamarine","632" =  "aquamarine3","633" =  "azure1","634" =  "azure4","635" =  "bisque1","636" =  "bisque4","637" =  "blue","638" =  "blue3","639" =  "brown","640" =  "brown3","641" =  "burlywood1","642" =  "burlywood4","643" =  "cadetblue2","644" =  "chartreuse","645" =  "chartreuse3","646" =  "chocolate1","647" =  "chocolate4","648" =  "coral2","649" =  "cornflowerblue","650" =  "cornsilk2","651" =  "cyan","652" =  "cyan3","653" =  "darkcyan","654" =  "darkgoldenrod2","655" =  "darkgray","656" =  "darkkhaki","657" =  "darkolivegreen1","658" =  "darkolivegreen4","659" =  "darkorange2","660" =  "darkorchid","661" =  "darkorchid3","662" =  "darksalmon","663" =  "darkseagreen2","664" =  "darkslateblue","665" =  "darkslategray2","666" =  "darkslategrey","667" =  "deeppink","668" =  "deeppink3","669" =  "deepskyblue1","670" =  "deepskyblue4","671" =  "dodgerblue","672" =  "dodgerblue3","673" =  "firebrick1","674" =  "firebrick4","675" =  "gainsboro","676" =  "gold1","677" =  "gold4","678" =  "goldenrod2","679" =  "gray","680" =  "gray17","681" =  "gray35","682" =  "gray53","683" =  "gray71","684" =  "gray92","685" =  "green","686" =  "green3","687" =  "honeydew","688" =  "honeydew3","689" =  "hotpink1","690" =  "hotpink4","691" =  "indianred2","692" =  "ivory","693" =  "ivory3","694" =  "khaki1","695" =  "khaki4","696" =  "lavenderblush1","697" =  "lavenderblush4","698" =  "lemonchiffon1","699" =  "lemonchiffon4","700" =  "lightblue2","701" =  "lightcoral","702" =  "lightcyan2","703" =  "lightgoldenrod","704" =  "lightgoldenrod3","705" =  "lightgray","706" =  "lightpink","707" =  "lightpink3","708" =  "lightsalmon1","709" =  "lightsalmon4","710" =  "lightskyblue1","711" =  "lightskyblue4","712" =  "lightslategrey","713" =  "lightsteelblue2","714" =  "lightyellow","715" =  "lightyellow3","716" =  "linen","717" =  "magenta2","718" =  "maroon","719" =  "maroon3","720" =  "mediumblue","721" =  "mediumorchid2","722" =  "mediumpurple","723" =  "mediumpurple3","724" =  "mediumslateblue","725" =  "mediumvioletred","726" =  "mistyrose","727" =  "mistyrose3","728" =  "navajowhite","729" =  "navajowhite3","730" =  "navyblue","731" =  "olivedrab1","732" =  "olivedrab4","733" =  "orange2","734" =  "orangered","735" =  "orangered3","736" =  "orchid1","737" =  "orchid4","738" =  "palegreen1","739" =  "palegreen4","740" =  "paleturquoise2","741" =  "palevioletred","742" =  "palevioletred3","743" =  "peachpuff","744" =  "peachpuff3","745" =  "pink","746" =  "pink3","747" =  "plum1","748" =  "plum4","749" =  "purple1","750" =  "purple4","751" =  "red2","752" =  "rosybrown","753" =  "rosybrown3","754" =  "royalblue1","755" =  "royalblue4",
           "756" =  "salmon1","757" =  "salmon4","758" =  "seagreen1","759" =  "seagreen4","760" =  "seashell2","761" =  "sienna","762" =  "sienna3","763" =  "skyblue1","764" =  "skyblue4","765" =  "slateblue2","766" =  "slategray","767" =  "slategray3","768" =  "snow","769" =  "snow3","770" =  "springgreen1","771" =  "springgreen4","772" =  "steelblue2","773" =  "tan","774" =  "tan3","775" =  "thistle1","776" =  "thistle4","777" =  "tomato2","778" =  "turquoise","779" =  "turquoise3","780" =  "violetred","781" =  "violetred3","782" =  "wheat1","783" =  "wheat4","784" =  "yellow1","785" =  "yellow4","786" =  "aliceblue","787" =  "antiquewhite2","788" =  "aquamarine","789" =  "aquamarine3","790" =  "azure1","791" =  "azure4","792" =  "bisque1","793" =  "bisque4","794" =  "blue","795" =  "blue3","796" =  "brown","797" =  "brown3","798" =  "burlywood1","799" =  "burlywood4","800" =  "cadetblue2","801" =  "chartreuse","802" =  "chartreuse3","803" =  "chocolate1","804" =  "chocolate4","805" =  "coral2","806" =  "cornflowerblue","807" =  "cornsilk2","808" =  "cyan","809" =  "cyan3","810" =  "darkcyan","811" =  "darkgoldenrod2","812" =  "darkgray","813" =  "darkkhaki","814" =  "darkolivegreen1","815" =  "darkolivegreen4","816" =  "darkorange2","817" =  "darkorchid","818" =  "darkorchid3","819" =  "darksalmon","820" =  "darkseagreen2","821" =  "darkslateblue","822" =  "darkslategray2","823" =  "darkslategrey","824" =  "deeppink","825" =  "deeppink3","826" =  "deepskyblue1","827" =  "deepskyblue4","828" =  "dodgerblue","829" =  "dodgerblue3","830" =  "firebrick1","831" =  "firebrick4","832" =  "gainsboro","833" =  "gold1","834" =  "gold4","835" =  "goldenrod2","836" =  "gray","837" =  "gray17","838" =  "gray35","839" =  "gray53","840" =  "gray71","841" =  "gray92","842" =  "green","843" =  "green3","844" =  "honeydew","845" =  "honeydew3","846" =  "hotpink1","847" =  "hotpink4","848" =  "indianred2","849" =  "ivory","850" =  "ivory3","851" =  "khaki1","852" =  "khaki4","853" =  "lavenderblush1","854" =  "lavenderblush4","855" =  "lemonchiffon1","856" =  "lemonchiffon4","857" =  "lightblue2","858" =  "lightcoral","859" =  "lightcyan2","860" =  "lightgoldenrod","861" =  "lightgoldenrod3","862" =  "lightgray","863" =  "lightpink","864" =  "lightpink3","865" =  "lightsalmon1","866" =  "lightsalmon4","867" =  "lightskyblue1","868" =  "lightskyblue4","869" =  "lightslategrey","870" =  "lightsteelblue2","871" =  "lightyellow","872" =  "lightyellow3","873" =  "linen","874" =  "magenta2","875" =  "maroon","876" =  "maroon3","877" =  "mediumblue","878" =  "mediumorchid2","879" =  "mediumpurple","880" =  "mediumpurple3","881" =  "mediumslateblue","882" =  "mediumvioletred","883" =  "mistyrose","884" =  "mistyrose3","885" =  "navajowhite","886" =  "navajowhite3","887" =  "navyblue","888" =  "olivedrab1","889" =  "olivedrab4","890" =  "orange2","891" =  "orangered","892" =  "orangered3","893" =  "orchid1","894" =  "orchid4","895" =  "palegreen1","896" =  "palegreen4","897" =  "paleturquoise2","898" =  "palevioletred","899" =  "palevioletred3","900" =  "peachpuff","901" =  "peachpuff3","902" =  "pink","903" =  "pink3","904" =  "plum1","905" =  "plum4","906" =  "purple1","907" =  "purple4","908" =  "red2","909" =  "rosybrown","910" =  "rosybrown3","911" =  "royalblue1","912" =  "royalblue4","913" =  "salmon1","914" =  "salmon4","915" =  "seagreen1","916" =  "seagreen4","917" =  "seashell2","918" =  "sienna","919" =  "sienna3","920" =  "skyblue1","921" =  "skyblue4","922" =  "slateblue2","923" =  "slategray","924" =  "slategray3","925" =  "snow","926" =  "snow3","927" =  "springgreen1","928" =  "springgreen4","929" =  "steelblue2","930" =  "tan","931" =  "tan3","932" =  "thistle1","933" =  "thistle4","934" =  "tomato2","935" =  "turquoise","936" =  "turquoise3","937" =  "violetred","938" =  "violetred3","939" =  "wheat1","940" =  "wheat4","941" =  "yellow1","942" =  "yellow4","943" =  "aliceblue","944" =  "antiquewhite2",
           "945" =  "aquamarine","946" =  "aquamarine3","947" =  "azure1","948" =  "azure4","949" =  "bisque1","950" =  "bisque4","951" =  "blue","952" =  "blue3","953" =  "brown","954" =  "brown3","955" =  "burlywood1","956" =  "burlywood4","957" =  "cadetblue2","958" =  "chartreuse","959" =  "chartreuse3","960" =  "chocolate1","961" =  "chocolate4","962" =  "coral2","963" =  "cornflowerblue","964" =  "cornsilk2","965" =  "cyan","966" =  "cyan3","967" =  "darkcyan","968" =  "darkgoldenrod2","969" =  "darkgray","970" =  "darkkhaki","971" =  "darkolivegreen1","972" =  "darkolivegreen4","973" =  "darkorange2","974" =  "darkorchid","975" =  "darkorchid3","976" =  "darksalmon","977" =  "darkseagreen2","978" =  "darkslateblue","979" =  "darkslategray2","980" =  "darkslategrey","981" =  "deeppink","982" =  "deeppink3","983" =  "deepskyblue1","984" =  "deepskyblue4","985" =  "dodgerblue","986" =  "dodgerblue3","987" =  "firebrick1","988" =  "firebrick4","989" =  "gainsboro","990" =  "gold1","991" =  "gold4","992" =  "goldenrod2","993" =  "gray","994" =  "gray17","995" =  "gray35","996" =  "gray53","997" =  "gray71","998" =  "gray92","999" =  "green","1000" =  "green3","1001" =  "honeydew","1002" =  "honeydew3","1003" =  "hotpink1","1004" =  "hotpink4","1005" =  "indianred2","1006" =  "ivory","1007" =  "ivory3","1008" =  "khaki1","1009" =  "khaki4","1010" =  "lavenderblush1","1011" =  "lavenderblush4","1012" =  "lemonchiffon1","1013" =  "lemonchiffon4","1014" =  "lightblue2","1015" =  "lightcoral","1016" =  "lightcyan2","1017" =  "lightgoldenrod","1018" =  "lightgoldenrod3","1019" =  "lightgray","1020" =  "lightpink","1021" =  "lightpink3","1022" =  "lightsalmon1","1023" =  "lightsalmon4","1024" =  "lightskyblue1","1025" =  "lightskyblue4","1026" =  "lightslategrey","1027" =  "lightsteelblue2","1028" =  "lightyellow","1029" =  "lightyellow3","1030" =  "linen","1031" =  "magenta2","1032" =  "maroon","1033" =  "maroon3","1034" =  "mediumblue","1035" =  "mediumorchid2","1036" =  "mediumpurple","1037" =  "mediumpurple3","1038" =  "mediumslateblue","1039" =  "mediumvioletred","1040" =  "mistyrose","1041" =  "mistyrose3","1042" =  "navajowhite","1043" =  "navajowhite3","1044" =  "navyblue","1045" =  "olivedrab1","1046" =  "olivedrab4","1047" =  "orange2","1048" =  "orangered","1049" =  "orangered3","1050" =  "orchid1","1051" =  "orchid4","1052" =  "palegreen1","1053" =  "palegreen4","1054" =  "paleturquoise2","1055" =  "palevioletred","1056" =  "palevioletred3","1057" =  "peachpuff","1058" =  "peachpuff3","1059" =  "pink","1060" =  "pink3","1061" =  "plum1","1062" =  "plum4","1063" =  "purple1","1064" =  "purple4","1065" =  "red2","1066" =  "rosybrown","1067" =  "rosybrown3","1068" =  "royalblue1","1069" =  "royalblue4","1070" =  "salmon1","1071" =  "salmon4","1072" =  "seagreen1","1073" =  "seagreen4","1074" =  "seashell2","1075" =  "sienna","1076" =  "sienna3","1077" =  "skyblue1","1078" =  "skyblue4","1079" =  "slateblue2","1080" =  "slategray","1081" =  "slategray3","1082" =  "snow","1083" =  "snow3","1084" =  "springgreen1","1085" =  "springgreen4","1086" =  "steelblue2","1087" =  "tan","1088" =  "tan3","1089" =  "thistle1","1090" =  "thistle4","1091" =  "tomato2","1092" =  "turquoise","1093" =  "turquoise3","1094" =  "violetred","1095" =  "violetred3","1096" =  "wheat1","1097" =  "wheat4","1098" =  "yellow1","1099" =  "yellow4","1100" =  "aliceblue","1101" =  "antiquewhite2","1102" =  "aquamarine","1103" =  "aquamarine3","1104" =  "azure1","1105" =  "azure4","1106" =  "bisque1","1107" =  "bisque4","1108" =  "blue","1109" =  "blue3","1110" =  "brown","1111" =  "brown3","1112" =  "burlywood1","1113" =  "burlywood4","1114" =  "cadetblue2","1115" =  "chartreuse","1116" =  "chartreuse3","1117" =  "chocolate1","1118" =  "chocolate4","1119" =  "coral2","1120" =  "cornflowerblue","1121" =  "cornsilk2","1122" =  "cyan","1123" =  "cyan3","1124" =  "darkcyan","1125" =  "darkgoldenrod2","1126" =  "darkgray","1127" =  "darkkhaki",
           "1128" =  "darkolivegreen1","1129" =  "darkolivegreen4","1130" =  "darkorange2","1131" =  "darkorchid","1132" =  "darkorchid3","1133" =  "darksalmon","1134" =  "darkseagreen2","1135" =  "darkslateblue","1136" =  "darkslategray2","1137" =  "darkslategrey","1138" =  "deeppink","1139" =  "deeppink3","1140" =  "deepskyblue1","1141" =  "deepskyblue4","1142" =  "dodgerblue","1143" =  "dodgerblue3","1144" =  "firebrick1","1145" =  "firebrick4","1146" =  "gainsboro","1147" =  "gold1","1148" =  "gold4","1149" =  "goldenrod2","1150" =  "gray","1151" =  "gray17","1152" =  "gray35","1153" =  "gray53","1154" =  "gray71","1155" =  "gray92","1156" =  "green","1157" =  "green3","1158" =  "honeydew","1159" =  "honeydew3","1160" =  "hotpink1","1161" =  "hotpink4","1162" =  "indianred2","1163" =  "ivory","1164" =  "ivory3","1165" =  "khaki1","1166" =  "khaki4","1167" =  "lavenderblush1","1168" =  "lavenderblush4","1169" =  "lemonchiffon1","1170" =  "lemonchiffon4","1171" =  "lightblue2","1172" =  "lightcoral","1173" =  "lightcyan2","1174" =  "lightgoldenrod","1175" =  "lightgoldenrod3","1176" =  "lightgray","1177" =  "lightpink","1178" =  "lightpink3","1179" =  "lightsalmon1","1180" =  "lightsalmon4","1181" =  "lightskyblue1","1182" =  "lightskyblue4","1183" =  "lightslategrey","1184" =  "lightsteelblue2","1185" =  "lightyellow","1186" =  "lightyellow3","1187" =  "linen","1188" =  "magenta2","1189" =  "maroon","1190" =  "maroon3","1191" =  "mediumblue","1192" =  "mediumorchid2","1193" =  "mediumpurple","1194" =  "mediumpurple3","1195" =  "mediumslateblue","1196" =  "mediumvioletred","1197" =  "mistyrose","1198" =  "mistyrose3","1199" =  "navajowhite","1200" =  "navajowhite3","1201" =  "navyblue","1202" =  "olivedrab1","1203" =  "olivedrab4","1204" =  "orange2","1205" =  "orangered","1206" =  "orangered3","1207" =  "orchid1","1208" =  "orchid4","1209" =  "palegreen1","1210" =  "palegreen4","1211" =  "paleturquoise2","1212" =  "palevioletred","1213" =  "palevioletred3","1214" =  "peachpuff","1215" =  "peachpuff3","1216" =  "pink","1217" =  "pink3","1218" =  "plum1","1219" =  "plum4","1220" =  "purple1","1221" =  "purple4","1222" =  "red2","1223" =  "rosybrown","1224" =  "rosybrown3","1225" =  "royalblue1","1226" =  "royalblue4","1227" =  "salmon1","1228" =  "salmon4","1229" =  "seagreen1","1230" =  "seagreen4","1231" =  "seashell2","1232" =  "sienna","1233" =  "sienna3","1234" =  "skyblue1","1235" =  "skyblue4","1236" =  "slateblue2","1237" =  "slategray","1238" =  "slategray3","1239" =  "snow","1240" =  "snow3","1241" =  "springgreen1","1242" =  "springgreen4","1243" =  "steelblue2","1244" =  "tan","1245" =  "tan3","1246" =  "thistle1","1247" =  "thistle4","1248" =  "tomato2","1249" =  "turquoise","1250" =  "turquoise3","1251" =  "violetred","1252" =  "violetred3","1253" =  "wheat1","1254" =  "wheat4","1255" =  "yellow1","1256" =  "yellow4","1257" =  "aliceblue","1258" =  "antiquewhite2","1259" =  "aquamarine","1260" =  "aquamarine3","1261" =  "azure1","1262" =  "azure4","1263" =  "bisque1","1264" =  "bisque4","1265" =  "blue","1266" =  "blue3","1267" =  "brown","1268" =  "brown3","1269" =  "burlywood1","1270" =  "burlywood4","1271" =  "cadetblue2","1272" =  "chartreuse","1273" =  "chartreuse3","1274" =  "chocolate1","1275" =  "chocolate4","1276" =  "coral2","1277" =  "cornflowerblue","1278" =  "cornsilk2","1279" =  "cyan","1280" =  "cyan3","1281" =  "darkcyan","1282" =  "darkgoldenrod2","1283" =  "darkgray","1284" =  "darkkhaki","1285" =  "darkolivegreen1","1286" =  "darkolivegreen4","1287" =  "darkorange2","1288" =  "darkorchid","1289" =  "darkorchid3","1290" =  "darksalmon","1291" =  "darkseagreen2","1292" =  "darkslateblue","1293" =  "darkslategray2","1294" =  "darkslategrey","1295" =  "deeppink","1296" =  "deeppink3","1297" =  "deepskyblue1","1298" =  "deepskyblue4","1299" =  "dodgerblue","1300" =  "dodgerblue3","1301" =  "firebrick1","1302" =  "firebrick4","1303" =  "gainsboro","1304" =  "gold1","1305" =  "gold4",
           "1306" =  "goldenrod2","1307" =  "gray","1308" =  "gray17","1309" =  "gray35","1310" =  "gray53","1311" =  "gray71","1312" =  "gray92","1313" =  "green","1314" =  "green3","1315" =  "honeydew","1316" =  "honeydew3","1317" =  "hotpink1","1318" =  "hotpink4","1319" =  "indianred2","1320" =  "ivory","1321" =  "ivory3","1322" =  "khaki1","1323" =  "khaki4","1324" =  "lavenderblush1","1325" =  "lavenderblush4","1326" =  "lemonchiffon1","1327" =  "lemonchiffon4","1328" =  "lightblue2","1329" =  "lightcoral","1330" =  "lightcyan2","1331" =  "lightgoldenrod","1332" =  "lightgoldenrod3","1333" =  "lightgray","1334" =  "lightpink","1335" =  "lightpink3","1336" =  "lightsalmon1","1337" =  "lightsalmon4","1338" =  "lightskyblue1","1339" =  "lightskyblue4","1340" =  "lightslategrey","1341" =  "lightsteelblue2","1342" =  "lightyellow","1343" =  "lightyellow3","1344" =  "linen","1345" =  "magenta2","1346" =  "maroon","1347" =  "maroon3","1348" =  "mediumblue","1349" =  "mediumorchid2","1350" =  "mediumpurple","1351" =  "mediumpurple3","1352" =  "mediumslateblue","1353" =  "mediumvioletred","1354" =  "mistyrose","1355" =  "mistyrose3","1356" =  "navajowhite","1357" =  "navajowhite3","1358" =  "navyblue","1359" =  "olivedrab1","1360" =  "olivedrab4","1361" =  "orange2","1362" =  "orangered","1363" =  "orangered3","1364" =  "orchid1","1365" =  "orchid4","1366" =  "palegreen1","1367" =  "palegreen4","1368" =  "paleturquoise2","1369" =  "palevioletred","1370" =  "palevioletred3","1371" =  "peachpuff","1372" =  "peachpuff3","1373" =  "pink","1374" =  "pink3","1375" =  "plum1","1376" =  "plum4","1377" =  "purple1","1378" =  "purple4","1379" =  "red2","1380" =  "rosybrown","1381" =  "rosybrown3","1382" =  "royalblue1","1383" =  "royalblue4","1384" =  "salmon1","1385" =  "salmon4","1386" =  "seagreen1","1387" =  "seagreen4","1388" =  "seashell2","1389" =  "sienna","1390" =  "sienna3","1391" =  "skyblue1","1392" =  "skyblue4","1393" =  "slateblue2","1394" =  "slategray","1395" =  "slategray3","1396" =  "snow","1397" =  "snow3","1398" =  "springgreen1","1399" =  "springgreen4","1400" =  "steelblue2","1401" =  "tan","1402" =  "tan3","1403" =  "thistle1","1404" =  "thistle4","1405" =  "tomato2","1406" =  "turquoise","1407" =  "turquoise3","1408" =  "violetred","1409" =  "violetred3","1410" =  "wheat1","1411" =  "wheat4","1412" =  "yellow1","1413" =  "yellow4","1414" =  "aliceblue","1415" =  "antiquewhite2","1416" =  "aquamarine","1417" =  "aquamarine3","1418" =  "azure1","1419" =  "azure4","1420" =  "bisque1","1421" =  "bisque4","1422" =  "blue","1423" =  "blue3","1424" =  "brown","1425" =  "brown3","1426" =  "burlywood1","1427" =  "burlywood4","1428" =  "cadetblue2","1429" =  "chartreuse","1430" =  "chartreuse3","1431" =  "chocolate1","1432" =  "chocolate4","1433" =  "coral2","1434" =  "cornflowerblue","1435" =  "cornsilk2","1436" =  "cyan","1437" =  "cyan3","1438" =  "darkcyan","1439" =  "darkgoldenrod2","1440" =  "darkgray","1441" =  "darkkhaki","1442" =  "darkolivegreen1","1443" =  "darkolivegreen4","1444" =  "darkorange2","1445" =  "darkorchid","1446" =  "darkorchid3","1447" =  "darksalmon","1448" =  "darkseagreen2","1449" =  "darkslateblue","1450" =  "darkslategray2","1451" =  "darkslategrey","1452" =  "deeppink","1453" =  "deeppink3","1454" =  "deepskyblue1","1455" =  "deepskyblue4","1456" =  "dodgerblue","1457" =  "dodgerblue3","1458" =  "firebrick1","1459" =  "firebrick4","1460" =  "gainsboro","1461" =  "gold1","1462" =  "gold4","1463" =  "goldenrod2","1464" =  "gray","1465" =  "gray17","1466" =  "gray35","1467" =  "gray53","1468" =  "gray71","1469" =  "gray92","1470" =  "green","1471" =  "green3","1472" =  "honeydew","1473" =  "honeydew3","1474" =  "hotpink1","1475" =  "hotpink4","1476" =  "indianred2","1477" =  "ivory","1478" =  "ivory3","1479" =  "khaki1","1480" =  "khaki4","1481" =  "lavenderblush1","1482" =  "lavenderblush4","1483" =  "lemonchiffon1","1484" =  "lemonchiffon4","1485" =  "lightblue2",
           "1486" =  "lightcoral","1487" =  "lightcyan2","1488" =  "lightgoldenrod","1489" =  "lightgoldenrod3","1490" =  "lightgray","1491" =  "lightpink","1492" =  "lightpink3","1493" =  "lightsalmon1","1494" =  "lightsalmon4","1495" =  "lightskyblue1","1496" =  "lightskyblue4","1497" =  "lightslategrey","1498" =  "lightsteelblue2","1499" =  "lightyellow","1500" =  "lightyellow3")
  output$BOEM1 <- renderPlot({
    sorted <- Col_id()
    D1 <- ggplot(sorted, aes(x = Xcoord, y = InvY, colour = as.factor(Color_id_D1), alpha = Type)) +
      geom_point(size = 0.5) +
      scale_color_manual(values = colss) +
      scale_alpha_manual(values = alphas) +
      theme(panel.background = element_rect(fill = "gray27")) +
      theme(panel.grid = element_blank(), legend.position = "none") +
      ggtitle("Day 1") +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title.x=element_blank(), axis.title.y=element_blank())
    print(D1)
  })
  output$BOEM2 <- renderPlot({
    sorted <- sorted_All_D2()
    D2 <- ggplot(sorted, aes(x = Xcoord, y = InvY, colour = as.factor(OrigID_st), alpha = Type_D2)) +
      geom_point(size = 0.5) +
      scale_color_manual(values = colss) +
      scale_alpha_manual(values = alphas) +
      theme(panel.background = element_rect(fill = "gray27")) +
      theme(panel.grid = element_blank(), legend.position = "none") +
      ggtitle("Day 2") +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title.x=element_blank(), axis.title.y=element_blank())
    plot(D2)
  })
  output$BOEM3 <- renderPlot({
    sorted_All_D3 <- sorted_All_D3()
    D3 <- ggplot(sorted_All_D3, aes(x = Xcoord, y = InvY, colour = as.factor(OrigID_st), alpha = Type)) +
      geom_point(size = 0.5) +
      scale_color_manual(values = colss) +
      scale_alpha_manual(values = alphas) +
      theme(panel.background = element_rect(fill = "gray27")) +
      theme(panel.grid = element_blank(), legend.position = "none") +
      ggtitle("Day 3") +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title.x=element_blank(), axis.title.y=element_blank())
    plot(D3)
  })
  output$BOEM4 <- renderPlot({
    sorted_All_D4 <- sorted_All_D4()
    D4 <- ggplot(sorted_All_D4, aes(x = Xcoord, y = InvY, colour = as.factor(OrigID_st), alpha = Type)) +
      geom_point(size = 0.5) +
      scale_color_manual(values = colss) +
      scale_alpha_manual(values = alphas) +
      theme(panel.background = element_rect(fill = "gray27")) +
      theme(panel.grid = element_blank(), legend.position = "none") +
      ggtitle("Day 4") +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title.x=element_blank(), axis.title.y=element_blank())
    plot(D4)
  })
  ######################################
  ## PC tracking 
  ######################################
  ### Values_D1 en D2
  tracking <- reactive({
    req(V_D1(), V_D2(), Values_D2(), sorted_All_D2())
    
    V_D1 <- V_D1()
    V_D2 <- V_D2()
    
    colnames(V_D1) <- c("cellid_D1", "area_D1", "CoMX_D1", "CoMY_D1", "Peri_D1", "areapx_D1", "Circ_D1", "Type_D1")
    print("this is V_D1: ")
    print(str(V_D1))
    
    colnames(V_D2) <- c("cellid_D2", "area_D2", "CoMX_D2", "CoMY_D2", "Peri_D2", "areapx_D2", "Circ_D2", "Type_D2")
    print("this is V_D2: ")
    print(str(V_D2))
    
    Values_D2 <- Values_D2()
    print("this is Values_D2: ")
    print(str(Values_D2))
    
    av_area_inc <- (mean(V_D2$area_D2) / mean(V_D1$area_D1))
    av_peri_inc <- (mean(V_D2$Peri_D2) / mean(V_D1$Peri_D1))
    
    CoMX_D1_px <- V_D1$CoMX_D1
    CoMY_D1_px <- V_D1$CoMY_D1
    CoMX_D2_px <- Values_D2$cor_CoMX_D2
    CoMY_D2_px <- Values_D2$cor_CoMY_D2
    av_pc <- ((((mean(CoMX_D2_px) - mean(CoMX_D1_px))^2) + ((mean(CoMY_D2_px) - mean(CoMY_D1_px))^2))^(1/2))
    
    area_rat <- c()
    ar <- c()
    peri_rat <- c()
    pr <- c()
    pos_ch <- c()
    pc <- c()
    shape_ch <- c()
    sc <- c()
    scores <- c()
    score <- c()
    
    for (j in 1:length(V_D2$area_D2)) {
      for (i in 1:length(V_D1$area_D1)) {
        ar <- (V_D2$area_D2[j] / V_D1$area_D1[i]) / av_area_inc
        pc <- ((((CoMX_D2_px[j] - CoMX_D1_px[i])^2) + ((CoMY_D2_px[j] - CoMY_D1_px[i])^2))^(1/2))
        ar <- abs(1 - ar)
        area_rat <- c(area_rat, ar)
        pos_ch <- c(pos_ch, pc)
        score <- pc + 5 * ar
        scores <- c(scores, score)
        ar <- c()
        pr <- c()
        pc <- c()
        sc <- c()
        score <- c()
      }
      if (j %in% c(10, 20, 30, 40, 50, 100, 200, 300, 400, 500)) {
        print(j)
      }
    }
    
    scoretable <- matrix(scores, nrow = length(V_D2$cellid_D2), ncol = length(V_D1$cellid_D1), byrow = TRUE)
    bestmatch <- apply(scoretable, 1, which.min)
    print(str(bestmatch))
    for (i in 1:length(bestmatch)) {
      Values_D2$D2_PrevID[i] <- V_D1$cellid_D1[bestmatch[i]]
    }
    #confidence score
    bestvalue<-c()
    lowvalues<-c()
    for (i in 1:nrow(scoretable)){
      bestvalue<-scoretable[i,bestmatch[i]]
      lowvalues<-c(lowvalues,bestvalue)
    }
    worse<-max(lowvalues)
    inv_lowvalues<-(worse-lowvalues)
    confidence<-rep(inv_lowvalues,Values_D2$areapx_D2)
    
    OrigID_D2 <- rep(Values_D2$D2_PrevID, Values_D2$areapx_D2)
    sorted_All_D2 <- sorted_All_D2()
    sorted_All_D2$OrigID <- OrigID_D2
    sorted_All_D2$confidence <- confidence
    print("Values_D2: ")
    print(head(Values_D2))
    print("sorted_All_D2: ")
    print(str(sorted_All_D2))
    list(sorted_All_D2 = sorted_All_D2, Values_D2 = Values_D2, inv_lowvalues = inv_lowvalues)
  })
  tracking_inv_lowvalues <- reactive({
    req(tracking())
    tracking_result <- tracking()
    inv_lowvalues <- tracking_result$inv_lowvalues
    print("BOEM inv_lowvalues: ")
    print(str(inv_lowvalues))
    return(inv_lowvalues)
  })
  tracking_Values_D2 <- reactive({
    req(tracking())
    tracking_result <- tracking()
    Values_D2 <- tracking_result$Values_D2
    print("BOEM VALUES-D2test: ")
    print(str(Values_D2))
    return(Values_D2)
  })
  tracking_sorted_All_D2 <- reactive({
    req(tracking())
    tracking_result <- tracking()
    sorted_all_D2 <- tracking_result$sorted_All_D2
    print("BOEM BAM THIS IS tracking_result_sorted_All_D2 test: ")
    print(str(sorted_all_D2))
    return(sorted_all_D2)
  })
  
  ### Values_D2 en D3
  tracking1 <- reactive({
    req(V_D3(), V_D2(), tracking_Values_D2(), Values_D3(), sorted_All_D3())
    
    V_D3 <- V_D3()
    V_D2 <- V_D2()
    
    colnames(V_D3) <- c("cellid_D3", "area_D3", "CoMX_D3", "CoMY_D3", "Peri_D3", "areapx_D3", "Circ_D3", "Type_D3")
    print("this is V_D3: ")
    print(str(V_D3))
    
    colnames(V_D2) <- c("cellid_D2", "area_D2", "CoMX_D2", "CoMY_D2", "Peri_D2", "areapx_D2", "Circ_D2", "Type_D2")
    print("this is V_D2: ")
    print(str(V_D2))
    
    Values_D2 <- tracking_Values_D2()
    print("Kijken of dit wel klopt: ")
    print(str(Values_D2))
    Values_D3 <- Values_D3()
    print("this is Values_D3: ")
    print(str(Values_D3))
    
    av_area_inc <- mean(V_D3$area_D3) / mean(V_D2$area_D2)
    av_peri_inc <- mean(V_D3$Peri_D3) / mean(V_D2$Peri_D2)
    
    CoMX_D2_px <- Values_D2$cor_CoMX_D2
    CoMY_D2_px <- Values_D2$cor_CoMY_D2
    CoMX_D3_px <- Values_D3$cor_CoMX_D3
    CoMY_D3_px <- Values_D3$cor_CoMY_D3
    av_pc <- sqrt((mean(CoMX_D3_px) - mean(CoMX_D2_px))^2 + (mean(CoMY_D3_px) - mean(CoMY_D2_px))^2)
    
    scores <- c()
    
    for (j in 1:length(V_D3$area_D3)) {
      for (i in 1:length(V_D2$area_D2)) {
        ar <- abs(1 - (V_D3$area_D3[j] / V_D2$area_D2[i]) / av_area_inc)
        pc <- sqrt((CoMX_D3_px[j] - CoMX_D2_px[i])^2 + (CoMY_D3_px[j] - CoMY_D2_px[i])^2)
        score <- pc + 5 * ar
        scores <- c(scores, score)
      }
      if (j %in% c(10, 20, 30, 40, 50, 100, 200, 300, 400, 500)) {
        print(j)
      }
    }
    
    scoretable <- matrix(scores, nrow = length(V_D3$cellid_D3), ncol = length(V_D2$cellid_D2), byrow = TRUE)
    bestmatch<-apply(scoretable, 1, which.min)
    for (i in 1:length(bestmatch)){
      Values_D3$D3_PrevID[i]<-Values_D2$cellid_D2[bestmatch[i]]
    }
    Values_D3
    print(head(Values_D3))
    
    cellnrD3<-c()
    PC_color<-c()
    for (j in 1:length(Values_D3$D3_PrevID)){
      cellnrD3<-Values_D3$cellid_D3[j]
      PC_color<-c(PC_color, Values_D2$D2_PrevID[Values_D3$D3_PrevID[cellnrD3]])
    }
    PC_color
    Values_D3$D3_PC_color<-PC_color
    Values_D3
    print(head(Values_D3))
    #confidence score
    bestvalue<-c()
    lowvalues<-c()
    for (i in 1:nrow(scoretable)){
      bestvalue<-scoretable[i,bestmatch[i]]
      lowvalues<-c(lowvalues,bestvalue)
    }
    worse<-max(lowvalues)
    inv_lowvalues<-(worse-lowvalues)
    confidence<-rep(inv_lowvalues,Values_D3$areapx_D3)
    
    OrigID_D3 <- rep(Values_D3$D3_PC_color, V_D3$areapx_D3)
    sorted_All_D3 <- sorted_All_D3()
    sorted_All_D3$OrigID <- OrigID_D3
    sorted_All_D3$confidence <- confidence
    print("this is sorted_All_D3: ")
    print(str(sorted_All_D3))
    
    list(sorted_All_D3 = sorted_All_D3, Values_D3 = Values_D3, inv_lowvalues = inv_lowvalues)
  })
  tracking_inv_lowvalues2 <- reactive({
    req(tracking1())
    tracking_result <- tracking1()
    inv_lowvalues <- tracking_result$inv_lowvalues
    print("BOEM inv_lowvalues: ")
    print(str(inv_lowvalues))
    return(inv_lowvalues)
  })
  tracking_Values_D3 <- reactive({
    req(tracking1())
    
    PC_result <- tracking1()
    
    print("Resultaat van PC_tracking1:")
    print(str(PC_result$Values_D3))
    
    values_D3 <- PC_result$Values_D3
    
    print("BOEM BAM THIS IS VALUES-D3: ")
    print(str(values_D3))
    
    return(values_D3)
  })
  
  tracking_sorted_All_D3 <- reactive({
    req(tracking1())
    
    PC_result <- tracking1()
    
    print("Resultaat van PC_result (sorted_All_D3):")
    print(str(PC_result$sorted_All_D3))
    
    sorted_all_d3 <- PC_result$sorted_All_D3
    
    print("BOEM BAM THIS IS sorted_All_D3: ")
    print(str(sorted_all_d3))
    
    return(sorted_all_d3)
  })
  
  ### Values_D3 en D4
  tracking2 <- reactive({
    req(V_D3(), V_D4(),tracking_Values_D2() ,tracking_Values_D3(), Values_D4(), sorted_All_D4())
    
    # Haal de waarden op uit de reactive functies
    V_D3 <- V_D3()
    V_D4 <- V_D4()
    Values_D2 <- tracking_Values_D2()
    Values_D3 <- tracking_Values_D3()
    Values_D4 <- Values_D4()
    sorted_All_D4 <- sorted_All_D4()
    
    # Voeg kolomnamen toe aan de datasets
    colnames(V_D3) <- c("cellid_D3", "area_D3", "CoMX_D3", "CoMY_D3", "Peri_D3", "areapx_D3", "Circ_D3", "Type_D3")
    colnames(V_D4) <- c("cellid_D4", "area_D4", "CoMX_D4", "CoMY_D4", "Peri_D4", "areapx_D4", "Circ_D4", "Type_D4")
    
    # Debug prints om de structuren van de datasets te controleren
    print("this is V_D3: ")
    print(str(V_D3))
    
    print("this is V_D4: ")
    print(str(V_D4))
    
    print("Kijken of dit wel klopt: ")
    print(str(Values_D3))
    
    print("this is Values_D4: ")
    print(str(Values_D4))
    
    # Bereken gemiddelde area en perimeter toename
    av_area_inc <- mean(V_D4$area_D4) / mean(V_D3$area_D3)
    av_peri_inc <- mean(V_D4$Peri_D4) / mean(V_D3$Peri_D3)
    
    # Haal de gecorrigeerde coördinaten op
    CoMX_D3_px <- Values_D3$cor_CoMX_D3
    CoMY_D3_px <- Values_D3$cor_CoMY_D3
    CoMX_D4_px <- Values_D4$cor_CoMX_D4
    CoMY_D4_px <- Values_D4$cor_CoMY_D4
    av_pc <- sqrt((mean(CoMX_D4_px) - mean(CoMX_D3_px))^2 + (mean(CoMY_D4_px) - mean(CoMY_D3_px))^2)
    
    scores <- c()
    
    # Bereken de scores tussen de cellen
    for (j in 1:length(V_D4$area_D4)) {
      for (i in 1:length(V_D3$area_D3)) {
        ar <- abs(1 - (V_D4$area_D4[j] / V_D3$area_D3[i]) / av_area_inc)
        pc <- sqrt((CoMX_D4_px[j] - CoMX_D3_px[i])^2 + (CoMY_D4_px[j] - CoMY_D3_px[i])^2)
        score <- pc + 5 * ar
        scores <- c(scores, score)
      }
      if (j %in% c(10, 20, 30, 40, 50, 100, 200, 300, 400, 500)) {
        print(j)
      }
    }
    
    # Maak een scoretabel en zoek de beste matches
    scoretable <- matrix(scores, nrow = length(V_D4$cellid_D4), ncol = length(V_D3$cellid_D3), byrow = TRUE)
    bestmatch <- apply(scoretable, 1, which.min)
    for (i in 1:length(bestmatch)) {
      Values_D4$D4_PrevID[i] <- V_D3$cellid_D3[bestmatch[i]]
    }
    print(head(Values_D4))
    
    # Update PC_color kolom
    cellnrD4 <- c()
    PC_color <- c()
    for (j in 1:length(Values_D4$D4_PrevID)) {
      cellnrD4 <- Values_D4$cellid_D4[j]
      PC_color <- c(PC_color, Values_D2$D2_PrevID[Values_D3$D3_PrevID[Values_D4$D4_PrevID[cellnrD4]]])
    }
    Values_D4$D4_PC_color <- PC_color
    print(head(Values_D4))
    
    # Bereken confidence score
    bestvalue <- c()
    lowvalues <- c()
    for (i in 1:nrow(scoretable)) {
      bestvalue <- scoretable[i, bestmatch[i]]
      lowvalues <- c(lowvalues, bestvalue)
    }
    worse <- max(lowvalues)
    inv_lowvalues <- (worse - lowvalues)
    confidence <- rep(inv_lowvalues, Values_D4$areapx_D4)
    
    OrigID_D4 <- rep(Values_D4$D4_PC_color, V_D4$areapx_D4)
    sorted_All_D4$OrigID <- OrigID_D4
    sorted_All_D4$confidence <- confidence
    print("this is sorted_All_D4: ")
    print(str(sorted_All_D4))
    
    list(sorted_All_D4 = sorted_All_D4, Values_D4 = Values_D4, inv_lowvalues = inv_lowvalues)
  })
  tracking_inv_lowvalues3 <- reactive({
    req(tracking())
    tracking_result <- tracking()
    inv_lowvalues <- tracking_result$inv_lowvalues
    print("BOEM inv_lowvalues: ")
    print(str(inv_lowvalues))
    return(inv_lowvalues)
  })
  tracking_Values_D4 <- reactive({
    req(tracking2())
    
    PC_result <- tracking2()
    
    print("Resultaat van tracking2:")
    print(str(PC_result$Values_D4))
    
    values_D4 <- PC_result$Values_D4
    
    print("BOEM BAM THIS IS VALUES-D4: ")
    print(str(values_D4))
    
    return(values_D4)
  })
  
  tracking_sorted_All_D4 <- reactive({
    req(tracking2())
    
    PC_result <- tracking2()
    
    print("Resultaat van PC_result (sorted_All_D4):")
    print(str(PC_result$sorted_All_D4))
    
    sorted_all_d4 <- PC_result$sorted_All_D4
    
    print("BOEM BAM THIS IS sorted_All_D3: ")
    print(str(sorted_all_d4))
    
    return(sorted_all_d4)
  })
  
  
  alphasss<-c("Stom"=0, "PC"=1, "NEW_Stom"=1, "Highlight"=1)
  colsss<-c("1" =  "black","2" =  "antiquewhite2","3" =  "aquamarine","4" =  "aquamarine3","5" =  "azure1","6" =  "azure4","7" =  "bisque1","8" =  "bisque4","9" =  "blue","10" =  "blue3","11" =  "brown","12" =  "brown3","13" =  "burlywood1","14" =  "burlywood4","15" =  "cadetblue2","16" =  "chartreuse","17" =  "chartreuse3","18" =  "chocolate1","19" =  "chocolate4","20" =  "coral2","21" =  "cornflowerblue","22" =  "cornsilk2","23" =  "cyan","24" =  "cyan3","25" =  "darkcyan","26" =  "darkgoldenrod2","27" =  "darkgray","28" =  "darkkhaki","29" =  "darkolivegreen1","30" =  "darkolivegreen4","31" =  "darkorange2","32" =  "darkorchid","33" =  "darkorchid3","34" =  "darksalmon","35" =  "darkseagreen2","36" =  "darkslateblue","37" =  "darkslategray2","38" =  "darkslategrey","39" =  "deeppink","40" =  "deeppink3","41" =  "deepskyblue1","42" =  "deepskyblue4","43" =  "dodgerblue","44" =  "dodgerblue3","45" =  "firebrick1","46" =  "firebrick4","47" =  "gainsboro","48" =  "gold1","49" =  "gold4","50" =  "goldenrod2","51" =  "gray","52" =  "gray17","53" =  "gray35","54" =  "gray53","55" =  "gray71","56" =  "gray92","57" =  "green","58" =  "green3","59" =  "honeydew","60" =  "honeydew3","61" =  "hotpink1","62" =  "hotpink4","63" =  "indianred2","64" =  "ivory","65" =  "ivory3","66" =  "khaki1","67" =  "khaki4","68" =  "lavenderblush1","69" =  "lavenderblush4","70" =  "lemonchiffon1","71" =  "lemonchiffon4","72" =  "lightblue2","73" =  "lightcoral","74" =  "lightcyan2","75" =  "lightgoldenrod","76" =  "lightgoldenrod3","77" =  "lightgray","78" =  "lightpink","79" =  "lightpink3","80" =  "lightsalmon1","81" =  "lightsalmon4","82" =  "lightskyblue1","83" =  "lightskyblue4","84" =  "lightslategrey","85" =  "lightsteelblue2","86" =  "lightyellow","87" =  "lightyellow3","88" =  "linen","89" =  "magenta2","90" =  "maroon","91" =  "maroon3","92" =  "mediumblue","93" =  "mediumorchid2","94" =  "mediumpurple","95" =  "mediumpurple3","96" =  "mediumslateblue","97" =  "mediumvioletred","98" =  "mistyrose","99" =  "mistyrose3","100" =  "navajowhite","101" =  "navajowhite3","102" =  "navyblue","103" =  "olivedrab1","104" =  "olivedrab4","105" =  "orange2","106" =  "orangered","107" =  "orangered3","108" =  "orchid1","109" =  "orchid4","110" =  "palegreen1","111" =  "palegreen4","112" =  "paleturquoise2","113" =  "palevioletred","114" =  "palevioletred3","115" =  "peachpuff","116" =  "peachpuff3","117" =  "pink","118" =  "pink3","119" =  "plum1","120" =  "plum4","121" =  "purple1","122" =  "purple4","123" =  "red2","124" =  "rosybrown","125" =  "rosybrown3","126" =  "royalblue1","127" =  "royalblue4","128" =  "salmon1","129" =  "salmon4","130" =  "seagreen1","131" =  "seagreen4","132" =  "seashell2","133" =  "sienna","134" =  "sienna3","135" =  "skyblue1","136" =  "skyblue4","137" =  "slateblue2","138" =  "slategray","139" =  "slategray3","140" =  "snow","141" =  "snow3","142" =  "springgreen1","143" =  "springgreen4","144" =  "steelblue2","145" =  "tan","146" =  "tan3","147" =  "thistle1","148" =  "thistle4","149" =  "tomato2","150" =  "turquoise","151" =  "turquoise3","152" =  "violetred","153" =  "violetred3","154" =  "wheat1","155" =  "wheat4","156" =  "yellow1","157" =  "yellow4","158" =  "aliceblue","159" =  "antiquewhite2","160" =  "aquamarine","161" =  "aquamarine3","162" =  "azure1","163" =  "azure4","164" =  "bisque1","165" =  "bisque4","166" =  "blue","167" =  "blue3","168" =  "brown","169" =  "brown3","170" =  "burlywood1","171" =  "burlywood4","172" =  "cadetblue2","173" =  "chartreuse","174" =  "chartreuse3","175" =  "chocolate1","176" =  "chocolate4","177" =  "coral2","178" =  "cornflowerblue","179" =  "cornsilk2","180" =  "cyan","181" =  "cyan3","182" =  "darkcyan","183" =  "darkgoldenrod2","184" =  "darkgray","185" =  "darkkhaki","186" =  "darkolivegreen1","187" =  "darkolivegreen4","188" =  "darkorange2","189" =  "darkorchid","190" =  "darkorchid3","191" =  "darksalmon","192" =  "darkseagreen2","193" =  "darkslateblue",
            "194" =  "darkslategray2","195" =  "darkslategrey","196" =  "deeppink","197" =  "deeppink3","198" =  "deepskyblue1","199" =  "deepskyblue4","200" =  "dodgerblue","201" =  "dodgerblue3","202" =  "firebrick1","203" =  "firebrick4","204" =  "gainsboro","205" =  "gold1","206" =  "gold4","207" =  "goldenrod2","208" =  "gray","209" =  "gray17","210" =  "gray35","211" =  "gray53","212" =  "gray71","213" =  "gray92","214" =  "green","215" =  "green3","216" =  "honeydew","217" =  "honeydew3","218" =  "hotpink1","219" =  "hotpink4","220" =  "indianred2","221" =  "ivory","222" =  "ivory3","223" =  "khaki1","224" =  "khaki4","225" =  "lavenderblush1","226" =  "lavenderblush4","227" =  "lemonchiffon1","228" =  "lemonchiffon4","229" =  "lightblue2","230" =  "lightcoral","231" =  "lightcyan2","232" =  "lightgoldenrod","233" =  "lightgoldenrod3","234" =  "lightgray","235" =  "lightpink","236" =  "lightpink3","237" =  "lightsalmon1","238" =  "lightsalmon4","239" =  "lightskyblue1","240" =  "lightskyblue4","241" =  "lightslategrey","242" =  "lightsteelblue2","243" =  "lightyellow","244" =  "lightyellow3","245" =  "linen","246" =  "magenta2","247" =  "maroon","248" =  "maroon3","249" =  "mediumblue","250" =  "mediumorchid2","251" =  "mediumpurple","252" =  "mediumpurple3","253" =  "mediumslateblue","254" =  "mediumvioletred","255" =  "mistyrose","256" =  "mistyrose3","257" =  "navajowhite","258" =  "navajowhite3","259" =  "navyblue","260" =  "olivedrab1","261" =  "olivedrab4","262" =  "orange2","263" =  "orangered","264" =  "orangered3","265" =  "orchid1","266" =  "orchid4","267" =  "palegreen1","268" =  "palegreen4","269" =  "paleturquoise2","270" =  "palevioletred","271" =  "palevioletred3","272" =  "peachpuff","273" =  "peachpuff3","274" =  "pink","275" =  "pink3","276" =  "plum1","277" =  "plum4","278" =  "purple1","279" =  "purple4","280" =  "red2","281" =  "rosybrown","282" =  "rosybrown3","283" =  "royalblue1","284" =  "royalblue4","285" =  "salmon1","286" =  "salmon4","287" =  "seagreen1","288" =  "seagreen4","289" =  "seashell2","290" =  "sienna","291" =  "sienna3","292" =  "skyblue1","293" =  "skyblue4","294" =  "slateblue2","295" =  "slategray","296" =  "slategray3","297" =  "snow","298" =  "snow3","299" =  "springgreen1","300" =  "springgreen4","301" =  "steelblue2","302" =  "tan","303" =  "tan3","304" =  "thistle1","305" =  "thistle4","306" =  "tomato2","307" =  "turquoise","308" =  "turquoise3","309" =  "violetred","310" =  "violetred3","311" =  "wheat1","312" =  "wheat4","313" =  "yellow1","314" =  "yellow4","315" =  "aliceblue","316" =  "antiquewhite2","317" =  "aquamarine","318" =  "aquamarine3","319" =  "azure1","320" =  "azure4","321" =  "bisque1","322" =  "bisque4","323" =  "blue","324" =  "blue3","325" =  "brown","326" =  "brown3","327" =  "burlywood1","328" =  "burlywood4","329" =  "cadetblue2","330" =  "chartreuse","331" =  "chartreuse3","332" =  "chocolate1","333" =  "chocolate4","334" =  "coral2","335" =  "cornflowerblue","336" =  "cornsilk2","337" =  "cyan","338" =  "cyan3","339" =  "darkcyan","340" =  "darkgoldenrod2","341" =  "darkgray","342" =  "darkkhaki","343" =  "darkolivegreen1","344" =  "darkolivegreen4","345" =  "darkorange2","346" =  "darkorchid","347" =  "darkorchid3","348" =  "darksalmon","349" =  "darkseagreen2","350" =  "darkslateblue","351" =  "darkslategray2","352" =  "darkslategrey","353" =  "deeppink","354" =  "deeppink3","355" =  "deepskyblue1","356" =  "deepskyblue4","357" =  "dodgerblue","358" =  "dodgerblue3","359" =  "firebrick1","360" =  "firebrick4","361" =  "gainsboro","362" =  "gold1","363" =  "gold4","364" =  "goldenrod2","365" =  "gray","366" =  "gray17","367" =  "gray35","368" =  "gray53","369" =  "gray71","370" =  "gray92","371" =  "green","372" =  "green3","373" =  "honeydew","374" =  "honeydew3","375" =  "hotpink1","376" =  "hotpink4","377" =  "indianred2","378" =  "ivory","379" =  "ivory3","380" =  "khaki1","381" =  "khaki4","382" =  "lavenderblush1",
            "383" =  "lavenderblush4","384" =  "lemonchiffon1","385" =  "lemonchiffon4","386" =  "lightblue2","387" =  "lightcoral","388" =  "lightcyan2","389" =  "lightgoldenrod","390" =  "lightgoldenrod3","391" =  "lightgray","392" =  "lightpink","393" =  "lightpink3","394" =  "lightsalmon1","395" =  "lightsalmon4","396" =  "lightskyblue1","397" =  "lightskyblue4","398" =  "lightslategrey","399" =  "lightsteelblue2","400" =  "lightyellow","401" =  "lightyellow3","402" =  "linen","403" =  "magenta2","404" =  "maroon","405" =  "maroon3","406" =  "mediumblue","407" =  "mediumorchid2","408" =  "mediumpurple","409" =  "mediumpurple3","410" =  "mediumslateblue","411" =  "mediumvioletred","412" =  "mistyrose","413" =  "mistyrose3","414" =  "navajowhite","415" =  "navajowhite3","416" =  "navyblue","417" =  "olivedrab1","418" =  "olivedrab4","419" =  "orange2","420" =  "orangered","421" =  "orangered3","422" =  "orchid1","423" =  "orchid4","424" =  "palegreen1","425" =  "palegreen4","426" =  "paleturquoise2","427" =  "palevioletred","428" =  "palevioletred3","429" =  "peachpuff","430" =  "peachpuff3","431" =  "pink","432" =  "pink3","433" =  "plum1","434" =  "plum4","435" =  "purple1","436" =  "purple4","437" =  "red2","438" =  "rosybrown","439" =  "rosybrown3","440" =  "royalblue1","441" =  "royalblue4","442" =  "salmon1","443" =  "salmon4","444" =  "seagreen1","445" =  "seagreen4","446" =  "seashell2","447" =  "sienna","448" =  "sienna3","449" =  "skyblue1","450" =  "skyblue4","451" =  "slateblue2","452" =  "slategray","453" =  "slategray3","454" =  "snow","455" =  "snow3","456" =  "springgreen1","457" =  "springgreen4","458" =  "steelblue2","459" =  "tan","460" =  "tan3","461" =  "thistle1","462" =  "thistle4","463" =  "tomato2","464" =  "turquoise","465" =  "turquoise3","466" =  "violetred","467" =  "violetred3","468" =  "wheat1","469" =  "wheat4","470" =  "yellow1","471" =  "yellow4","472" =  "aliceblue","473" =  "antiquewhite2","474" =  "aquamarine","475" =  "aquamarine3","476" =  "azure1","477" =  "azure4","478" =  "bisque1","479" =  "bisque4","480" =  "blue","481" =  "blue3","482" =  "brown","483" =  "brown3","484" =  "burlywood1","485" =  "burlywood4","486" =  "cadetblue2","487" =  "chartreuse","488" =  "chartreuse3","489" =  "chocolate1","490" =  "chocolate4","491" =  "coral2","492" =  "cornflowerblue","493" =  "cornsilk2","494" =  "cyan","495" =  "cyan3","496" =  "darkcyan","497" =  "darkgoldenrod2","498" =  "darkgray","499" =  "darkkhaki","500" =  "darkolivegreen1","501" =  "darkolivegreen4","502" =  "darkorange2","503" =  "darkorchid","504" =  "darkorchid3","505" =  "darksalmon","506" =  "darkseagreen2","507" =  "darkslateblue","508" =  "darkslategray2","509" =  "darkslategrey","510" =  "deeppink","511" =  "deeppink3","512" =  "deepskyblue1","513" =  "deepskyblue4","514" =  "dodgerblue","515" =  "dodgerblue3","516" =  "firebrick1","517" =  "firebrick4","518" =  "gainsboro","519" =  "gold1","520" =  "gold4","521" =  "goldenrod2","522" =  "gray","523" =  "gray17","524" =  "gray35","525" =  "gray53","526" =  "gray71","527" =  "gray92","528" =  "green","529" =  "green3","530" =  "honeydew","531" =  "honeydew3","532" =  "hotpink1","533" =  "hotpink4","534" =  "indianred2","535" =  "ivory","536" =  "ivory3","537" =  "khaki1","538" =  "khaki4","539" =  "lavenderblush1","540" =  "lavenderblush4","541" =  "lemonchiffon1","542" =  "lemonchiffon4","543" =  "lightblue2","544" =  "lightcoral","545" =  "lightcyan2","546" =  "lightgoldenrod","547" =  "lightgoldenrod3","548" =  "lightgray","549" =  "lightpink","550" =  "lightpink3","551" =  "lightsalmon1","552" =  "lightsalmon4","553" =  "lightskyblue1","554" =  "lightskyblue4","555" =  "lightslategrey","556" =  "lightsteelblue2","557" =  "lightyellow","558" =  "lightyellow3","559" =  "linen","560" =  "magenta2","561" =  "maroon","562" =  "maroon3","563" =  "mediumblue","564" =  "mediumorchid2","565" =  "mediumpurple","566" =  "mediumpurple3","567" =  "mediumslateblue",
            "568" =  "mediumvioletred","569" =  "mistyrose","570" =  "mistyrose3","571" =  "navajowhite","572" =  "navajowhite3","573" =  "navyblue","574" =  "olivedrab1","575" =  "olivedrab4","576" =  "orange2","577" =  "orangered","578" =  "orangered3","579" =  "orchid1","580" =  "orchid4","581" =  "palegreen1","582" =  "palegreen4","583" =  "paleturquoise2","584" =  "palevioletred","585" =  "palevioletred3","586" =  "peachpuff","587" =  "peachpuff3","588" =  "pink","589" =  "pink3","590" =  "plum1","591" =  "plum4","592" =  "purple1","593" =  "purple4","594" =  "red2","595" =  "rosybrown","596" =  "rosybrown3","597" =  "royalblue1","598" =  "royalblue4","599" =  "salmon1","600" =  "salmon4","601" =  "seagreen1","602" =  "seagreen4","603" =  "seashell2","604" =  "sienna","605" =  "sienna3","606" =  "skyblue1","607" =  "skyblue4","608" =  "slateblue2","609" =  "slategray","610" =  "slategray3","611" =  "snow","612" =  "snow3","613" =  "springgreen1","614" =  "springgreen4","615" =  "steelblue2","616" =  "tan","617" =  "tan3","618" =  "thistle1","619" =  "thistle4","620" =  "tomato2","621" =  "turquoise","622" =  "turquoise3","623" =  "violetred","624" =  "violetred3","625" =  "wheat1","626" =  "wheat4","627" =  "yellow1","628" =  "yellow4","629" =  "aliceblue","630" =  "antiquewhite2","631" =  "aquamarine","632" =  "aquamarine3","633" =  "azure1","634" =  "azure4","635" =  "bisque1","636" =  "bisque4","637" =  "blue","638" =  "blue3","639" =  "brown","640" =  "brown3","641" =  "burlywood1","642" =  "burlywood4","643" =  "cadetblue2","644" =  "chartreuse","645" =  "chartreuse3","646" =  "chocolate1","647" =  "chocolate4","648" =  "coral2","649" =  "cornflowerblue","650" =  "cornsilk2","651" =  "cyan","652" =  "cyan3","653" =  "darkcyan","654" =  "darkgoldenrod2","655" =  "darkgray","656" =  "darkkhaki","657" =  "darkolivegreen1","658" =  "darkolivegreen4","659" =  "darkorange2","660" =  "darkorchid","661" =  "darkorchid3","662" =  "darksalmon","663" =  "darkseagreen2","664" =  "darkslateblue","665" =  "darkslategray2","666" =  "darkslategrey","667" =  "deeppink","668" =  "deeppink3","669" =  "deepskyblue1","670" =  "deepskyblue4","671" =  "dodgerblue","672" =  "dodgerblue3","673" =  "firebrick1","674" =  "firebrick4","675" =  "gainsboro","676" =  "gold1","677" =  "gold4","678" =  "goldenrod2","679" =  "gray","680" =  "gray17","681" =  "gray35","682" =  "gray53","683" =  "gray71","684" =  "gray92","685" =  "green","686" =  "green3","687" =  "honeydew","688" =  "honeydew3","689" =  "hotpink1","690" =  "hotpink4","691" =  "indianred2","692" =  "ivory","693" =  "ivory3","694" =  "khaki1","695" =  "khaki4","696" =  "lavenderblush1","697" =  "lavenderblush4","698" =  "lemonchiffon1","699" =  "lemonchiffon4","700" =  "lightblue2","701" =  "lightcoral","702" =  "lightcyan2","703" =  "lightgoldenrod","704" =  "lightgoldenrod3","705" =  "lightgray","706" =  "lightpink","707" =  "lightpink3","708" =  "lightsalmon1","709" =  "lightsalmon4","710" =  "lightskyblue1","711" =  "lightskyblue4","712" =  "lightslategrey","713" =  "lightsteelblue2","714" =  "lightyellow","715" =  "lightyellow3","716" =  "linen","717" =  "magenta2","718" =  "maroon","719" =  "maroon3","720" =  "mediumblue","721" =  "mediumorchid2","722" =  "mediumpurple","723" =  "mediumpurple3","724" =  "mediumslateblue","725" =  "mediumvioletred","726" =  "mistyrose","727" =  "mistyrose3","728" =  "navajowhite","729" =  "navajowhite3","730" =  "navyblue","731" =  "olivedrab1","732" =  "olivedrab4","733" =  "orange2","734" =  "orangered","735" =  "orangered3","736" =  "orchid1","737" =  "orchid4","738" =  "palegreen1","739" =  "palegreen4","740" =  "paleturquoise2","741" =  "palevioletred","742" =  "palevioletred3","743" =  "peachpuff","744" =  "peachpuff3","745" =  "pink","746" =  "pink3","747" =  "plum1","748" =  "plum4","749" =  "purple1","750" =  "purple4","751" =  "red2","752" =  "rosybrown","753" =  "rosybrown3","754" =  "royalblue1","755" =  "royalblue4",
            "756" =  "salmon1","757" =  "salmon4","758" =  "seagreen1","759" =  "seagreen4","760" =  "seashell2","761" =  "sienna","762" =  "sienna3","763" =  "skyblue1","764" =  "skyblue4","765" =  "slateblue2","766" =  "slategray","767" =  "slategray3","768" =  "snow","769" =  "snow3","770" =  "springgreen1","771" =  "springgreen4","772" =  "steelblue2","773" =  "tan","774" =  "tan3","775" =  "thistle1","776" =  "thistle4","777" =  "tomato2","778" =  "turquoise","779" =  "turquoise3","780" =  "violetred","781" =  "violetred3","782" =  "wheat1","783" =  "wheat4","784" =  "yellow1","785" =  "yellow4","786" =  "aliceblue","787" =  "antiquewhite2","788" =  "aquamarine","789" =  "aquamarine3","790" =  "azure1","791" =  "azure4","792" =  "bisque1","793" =  "bisque4","794" =  "blue","795" =  "blue3","796" =  "brown","797" =  "brown3","798" =  "burlywood1","799" =  "burlywood4","800" =  "cadetblue2","801" =  "chartreuse","802" =  "chartreuse3","803" =  "chocolate1","804" =  "chocolate4","805" =  "coral2","806" =  "cornflowerblue","807" =  "cornsilk2","808" =  "cyan","809" =  "cyan3","810" =  "darkcyan","811" =  "darkgoldenrod2","812" =  "darkgray","813" =  "darkkhaki","814" =  "darkolivegreen1","815" =  "darkolivegreen4","816" =  "darkorange2","817" =  "darkorchid","818" =  "darkorchid3","819" =  "darksalmon","820" =  "darkseagreen2","821" =  "darkslateblue","822" =  "darkslategray2","823" =  "darkslategrey","824" =  "deeppink","825" =  "deeppink3","826" =  "deepskyblue1","827" =  "deepskyblue4","828" =  "dodgerblue","829" =  "dodgerblue3","830" =  "firebrick1","831" =  "firebrick4","832" =  "gainsboro","833" =  "gold1","834" =  "gold4","835" =  "goldenrod2","836" =  "gray","837" =  "gray17","838" =  "gray35","839" =  "gray53","840" =  "gray71","841" =  "gray92","842" =  "green","843" =  "green3","844" =  "honeydew","845" =  "honeydew3","846" =  "hotpink1","847" =  "hotpink4","848" =  "indianred2","849" =  "ivory","850" =  "ivory3","851" =  "khaki1","852" =  "khaki4","853" =  "lavenderblush1","854" =  "lavenderblush4","855" =  "lemonchiffon1","856" =  "lemonchiffon4","857" =  "lightblue2","858" =  "lightcoral","859" =  "lightcyan2","860" =  "lightgoldenrod","861" =  "lightgoldenrod3","862" =  "lightgray","863" =  "lightpink","864" =  "lightpink3","865" =  "lightsalmon1","866" =  "lightsalmon4","867" =  "lightskyblue1","868" =  "lightskyblue4","869" =  "lightslategrey","870" =  "lightsteelblue2","871" =  "lightyellow","872" =  "lightyellow3","873" =  "linen","874" =  "magenta2","875" =  "maroon","876" =  "maroon3","877" =  "mediumblue","878" =  "mediumorchid2","879" =  "mediumpurple","880" =  "mediumpurple3","881" =  "mediumslateblue","882" =  "mediumvioletred","883" =  "mistyrose","884" =  "mistyrose3","885" =  "navajowhite","886" =  "navajowhite3","887" =  "navyblue","888" =  "olivedrab1","889" =  "olivedrab4","890" =  "orange2","891" =  "orangered","892" =  "orangered3","893" =  "orchid1","894" =  "orchid4","895" =  "palegreen1","896" =  "palegreen4","897" =  "paleturquoise2","898" =  "palevioletred","899" =  "palevioletred3","900" =  "peachpuff","901" =  "peachpuff3","902" =  "pink","903" =  "pink3","904" =  "plum1","905" =  "plum4","906" =  "purple1","907" =  "purple4","908" =  "red2","909" =  "rosybrown","910" =  "rosybrown3","911" =  "royalblue1","912" =  "royalblue4","913" =  "salmon1","914" =  "salmon4","915" =  "seagreen1","916" =  "seagreen4","917" =  "seashell2","918" =  "sienna","919" =  "sienna3","920" =  "skyblue1","921" =  "skyblue4","922" =  "slateblue2","923" =  "slategray","924" =  "slategray3","925" =  "snow","926" =  "snow3","927" =  "springgreen1","928" =  "springgreen4","929" =  "steelblue2","930" =  "tan","931" =  "tan3","932" =  "thistle1","933" =  "thistle4","934" =  "tomato2","935" =  "turquoise","936" =  "turquoise3","937" =  "violetred","938" =  "violetred3","939" =  "wheat1","940" =  "wheat4","941" =  "yellow1","942" =  "yellow4","943" =  "aliceblue","944" =  "antiquewhite2",
            "945" =  "aquamarine","946" =  "aquamarine3","947" =  "azure1","948" =  "azure4","949" =  "bisque1","950" =  "bisque4","951" =  "blue","952" =  "blue3","953" =  "brown","954" =  "brown3","955" =  "burlywood1","956" =  "burlywood4","957" =  "cadetblue2","958" =  "chartreuse","959" =  "chartreuse3","960" =  "chocolate1","961" =  "chocolate4","962" =  "coral2","963" =  "cornflowerblue","964" =  "cornsilk2","965" =  "cyan","966" =  "cyan3","967" =  "darkcyan","968" =  "darkgoldenrod2","969" =  "darkgray","970" =  "darkkhaki","971" =  "darkolivegreen1","972" =  "darkolivegreen4","973" =  "darkorange2","974" =  "darkorchid","975" =  "darkorchid3","976" =  "darksalmon","977" =  "darkseagreen2","978" =  "darkslateblue","979" =  "darkslategray2","980" =  "darkslategrey","981" =  "deeppink","982" =  "deeppink3","983" =  "deepskyblue1","984" =  "deepskyblue4","985" =  "dodgerblue","986" =  "dodgerblue3","987" =  "firebrick1","988" =  "firebrick4","989" =  "gainsboro","990" =  "gold1","991" =  "gold4","992" =  "goldenrod2","993" =  "gray","994" =  "gray17","995" =  "gray35","996" =  "gray53","997" =  "gray71","998" =  "gray92","999" =  "green","1000" =  "green3","1001" =  "honeydew","1002" =  "honeydew3","1003" =  "hotpink1","1004" =  "hotpink4","1005" =  "indianred2","1006" =  "ivory","1007" =  "ivory3","1008" =  "khaki1","1009" =  "khaki4","1010" =  "lavenderblush1","1011" =  "lavenderblush4","1012" =  "lemonchiffon1","1013" =  "lemonchiffon4","1014" =  "lightblue2","1015" =  "lightcoral","1016" =  "lightcyan2","1017" =  "lightgoldenrod","1018" =  "lightgoldenrod3","1019" =  "lightgray","1020" =  "lightpink","1021" =  "lightpink3","1022" =  "lightsalmon1","1023" =  "lightsalmon4","1024" =  "lightskyblue1","1025" =  "lightskyblue4","1026" =  "lightslategrey","1027" =  "lightsteelblue2","1028" =  "lightyellow","1029" =  "lightyellow3","1030" =  "linen","1031" =  "magenta2","1032" =  "maroon","1033" =  "maroon3","1034" =  "mediumblue","1035" =  "mediumorchid2","1036" =  "mediumpurple","1037" =  "mediumpurple3","1038" =  "mediumslateblue","1039" =  "mediumvioletred","1040" =  "mistyrose","1041" =  "mistyrose3","1042" =  "navajowhite","1043" =  "navajowhite3","1044" =  "navyblue","1045" =  "olivedrab1","1046" =  "olivedrab4","1047" =  "orange2","1048" =  "orangered","1049" =  "orangered3","1050" =  "orchid1","1051" =  "orchid4","1052" =  "palegreen1","1053" =  "palegreen4","1054" =  "paleturquoise2","1055" =  "palevioletred","1056" =  "palevioletred3","1057" =  "peachpuff","1058" =  "peachpuff3","1059" =  "pink","1060" =  "pink3","1061" =  "plum1","1062" =  "plum4","1063" =  "purple1","1064" =  "purple4","1065" =  "red2","1066" =  "rosybrown","1067" =  "rosybrown3","1068" =  "royalblue1","1069" =  "royalblue4","1070" =  "salmon1","1071" =  "salmon4","1072" =  "seagreen1","1073" =  "seagreen4","1074" =  "seashell2","1075" =  "sienna","1076" =  "sienna3","1077" =  "skyblue1","1078" =  "skyblue4","1079" =  "slateblue2","1080" =  "slategray","1081" =  "slategray3","1082" =  "snow","1083" =  "snow3","1084" =  "springgreen1","1085" =  "springgreen4","1086" =  "steelblue2","1087" =  "tan","1088" =  "tan3","1089" =  "thistle1","1090" =  "thistle4","1091" =  "tomato2","1092" =  "turquoise","1093" =  "turquoise3","1094" =  "violetred","1095" =  "violetred3","1096" =  "wheat1","1097" =  "wheat4","1098" =  "yellow1","1099" =  "yellow4","1100" =  "aliceblue","1101" =  "antiquewhite2","1102" =  "aquamarine","1103" =  "aquamarine3","1104" =  "azure1","1105" =  "azure4","1106" =  "bisque1","1107" =  "bisque4","1108" =  "blue","1109" =  "blue3","1110" =  "brown","1111" =  "brown3","1112" =  "burlywood1","1113" =  "burlywood4","1114" =  "cadetblue2","1115" =  "chartreuse","1116" =  "chartreuse3","1117" =  "chocolate1","1118" =  "chocolate4","1119" =  "coral2","1120" =  "cornflowerblue","1121" =  "cornsilk2","1122" =  "cyan","1123" =  "cyan3","1124" =  "darkcyan","1125" =  "darkgoldenrod2","1126" =  "darkgray","1127" =  "darkkhaki",
            "1128" =  "darkolivegreen1","1129" =  "darkolivegreen4","1130" =  "darkorange2","1131" =  "darkorchid","1132" =  "darkorchid3","1133" =  "darksalmon","1134" =  "darkseagreen2","1135" =  "darkslateblue","1136" =  "darkslategray2","1137" =  "darkslategrey","1138" =  "deeppink","1139" =  "deeppink3","1140" =  "deepskyblue1","1141" =  "deepskyblue4","1142" =  "dodgerblue","1143" =  "dodgerblue3","1144" =  "firebrick1","1145" =  "firebrick4","1146" =  "gainsboro","1147" =  "gold1","1148" =  "gold4","1149" =  "goldenrod2","1150" =  "gray","1151" =  "gray17","1152" =  "gray35","1153" =  "gray53","1154" =  "gray71","1155" =  "gray92","1156" =  "green","1157" =  "green3","1158" =  "honeydew","1159" =  "honeydew3","1160" =  "hotpink1","1161" =  "hotpink4","1162" =  "indianred2","1163" =  "ivory","1164" =  "ivory3","1165" =  "khaki1","1166" =  "khaki4","1167" =  "lavenderblush1","1168" =  "lavenderblush4","1169" =  "lemonchiffon1","1170" =  "lemonchiffon4","1171" =  "lightblue2","1172" =  "lightcoral","1173" =  "lightcyan2","1174" =  "lightgoldenrod","1175" =  "lightgoldenrod3","1176" =  "lightgray","1177" =  "lightpink","1178" =  "lightpink3","1179" =  "lightsalmon1","1180" =  "lightsalmon4","1181" =  "lightskyblue1","1182" =  "lightskyblue4","1183" =  "lightslategrey","1184" =  "lightsteelblue2","1185" =  "lightyellow","1186" =  "lightyellow3","1187" =  "linen","1188" =  "magenta2","1189" =  "maroon","1190" =  "maroon3","1191" =  "mediumblue","1192" =  "mediumorchid2","1193" =  "mediumpurple","1194" =  "mediumpurple3","1195" =  "mediumslateblue","1196" =  "mediumvioletred","1197" =  "mistyrose","1198" =  "mistyrose3","1199" =  "navajowhite","1200" =  "navajowhite3","1201" =  "navyblue","1202" =  "olivedrab1","1203" =  "olivedrab4","1204" =  "orange2","1205" =  "orangered","1206" =  "orangered3","1207" =  "orchid1","1208" =  "orchid4","1209" =  "palegreen1","1210" =  "palegreen4","1211" =  "paleturquoise2","1212" =  "palevioletred","1213" =  "palevioletred3","1214" =  "peachpuff","1215" =  "peachpuff3","1216" =  "pink","1217" =  "pink3","1218" =  "plum1","1219" =  "plum4","1220" =  "purple1","1221" =  "purple4","1222" =  "red2","1223" =  "rosybrown","1224" =  "rosybrown3","1225" =  "royalblue1","1226" =  "royalblue4","1227" =  "salmon1","1228" =  "salmon4","1229" =  "seagreen1","1230" =  "seagreen4","1231" =  "seashell2","1232" =  "sienna","1233" =  "sienna3","1234" =  "skyblue1","1235" =  "skyblue4","1236" =  "slateblue2","1237" =  "slategray","1238" =  "slategray3","1239" =  "snow","1240" =  "snow3","1241" =  "springgreen1","1242" =  "springgreen4","1243" =  "steelblue2","1244" =  "tan","1245" =  "tan3","1246" =  "thistle1","1247" =  "thistle4","1248" =  "tomato2","1249" =  "turquoise","1250" =  "turquoise3","1251" =  "violetred","1252" =  "violetred3","1253" =  "wheat1","1254" =  "wheat4","1255" =  "yellow1","1256" =  "yellow4","1257" =  "aliceblue","1258" =  "antiquewhite2","1259" =  "aquamarine","1260" =  "aquamarine3","1261" =  "azure1","1262" =  "azure4","1263" =  "bisque1","1264" =  "bisque4","1265" =  "blue","1266" =  "blue3","1267" =  "brown","1268" =  "brown3","1269" =  "burlywood1","1270" =  "burlywood4","1271" =  "cadetblue2","1272" =  "chartreuse","1273" =  "chartreuse3","1274" =  "chocolate1","1275" =  "chocolate4","1276" =  "coral2","1277" =  "cornflowerblue","1278" =  "cornsilk2","1279" =  "cyan","1280" =  "cyan3","1281" =  "darkcyan","1282" =  "darkgoldenrod2","1283" =  "darkgray","1284" =  "darkkhaki","1285" =  "darkolivegreen1","1286" =  "darkolivegreen4","1287" =  "darkorange2","1288" =  "darkorchid","1289" =  "darkorchid3","1290" =  "darksalmon","1291" =  "darkseagreen2","1292" =  "darkslateblue","1293" =  "darkslategray2","1294" =  "darkslategrey","1295" =  "deeppink","1296" =  "deeppink3","1297" =  "deepskyblue1","1298" =  "deepskyblue4","1299" =  "dodgerblue","1300" =  "dodgerblue3","1301" =  "firebrick1","1302" =  "firebrick4","1303" =  "gainsboro","1304" =  "gold1","1305" =  "gold4",
            "1306" =  "goldenrod2","1307" =  "gray","1308" =  "gray17","1309" =  "gray35","1310" =  "gray53","1311" =  "gray71","1312" =  "gray92","1313" =  "green","1314" =  "green3","1315" =  "honeydew","1316" =  "honeydew3","1317" =  "hotpink1","1318" =  "hotpink4","1319" =  "indianred2","1320" =  "ivory","1321" =  "ivory3","1322" =  "khaki1","1323" =  "khaki4","1324" =  "lavenderblush1","1325" =  "lavenderblush4","1326" =  "lemonchiffon1","1327" =  "lemonchiffon4","1328" =  "lightblue2","1329" =  "lightcoral","1330" =  "lightcyan2","1331" =  "lightgoldenrod","1332" =  "lightgoldenrod3","1333" =  "lightgray","1334" =  "lightpink","1335" =  "lightpink3","1336" =  "lightsalmon1","1337" =  "lightsalmon4","1338" =  "lightskyblue1","1339" =  "lightskyblue4","1340" =  "lightslategrey","1341" =  "lightsteelblue2","1342" =  "lightyellow","1343" =  "lightyellow3","1344" =  "linen","1345" =  "magenta2","1346" =  "maroon","1347" =  "maroon3","1348" =  "mediumblue","1349" =  "mediumorchid2","1350" =  "mediumpurple","1351" =  "mediumpurple3","1352" =  "mediumslateblue","1353" =  "mediumvioletred","1354" =  "mistyrose","1355" =  "mistyrose3","1356" =  "navajowhite","1357" =  "navajowhite3","1358" =  "navyblue","1359" =  "olivedrab1","1360" =  "olivedrab4","1361" =  "orange2","1362" =  "orangered","1363" =  "orangered3","1364" =  "orchid1","1365" =  "orchid4","1366" =  "palegreen1","1367" =  "palegreen4","1368" =  "paleturquoise2","1369" =  "palevioletred","1370" =  "palevioletred3","1371" =  "peachpuff","1372" =  "peachpuff3","1373" =  "pink","1374" =  "pink3","1375" =  "plum1","1376" =  "plum4","1377" =  "purple1","1378" =  "purple4","1379" =  "red2","1380" =  "rosybrown","1381" =  "rosybrown3","1382" =  "royalblue1","1383" =  "royalblue4","1384" =  "salmon1","1385" =  "salmon4","1386" =  "seagreen1","1387" =  "seagreen4","1388" =  "seashell2","1389" =  "sienna","1390" =  "sienna3","1391" =  "skyblue1","1392" =  "skyblue4","1393" =  "slateblue2","1394" =  "slategray","1395" =  "slategray3","1396" =  "snow","1397" =  "snow3","1398" =  "springgreen1","1399" =  "springgreen4","1400" =  "steelblue2","1401" =  "tan","1402" =  "tan3","1403" =  "thistle1","1404" =  "thistle4","1405" =  "tomato2","1406" =  "turquoise","1407" =  "turquoise3","1408" =  "violetred","1409" =  "violetred3","1410" =  "wheat1","1411" =  "wheat4","1412" =  "yellow1","1413" =  "yellow4","1414" =  "aliceblue","1415" =  "antiquewhite2","1416" =  "aquamarine","1417" =  "aquamarine3","1418" =  "azure1","1419" =  "azure4","1420" =  "bisque1","1421" =  "bisque4","1422" =  "blue","1423" =  "blue3","1424" =  "brown","1425" =  "brown3","1426" =  "burlywood1","1427" =  "burlywood4","1428" =  "cadetblue2","1429" =  "chartreuse","1430" =  "chartreuse3","1431" =  "chocolate1","1432" =  "chocolate4","1433" =  "coral2","1434" =  "cornflowerblue","1435" =  "cornsilk2","1436" =  "cyan","1437" =  "cyan3","1438" =  "darkcyan","1439" =  "darkgoldenrod2","1440" =  "darkgray","1441" =  "darkkhaki","1442" =  "darkolivegreen1","1443" =  "darkolivegreen4","1444" =  "darkorange2","1445" =  "darkorchid","1446" =  "darkorchid3","1447" =  "darksalmon","1448" =  "darkseagreen2","1449" =  "darkslateblue","1450" =  "darkslategray2","1451" =  "darkslategrey","1452" =  "deeppink","1453" =  "deeppink3","1454" =  "deepskyblue1","1455" =  "deepskyblue4","1456" =  "dodgerblue","1457" =  "dodgerblue3","1458" =  "firebrick1","1459" =  "firebrick4","1460" =  "gainsboro","1461" =  "gold1","1462" =  "gold4","1463" =  "goldenrod2","1464" =  "gray","1465" =  "gray17","1466" =  "gray35","1467" =  "gray53","1468" =  "gray71","1469" =  "gray92","1470" =  "green","1471" =  "green3","1472" =  "honeydew","1473" =  "honeydew3","1474" =  "hotpink1","1475" =  "hotpink4","1476" =  "indianred2","1477" =  "ivory","1478" =  "ivory3","1479" =  "khaki1","1480" =  "khaki4","1481" =  "lavenderblush1","1482" =  "lavenderblush4","1483" =  "lemonchiffon1","1484" =  "lemonchiffon4","1485" =  "lightblue2",
            "1486" =  "lightcoral","1487" =  "lightcyan2","1488" =  "lightgoldenrod","1489" =  "lightgoldenrod3","1490" =  "lightgray","1491" =  "lightpink","1492" =  "lightpink3","1493" =  "lightsalmon1","1494" =  "lightsalmon4","1495" =  "lightskyblue1","1496" =  "lightskyblue4","1497" =  "lightslategrey","1498" =  "lightsteelblue2","1499" =  "lightyellow","1500" =  "lightyellow3")
  output$PC1 <- renderPlot({
    test_result <- Col_id()
    plot <- ggplot(test_result, aes(x = Xcoord, y = InvY, colour = as.factor(ids), alpha=Type)) +
      geom_point(size = 0.1) +
      scale_color_manual(values = colsss)+
      scale_alpha_manual(values = alphasss)+
      theme(panel.background = element_rect(fill = "gray27")) +
      theme(panel.grid = element_blank(), legend.position = "none")
    plot
  })
  output$PC2 <- renderPlot({
    test_result <- tracking_sorted_All_D2()
    plot <- ggplot(test_result, aes(x = Xcoord, y = InvY, colour = as.factor(OrigID), alpha=Type)) +
      geom_point(size = 0.1) +
      scale_color_manual(values = colsss)+
      scale_alpha_manual(values = alphasss)+
      theme(panel.background = element_rect(fill = "gray27")) +
      theme(panel.grid = element_blank(), legend.position = "none")
    plot
  })
  output$PC3 <- renderPlot({
    test_result <- tracking_sorted_All_D3()
   plot <- ggplot(test_result, aes(x = Xcoord, y = InvY, colour = as.factor(OrigID), alpha = Type)) +
      geom_point(size = 0.1) +
      scale_color_manual(values = colsss) +
      scale_alpha_manual(values = alphasss) +
      theme(panel.background = element_rect(fill = "gray27")) +
      theme(panel.grid = element_blank(), legend.position = "none")
    plot
  })
  output$PC4 <- renderPlot({
    test_result <- tracking_sorted_All_D4()
    plot <- ggplot(test_result, aes(x = Xcoord, y = InvY, colour = as.factor(OrigID), alpha = Type)) +
      geom_point(size = 0.1) +
      scale_color_manual(values = colsss) +
      scale_alpha_manual(values = alphasss) +
      theme(panel.background = element_rect(fill = "gray27")) +
      theme(panel.grid = element_blank(), legend.position = "none")
    plot
  })
  
  output$PC1.1 <- renderPlot({
    tracking <- tracking_sorted_All_D2()
    inv_lowvalues <- tracking_inv_lowvalues()
    plot <- ggplot(tracking, aes(x = Xcoord, y = InvY, colour = confidence, alpha=Type_D2)) +
      geom_point(size = 0.1) +
      scale_alpha_manual(values = alphasss)+
      scale_colour_gradient2(low = "red", mid = "grey94", midpoint = mean(inv_lowvalues), high = "grey94")+
      theme(panel.background = element_rect(fill = "gray27")) +
      theme(panel.grid = element_blank(), legend.position = "none", axis.title = element_blank())
    plot
  })
  output$PC1.2 <- renderPlot({
    tracking <- tracking_sorted_All_D3()
    inv_lowvalues <- tracking_inv_lowvalues2()
    plot <- ggplot(tracking, aes(x = Xcoord, y = InvY, colour = confidence, alpha=Type)) +
      geom_point(size = 0.1) +
      scale_alpha_manual(values = alphasss)+
      scale_colour_gradient2(low = "red", mid = "grey94", midpoint = mean(inv_lowvalues), high = "grey94")+
      theme(panel.background = element_rect(fill = "gray27")) +
      theme(panel.grid = element_blank(), legend.position = "none", axis.title = element_blank())
    plot
  })
  output$PC1.3 <- renderPlot({
    tracking <- tracking_sorted_All_D4()
    inv_lowvalues <- tracking_inv_lowvalues3()
    plot <- ggplot(tracking, aes(x = Xcoord, y = InvY, colour = confidence, alpha=Type)) +
      geom_point(size = 0.1) +
      scale_alpha_manual(values = alphasss)+
      scale_colour_gradient2(low = "red", mid = "grey94", midpoint = mean(inv_lowvalues), high = "grey94")+
      theme(panel.background = element_rect(fill = "gray27")) +
      theme(panel.grid = element_blank(), legend.position = "none", axis.title = element_blank())
    plot
  })
  
}
shinyApp(ui = ui, server = server)