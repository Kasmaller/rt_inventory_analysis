library(shiny)
library(bslib)
library(shinyjs)
library(networkD3)
library(tidyverse)
library(tools)
library(shinyWidgets) 
library(shinythemes)
library(htmlwidgets)

# Read and process the data
sankey_df <- read.csv("inventory_data_sankey.csv") %>%
  group_by(across(everything())) %>%
  summarize(value = n(), .groups = "drop")


data <- read.csv("inventory_data_pi.csv") 

choiceOrder <- list(
  observed = c("No", "Yes"),
  training = c("Entry-Level Training", "Short Training", "Complex Training"),
  purpose = c("Single-Purpose", "Multi-Purpose"),
  power = c("Unpowered", "Battery Power", "AC Power"),
  actuation = c("Unactuated", "Actuated"),
  monitor = c("No Monitor", "Monitor"),
  objective = c("Measurement", "Intervention", "Combined")
)

ui <- navbarPage(
  title = "Rehab Tech Inventory Analysis",
  id = "main_navbar",
  tags$head(
    tags$style(HTML("
      #Introduction {
        background: url('lab_bkg.jpg') no-repeat center center fixed; 
        background-size: cover;
      }
      .navbar {
        background-color: #E27438 !important;
      }
      .navbar .navbar-nav .nav-link, .navbar .navbar-brand {
        color: #FFFFFF !important; /* White text color */
      }
      .navbar .navbar-nav .nav-link:hover, .navbar .navbar-nav .nav-link:focus {
        color: #DDDDDD !important; /* Slightly lighter white on hover/focus */
      }
      .navbar .navbar-nav .nav-link.active {
        background-color: #FFFFFF !important; /* White background for active tab */
        color: #E27438 !important; /* Original navbar color for text on active tab */
      }

      h4 {
        color: #E27438 !important; /* Apply the color to all h5 elements */
      }
      .btn {
        background-color: #E27438; /* Button background color */
        color: white !important; /* Button text color */
        margin-bottom: 10px; /* Space below buttons */
      }
    ")),
    useShinyjs() # Initialize shinyjs
  ),
  
  theme = bs_theme(
    bootswatch = "zephyr",
    "navbar-bg" = "#E27438",
    "navbar-dark-color" = "#FFFFFF",
    "navbar-dark-active-color" = "#FFFFFF",
    "navbar-dark-hover-color" = "#FFFFFF",
    "navbar-border" = "#E27438"
  ),
  
  tabPanel("Introduction",
           fluidRow(
             column(12,
                    div(class = "card bg-light mb-3",
                        div(class = "card-body",
                            tags$h2("Explore the use of rehabilitation technology in clinical practice", 
                                    style = "text-align: center; margin-bottom: 30px;"),
                            tags$p("Understanding the characteristics of the devices that have been successfully adopted into clinical practice can provide important insights for the developers of new rehabilitation technology.", class = "card-text"),
                            tags$p("The members of the Shirley Ryan AbilityLab Strength + Endurance Lab, lead by Dr. Miriam Rafferty, completed approximately 40 hours of clinical observation across all settings in the hospital. While doing so, they recorded which available devices were used during therapy sessions.", class = "card-text"),
                            
                            fluidRow(
                              column(6, # Left half for Analysis Tools
                                     tags$h3("Analysis Tools", style = "text-align: center; margin-bottom: 15px;"), # Header for Analysis Tools
                                     div(style = "display: flex; justify-content: center;", # Centering the buttons
                                         actionButton("dev_char_btn", "Observation Rates", class = "btn", style = "margin-right: 10px;"),
                                         actionButton("assoc_char_btn", "Associate Characteristics", class = "btn")
                                     )
                              ),
                              column(6, # Right half for Learn More
                                     tags$h3("Learn More", style = "text-align: center; margin-bottom: 15px;"), # Header for Learn More
                                     div(style = "display: flex; justify-content: center;", # Centering the buttons
                                         actionButton("methodology_btn", "Methodology", class = "btn", style = "margin-right: 10px;"),
                                         actionButton("pub_btn", "Publication (Coming Soon)", class = "btn")
                                     )
                              )
                            )
                        )
                    )
             )
           ),
           id = "Introduction" # This id is redundant if you already have it in the navbarPage and should be removed
  ),
  
  
  
  # Observation Rates Tab
  tabPanel("Observation Rates",
           sidebarLayout(
             
             # Sidebar panel for checkboxes
             sidebarPanel(
               
               div(class = "card border-secondary mb-3",
                   div(class = "card-body", style = "padding-bottom:3px",     
                       h4(style = "color: #E27438;text-align: center;", "Filters"))
               ),
               uiOutput("dynamicCheckboxes"),
               width =2
             ),
             
             # Main panel for the rest of the content
             mainPanel(
               # First row with instructions and buttons
               fluidRow(
                 column(12,
                        div(class = "card bg-light mb-3",
                            div(class = "card-header",style = "padding-bottom:3px", 
                                tags$h4("How to use")),
                            div(class = "card-body",
                                tags$p("This tool allows you to explore trends in the devices we observed clinicians using. Below are six characteristics of the 90 rehabilitation devices available to the clinicians. Select a combination of characteristics to see which devices match that description and the proportion of those devices we observed being used."),
                                tags$p("For more help using this tool you can click on the buttons to the right."),
                                actionButton("or_full_explanation_btn", "Full Explanation", class = "btn-block", style = "width: 100%;")
                            )
                        )
                 ),
               ),
               
               # Custom styles
               tags$head(
                 tags$style(HTML("
          .equal-height-row {
            display: flex;
            flex-wrap: wrap;
          }
          .equal-height-row .col {
            display: flex;
          }
          .equal-height-row .col .card {
            width: 100%; /* Ensures the card stretches to fill the column */
          }
          .card-header {
            font-weight: bold; /* Make card headers bold */
          }
        "))
               ),
        
        # Second row for lists and pie chart
        fluidRow(
          column(3, uiOutput("observedDeviceList")),
          column(3, uiOutput("notObservedDeviceList")),
          column(6,
                 div(class = "card border-secondary mb-3",
                     uiOutput("pieChartHeader"),
                     div(class = "card-body",
                         plotOutput("observedChart")
                     )
                 )
          )
        ),
        width = 10
             )
           )
  ),
  # Associated Characteristics Tab
  tabPanel("Associated Characteristics",
           sidebarLayout(
             # Sidebar panel for 'Choose Categories' and dynamic UI output
             sidebarPanel(
               # 'Choose Categories' widget in a card
               div(class = "card border-secondary mb-3",
                   div(class = "card-header", "Categories"),
                   div(class = "card-body",
                       selectInput("columns", label = NULL, 
                                   choices = head(colnames(sankey_df)[-8], 7),
                                   selected = head(colnames(sankey_df)[-8], 7),
                                   multiple = TRUE)
                   )
               ),
                 div(class = "card border-secondary mb-3",
                div(class = "card-body", style = "padding-bottom:3px",     
                    h4(style = "color: #E27438;text-align: center;", "Filters"))
  ),

               # Dynamic UI output (node selection) in cards
               uiOutput("nodeSelectionUI"),
               
               width = 2  # Set the width of the sidebar
             ),
             
             # Main panel for the remaining content
             mainPanel(
               # Instructions and buttons
               fluidRow(
                 column(12,
                        div(class = "card bg-light mb-3",
                          div(class = "card-header",style = "padding-bottom:3px", 
                              tags$h4("How to use")),
                            div(class = "card-body",
                                tags$p("This tool allows you to explore the relationships between different characteristics using a Sankey Diagram. A Sankey Diagram visualizes the number of associations between two characteristics, making it easier to understand complex interactions. The size of the colored bars represents the proportion of inventory devices that match that characteristic. The size of the gray connection between those two bars represents the number of devices that share those two characteristics."),
                                tags$p("To explore these relationships, first select which categories of characteristics you’d like included in the diagram and in what order. Then, select which values from each of those columns you’d like included."),
                                tags$p("When selecting categories to display, click on the category and hit the backspace key to remove a category from the diagram. Click inside the white box and use your arrow keys to select a place to add that category back into the diagram."),
                            )
                        )
                 ),
               ),
               
               # Sankey diagram
               fluidRow(
                 column(12,
                        div(class = "card border-secondary mb-3",
                            div(class = "card-header", "Sankey Diagram"),
                            div(class = "card-body",
                                sankeyNetworkOutput("sankeyDiagram")
                            )
                        )
                 )
               ),
               
               width = 10  # Set the width of the main panel
             )
           )
  )
  ,
  
  tabPanel("Methodology",
           fluidRow(
             column(12,
                    div(class = "card bg-light mb-3",
                        div(class = "card-body",
                            tags$h4("Methodology"),
                            tags$p("This study was approved by the Northwestern Institutional Review Board (STU00212839), with a waiver of informed consent from observed patients or clinicians. No identifying information about the clinicians or patient information was recorded. Clinicians were not asked or encouraged to change their behavior during observation."),
                            tags$p("Our team of trained clinicians conducted direct field observations in 10 adult patient units: 7 inpatient and three outpatient. The observations included therapy including approximately 100 Physical Therapists (PTs), 80 Occupational Therapists (OTs), and 50 Speech-Language Pathologists (SLPs) sessions during standard care in shared gym spaces."),
                            tags$p("Observations were recorded using a semi-structured template to capture as much relevant context on each device's use as possible, including therapist discipline, familiarity with the device, and behavior while using it. A limited amount of this data has been included in this data visualization tool. Ongoing, iterative analyses were conducted until saturation of devices and themes was achieved at approximately 40 hours of observation."),
                            tags$p("For this study, rehabilitation technology only included devices designed with the intention of being used as rehabilitation technology. This excludes objects like towels, canes, resistance bands, and items not directly instrumental to care, such as clinician laptops."),
                            tags$p("Inductive coding was performed to simplify the categorization of descriptive variables down to those available in the tools on this site. This was carried out by two coders, with a third brought in to aid in consensus building."),
                            tags$p("This study was conducted by the Rafferty Knowledge Translation: Exercise and Activity for Symptom Management (KTEAM) Lab in the Nancy Knowles Strength + Endurance Lab at Shirley Ryan AbilityLab, in collaboration with the Patton Robotics Lab."),
                            tags$p("This web app was developed using the Shiny R package from data analyzed in R (Version 4.3.2)."),
                            tags$h4("Limitations"),
                            tags$p("This toolset is not intended for hypothesis testing. There is insufficient data to validate any pattern you may find while using these tools. While these results may assist with hypothesis building and preliminary discovery, we do not make any conclusions from our data outside of those made in the publication."),
                            tags$p("Observations were only performed in shared gyms. Any device used in private areas, such as patient rooms, was not included; thus, some devices may appear underutilized in these tools as they are predominantly used in these private rooms."),
                            tags$p("Observation rates for devices with a particular set of characteristics do not equate to the probability of adoption of similar devices. They should not be the sole influence on determinations to develop or purchase devices."),
                            tags$h4("Acknowledgments"),
                            tags$p("The study was made possible by funding through the National Institute on Disability, Independent Living, and Rehabilitation Research (NIDILLR) Rehabilitation Engineering Research Center grant (90REGE0010)."),
                        )
                    )
             )
           )
  )
)



server <- function(input, output, session) {
  output$dynamicCheckboxes <- renderUI({
    lapply(colnames(data)[-c(1, which(colnames(data) == "observed"))], function(col) {
      choices <- unique(data[[col]])
      
      if (col %in% names(choiceOrder)) {
        orderedChoices <- choiceOrder[[col]][choiceOrder[[col]] %in% choices]
        remainingChoices <- setdiff(choices, orderedChoices)
        choices <- c(orderedChoices, remainingChoices)
      }
      
      div(class = "card border-secondary mb-3",
          h6(class = "card-header", tools::toTitleCase(col)),
          div(class = "card-body", style = "padding-bottom: 0px;",
              checkboxGroupInput(inputId = col, label = NULL,
                                 choices = choices,
                                 selected = choices)
          )
      )
    })
  })
  # Observe button click and switch to the Observation Rates tab
  observeEvent(input$dev_char_btn, {
    updateNavbarPage(session, "main_navbar", selected = "Observation Rates")
  })
  
  # Observe button click and switch to the Associated Characteristics tab
  observeEvent(input$assoc_char_btn, {
    updateNavbarPage(session, "main_navbar", selected = "Associated Characteristics")
  })
  
  # Observe button click and switch to the Methodology tab
  observeEvent(input$methodology_btn, {
    updateNavbarPage(session, "main_navbar", selected = "Methodology")
  })
  
  
  # Filter data based on selected characteristics
  filteredData <- reactive({
    selected_values <- sapply(colnames(data)[-c(1, which(colnames(data) == "observed"))], 
                              function(col) input[[col]])
    
    # Initialize the filtered data as the entire dataset
    filtered <- data
    
    # Apply filters for each characteristic
    for (col in names(selected_values)) {
      selected_options <- selected_values[[col]]
      if (length(selected_options) > 0) {
        filtered <- filtered %>% 
          filter(.data[[col]] %in% selected_options)
      }
    }
    
    return(filtered)
  })
  
  # Reactive expression for calculating the percentage
  matchingPercentage <- reactive({
    req(filteredData())
    percent <- round(nrow(filteredData()) / nrow(data) * 100, 0)
    paste(percent, "% of Devices Selected")
  })
  
  output$pieChartHeader <- renderUI({
    req(matchingPercentage())
    div(class = "card-header", 
        HTML(paste("Observation Status Distribution (", matchingPercentage(), ")"))
    )
  })
  
  # Reactive expression for the list and count of observed devices
  observedDevices <- reactive({
    req(filteredData())
    devices <- filteredData()$device[filteredData()$observed == "Observed"]
    sortedDevices <- sort(unique(devices)) # Sort devices alphabetically
    list(devices = sortedDevices, count = length(sortedDevices))
  })
  
  # Reactive expression for the list and count of non-observed devices
  notObservedDevices <- reactive({
    req(filteredData())
    devices <- filteredData()$device[filteredData()$observed == "Non-Observed"]
    sortedDevices <- sort(unique(devices)) # Sort devices alphabetically
    list(devices = sortedDevices, count = length(sortedDevices))
  })
  
  # Render the card for observed devices with count in the title
  output$observedDeviceList <- renderUI({
    data <- observedDevices()
    req(data$devices)
    div(class = "card border-secondary mb-3",
        div(class = "card-header", sprintf("Observed Devices (n=%d)", data$count)),
        div(class = "card-body", tags$ul(class = "device-list", lapply(data$devices, tags$li)))
    )
  })
  
  # Render the card for non-observed devices with count in the title
  output$notObservedDeviceList <- renderUI({
    data <- notObservedDevices()
    req(data$devices)
    div(class = "card border-secondary mb-3",
        div(class = "card-header", sprintf("Non-Observed Devices (n=%d)", data$count)),
        div(class = "card-body", tags$ul(class = "device-list", lapply(data$devices, tags$li)))
    )
  })
  
  # Pie chart for observed vs. not observed
  output$observedChart <- renderPlot({
    req(filteredData())
    observedCounts <- table(filteredData()$observed)
    
    # Ensure there are always at least two categories for the pie chart
    observedDF <- as.data.frame(observedCounts)
    if (ncol(observedDF) == 1) {
      observedDF <- data.frame(Observed = names(observedCounts), Count = as.numeric(observedCounts))
    } else {
      names(observedDF) <- c("Observed", "Count")
    }
    
    # Handle the case where there might be no data to plot
    if (nrow(observedDF) == 0) {
      return(ggplot() + annotate("text", x = 0, y = 0, label = "No data to display", size = 5))
    }
    
    # Calculate percentages for labels
    total <- sum(observedDF$Count)
    observedDF$Label <- paste(observedDF$Observed, ": ", round(observedDF$Count / total * 100, 0), "%", sep = "")
    
    ggplot(observedDF, aes(x = "", y = Count, fill = Observed)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      geom_text(aes(label = Label), position = position_stack(vjust = 0.5), 
                color = "white", size = 5, fontface = "bold") +
      theme_void() +
      scale_fill_manual(values = c("#6f2877","#4ac0e0"))+
      theme(legend.position = "none",
            panel.background = element_blank(), # Set panel background to blank
            plot.background = element_rect(fill = "transparent", colour = NA)) # Transparent plot background
  }, bg = "transparent") # Ensure the background of the rendered plot is transparent
  
  observeEvent(input$or_full_explanation_btn, {
    showModal(modalDialog(
      title = "Full Explanation",
      "The reasons a clinician may choose to use a device are multi-faceted. Some of these reasons may include the device's complexity, function, and utility. The Observation Rates tool provides six characteristic categories about the devices available to clinicians. It allows you to explore how single characteristics or combinations of characteristics were associated with a higher or lower proportion of observed use.",
      tags$p("The characteristics include:", style = "margin-top: 10px; margin-bottom: 5px;"),  # Adjusted margins for the paragraph
      tags$ul(
        tags$li("Training - The level of instruction a clinician needs before they can use the device."),
        tags$li("Purpose - The number of uses for the device."),
        tags$li("Power - The power source required to operate the device."),
        tags$li("Actuation - Whether a device moves on its own using a motor.."),
        tags$li("Monitor - Whether the device includes an electronic display to provide feedback."),
        tags$li("Objective - The device's intended purpose: perform a measurement, provide intervention (treatment), or both."),
        style = "margin-top: 0; margin-bottom: 10px;"  # Reduced margin for the bullet points
      ),
      "Try deselecting some of the options from the checkboxes to filter out devices with that characteristic. As a result, the lists of observed and unobserved devices will remove those devices, and the proportion of observed use in the pie chart may change. A dramatic change in these proportions may indicate that this characteristic may have a meaningful impact on a clinician's decision to use a device.",
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  observeEvent(input$or_examples_btn, {    
    showModal(modalDialog(
      title = "Examples of Use",
      "Coming Soon",
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
    
  })
  
  #``````````````````````````
  #``````Associated Characteristics Tab ````````
  # Define the specific order for each column
  choiceOrder <- list(
    observed = c("No (58%)", "Yes (42%)"),
    training = c("Entry-Level Training (24%)","Short Training (43%)","Complex Training (32%)"),
    purpose = c("Single-Purpose (64%)","Multi-Purpose (36%)"),
    power = c("Unpowered (32%)","Battery Power (27%)","AC Power (41%)"),
    actuation = c("Unactuated (68%)", "Actuated (32%)"),
    monitor = c("No Monitor (56%)", "Monitor (44%)"),
    objective = c("Measurement (14%)", "Intervention (75%)", "Combined (11%)")
  )
  
  output$nodeSelectionUI <- renderUI({
             lapply(input$columns, function(col) {
               # Get the unique choices for the column
               choices <- unique(sankey_df[[col]])
               
               # Order the choices based on the predefined order
               if (col %in% names(choiceOrder)) {
                 orderedChoices <- choiceOrder[[col]][choiceOrder[[col]] %in% choices]
                 remainingChoices <- setdiff(choices, orderedChoices)
                 choices <- c(orderedChoices, remainingChoices)
               }
               
               # Create the checkbox group input with ordered choices
                      div(class = "card border-secondary mb-3",
                          h6(class = "card-header", tools::toTitleCase(col)),
                          div(class = "card-body",
                              checkboxGroupInput(paste0("select_", col),
                                                 label = NULL,  # Label is now part of the card header
                                                 choices = choices,
                                                 selected = choices)
                          )
                      )
             })
  })
  
  sankey_data <- reactive({
    if (length(input$columns) < 2) return(NULL)
    
    filtered_sankey_df <- sankey_df
    for (col in input$columns) {
      filter_input <- input[[paste0("select_", col)]]
      filtered_sankey_df <- filtered_sankey_df %>% filter(get(col) %in% filter_input)
    }
    
    selected_sankey_df <- filtered_sankey_df %>% select(all_of(c(input$columns, "value")))
    
    # Create nodes with unique names and labels
    nodes_list <- lapply(selected_sankey_df %>% select(-value), unique)
    nodes <- data.frame(name = unlist(nodes_list))
    nodes$label <- sub('_[0-9]*$', '', nodes$name) # Generate labels by removing numeric suffixes
    
    # Create links
    links_list <- list()
    for (i in 1:(ncol(selected_sankey_df) - 2)) {
      links_temp <- data.frame(
        source = match(selected_sankey_df[[i]], nodes$name) - 1,
        target = match(selected_sankey_df[[i + 1]], nodes$name) - 1,
        value = selected_sankey_df$value
      )
      links_list[[i]] <- links_temp
    }
    
    links <- do.call(rbind, links_list)
    links <- links %>%
      group_by(source, target) %>%
      summarize(value = sum(value)) %>%
      ungroup()
    
    list(nodes = nodes, links = links)
  })
  
  output$sankeyDiagram <- renderSankeyNetwork({
    dat <- sankey_data()
    if (is.null(dat)) return(NULL)
    
    sankeyPlot <- sankeyNetwork(Links = dat$links, Nodes = dat$nodes, Source = "source",
                                Target = "target", Value = "value", NodeID = "label",
                                units = "Devices", fontSize = 17, nodeWidth = 10, iterations = 0)
    
    # Get the selected columns for headers
    selected_cols <- input$columns
    
    # Add category names as headers using onRender
    htmlwidgets::onRender(
      sankeyPlot, 
      sprintf('
  function(el) { 
    function toTitleCase(str) {
      return str.replace(/\\w\\S*/g, function(txt) { 
        return txt.charAt(0).toUpperCase() + txt.substr(1).toLowerCase(); 
      });
    }

    var cols_x = this.sankey.nodes().map(d => d.x).filter((v, i, a) => a.indexOf(v) === i).sort(function(a, b){return a - b});
    var col_names = %s.map(toTitleCase);  // Convert column names to title case
    cols_x.forEach((d, i) => {
      d3.select(el).select("svg")
        .append("text")
        .attr("x", d)
        .attr("y", 12)
        .style("font-weight", "bold")  // Make text bold
        .text(col_names[i]);
    })
  }
  ', jsonlite::toJSON(selected_cols, auto_unbox = TRUE))
    )
    sankeyPlot
  })
  
  
}

# Run the Shiny App
shinyApp(ui = ui, server = server)