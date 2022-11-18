library(tidyverse)
library(shiny)
library(ggplot2)
library(bslib)
library(shiny.pwa)

#
SDI_Max_Values <- readr::read_csv("~/Projects/Operations Shiny App/PWA Attempts/PWA/data/SDIMaxValues.csv")
sg.sf <- readr::read_csv("~/Projects/Operations Shiny App/PWA Attempts/PWA/data/QMD_Data_For_DMD.csv")

options(shiny.host = '127.0.0.1')
options(shiny.port = 4088)
# Read in Data for EcoRegions --------------------------------------
#SDI_Max_Values <- readr::read_csv(here::here("data/SDIMaxValues.csv")) # SDI Max Values for Each EcoRegion and ForestType
#sg.sf <- readr::read_csv(here::here("data/QMD_Data_For_DMD.csv")) #data block for qmd isolines (copy from sheet=sg.dat, cols=A-Q)

shinyApp(
  # Define UI for application that draws a histogram
  ui <- fluidPage(
    # tags$head(tags$link(rel = "manifest", href = "manifest.json")),
    theme = bslib::bs_theme(bg = "rgb(253, 253, 253)", fg = "#40543E",
                     primary = "#40543E", secondary = "#5F7E62", base_font = font_google("Lato"),
                     heading_font = font_google("Oswald"), `enable-shadows` = TRUE),

    # Application title
    titlePanel("Density Management Diagrams: Relative Density"),
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      sidebarPanel( 
        width = 2,
        selectInput(
          "district", "Select District",
          choices = sort(unique(SDI_Max_Values$District)),
          selected = SDI_Max_Values$District[1]
        ),
        selectInput(
          "matrix", "Select Matrix",
          choices = c(),
          multiple = FALSE
        ),
        fluidRow(
          h4("Current RD")
        ),
        fluidRow(
          sliderInput("ba_adjuster1", "Adjust Initial Basal Area", min = 0, max = 300,
                      value = 100, step = 1)
        ),
        fluidRow(
          sliderInput("tpa_adjuster1", "Adjust Initial Strata TPA", min = 0, max = 1200,
                      value = 500, step = 5)
      ),
      fluidRow(
        h1(" ")
      ),
      fluidRow(
        h4("Post Treatment")
      ),
      fluidRow(
        sliderInput("ba_adjuster2", "Adjust Post Basal Area", min = 0, max = 300,
                    value = 15, step = 1)
      ),
      fluidRow(
        sliderInput("tpa_adjuster2", "Adjust Post Harvest TPA", min = 0, max = 1200,
                    value = 100, step = 5)
      ),
      fluidRow(
        h4("Predicted Removal")
      ),
      fluidRow(
        h4(textOutput("volumeremoval"))
      ),
      fluidRow(
        downloadButton('downloadPlot', 'Download DMD')
      )
    ),
      mainPanel(
        pwa("http://.",
            title = "SILC DMDs",
            icon = "Logo.png",
            color = "#40543E"),
        fluidRow(
          h2(textOutput("sdiheading")),
          br(),
          h3(textOutput("sdiheading2"))
        ),
        fluidRow(
          column(width = 10,
                 plotOutput("dmd", height=725)
          ),
          column(width = 2,
                 strong(textOutput("strataqmd")),
                 strong(textOutput("stratard")),
                 br(),
                 strong(textOutput("newqmd")),
                 strong(textOutput("newrd")),
                 br(),
                 br(),
                 strong("Isoline Definitions:", style = "font-size: 16px"),
                 br(),
                 br(),
                 # p("The",
                 #   span(strong("RED", style = "color:red")),
                 #   "line equals an RD of",
                 #   span(strong(".15", style = "color:red")),
                 #   style = "color:red"
                 # ),
                 # 
                 # p("The first and second",
                 #   span(strong("BLUE")),
                 #   "lines equal an RD of",
                 #   span(strong(".30 and .55")),
                 #   style = "color:blue"
                 # ),
                 # 
                 # p("The dashed",
                 #   span(strong("ORANGE", style = "color:orange")),
                 #   "line represents an RD of",
                 #   span(strong(".65", style = "color:orange")),
                 #   style = "color:orange"
                 # ),
                 # 
                 # p("The",
                 #   span(strong("BLACK", style = "color:black")),
                 #   "line equals an RD of",
                 #   span(strong("1"))
                 # ),
                 
                 p("All values between the ",
                   span(strong("RED .15", style = "color:red")),
                   "line and the first",
                   span(strong("BLUE .30", style = "color:blue")),
                   "line indicate the stand is",
                   span(strong("Understocked", style = "color:black")),
                   style = "color:red", style = "font-size: 16px"
                 ),
                 
                 p("All values between the",
                          span(strong("BLUE .30 and .55", style = "color:blue")),
                          "lines represent",
                          span(strong("Optimal Stocking", style = "color:black")), 
                          style = "color:black"),   
                 
                 p("All values between the ",
                   span(strong("BLUE .55", style = "color:blue")),
                   "line and the dashed",
                   span(strong("ORANGE .65", style = "color:orange")),
                   "line indicate the stand is",
                   span(strong("Approaching Natural Morality and 
                                  Likely Requires Treatment.", style = "color:black")),
                   style = "color:black"
                 ),
                 
                 p("All values between the dashed",
                          span(strong("Orange .65", style = "color:orange")),
                          "line and the",
                          span(strong("BLACK 1")),
                          "line indicate the stand is",
                          span(strong("OVERSTOCKED and At Risk of VALUE LOSS.", style = "color:black"))
                 ), 
                 
                 p("All values above the",
                   span(strong("BLACK 1", style = "color:black")),
                   "Isoline indicate the stand is",
                   span(strong("OVERSTOCKED and Is AT IMMEDIATE Risk Of MORTALITY and VALUE LOSS", style = "color:black"))
                 )
          )
        )
      )
     )
    # tags$script(
    #   HTML(
    #     "if ('serviceWorker' in navigator) { navigator.serviceWorker.register('/sw.js', { scope: '/' }).then(function(registration) { console.log('Service Worker Registered'); }); navigator.serviceWorker.ready.then(function(registration) { console.log('Service Worker Ready'); }); }"
    #   )
    #   ),
    ),
  
  # Define server logic required to draw a histogram
  server <- function(input, output) {

    SDIMaxDF = reactiveVal(SDI_Max_Values)
    
    
    # Sidebar Inputs ----------------------------------------------------------
    output$place <- renderText({
      print(reactiveValuesToList(session$clientData))
    })
    
    
    observe({
      req(input$district)
      Matrix = SDIMaxDF() %>%
        filter(District %in% input$district) %>%
        pull(Matrix) %>%
        unique() %>%
        sort()
      
      updateSelectInput(
        inputId = "matrix", 
        choices = Matrix
      )
    })


# Start Current RD at predicted value from inventory ----------------------

    
    observe({
      req(input$district)
      BA = SDIMaxDF() %>%
        filter(District %in% input$district) %>%
        filter(Matrix %in% input$matrix) %>% 
        summarize(round(unique(BAPA),1))
      BA = as.vector(BA)
      BA = as.numeric(BA)
      BA
      
      updateSliderInput(
        inputId = "ba_adjuster1", 
        value = BA
      )
    })
    
    observe({
      req(input$district)
      TPA = SDIMaxDF() %>%
        filter(District %in% input$district) %>%
        filter(Matrix %in% input$matrix) %>% 
        summarize(round(unique(TPA),1))
      TPA = as.vector(TPA)
      TPA = as.numeric(TPA)
      TPA
      
      updateSliderInput(
        inputId = "tpa_adjuster1", 
        value = TPA
      )
    })
 
    # SDI Max Value -----------------------------------------------------------
    
    SDIVal <- reactive({
      SDIMaxDF() %>%
        filter(District %in% input$district) %>%
        filter(Matrix %in% input$matrix) %>% 
        summarize(round(unique(SDImax),1))
    })
    
    output$sdiheading <- renderText({
      paste0(input$district, ": ", input$matrix)    
    })
    
    output$sdiheading2 <- renderText({
      paste0("SDI Max: ", SDIVal())
    })
    
    StrataQMD = reactive({
      qmd = sqrt(input$ba_adjuster1/(.005454*input$tpa_adjuster1))
      qmd = as.numeric(qmd)
      qmd = round(qmd,1)
      qmd
    })
      
    StrataRD = reactive({
      sdi = ((StrataQMD()/10)^1.605)*input$tpa_adjuster1
      rd = sdi/SDIVal()
      rd = as.numeric(rd)
      rd = round(rd,2)
      rd
    })
    
    NewQMD = reactive({
      qmd = sqrt(input$ba_adjuster2/(.005454*input$tpa_adjuster2))
      qmd = as.numeric(qmd)
      qmd = round(qmd,1)
      qmd
    })
    
    NewRD = reactive({
      sdi = ((NewQMD()/10)^1.605)*input$tpa_adjuster2
      rd = sdi/SDIVal()
      rd = as.numeric(rd)
      rd = round(rd,2)
      rd
    })
    
    output$strataqmd <- renderText({
      paste0("Initial QMD: ", StrataQMD(), "in")
    })
    
    output$stratard <- renderText({
      paste0("Initial RD: ", StrataRD())
    })
    
    output$newqmd <- renderText({
      paste0("Proposed QMD: ", NewQMD(), "in")
    })
    
    output$newrd <- renderText({
      paste0("Proposed RD: ", NewRD())
    })
    
    # Functions for Isolines --------------------------------------------------
    
    TPA_SDI = function(QMD, SDI){                         # Function Used To Calculate the TPA for a SDI Value at specific QMD
      TPA <- SDI/(QMD/10)^1.605
      return(TPA)
    }
    
    BA_SDI = function(QMD, TPA){                          # Function Used To Calculate the BA for a SDI Value at specific QMD and TPA 
      BA <- QMD*sqrt(TPA)*sqrt(.005454)
      BA.SDI = BA^2
      return(BA.SDI)
    }
    
    Trees_Per_Acre <- function(QMD, SDI_Iso_Value){       # Calculate Vector for X Axis Isoline Values
      i <- 0
      
      while (i < length(QMDS)) {
        i <- i+1
        RD_Trees[i] <- TPA_SDI(QMDS[i], SDI_Iso_Value)
      }
      RD_Trees <- as.numeric(RD_Trees)
      RD_Trees <- round(RD_Trees, 3)
      return(RD_Trees)
    }
    
    
    Basal_Area_Per_Acre <- function(QMD, RD_TPA) {       # Calculate Vector for Y Axis Isoline Values
      i <- 0
      while (i < length(QMDS)) {
        i <- i + 1
        BA_Trees[i] <- BA_SDI(QMDS[i], RD_TPA[i])
      }
      BA_Trees <- as.numeric(BA_Trees)
      BA_Trees <- round(BA_Trees, 3)
      return(BA_Trees)
    }
    
    # Reactive Values Needed For Isoline Functions------------------------------
    
    SDI_Iso_Values <- reactiveValues()
    SDI_Iso_Values$SDI_15 <- reactive({SDIVal()*.15})
    SDI_Iso_Values$SDI_30 <- reactive({SDIVal()*.3})
    SDI_Iso_Values$SDI_55 <- reactive({SDIVal()*.55})
    SDI_Iso_Values$SDI_67 <- reactive({SDIVal()*.67})
    QMDS <- seq(.1, 18, .1)
    RD_Trees <- 1
    BA_Trees <- 1
    
    # Create reactiveValue Container  -----------------------------------------
    
    SDI_Vals <- reactiveValues
    Isolines <- reactiveValues()
    
    
    # Create reactiveValue Observers ------------------------------------------
    
    
    Isolines$rd_15_trees <- reactive({                     #RD .15 X Axis
      Trees_Per_Acre(QMDS, SDI_Iso_Values$SDI_15())
    })
    
    Isolines$rd_30_trees <- reactive({                     #RD .30 X Axis
      Trees_Per_Acre(QMDS, SDI_Iso_Values$SDI_30())
    })
    
    Isolines$rd_55_trees <- reactive({                     #RD .55 X Axis
      Trees_Per_Acre(QMDS, SDI_Iso_Values$SDI_55())
    })
    
    Isolines$rd_67_trees <- reactive({                     #RD .67 X Axis
      Trees_Per_Acre(QMDS, SDI_Iso_Values$SDI_67())
    })
    
    Isolines$rd_1_trees <- reactive({                     #RD 1 X Axis
      Trees_Per_Acre(QMDS, SDIVal())
    })
    
    Isolines$ba_15_trees <- reactive({                     #RD .15 Y Axis
      Basal_Area_Per_Acre(QMDS, Isolines$rd_15_trees())
    })
    
    Isolines$ba_30_trees <- reactive({                     #RD .30 Y Axis
      Basal_Area_Per_Acre(QMDS, Isolines$rd_30_trees())
    })
    
    Isolines$ba_55_trees <- reactive({                     #RD .55 Y Axis
      Basal_Area_Per_Acre(QMDS, Isolines$rd_55_trees())
    })
    
    Isolines$ba_67_trees <- reactive({                     #RD .67 Y Axis
      Basal_Area_Per_Acre(QMDS, Isolines$rd_67_trees())
    })
    
    Isolines$ba_1_trees <- reactive({                     #RD 1 Y Axis
      Basal_Area_Per_Acre(QMDS, Isolines$rd_1_trees())
    })
    
    #  -------------------------------------------------------------------------
  
    
    # DMD Plot ----------------------------------------------------------------
    
    output$dmd = renderPlot({
      ggplot() +
        # geom_point(data=NE.SDImax_SF, aes(x=TPA, y=BAAC), show.legend=F) + #display sample data
        geom_line(data=sg.sf, aes(x=tpa, y=qmd.3in), color="black", show.legend = FALSE) +
        annotate("text", x=1250, y=60, label="3", color="black", size=6) +
        geom_line(data=sg.sf, aes(x=tpa, y=qmd.4in), color="black", show.legend = FALSE) +
        annotate("text", x=1250, y=105, label="4", color="black", size=6) +
        geom_line(data=sg.sf, aes(x=tpa, y=qmd.5in), color="black", show.legend = FALSE) +
        annotate("text", x=1250, y=165, label="5", color="black", size=6) +
        geom_line(data=sg.sf, aes(x=tpa, y=qmd.6in), color="black", show.legend = FALSE) +
        annotate("text", x=1250, y=240, label="6", color="black", size=6) +
        geom_line(data=sg.sf, aes(x=tpa, y=qmd.7in), color="black", show.legend = FALSE) +
        annotate("text", x=975, y=265, label="7", color="black", size=6) +
        geom_line(data=sg.sf, aes(x=tpa, y=qmd.8in), color="black", show.legend = FALSE) +
        annotate("text", x=775, y=280, label="8", color="black", size=6) +
        geom_line(data=sg.sf, aes(x=tpa, y=qmd.9in), color="black", show.legend = FALSE) +
        annotate("text", x=650, y=292, label="9", color="black", size=6) +
        geom_line(data=sg.sf, aes(x=tpa, y=qmd.10in), color="black", show.legend = FALSE) +
        annotate("text", x=550, y=305, label="10", color="black", size=6) +
        geom_line(data=sg.sf, aes(x=tpa, y=qmd.11in), color="black", show.legend = FALSE) +
        geom_line(data=sg.sf, aes(x=tpa, y=qmd.12in), color="black", show.legend = FALSE) +
        annotate("text", x=475, y=315, label="11", color="black", size=6) +
        geom_line(data=sg.sf, aes(x=tpa, y=qmd.13in), color="black", show.legend = FALSE) +
        annotate("text", x=415, y=325, label="12", color="black", size=6) +
        geom_line(data=sg.sf, aes(x=tpa, y=qmd.14in), color="black", show.legend = FALSE) +
        annotate("text", x=360, y=337, label="13", color="black", size=6) +
        geom_line(data=sg.sf, aes(x=tpa, y=qmd.15in), color="black", show.legend = FALSE) +
        geom_line(data=sg.sf, aes(x=tpa, y=qmd.16in), color="black", show.legend = FALSE) +
        annotate("text", x=325, y=345, label="14", color="black", size=6) +
        annotate("text", x=285, y=355, label="15", color="black", size=6) +
        geom_line(data=sg.sf, aes(x=tpa, y=qmd.17in), color="black", show.legend = FALSE) +
        geom_line(data=sg.sf, aes(x=tpa, y=qmd.18in), color="black", show.legend = FALSE) +
        annotate("text", x=270, y=379, label="18-in QMD", color="black", size=6) +
        geom_line(aes(x=Isolines$rd_15_trees(), y=Isolines$ba_15_trees()), color="red", size=1, show.legend = FALSE) +
        geom_line(aes(x=Isolines$rd_30_trees(), y=Isolines$ba_30_trees()), color="blue", size=1, show.legend = FALSE) +
        geom_line(aes(x=Isolines$rd_55_trees(), y=Isolines$ba_55_trees()), color="blue", size=1, show.legend = FALSE) +
        geom_line(aes(x=Isolines$rd_1_trees(),  y=Isolines$ba_1_trees()), color="black", size=1, show.legend = FALSE) +
        geom_line(aes(x=Isolines$rd_67_trees(), y=Isolines$ba_67_trees()), color="orange", linetype="dashed", size=1, show.legend = FALSE) +
        theme(panel.grid.major = element_line(size = 1), panel.grid.minor = element_line(size = 0.75)) +
        scale_x_continuous(name="Stem density (tpa)", limits=c(50, 1255),
                           breaks=c(50,100,200,400,600,800,1000,1200)) +
        theme_minimal() +
        theme(axis.text = element_text(size = 14),
              axis.title = element_text(size=16, face="bold")) +
        scale_y_continuous(name="Basal area (ft2/ac)", limits=c(0, 400),
                           breaks=seq(25, 450, 25)) +
        # geom_point(aes(x = InitialTPA()[1], y = InitialBA()[1]), size = 5, color = "blue", fill = "red", shape=23 ) +
        # geom_text(aes(x = InitialTPA()[1] + 20, y = InitialBA()[1] + 20), label="Predicted RD", size = 4) +
        geom_point(aes(x = input$tpa_adjuster1, y = input$ba_adjuster1), size = 6, color = "blue", fill = "blue", shape=21) +
        geom_text(aes(x = input$tpa_adjuster1 + 15, y = input$ba_adjuster1 + 15), label="Current RD", size = 6) +
        geom_point(aes(x = input$tpa_adjuster2, y = input$ba_adjuster2), size = 6, color = "blue", fill = "darkgreen", shape=21) +
        geom_text(aes(x = input$tpa_adjuster2 + 15, y = input$ba_adjuster2 + 15), label="Post-Treatment", size = 6) 
    })

# Render Plot For Download ------------------------------------------------

    plotInput = reactive({
      ggplot() +
        # geom_point(data=NE.SDImax_SF, aes(x=TPA, y=BAAC), show.legend=F) + #display sample data
        geom_line(data=sg.sf, aes(x=tpa, y=qmd.3in), color="black", show.legend = FALSE) +
        annotate("text", x=1250, y=60, label="3", color="black", size=6) +
        geom_line(data=sg.sf, aes(x=tpa, y=qmd.4in), color="black", show.legend = FALSE) +
        annotate("text", x=1250, y=105, label="4", color="black", size=6) +
        geom_line(data=sg.sf, aes(x=tpa, y=qmd.5in), color="black", show.legend = FALSE) +
        annotate("text", x=1250, y=165, label="5", color="black", size=6) +
        geom_line(data=sg.sf, aes(x=tpa, y=qmd.6in), color="black", show.legend = FALSE) +
        annotate("text", x=1250, y=240, label="6", color="black", size=6) +
        geom_line(data=sg.sf, aes(x=tpa, y=qmd.7in), color="black", show.legend = FALSE) +
        annotate("text", x=975, y=265, label="7", color="black", size=6) +
        geom_line(data=sg.sf, aes(x=tpa, y=qmd.8in), color="black", show.legend = FALSE) +
        annotate("text", x=775, y=280, label="8", color="black", size=6) +
        geom_line(data=sg.sf, aes(x=tpa, y=qmd.9in), color="black", show.legend = FALSE) +
        annotate("text", x=650, y=292, label="9", color="black", size=6) +
        geom_line(data=sg.sf, aes(x=tpa, y=qmd.10in), color="black", show.legend = FALSE) +
        annotate("text", x=550, y=305, label="10", color="black", size=6) +
        geom_line(data=sg.sf, aes(x=tpa, y=qmd.11in), color="black", show.legend = FALSE) +
        geom_line(data=sg.sf, aes(x=tpa, y=qmd.12in), color="black", show.legend = FALSE) +
        annotate("text", x=475, y=315, label="11", color="black", size=6) +
        geom_line(data=sg.sf, aes(x=tpa, y=qmd.13in), color="black", show.legend = FALSE) +
        annotate("text", x=415, y=325, label="12", color="black", size=6) +
        geom_line(data=sg.sf, aes(x=tpa, y=qmd.14in), color="black", show.legend = FALSE) +
        annotate("text", x=360, y=337, label="13", color="black", size=6) +
        geom_line(data=sg.sf, aes(x=tpa, y=qmd.15in), color="black", show.legend = FALSE) +
        geom_line(data=sg.sf, aes(x=tpa, y=qmd.16in), color="black", show.legend = FALSE) +
        annotate("text", x=325, y=345, label="14", color="black", size=6) +
        annotate("text", x=285, y=355, label="15", color="black", size=6) +
        geom_line(data=sg.sf, aes(x=tpa, y=qmd.17in), color="black", show.legend = FALSE) +
        geom_line(data=sg.sf, aes(x=tpa, y=qmd.18in), color="black", show.legend = FALSE) +
        annotate("text", x=270, y=379, label="18-in QMD", color="black", size=6) +
        geom_line(aes(x=Isolines$rd_15_trees(), y=Isolines$ba_15_trees()), color="red", size=1, show.legend = FALSE) +
        geom_line(aes(x=Isolines$rd_30_trees(), y=Isolines$ba_30_trees()), color="blue", size=1, show.legend = FALSE) +
        geom_line(aes(x=Isolines$rd_55_trees(), y=Isolines$ba_55_trees()), color="blue", size=1, show.legend = FALSE) +
        geom_line(aes(x=Isolines$rd_1_trees(),  y=Isolines$ba_1_trees()), color="black", size=1, show.legend = FALSE) +
        geom_line(aes(x=Isolines$rd_67_trees(), y=Isolines$ba_67_trees()), color="orange", linetype="dashed", size=1, show.legend = FALSE) +
        theme(panel.grid.major = element_line(size = 1), panel.grid.minor = element_line(size = 0.75)) +
        scale_x_continuous(name="Stem density (tpa)", limits=c(50, 1255),
                           breaks=c(50,100,200,300,400,500,600,700,800,900,1000,1200)) +
        theme(axis.text = element_text(size = 14),
              axis.title = element_text(size=16, face="bold")) +
        scale_y_continuous(name="Basal area (ft2/ac)", limits=c(0, 400),
                           breaks=seq(25, 450, 25)) +
        # geom_point(aes(x = InitialTPA()[1], y = InitialBA()[1]), size = 5, color = "blue", fill = "red", shape=23 ) +
        # geom_text(aes(x = InitialTPA()[1] + 20, y = InitialBA()[1] + 20), label="Predicted RD", size = 4) +
        geom_point(aes(x = input$tpa_adjuster1, y = input$ba_adjuster1), size = 6, color = "blue", fill = "blue", shape=21) +
        geom_text(aes(x = input$tpa_adjuster1 + 15, y = input$ba_adjuster1 + 15), label="Current RD", size = 4) +
        geom_point(aes(x = input$tpa_adjuster2, y = input$ba_adjuster2), size = 6, color = "blue", fill = "darkgreen", shape=21) +
        geom_text(aes(x = input$tpa_adjuster2 + 15, y = input$ba_adjuster2 + 15), label="Post-Treatment", size = 4) +
        geom_text(aes(x = 925, y = 375), label = paste0(input$district, ": ", input$matrix ), size = 7) +
        geom_text(aes(x = 925, y = 350), label = paste0("Predicted Cords Removed: ", VolumeRemoved()), size = 5) +
        geom_text(aes(x = 925, y = 330), label = paste0("Residual RD: ", NewRD()), size = 5)
    })
    
# Predicted Removal -------------------------------------------------------
   
    VolumeRemoved = reactive({
      
      VBAR = SDIMaxDF() %>%
        filter(District %in% input$district) %>%
        filter(Matrix %in% input$matrix) %>% 
        summarize(round(unique(VBAR),1))
      VBAR = as.vector(VBAR)
      VBAR = as.numeric(VBAR)
      VBAR
      
      volume = (input$ba_adjuster1 - input$ba_adjuster2)*VBAR
      volume  
   })
    
   output$volumeremoval = renderText({
     paste0(VolumeRemoved(), " Cords")
   })    
   

# Download Plot -----------------------------------------------------------

   output$downloadPlot = downloadHandler(
     filename = 'SILC_DMD.png',
       content = function(file) {
       device <- function(..., width, height) {
         grDevices::png(..., width = 12, height = 9,
                        res = 300, units = "in")
       }
       ggsave(file, plot = plotInput(), device = device)
     })
   
  }
  
)



# Run the application 
shinyApp(ui = ui, server = server)
