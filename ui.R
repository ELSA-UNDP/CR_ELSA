

header <- dashboardHeader(
  title = "I AM ELSA Costa Rica v2",
  titleWidth = 450
)

body <- dashboardBody(tags$head(tags$style(HTML('
        .box { margin-bottom: 8px; } 
        
        [class*="col-lg-"],[class*="col-md-"],
        [class*="col-sm-"],[class*="col-xs-"]{
        padding-right:4px !important;
        padding-left:4px !important;
        }
        '))),
  
  fluidRow(
    column(width = 4,
           box(
             title = "Action Mapping for Essential Life Support Areas",
             # The id lets us use input$tabset1 on the server to find the current tab
             id = "in", width = NULL,height = "100%",#"600px", 
             actionButton("mrun",HTML("<h4>Run Optimization</h4>")), 
             helpText("================================="), 
             helpText(HTML("<h4><strong>Global parameters:</strong></h4>")),
             selectInput("cost", "What cost metric should be used:",
                         c("Area" = "area"
                           ,"Human Footprint" = "human"
                           )),
             
             tags$b("Do you want to lock in the following features?"),
             helpText(HTML("Minimum targets PA locked in: protect (26%)")),
             helpText(HTML("Minimum targets PES locked in: protect (19%), restore (3%), manage (1%)")),
             helpText(HTML("Minimum targets PA + PES locked in: protect (40%), restore (3%), manage (1%)")),
             checkboxInput("protect", "Protected Areas", TRUE),
             checkboxInput("pes", "Payment for Ecosystem Services", FALSE),
             tags$hr(),
             helpText(HTML("Given the current problem formulations values need to be set >= 0.001.")),
             numericInput("zone_1_target", "Protect target", 27, min = 0, max = 100, step = 0.1),
             numericInput("zone_2_target", "Restore target", 8, min = 0, max = 100, step = 0.1),
             numericInput("zone_3_target", "Manage target", 5, min = 0, max = 100, step = 0.1),
             numericInput("zone_4_target", "Urban Green target", 0.3, min = 0, max = 100, step = 0.1),
             tags$hr(),
             numericInput("blm", "Boundary penalty factor", 0, min = 0, max = 1000000, step = 0.1)
             )
           ),
    
    column(width = 8,
           tabBox(
             title = "",
             # The id lets us use input$tabset1 on the server to find the current tab
             id = "out", width = NULL,height = "100%", #"600px",
             tabPanel("Edit weights", id = "rhweights", 
                      helpText("Information about weights to go here"), 
                      rHandsontableOutput("hot_wgt")),
             tabPanel("Edit impacts", id = "rhimpacts", 
                      helpText("Information about impacts to go here"), 
                      rHandsontableOutput("hot_imp")),
             tabPanel("Results + Download",
                      helpText(HTML("<h4>Result Summary Table</h4>")),
                      helpText(HTML("Placeholder for result summary table text. Numeric values are in percent.")),
                      DT::dataTableOutput("summary"),
                      #DTOutput("summary"),
                      helpText(HTML("<br>")),
                      helpText(HTML("<h4>Results download (output layers):</h4>")),
                      downloadButton("downloadSHP", label = "Download"),
                      helpText(HTML("<br>")),
                      helpText(HTML("<h4>Results download (summary table):</h4>")),
                      downloadButton("download_ssoln",label = "Download"),
                      helpText(HTML("<br>"))
             ),
             tabPanel("Result Map",leafletOutput("cadMap",height=700))
             
             
           ))
    )
  )


dashboardPage(
  header,
  dashboardSidebar(disable = TRUE),
  body
)

