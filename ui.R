

## HHS Dashboard UI ###
fillPage(fluidPage(
  theme = shinytheme("lumen"),
  #sidebarLayout (
  fluidRow(
    column(
      3,
      #### Input Data ####
      #Select the type of analysis to run
      selectInput(
        inputId = "Analysis",
        label = strong("TYPE OF ANALYSIS"),
        choices = c("Socio-economic Baseline", "Socio-economic Trends"),
        selected = "Socio-economic Baseline"
      ),
      ## help text
      bsTooltip(
        id = "Analysis",
        title = "Select the type of analysis you would like to perform",
        placement = "right",
        trigger = "hover",
        options = list(container = "body", expire = 2000)
        
      )
    ),
    
    #Select the type of plot to use
    #selectInput(inputId = 'grouping_level', strong("ANALYSIS LEVEL"),
    #            choices = c("Country" = 'country', "Subnational Government" = 'level1_name',
    #                      "Local Government"= "level2_name", "Managed Access"='ma_name'),
    #          selected = "ma_name"),
    ## help text
    # bsTooltip(id="grouping_level", title ="Select a grouping level to calculate means and errors",
    #       placement ="right", trigger = "hover", options = list(container = "body")),
    
    #Select the Year
    #uiOutput(outputId = "Year"),
    
    #Select the Country
    column(
      2,
      selectInput(
        inputId = "Country",
        label = strong("Country"),
        choices = c(
          "Choose country",
          "Brazil" = 'BRA',
          "Honduras" = 'HND',
          "Indonesia" = 'IDN',
          "Mozambique" = 'MOZ',
          "Philippines" = 'PHL',
          "Federated States of Micronesia" = 'FSM',
          "Palau" = "PLW"
        ),
        selected = "IDN",
        multiple = FALSE,
        selectize = TRUE
      ),
      bsTooltip(
        id = "Country",
        title = "Select a country",
        placement = "rigth",
        trigger = "hover",
        options = list(container = "body")
      )
    ),
    
    column(
      2,
      #Select the Region/Province range of the analyses \
      uiOutput("Subnational_Government"),
      bsTooltip(
        id = "Subnational_Government",
        title = "Select a subnational goverment. Generally provinces or states",
        placement = "bottom",
        trigger = "hover",
        options = list(container = "body")
      )
    ),
    
    column(
      2,
      #Select the District/Adminstration range of the analyses
      uiOutput(outputId = "Local_Government"),
      bsTooltip(
        id = "Local_Government",
        title = "Select all local governments using Ctrl+A or Comm+A",
        placement = "bottom",
        trigger = "hover",
        options = list(container = "body")
      )
    ),
    
    column(
      3,
      #Select the Municipality/Subdistrict range of the analyses
      uiOutput(outputId = "Managed_Access"),
      bsTooltip(
        id = "Managed_Access",
        title = "Select all managed access areas using Ctrl+A or Comm+A",
        placement = "bottom",
        trigger = "hover",
        options = list(container = "body")
      )
    )
  ),
  fluidRow(column(
    width = 3,
    wellPanel(
      #Select a HHS section and questions
      conditionalPanel(condition = "input.tabs == 'Household Survey Questions'",
                       uiOutput(outputId = "hhs_section")),
      
      conditionalPanel(
        condition = "input.tabs == 'Household Survey Questions'",
        uiOutput(outputId = "hhs_question")
       
      ),
      
      bsTooltip(
        id = "hhs_section",
        title = "Select a household survey section",
        placement = "right",
        trigger = "hover",
        options = list(container = "body")
      ),
      bsTooltip(
        id = "hhs_question",
        title = "Select a household survey question",
        placement = "right",
        trigger = "hover",
        options = list(container = "body")
      ),
      
      #Select the basemap
      conditionalPanel(
        condition = "input.tabs == 'View Map'",
        selectInput(
          inputId = "basemap",
          label = strong("CHOOSE A BASEMAP"),
          choices = c(
            "Gray Canvas basemap" = providers$Esri.WorldGrayCanvas,
            "National Geographic basemap" = providers$Esri.NatGeoWorldMap,
            "Ocean basemap" = providers$Esri.OceanBasemap,
            "Satellite basemap" = providers$Esri.WorldImagery,
            "World Topo basemap" = providers$Esri.WorldTopoMap
          ),
          selected = providers$Esri.OceanBasemap,
          selectize = TRUE
        )
      ),
      bsTooltip(
        id = "basemap",
        title = "Select a basemap for mapping. This will take few seconds",
        placement = "right",
        trigger = "hover",
        options = list(container = "body")
      ),
      
      conditionalPanel(
        condition = "input.tabs == 'Generate Report'",
        strong(textOutput(outputId = "report_HHS")),
        br(),
        textOutput(outputId = "report_instructions"),
        br(),
        imageOutput("report_cover")
      ),
      br(),
      #Note about page reloading
      strong(textOutput(outputId = "Note")),
      textOutput(outputId = "Notetext")
      
    )
  ),
  
  column(
    width = 9,
    mainPanel(
      tags$style(type="text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }"),
      width = 14,
      tabsetPanel(
        type = "tabs",
        id = "tabs",
        
        ## Outcome Metric
        tabPanel(
          "Household Survey Questions",
          downloadLink(outputId = "downloadPlot0", "| download figure",  
                       style = "float:right"),
          downloadLink("downloadData0", "download data |", 
                        style = "float:right"),
          br(),
          tableOutput("table_summary"),
          plotlyOutput(
            outputId = "plot_hhs_question",
            height = "750px",
            width = 'auto'
            )
        ),
        
        #Create Map Tab
        tabPanel(
          "View Map",
          downloadLink(outputId = "downloadMap", "Download Map",  
                       style = "float:right"),
          leafletOutput(
            outputId = 'map',
            height = '700px',
            width = 'auto'
          )
        ),
        
        # Create Report Tab
        tabPanel(
          "Generate Report",
          br(),
          column(
            6,
            wellPanel(
              strong(h4(
                textOutput(outputId = "create_reporttitle1")
              )),
              hr(),
              checkboxGroupInput(
                "Socio_economic_outcomes",
                strong(h4("Select Socio-economic Outcomes")),
                choices = c(
                  "Financial and Market Inclusion",
                  "Effective Management Bodies",
                  "Managed Access",
                  "Behavior Adoption",
                  "Gender Mainstreaming"
                ),
                selected = c(
                  "Financial and Market Inclusion",
                  "Effective Management Bodies",
                  "Managed Access",
                  "Behavior Adoption",
                  "Gender Mainstreaming"
                ),
                inline = FALSE
              ),
              checkboxGroupInput(
                "Socio_economic_impacts",
                strong(h4("Select Socio-economic Impacts")),
                choices = c(
                  "Improve Well-being",
                  "Secure Food Supply",
                  "Sustain Livelihoods"
                ),
                selected = c(
                  "Improve Well-being",
                  "Secure Food Supply",
                  "Sustain Livelihoods"
                ),
                inline = FALSE
              ),
              br(),
              checkboxGroupInput(
                "Graphs_Tables1",
                strong(h4("Select Graphs and/or Tables")),
                choices = c("Graphs", "Tables"),
                selected = c("Graphs", "Tables"),
                inline = TRUE
              ),
              br(),
              br(),
              downloadButton(
                outputId = 'downloadReport1',
                label = 'Generate Report',
                class = 'butt',
                style = "right"
              ),
              tags$head(
                tags$style(
                  ".butt{background-color:#005BBB;} .butt{color: white;} .butt{font-family: Arial}"
                )
              ),
              
              downloadButton(
                outputId = 'downloadRawData1',
                label = 'Download Data',
                class = 'butt1'
              ),
              tags$head(
                tags$style(
                  ".butt1{background-color:#378249;} .butt1{color: white;} .butt1{font-family: Arial}"
                )
              ),
              
              downloadButton(
                outputId = 'downloadMedata1',
                label = 'Download Metadata',
                class = 'butt2'
              ),
              tags$head(
                tags$style(
                  ".butt2{background-color:#d3d3d3;} .butt2{color: white;} .butt2{font-family: Arial}"
                )
              )
              
            )
          ),
          
          column(
            6,
            wellPanel(
              strong(h4(
                textOutput(outputId = "create_reporttitle2")
              )),
              hr(),
              checkboxGroupInput(
                "hhs_sections",
                strong(h4("Select Household Survey Sections")),
                choices = unique(hhs_questions$section),
                selected = unique(hhs_questions$section),
                inline = FALSE
              ),
              checkboxGroupInput(
                "Graphs_Tables2",
                strong(h4("Select Graphs and/or Tables")),
                choices = c("Graphs", "Tables"),
                selected = c("Graphs", "Tables"),
                inline = TRUE
              ),
              br(),
              br(),
              downloadButton(
                outputId = 'downloadReport2',
                label = 'Generate Report',
                class = 'butt'
              ),
              tags$head(
                tags$style(
                  ".butt{background-color:#005BBB;} .butt{color: white;} .butt{font-family: Arial}"
                )
              ),
              
              downloadButton(
                outputId = 'downloadRawData2',
                label = 'Download Data',
                class = 'butt1'
              ),
              tags$head(
                tags$style(
                  ".butt1{background-color:#378249;} .butt1{color: white;} .butt1{font-family: Arial}"
                )
              ),
              downloadButton(
                outputId = 'downloadMedata2',
                label = 'Download Metadata',
                class = 'butt2'
              ),
              tags$head(
                tags$style(
                  ".butt2{background-color:#d3d3d3;} .butt2{color: white;} .butt2{font-family: Arial}"
                )
              )
            )
          )
        )#,
        
        #tabPanel(style = "overflow-y:scroll; max-height: 600px",
        #        "Household Survey Summary", 
        #         br(),
         #       htmlOutput("renderedReport")
        #)
      )
    )
  ))
))

