# Phylogenetic Visualization Of cgMLST data in a Shiny App
# Author: Marian Freisleben
# Date: 07.10.2022


# Packages
library(treedataverse) # includes all relevant phylo tree packages
library(TDbook) # phylo tree test data package
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(dashboardthemes)
library(ggplotify)


################ User Interface ################

ui <- dashboardPage(
  
    # Title 
    dashboardHeader(title = span(img(src="PhyloTree.jpg", width = 190))),

    dashboardSidebar(
        collapsed = TRUE
      
    ),

    dashboardBody(shinyDashboardThemeDIY(
      ### general
      appFontFamily = "Tahoma"
      ,appFontColor = "rgb(0,0,0)"
      ,primaryFontColor = "rgb(0,0,0)"
      ,infoFontColor = "rgb(0,0,0)"
      ,successFontColor = "rgb(0,0,0)"
      ,warningFontColor = "rgb(0,0,0)"
      ,dangerFontColor = "rgb(0,0,0)"
      ,bodyBackColor = cssGradientThreeColors(
        direction = "down"
        ,colorStart = "#282f38"
        ,colorMiddle = "#384454"
        ,colorEnd = "#495d78"
        ,colorStartPos = 0
        ,colorMiddlePos = 50
        ,colorEndPos = 100
      )
      
      ### header
      ,logoBackColor = "#282f38"
      
      ,headerButtonBackColor = "#282f38"
      ,headerButtonIconColor = "#ffffff"
      ,headerButtonBackColorHover = "#282f38"
      ,headerButtonIconColorHover = "#181a21"
      
      ,headerBackColor = "#282f38"
      ,headerBoxShadowColor = "#aaaaaa"
      ,headerBoxShadowSize = "0px 0px 0px"
      
      ### sidebar
      ,sidebarBackColor = cssGradientThreeColors(
        direction = "down"
        ,colorStart = "#282f38"
        ,colorMiddle = "#384454"
        ,colorEnd = "#495d78"
        ,colorStartPos = 0
        ,colorMiddlePos = 50
        ,colorEndPos = 100
      )
      ,sidebarPadding = 0
      
      ,sidebarMenuBackColor = "transparent"
      ,sidebarMenuPadding = 0
      ,sidebarMenuBorderRadius = 0
      
      ,sidebarShadowRadius = "0px 0px 0px"
      ,sidebarShadowColor = "#aaaaaa"
      
      ,sidebarUserTextColor = "rgb(255,255,255)"
      
      ,sidebarSearchBackColor = "rgb(55,72,80)"
      ,sidebarSearchIconColor = "rgb(153,153,153)"
      ,sidebarSearchBorderColor = "rgb(55,72,80)"
      
      ,sidebarTabTextColor = "rgb(255,255,255)"
      ,sidebarTabTextSize = 13
      ,sidebarTabBorderStyle = "none none solid none"
      ,sidebarTabBorderColor = "rgb(35,106,135)"
      ,sidebarTabBorderWidth = 1
      
      ,sidebarTabBackColorSelected = cssGradientThreeColors(
        direction = "right"
        ,colorStart = "rgba(44,222,235,1)"
        ,colorMiddle = "rgba(44,222,235,1)"
        ,colorEnd = "rgba(0,255,213,1)"
        ,colorStartPos = 0
        ,colorMiddlePos = 30
        ,colorEndPos = 100
      )
      ,sidebarTabTextColorSelected = "rgb(0,0,0)"
      ,sidebarTabRadiusSelected = "0px 20px 20px 0px"
      
      ,sidebarTabBackColorHover = cssGradientThreeColors(
        direction = "right"
        ,colorStart = "rgba(44,222,235,1)"
        ,colorMiddle = "rgba(44,222,235,1)"
        ,colorEnd = "rgba(0,255,213,1)"
        ,colorStartPos = 0
        ,colorMiddlePos = 30
        ,colorEndPos = 100
      )
      ,sidebarTabTextColorHover = "rgb(50,50,50)"
      ,sidebarTabBorderStyleHover = "none none solid none"
      ,sidebarTabBorderColorHover = "rgb(75,126,151)"
      ,sidebarTabBorderWidthHover = 1
      ,sidebarTabRadiusHover = "0px 20px 20px 0px"
      
      ### boxes
      ,boxBackColor = "#ffffff"
      ,boxBorderRadius = 7
      ,boxShadowSize = "0px 1px 1px"
      ,boxShadowColor = "#ffffff"
      ,boxTitleSize = 16
      ,boxDefaultColor = "#3d9970"
      ,boxPrimaryColor = "#7d9aa0"
      ,boxInfoColor = "#3d9970"
      ,boxSuccessColor = "#3d9970"
      ,boxWarningColor = "#ffffff"
      ,boxDangerColor = "#ffffff"
      
      ,tabBoxTabColor = "#ffffff"
      ,tabBoxTabTextSize = 14
      ,tabBoxTabTextColor = "rgb(0,0,0)"
      ,tabBoxTabTextColorSelected = "rgb(0,0,0)"
      ,tabBoxBackColor = "#ffffff"
      ,tabBoxHighlightColor = "#ffffff"
      ,tabBoxBorderRadius = 5
      
      ### inputs
      ,buttonBackColor = "#f5f5f5"
      ,buttonTextColor = "rgb(0,0,0)"
      ,buttonBorderColor = "rgb(200,200,200)"
      ,buttonBorderRadius = 5
      
      ,buttonBackColorHover = "rgb(235,235,235)"
      ,buttonTextColorHover = "rgb(100,100,100)"
      ,buttonBorderColorHover = "rgb(200,200,200)"
      
      ,textboxBackColor = "#ffffff"
      ,textboxBorderColor = "rgb(200,200,200)"
      ,textboxBorderRadius = 5
      ,textboxBackColorSelect = "#ffffff"
      ,textboxBorderColorSelect = "rgb(200,200,200)"
      
      ### tables
      ,tableBackColor = "rgb(255,255,255)"
      ,tableBorderColor = "rgb(240,240,240)"
      ,tableBorderTopSize = 1
      ,tableBorderRowSize = 1
    ),
      
        fluidRow(
            
            column(width = 3,
                   br(),
                   br(),
                   box(width = 12,
                       height = "500px",
                       solidHeader = TRUE,
                       br(), br(),
                       fileInput(inputId = "file",
                                 label = NULL,
                                 multiple = TRUE,
                                 placeholder = "Select file"),
                       hr(), br(),
                       selectInput("select", 
                                   label = h4("Select format"), 
                                   choices = list("BEAST" = 1, "MEGA" = 2, "MrBayes" = 3), 
                                   selected = NULL),
                       hr(), br(),
                       fluidRow(
                           column(width = 12, 
                                  align = "center", 
                                  actionButton(inputId = "parse",
                                               label = "Parse Data")
                           )
                       ),
                       br(),
                       hr(),
                       br(),
                       fluidRow(
                           column(width = 12,
                                  align = "center",
                                  actionButton(inputId = "make_tree",
                                           label = "PhyloTree")
                       )
                       ),
                       br(), br()
                       )
            ),
            br(),br(),
            column(width = 9,
                   box(
                      width = 11,
                      height = "500px",
                      solidHeader = TRUE,
                      
                      fluidRow(
                          column(width = 12,
                                 br(),
                                 plotOutput("tree")
                          )
                      )
                  )
                  )
            
        ),
        br(),
        fluidRow(
          column(width = 12,
          box(width = 3,
              height = "150px",
              solidHeader = TRUE,
              title = "Orientation",
              background = "olive",
              column(width = 5,
                     checkboxInput(inputId = "rev_x_axis", 
                                   label = "Reverse x-Axis", 
                                   value = FALSE
                     ),
                     checkboxInput(inputId = "rev_y_axis",
                                   label = "Reverse y-Axis",
                                   value = FALSE)
              ),
              column(width = 7,
                     noUiSliderInput(inputId = "rotate",
                                     label = "Rotate",
                                     min = -180,
                                     max = 180,
                                     value = 0,
                                     step = 1,
                                     width = "200px",
                                     update_on = "end",
                                     color = "#374850",
                                     tooltips = TRUE)
              )
          ),
          box(width = 3,
              height = "150px",
              solidHeader = TRUE,
              title = "Tree layout",
              background = "olive",
              column(width = 12,
                     selectInput("layout",
                                 label = NULL,
                                 choices = list('rectangular', 'dendrogram', 
                                                'slanted', 'ellipse', 
                                                'roundrect', 'fan', 
                                                'circular', 'inward_circular', 
                                                'radial', 'equal_angle', 
                                                'daylight', 'ape'), 
                                 selected = "roundrect")
              )
          ),
          box(width = 3,
              height = "150px",
              solidHeader = TRUE,
              title = "Treescale",
              background = "olive"),
          box(width = 2,
              height = "150px",
              solidHeader = TRUE,
              title = "Nodes/Tips",
              background = "olive")
        ))
    )
  
)


################### Server ###################

server <- function(input, output) {
    
  
    # parse file depending on select input
    observeEvent(input$parse, {
        
        if (input$select == 1) {
          
          parsed_file <<- read.beast(input$file$datapath)
          phylo <<- as.phylo(parsed_file)
          plot <- ggtree(phylo)
          
            if (class(parsed_file) == "treedata" & class(phylo) == "phylo") {
              
              show_toast(title = "Success",
                         type = "success",
                         position = "bottom",
                         width = "400px",
                         timer = 4000)
              
            } else {
              
              print("Error")
              
            }
          
        } else if (input$select == 2) {
          
          parsed_file <<- read.mega(input$file$datapath)
          phylo <<- as.phylo(parsed_file)
          plot <- ggtree(phylo)
          
            if (class(parsed_file) == "treedata" & class(phylo) == "phylo") {
              
              show_toast(title = "Success",
                         type = "success",
                         position = "bottom",
                         width = "400px",
                         timer = 4000)
             
            } else {
              
              print("Error")
              
            }
          
        } else if (input$select == 3) {
          
          parsed_file <<- read.mrbayes(input$file$datapath)
          phylo <<- as.phylo(parsed_file)
          plot <- ggtree(phylo)
          
          if (class(parsed_file) == "treedata" & class(phylo) == "phylo") {
            show_toast(title = "Success",
                       type = "success",
                       position = "bottom",
                       width = "400px",
                       timer = 4000)
            
          } else {
            
            print("Error")
            
          }
        }
        
        
      
    })
    
    
    # Define Tree Characteristics
    # Reverse X Scale
    revx <- reactive({
      if(input$rev_x_axis == FALSE) {
        NULL
      } else if (input$rev_x_axis == TRUE) {
        scale_x_reverse()
      }
    })
    
    # Reverse Y Scale
    revy <- reactive({
      if(input$rev_y_axis == FALSE) {
        NULL
      } else if (input$rev_y_axis == TRUE) {
        scale_y_reverse()
      }
    })
  
    # Make Tree
    observeEvent(input$make_tree, 
                 {output$tree <- renderPlot({
                   
                    as.ggplot(ggtree(tr = phylo,
                                     layout = input$layout) +
                                revx() +
                                revy() +
                                geom_treescale() +
                                geom_tippoint(color="#FDAC4F", shape=8, size=3) +
                                geom_nodepoint(color="#b5e521", alpha=1/4, size=10), 
                              angle = input$rotate
                    )
                   
                   
                   })
                 }
    
    )
    
    
}


################## Shiny #####################

shinyApp(ui = ui, server = server)
