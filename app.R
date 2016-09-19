library(shiny)
library(shinydashboard)
library(ggplot2)
library(shinyBS)
library(ggmap)
library(leaflet)
library(RColorBrewer)

MNG <- read.csv("MNG_VEI.csv", header=T)
MNG$name <- as.character(MNG$name)

saveData <- function(mydata) {
  mydata <- as.data.frame(mydata, stringsAsFactors = FALSE)
  mydata$score <- as.numeric(mydata$culture) + as.numeric(mydata$energy) + as.numeric(mydata$placeness) + as.numeric(mydata$systems) + as.numeric(mydata$vernacular)
  MNG <- rbind(MNG, mydata)
  print(MNG)
  if (exists("responses")) {
    responses <<- rbind(responses, mydata)
  } else {
    responses <<- mydata
  }
}

loadData <- function() {
  if (exists("responses")) {
    responses
  }
}


myfields <- c("name", "score", "culture", "energy", "placeness", "systems", "vernacular", "long", "lat")

########### USER INTERFACE #############

ui <- dashboardPage(skin = "blue",
                    dashboardHeader(title = "Vernacular Ecology Index - VEI", titleWidth = 300),
                    
                    dashboardSidebar(width = 200,
                                     sidebarMenu(
                                         menuItem("VEI Assessment", tabName = "assessment"),
                                         menuItem("Google Map", tabName = "showdata"),
                                         menuItem("Data Table", tabName = "datatab")
                                     )), 
                    
                    dashboardBody(
# ====================== First Tab: Assessment Chart ======================
                        tabItems(
                            tabItem(tabName = "assessment",
# ---------------- Input Sliders ------------------------------------------------                                        
                                        box(title = strong("Please input the scores for the five VEI categories", align = "center"),
                                            
                                            fluidRow(
                                                column(7, sliderInput("culture", 
                                                        label = h4("Culture:"),
                                                        min = 0, max = 5, value = c(0), step = 1, round =1, width = "150px")), 
                                                column(1, actionButton("goC", "Grading"))
                                            ),
                                            fluidRow(
                                                column(7, sliderInput("energy", 
                                                        label = h4("Energy:"),
                                                        min = 0, max = 5, value = c(0), step = 1, round =1, width = "150px")),
                                                column(1, actionButton("goE", "Grading"))
                                            ),
                                        
                                            fluidRow(
                                                column(7, sliderInput("placeness", 
                                                        label = h4("Placeness:"),
                                                        min = 0, max = 5, value = c(0), step = 1, round =1, width = "150px")),
                                                column(1, actionButton("goP", "Grading"))
                                            ),
                                            
                                            fluidRow(
                                                column(7, sliderInput("systems", 
                                                        label = h4("Systems:"),
                                                        min = 0, max = 5, value = c(0), step = 1, round =1, width = "150px")),
                                                column(1, actionButton("goS", "Grading"))
                                            ),
                                                       
                                            fluidRow(
                                                column(7, sliderInput("vernacular", 
                                                        label = h4("Vernacular:"),
                                                        min = 0, max = 5, value = c(0), step = 1, round =1, width = "150px")),
                                                column(1, actionButton("goV", "Grading"))
                                            ),
                                            width = 4, height = "100%"),
# ---------------- Rose Chart --------------------------------------------------- 
# ---------------- VEI Chart Box ------------------------------------------------
                                        box(title = h2("VEI Rose Chart", align = "center"),
                                            plotOutput("myplot", width = "auto", height = 393), 
                                            fluidRow(column(7, textInput("name", "Name", ""))),
                                            fluidRow(column(5, textInput("long", "longitude", "")),
                                                     column(5, textInput("lat", "latitude", ""))
                                                            ),
                                            actionButton("submit", "Submit"),
                                            width = 8, height = "100%"),
                                            
                                        

# ---------------- Buttons Windows ------------------------------------------------ 
                                        bsModal("CultureExample", "Culture", "goC", size = "large", 
                                                wellPanel(includeHTML("HTMLCulture.html"),
                                                  helpText(a("Open as a pdf file", target="_blank", href="PDFCulture.pdf")))),
                                    
                                        bsModal("EnergyExample", "Energy", "goE", size = "large", 
                                                includeHTML("HTMLEnergy.html"), helpText(a("Open as a pdf file", target="_blank", href="PDFEnergy.pdf"))),                                    
                                    
                                        bsModal("PlacenessExample", "Placeness", "goP", size = "large", 
                                                includeHTML("HTMLPlaceness.html"), helpText(a("Open as a pdf file", target="_blank", href="PDFPlaceness.pdf"))),
                                    
                                        bsModal("SystemsExample", "Systems", "goS", size = "large", 
                                                includeHTML("HTMLSystems.html"), helpText(a("Open as a pdf file", target="_blank", href="PDFSystems.pdf"))),
                                    
                                        bsModal("VernacularExample", "Vernacular", "goV", size = "large", 
                                                includeHTML("HTMLVernacular.html"), helpText(a("Open as a pdf file", target="_blank", href="PDFVernacular.pdf")))
                                    
                            ), #tabItam "assessment"

# ====================== Second Tab: Google Map ======================
                            tabItem(tabName = "showdata", 
                                    
                                    sidebarPanel( sliderInput("range", "Scores", min(MNG$score), max(MNG$score),
                                                              value = range(MNG$score), step = 1),
                                                  checkboxInput("legend", "Show legend", F)
                                                  #selectInput("select", label = h5("Category to be displayed"), 
                                                  #            choices = list("Total" = 1, "Culture" = 2,
                                                  #            "Energy" = 3, "Systems" = 4, "Placeness" = 5, "Varnacular" = 5 ), 
                                                  #            selected = 1)
                                                 ),
                                    #----------------- GoogleMap ---------------------------------------------------                           
                                    mainPanel(  
                                                  leafletOutput("map", width = "100%", height = 600)
                                    )
                                     
                                     ), #tabItam "showdata" 

# ====================== Third Tab: data Table ======================
                        tabItem(tabName = "datatab",
                                  DT::dataTableOutput("responses"), tags$hr()
                                )
                            )
                        ) #tabItams      
                    ) #dashboardBody 
                    
 #dashboardPage 


########### SERVER #############

server <- function(input, output) {

  
  formData <- reactive({
    mydata <- sapply(myfields, function(x) input[[x]])
    mydata <- t(mydata)
    mydata
  }) 
  
  observeEvent(input$submit, {
    saveData(formData()
             ) 
  })
  
  
  output$responses <- DT::renderDataTable({
    input$submit
    loadData()
  })
  
  
# ================ leaflet: google map ================     
# ---- Reactive expression for the data subsetted to what the user selected ----
  
  filteredData <- reactive({
    MNG[MNG$score >= input$range[1] & MNG$score <= input$range[2],]
  })

# --------- the palette function ---------
  pal <- colorNumeric(brewer.pal(7,"Set2"), MNG$score)

# --------- mapping the data --------- 
  output$map <- renderLeaflet({
    # Use leaflet() and include aspects of the map that
    # won't need to change dynamically.
    
    leaflet(MNG) %>% addTiles() %>%
      fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat)) %>% 
      addCircles(radius = ~log(score)*100/1.2, weight = 2, color = "black",
                 fillColor = ~pal(score), fillOpacity = 0.7, popup = ~paste(
                   "<b>", name, "<br>", "Total Score: ", score, "</b>", "<br>",
                   "Energy: ", energy, "<br>", 
                   "Systems: ", systems, "<br>",
                   "Culture: ", culture, "<br>",
                   "Placeness: ", placeness, "<br>",
                   "Vernacular: ", vernacular, "<br>"
                 ))
  })
  
# ---------- The leaflet: color ----------     
  observe({
    leafletProxy("map", data = filteredData()) %>%
      clearShapes() %>%
      addCircles(radius = ~log(score)*100/1.2, weight = 2.5, color = "black",
                 fillColor = ~pal(score), fillOpacity = 0.7, popup = ~paste(score)
      )
  })
  
# ---------- Use a separate observer to recreate the legend as needed.
  observe({
    proxy <- leafletProxy("map", data = MNG)
    proxy %>% clearControls()
    if (input$legend) {
      proxy %>% addLegend(position = "bottomright",
                          pal = pal, values = ~score
      )
    }
  })

# ================ ggplot: rose chart ================    

  output$myplot <- renderPlot({
        category<-c("Culture", "Energy", "Placeness", "Systems", "Vernacular")
        score <- c(input$culture, input$energy, input$placeness, input$systems, input$vernacular)
        myd<-data.frame(category, score)
        
        ggplot(myd, aes(category, weight=score, fill = category)) + geom_bar(width = 1, color=1, lwd=0.8) + 
               scale_fill_brewer(palette = "Set1") + geom_hline(yintercept=seq(0, 5, by=1), 
               colour = "black", size = 0.2) + scale_y_continuous(breaks = 0:5) + theme_linedraw() + 
               coord_polar()  + labs(x = "", y = "") + 
               theme(panel.grid.major = element_line(color="white", size=0.2), 
               legend.position = "bottom", axis.ticks.y=element_line(size=0), 
               axis.ticks = element_blank(), axis.text.x=element_text(size=8, face="bold"),  
               axis.text.y=element_text(size=0))
   })

} #server

########### APP #############    
shinyApp(ui, server)
