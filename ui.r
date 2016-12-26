library(shiny)

avgDistTraj <- 0

# Define UI for trajectory viewer application
fluidPage(

  tags$script('
    Shiny.addCustomMessageHandler("myCallbackHandler",
      function(avg) {
        document.getElementById("minDistance").value = avg;
      }
    );
  '),
    
  # Application title
  titlePanel("MEP trajectories - Test"),
  
  # Sidebar with control to select a trajectory
  sidebarLayout(
    sidebarPanel(
      selectInput("trajectory", "Choose a trajectory:", 
                  choices = c("Test trajectory 1", "Test trajectory 2", "Test trajectory 3", "Test trajectory 4")),
      
      numericInput("minDistance", "Minimum distance between two consecutive points:\n(Average by default)", 
                   min = 0, max = 10, value = avgDistTraj, step = 0.00001),
      
      numericInput("cutParameter", "Cutting value:", 
                   min = 0, max = 10, value = 0.001, step = 0.001)
      
      #h5("Trajectory sumary (original)"),
      #verbatimTextOutput("summary")
    ),
    
    mainPanel(
      fluidRow(
        column(6, plotOutput("trajectoryPlot", height = "400px")),
        column(6, plotOutput("minDistTrajectoryPlot", height = "400px"))
      ),
      
      fluidRow(
        column(6, offset = 3, plotOutput("subtrajectoriesPlot", height = "400px", width = "400px"))
      ),
      
      h5("Variance (autocorrelation test of relative angles):"),
      fluidRow(
        column(6, verbatimTextOutput("varianceRelativeAngles")),
        column(6, verbatimTextOutput("varianceRelativeAnglesMinDist"))
      ),
      
      h5("Average distance between consecutive points:"),
      fluidRow(
        column(6, verbatimTextOutput("average"))
      ),
      
      h5("Standard deviation:"),
      fluidRow(
        column(6, verbatimTextOutput("standardDeviation"))
      ),
      
      h5("Brush info:"),
      fluidRow(
        column(6, verbatimTextOutput("brush_info"))
      )
    )
  )
)