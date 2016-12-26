library(adehabitatLT)
library(shiny)
library(RPostgreSQL)

# Load utility functions
if (!exists("AvgDistTraj", mode="function")) 
  source("C:/Users/User/Documents/University/Thesys/thesis-app/avg_distance_trajectory.r")

if (!exists("StdDevTraj", mode="function")) 
  source("C:/Users/User/Documents/University/Thesys/thesis-app/std_dev_trajectory.r")

if (!exists("CriterionDistance", mode="function"))
  source("C:/Users/User/Documents/University/Thesys/thesis-app/criterion_distance.r")

# Load datasets
data1 <- read.csv("C:/Users/User/Documents/University/Thesys/points_subset1.csv")
data2 <- read.csv("C:/Users/User/Documents/University/Thesys/points_subset2.csv")
data3 <- read.csv("C:/Users/User/Documents/University/Thesys/points_subset3.csv")
data4 <- read.csv("C:/Users/User/Documents/University/Thesys/points_subset4.csv")

# Initialization of test trajectory 1
da1 <- as.POSIXct(strptime(as.character(data1$timestamp),"%Y-%m-%d %H:%M:%S"))
traj1 <- as.ltraj(xy = data1[,c("longitude","latitude")], date = da1, id = "test1")
# Initialization of test trajectory 2
da2 <- as.POSIXct(strptime(as.character(data2$timestamp),"%Y-%m-%d %H:%M:%S"))
traj2 <- as.ltraj(xy = data2[,c("longitude","latitude")], date = da2, id = "test2")
# Initialization of test trajectory 3
da3 <- as.POSIXct(strptime(as.character(data3$timestamp),"%Y-%m-%d %H:%M:%S"))
traj3 <- as.ltraj(xy = data3[,c("longitude","latitude")], date = da3, id = "test3")
# Initialization of test trajectory 4
da4 <- as.POSIXct(strptime(as.character(data4$timestamp),"%Y-%m-%d %H:%M:%S"))
traj4 <- as.ltraj(xy = data4[,c("longitude","latitude")], date = da4, id = "test4")

# Define server logic required to summarize and view the selected
# trajectory
function(input, output, session) {
  
  # Return the requested trajectory
  trajectoryInput <- reactive({
    switch(input$trajectory,
           "Test trajectory 1" = traj1,
           "Test trajectory 2" = traj2,
           "Test trajectory 3" = traj3,
           "Test trajectory 4" = traj4)
  })
  
  # Return mean distance for selected trajectory
  AvgDistTrajectory <- function() {
    trajectory <- trajectoryInput()
    avgDistance <- 0
    if (identical(trajectory, traj1)) {
      avgDistance <- AvgDistTraj(data1)
    } else if (identical(trajectory, traj2)) {
      avgDistance <- AvgDistTraj(data2)
    } else if (identical(trajectory, traj3)) {
      avgDistance <- AvgDistTraj(data3)
    } else {
      avgDistance <- AvgDistTraj(data4)
    }
    return(avgDistance)
  }
  
  # Return standatd deviation of distances for selected trajectory
  stdDevDistTrajectory <- function() {
    trajectory <- trajectoryInput()
    standardDeviation <- 0
    if (identical(trajectory, traj1)) {
      standardDeviation <- StdDevTraj(data1)
    } else if (identical(trajectory, traj2)) {
      standardDeviation <- StdDevTraj(data2)
    } else if (identical(trajectory, traj3)) {
      standardDeviation <- StdDevTraj(data3)
    } else {
      standardDeviation <- StdDevTraj(data4)
    }
    return(standardDeviation)
  }
  
  # Generate a avg distance between points of the trajectory
  output$average <- renderPrint({
    avg <- AvgDistTrajectory()
    print(avg)
  })
  
  # Generate a standard deviation of distances between points of the trajectory
  output$standardDeviation <- renderPrint({
    stdDev <- stdDevDistTrajectory()
    print(stdDev)
  })
 
  # Generate the variance of the autocorrelation test of relative angles
  output$varianceRelativeAngles <- renderPrint({
    trajectory <- trajectoryInput()
    varRelAng <- testang.ltraj(trajectory, "relative")
    varRelAngVector <- unlist(varRelAng)
    print(varRelAngVector$expvar.Variance)
  })
   
  # Generate a summary of the trajectory
  output$summary <- renderPrint({
    trajectory <- trajectoryInput()
    print(trajectory)
  })
  
  # Plot selected trajectory
  output$trajectoryPlot <- renderPlot({
    trajectory <- trajectoryInput()
    plot(trajectory)
    title(main = "Original")
    brush = brushOpts( id = "plot_brush")
  })
  
  output$brush_info <- renderPrint({
    cat("input$plot_brush:\n")
    str(input$plot_brush)
  })
  
  # Plot trajectory using function mindistkeep 
  # https://www.rdocumentation.org/packages/adehabitatLT/versions/0.3.21/topics/mindistkeep
  output$minDistTrajectoryPlot <- renderPlot({
    trajectory <- trajectoryInput()
    minDistTrajectory <<- mindistkeep(trajectory, input$minDistance)
    plot(minDistTrajectory)
    title(main = "With minimum distance applied")
  })
  
  # Generate the variance of the autocorrelation test of relative angles
  # for the minimum distance trajectory 
  output$varianceRelativeAnglesMinDist <- renderPrint({
    trajectory <- trajectoryInput()
    minDistance <- as.numeric(input$minDistance)
    minDistTrajectory <- mindistkeep(trajectory, minDistance)
    
    tryCatch({ 
      minDistVarRelAng <- testang.ltraj(minDistTrajectory, which = "relative")
      varRelAngVector <- unlist(minDistVarRelAng)
      print(varRelAngVector$expvar.Variance)
    },
    error = function(cond) {
      message("Reduce the parameter minimum distance to generate the variance of the autocorrelation")
      message("Original error message")
      message(cond)
      print(NA)
    },
    warning = function(cond) {
      message("Reduce the parameter minimum distance to generate the variance of the autocorrelation")
      message("Original warning message")
      message(cond)
      print(NA)
    })
  })
  
  # Plot sub-trajectories obtained after 'cutting' trajectory using the criterion 
  # defined in the function 'criterionDistance'
  output$subtrajectoriesPlot <- renderPlot({
    cutParam <<- input$cutParameter 
    subtrajPlot <- cutltraj(minDistTrajectory, "CriterionDistance(dist)", nextr = FALSE)
    # Change id's of every burst in order to generate as many plots as burst 
    # obtained after the cut
    lengthIds <- length(id(subtrajPlot))
    vectorIds <- numeric()
    for (i in 1:lengthIds) {
      vectorIds[i] <- paste(c("sub-trajectory", i), collapse = "")
    }
    id(subtrajPlot) <- vectorIds
    plot(subtrajPlot)
  })
  
  # Update minimum distance field when a trajectory is selected
  # and send the average distance between consecutive points to
  # the client
  observe({
    input$trajectory
    avg <- AvgDistTrajectory()
    session$sendCustomMessage(type='myCallbackHandler', avg) 
  })
}