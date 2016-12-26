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

# Create connection to DB
pw <- {
  "nemesis"
}
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = "mep", host = "localhost", port = 5432, user = "postgres", password = pw)
rm(pw)

# Query definitions
query.sub.trajectory.names <- "SELECT 
                                pgtraj_name
                               FROM traj.pgtraj"

# Load sub-trajectories
sub.trajectory.names <- dbGetQuery(con, query.sub.trajectory.names)

sub.trajectories <- list()
dataset.size <- nrow(sub.trajectory.names)
for (i in 1:dataset.size) {
  name <- sub.trajectory.names[i, ]$pgtraj_name
  sub.trajectory <- pgtraj2ltraj(con, pgtraj = name)
  sub.trajectories[[i]] <- sub.trajectory
}

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
    subtrajPlot <- cutltraj(minDistTrajectory, "CriterionDistance(dist)", nextr = TRUE)
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