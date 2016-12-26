StdDevTraj <- function(data.points) {  
  min.id <- min(data.points$id, na.rm = TRUE)
  max.id <- max(data.points$id, na.rm = TRUE) - 1
  sum <- 0

  # Get average distance
  avg.dist <- AvgDistTraj(data.points)
  
  for (i in min.id:max.id) {
    j <- i + 1
    coord.i.j <- subset(data.points, id >= i & id <= j, select = c("latitude", "longitude"))
    # Longitude point i
    x1 <- coord.i.j[1,2]
    # Latitude point i
    y1 <- coord.i.j[1,1]
    # Longitude point i + 1
    x2 <- coord.i.j[2,2]
    # Latitude point i + 1
    y2 <- coord.i.j[2,1]
    # Calculate distance between point i and i + 1 with function points.dist
    #dist.i.j <-  distm(c(x1, y1), c(x2, y2), fun = distHaversine)
	dist.i.j <- points.dist(x1, y1, x2, y2)
	square.dist.to.mean <- (dist.i.j - avg.dist)^2
    sum <- sum + square.dist.to.mean
  }

  # Number of nodes/vertices/locations of the trajectory
  n <- length(data.points[[1]]);

  # Apply formula to compute the standard deviation 
  std.dev <- sqrt(sum / n)
  return(std.dev)
}