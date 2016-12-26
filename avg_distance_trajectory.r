AvgDistTraj <- function(data.points) {
  # install.packahe("geosphere")
  # library(geosphere)

  min.id <- min(data.points$id, na.rm = TRUE)
  max.id <- max(data.points$id, na.rm = TRUE) - 1
  traj.dist <- 0

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
    traj.dist <- traj.dist + dist.i.j  
  }

  # Number of nodes/vertices/locations of the trajectory
  n <- length(data.points[[1]]);

  # Apply formula to compute the avegarge distance of the trajectory
  # The choose function computes the binomial coeficient
  avg.traj.dist <- traj.dist / choose(n, 2)
  return(avg.traj.dist)
}