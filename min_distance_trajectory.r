MinDistTraj <- function(data.points) {
  # install.packahe("geosphere")
  # library(geosphere)

  min.id <- min(data.points$id, na.rm = TRUE)
  max.id <- max(data.points$id, na.rm = TRUE) - 1
  min.dist <- 0
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
    #dist.i.j <- distm(c(x1, y1), c(x2, y2), fun = distHaversine)
	dist.i.j <- points.dist(x1, y1, x2, y2)
	# Update min distance if current distance is greater
	if (i == min.id) {
	  min.dist <- dist.i.j	  
	} else if (dist.i.j < min.dist) {
	  min.dist <- dist.i.j
	}	
  }
  return(min.dist)
}