MaxDistTraj <- function(data.points) {
  # install.packahe("geosphere")
  # library(geosphere)

  min.id <- min(data.points$id, na.rm = TRUE)
  max.id <- max(data.points$id, na.rm = TRUE) - 1
  max.dist <- 0
  for (i in min.id:max.id) {
    j <- i + 1
    #coord.i.j <- subset(data.points, id >= i & id <= j, select = c("latitude", "longitude"))
	coord.i <- subset(data.points, id == i, select = c("latitude", "longitude"))
	coord.j <- subset(data.points, id == j, select = c("latitude", "longitude"))
    # Longitude point i
    x1 <- coord.i[1,2]
    # Latitude point i
    y1 <- coord.i[1,1]
    # Longitude point i + 1
    x2 <- coord.j[1,2]
    # Latitude point i + 1
    y2 <- coord.j[1,1]
	# Calculate distance between point i and i + 1 with function points.dist
    #dist.i.j <- distm(c(x1, y1), c(x2, y2), fun = distHaversine)
	dist.i.j <- points.dist(x1, y1, x2, y2)
	# Update max distance if current distance is greater
	if (dist.i.j > max.dist) {
	  max.dist <- dist.i.j	  	  
	}
  }
  return(max.dist)  
}