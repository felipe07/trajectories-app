library(RPostgreSQL)
library(rpostgisLT)

# Create connection to DB
pw <- {
  "nemesis"
}
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = "mep", host = "localhost", port = 5432, user = "postgres", password = pw)
rm(pw)

# Query definitions

# This query exclude groups of relocations with less than 3 points because
# is not possible to compute the relative angle with 2 points or less.
query.tms <- "SELECT 
                timestamp_trajectory_end, 
                \"user\", 
                location, 
                count(id) 
              FROM relocation_fs 
              GROUP BY 1,2,3
              HAVING count(id) > 3
              ORDER BY timestamp_trajectory_end ASC"

query.points <- "SELECT 
                  id, 
                  location, TO_TIMESTAMP(\"timestamp\" / 1000) AS timestamp, 
                  latitude, 
                  longitude
                 FROM relocation_fs 
                 WHERE timestamp_trajectory_end LIKE"

query.truncate.traj.metadata <- "TRUNCATE TABLE sub_trajectory_fs"

query.restart.seq.traj.metadata <- "ALTER SEQUENCE sub_trajectory_fs_id_seq RESTART WITH 1"

query.insert.traj.metadata <- "INSERT INTO 
                                sub_trajectory_fs (
                                  timestamp_trajectory_end, 
                                  name, 
                                  location, 
                                  type
                                )
                               VALUES ($1,$2,$3,$4)"

# Get sub-trajectories timestamps
dataset.tms <- dbGetQuery(con, query.tms)


# Initialize data structures to save sub-trajectories and meta-data
sub.trajectories <- list()
sub.trajectories.metadata <- data.frame(timestamp_trajectory_end=character(),
                                        name=character(),
                                        location=character(),
                                        type=character(),
                                        stringsAsFactors = FALSE)

# Clean tables first. Restart indices
dbGetQuery(con, query.restart.seq.traj.metadata)
dbGetQuery(con, query.truncate.traj.metadata)

dataset.size <- nrow(dataset.tms)
for (i in 1:dataset.size) {
  # Get datasets by sub-trajectories timestamps
  tmstmp <- paste0("'", dataset.tms[i, ]$timestamp_trajectory_end, "'")
  query <- paste(query.points, tmstmp)
  data.points <- dbGetQuery(con, query)
  
  # Create data frame without duplicated timestamps
  data.points.wd <- data.points[!duplicated(data.points), ]
  
  # Create the actual trajectory object
  date.time <- as.POSIXct(strptime(as.character(data.points.wd$timestamp),"%Y-%m-%d %H:%M:%S"))
  sub.trajectory <- as.ltraj(xy = data.points.wd[,c("longitude","latitude")], date = date.time, id = paste0("test", i))
  
  # Set time zone
  data.frame.temp <- ld(sub.trajectory)
  attr(data.frame.temp$date, "tzone") <- "Europe/Rome"
  sub.trajectory <- dl(data.frame.temp)
  
  # Definition of sub-trajectory meta-data attributes  
  name <- paste(dataset.tms[i, ]$user, tmstmp, sep = "-")
  location <- dataset.tms[i, ]$location
  type <- "anomalous"
  
  # Declare error flag. In R once the execution is inside 'error' the program is no longer in a loop.
  error.flag <- FALSE
  
  tryCatch({ 
    # Test the autocorrelation of the relative angles. In case of error don't insert sub-trajectory.
    testang.ltraj(sub.trajectory, "relative")
    
    # Insert sub-trajectories. Library rpostgisLT stores the trajectories in traj.pgtraj by default.  
    # Common errors trying to insert the sub-trajectory:
    # 1. There is a sub-trajectory whose name is equal to the name of the sub-trajectory
    #    being inserted.
    # 2. Among the group of relocations of the sub-trajectory there isn't a minimum of 3
    #    relocations with different latitude/longitude.
    ltraj2pgtraj(conn = con, ltraj = sub.trajectory, pgtraj = name)
    
    # Insert sub-trajectory meta-data
    dbGetQuery(con, query.insert.traj.metadata, c(tmstmp, name, location, type))
  },
  error = function(cond) {
    message(cond)
    error.flag <<- TRUE
  },
  warning = function(cond) {
    message(cond)
    next
  },
  finally = print("Sub-trajectory inserted."))
  
  if (error.flag) next
}

#pgtrajSchema(con)

# close the connection
dbDisconnect(con)
dbUnloadDriver(drv)