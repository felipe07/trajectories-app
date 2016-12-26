# Criterion function used to cut the trajectory according to distance
CriterionDistance <- function(dist) {
  return (dist < cutParam)
}