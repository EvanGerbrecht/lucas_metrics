# Load any packages
library(lidR)
library(lidRmetrics)

run_all_metrics <- function(las, X, Y, Z, filename) {
  las.dispersion.metric <- cloud_metrics(las, ~metrics_dispersion(Z, dz = 1), res = 20)
  las.lad.metric <- cloud_metrics(las, ~metrics_lad(Z), res = 20)
  las.interval.metric <- cloud_metrics(las, ~metrics_interval(Z), res = 20)
  las.rumple.metric <- cloud_metrics(las, ~metrics_rumple(x = X, y = Y, z = Z, pixel_size = 1), res = 20)
  las.voxel.metric <- cloud_metrics(las, ~metrics_voxels(x = X, y = Y, z = Z, vox_size = 1))

  new_row <- data.frame(
    filename = filename,
    dispersion = las.dispersion.metric,
    lad = las.lad.metric,
    interval = las.interval.metric,
    rumple = las.rumple.metric,
    voxel = las.voxel.metric
  )

  if (exists("metrics_dataframe")) {
    metrics_dataframe <<- rbind(metrics_dataframe, new_row)
  } else {
    metrics_dataframe <<- new_row
  }

  metrics_dataframe
}

# Load in file twice to conduct metrics and add name for organization purposes
las <- readLAS("path to las file")
filename <- "same thing as above"
metrics <- run_all_metrics(las, X, Y, Z, filename)


# - next I selected only the metrics, removing the file name once I'd calculated for all of them
#   this was so I could actually conduct the modelling which I mentioned was an issue of mine
# - dispersion.ziqr:voxel.Oligophotic includes all of my metrics, you'll need to change based off
#   what you want to calculate
metrics_dataframe.copy = dplyr::select(metrics_dataframe, dispersion.ziqr:voxel.Oligophotic)

# finally combine your y variable(s), which are fuels for me, with the metrics from above
master.table.metrics <- data.frame(fuels, metrics_dataframe.copy)
