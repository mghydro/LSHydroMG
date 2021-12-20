#' @title mg_create_template_raster
#'
#' @description Takes a data set with coordinates and creates a template raster with a specifies resolution and crs
#'
#' @param df_col_x The column of the dataset with the x coordinate (should be numeric)
#' @param df_col_y The column of the dataset with the x coordinate (should be numeric)
#' @param resolution Resolution of the raster which is created (an integer, eg. 100 for 100x100 raster cell size)
#' @param crs_to_assign Coordinate reference system to assign to the created raster (integer, eg. 25832 for UTM 32, 31468 for Gauss krueger)
#'
#'
#' @return a list with two elements, first is a grid, second is a raster file
#'
#' @examples
#' grid_project_area <- mg_func_template_raster(df_col_x = meta_projekt$ostwert_crs_25832,
#' df_col_y = meta_projekt$nordwert_crs_25832,
#' resolution = 200,
#' crs_to_assign = 25832)
#'
#' @export
#' @importFrom dplyr %>% mutate
#' @importFrom raster rasterFromXYZ
#'
#'


#function to create template raster
mg_func_template_raster <- function(df_col_x,df_col_y,resolution, crs_to_assign){

  xx_list<- list()
  # template raster erstellen
  xx_bbox <- c(
    "xmin" = min(df_col_x),
    "ymin" = min(df_col_y),
    "xmax" = max(df_col_x),
    "ymax" = max(df_col_y)
  )
  xx_grd_template <- expand.grid(
    X = seq(from = xx_bbox["xmin"]-resolution, to = xx_bbox["xmax"]+resolution, by = resolution),
    Y = seq(from = xx_bbox["ymin"]-resolution, to = xx_bbox["ymax"]+resolution, by = resolution) # 20 m resolution
  )

  #
  # grid_plot <- ggplot() +
  #   geom_point(data = xx_grd_template, aes(x = X, y = Y), size = 0.01) +
  #   geom_point(data = xx_wasserstand,
  #              mapping = aes(x = ostwert_crs_25832, y = nordwert_crs_25832), size = 3)
  #
  #
  # grid_plot

  grd_template_raster <- xx_grd_template %>%
    dplyr::mutate(Z = 0) %>%
    raster::rasterFromXYZ(
      crs = crs_to_assign)


  xx_list[[1]] <- xx_grd_template
  xx_list[[2]] <- grd_template_raster

  return(xx_list)

}
