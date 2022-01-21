#' @title mg_function_impute_spreaded_ts
#'
#' @description impute spreaded time series using package imputeTS
#' @param spreaded_ts spreaded df with time series data (e.g. from function mg_function_extract_spreaded_ts)
#' @param plot_impute True/False if time series with imputed values should be plotted
#' @param export_path export path for plots
#' @param imputeTS_model type of model passed to impute TS function, default is StructTS
#' @param imputeTS_type type of type passed to impute TS function, default is "trend"
#'
#'
#' @return dataframe of the time series in wide format with equal distances:
#'
#'
#'
#'
#' @examples
#' spreaded_ts<- mg_function_extract_spreaded_ts(start_zeitpunkt = df_complete_time_series$number_of_timeseries$start_zeitpunkt[1],
#'                                               input_settings = df_complete_time_series$input_settings,
#'                                               df = wasserstand_projekt)
#'
#' @export
#' @importFrom dplyr %>% mutate group_by
#' @importFrom tidyr spread
#' @importFrom xts xts
#' @importFrom imputeTS na_kalman ggplot_na_imputations
#'
#'


mg_function_impute_spreaded_ts <- function(spreaded_ts,plot_impute,export_path = NA,imputeTS_model = "StructTS", imputeTS_type = "trend"){



  #create xts time series
  xx_xts <- xts(spreaded_ts[,-1],order.by = spreaded_ts$mean_date)


  #impute
  xx_xts_imputed <- na_kalman(xx_xts, smooth = T, model = imputeTS_model, type = imputeTS_type)

  ##plot imputed values
  if(plot_impute){
    ifelse(!dir.exists(export_path), dir.create(export_path), FALSE)

    p_impute <- names(xx_xts)
    for (pp in seq_along(1:length(p_impute))){

      if(sum(is.na(xx_xts[,pp])) >1){

        xx_plot <- ggplot_na_imputations(xx_xts[,pp],xx_xts_imputed[,pp],ylab = "GW-Stand [mNN]",x_axis_labels = index(xx_xts))+
          labs(title = p_impute[pp])

        plot_height <- 14
        plot_width <- 17.6
        name_plot <- p_impute[pp]
        path <-  export_path
        scale <-  1.2

        ggsave(filename=paste0(name_plot,".jpg"), plot=xx_plot,
               path = path,width = plot_width, height = plot_height,units = "cm", dpi = 150, scale = scale)

      } else {
       print(paste0(p_impute[pp]," has no missing values"))
      }


    }
  }

  xx_df_imputed <- data.frame(date=index(xx_xts_imputed), coredata(xx_xts_imputed))

  return(xx_df_imputed)
}
