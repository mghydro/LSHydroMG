#' @title mg_function_time_series_preparation
#'
#' @description Takes hydraulic head time series datafram and calculates mean values and dates for a certain timespan.
#'df,date_column_name,value_column_name,timespan
#' @param df dataframe which contains the value and date column
#' @param date_column_name column name of the date column (should be format date)
#' @param value_column_name column name1of the value column (numeric format)
#' @param id_column_name column name of identifier column (e.g. name of Grundwasssermessstelle) (character format)
#' @param timespan_weeks number of weeks for which mean value and mean date are calculated
#'
#'
#' @return input dataframe with 4 new columns,
#'  timespan_weeks: mean isoweek of timespan (e.g. isoweek 49-51 --> 50)
#'  timespan_weeks_label: character column showing the isoweeks e.g. '49-51'
#'  mean_value: mean value in time span
#'  mean_date: mean date in time span (format date)
#'
#' @examples
#' test<- mg_function_time_series_preparation(df = wasserstand_projekt,
#'date_column_name = "datum",
#'value_column_name = "mess_wert",
#'timespan = 3)

#'
#' @export
#' @importFrom dplyr %>% mutate group_by select starts_with
#' @importFrom lubridate isoyear month isoweek
#' @importFrom ISOweek ISOweek2date
#'
#'

mg_function_time_series_preparation <- function(df,date_column_name,value_column_name,id_column_name,timespan_weeks){
  xx_df <- as.data.frame(df)
  xx_df$xx_date <- xx_df[,date_column_name]
  xx_df$xx_value <- xx_df[,value_column_name]
  xx_df$xx_id_column_name <- xx_df[,id_column_name]


  xx_df$jahr <- isoyear(xx_df$xx_date)
  xx_df$monat <- month(xx_df$xx_date)
  xx_df$isoweek <- isoweek(xx_df$xx_date)


  #create breaks for timespan
  xx_list_of_weeks <- split(1:53, ceiling(seq_along(1:53)/timespan_weeks))
  xx_labels <- paste0(lapply(xx_list_of_weeks,min),"-", lapply(xx_list_of_weeks,max))

  #calculate values for timespan
  xx_df$timespan_weeks <-  xx_df$isoweek
  xx_df$timespan_weeks_label <- xx_df$isoweek
  for(i in seq_along(1:length(xx_list_of_weeks))){
    xx_df$timespan_weeks[(xx_df$isoweek <= max(xx_list_of_weeks[[i]]) & xx_df$isoweek >= min(xx_list_of_weeks[[i]]))] <- floor(mean(xx_list_of_weeks[[i]]))
    xx_df$timespan_weeks_label[(xx_df$isoweek <= max(xx_list_of_weeks[[i]]) & xx_df$isoweek >= min(xx_list_of_weeks[[i]]))] <- xx_labels[[i]]
  }


  #mittlerer Wasserstand für 14 Tage berechnen
  xx_df <- xx_df %>%
    group_by(jahr,timespan_weeks,xx_id_column_name) %>%
    mutate(mean_value = mean(xx_value),
           mean_date = ISOweek2date(paste0(jahr,"-W",sprintf("%02d",timespan_weeks),"-",1))) %>%
    ungroup() %>%
    dplyr::select(!starts_with("xx"))



  #mean_date = as.Date(paste(jahr, timespan_weeks, 1, sep="-"), "%Y-%U-%u"))
  return(xx_df)

}

