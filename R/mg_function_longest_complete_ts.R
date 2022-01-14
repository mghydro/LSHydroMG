#' @title mg_function_longest_complete_ts
#'
#' @description Takes hydraulic head time series dataframe and extracts for each dataset the longest complete time series
#' @param df dataframe which contains the value and date column
#' @param date_column_name column name of the date column (should be format date)
#' @param value_column_name column name of the value column (numeric format)
#' @param id_column_name column name ofidentifier column (e.g. name of Grundwasssermessstelle) (character format)
#' @param minimum_timespan_ts_years number of years which the ts should have at least (float)
#' @param n_allowed_missing_values_days timespan in number of days for which missing values are allowed
#'
#'
#' @return subset of initial dataframe which satisfies the conditions minimum_timespan_ts_years and n_allowed_missing_values_days
#'
#'
#'
#'
#' @examples
#'mg_function_longest_complete_ts(df = wasserstand_projekt,
#'                                date_column_name = "datum",
#'                                value_column_name = "mess_wert",
#'                                id_column_name = datenquelle,
#'                                n_allowed_missing_values_days = 60,
#'                                minimum_timespan_ts_years = 2.5)
#' @export
#' @importFrom dplyr %>% mutate group_by filter select lag
#'
#'
#'

mg_function_longest_complete_ts <- function(df,date_column_name,value_column_name,
                                            id_column_name,
                                            n_allowed_missing_values_days,
                                            minimum_timespan_ts_years){
  #create output list
  output <- list()

  #unify
  xx_df <- as.data.frame(df)
  xx_df$xx_date <- (xx_df[,date_column_name])
  xx_df$xx_value <- xx_df[,value_column_name]
  xx_df$xx_id_col <- xx_df[,id_column_name]

  #Berechnung der Differenz in Tagen
  xx_df_subset<- xx_df %>%
    group_by(xx_id_col)%>%
    mutate(xx_lagged_days = lag(xx_date,n = 1, default = NA),
           xx_difference_in_days = as.numeric(difftime(xx_date,xx_lagged_days,tz ="",units = "days")),
           xx_max_diff = max(xx_difference_in_days,na.rm = T),
           xx_length_ts = as.numeric(difftime(max(xx_date,na.rm=T),min(xx_date,na.rm=T),tz ="",units = "days"))) %>%
    filter(xx_max_diff < n_allowed_missing_values_days) %>%
    filter(xx_length_ts > minimum_timespan_ts_years*365) %>%
    ungroup()

  print(paste0(length(unique(xx_df_subset$xx_id_col)), " datasets have less than a timespan of ",n_allowed_missing_values_days," days with missing values and are longer than ",minimum_timespan_ts_years," years"))
  xx_df_subset %>%
    dplyr::select(!starts_with("xx"))

  return(xx_df_subset)

}

