#' @title mg_function_check_number_complete_time_series
#'
#' @description Takes hydraulic head time series dataframe and counts the number of complete time series for a certain timespan
#' @param df dataframe which contains the value and date column
#' @param date_column_name column name of the date column (should be format date)
#' @param value_column_name column name of the value column (numeric format)
#' @param id_column_name column name ofidentifier column (e.g. name of Grundwasssermessstelle) (character format)
#' @param time_span_years number of years for which complete time series are searched
#' @param moving_window_width_days number of days for which time_span_years is advancing (e.g. 30 to andvance 30 days)
#' @param fraction_of_NAs_allowed fraction of missing values allowed to count as a complete time series
#'
#'
#' @return list with two dataframes:
#'  df "number_of_timeseries" with 3  columns,
#'  start_zeitpunkt: start of time span
#'  end_zeitpunkt: end of time span
#'  anzahl_ts: number of complete time series in this time span
#'  df "input_settings" contains the input data. Can be used with function mg_function_extract_spreaded_ts
#'
#'
#'
#'
#' @examples
#'  mg_function_check_number_complete_time_series(df = wasserstand_projekt,
#'                                                value_column_name = "mean_value",
#'                                                date_column_name ="mean_date",
#'                                                id_column_name = "datenquelle",
#'                                                time_span_years = 10,
#'                                                moving_window_width_days = 30,
#'                                                fraction_of_NAs_allowed = 0.1)
#'
#' @export
#' @importFrom dplyr %>% mutate group_by
#' @importFrom tidyr spread
#'
#'

mg_function_check_number_complete_time_series <- function(df,date_column_name,value_column_name,
                                                          id_column_name,
                                                          time_span_years,moving_window_width_days,
                                                          fraction_of_NAs_allowed){
  #create output list
  output <- list()

  #unify
  xx_df <- as.data.frame(df)
  xx_df$date <- xx_df[,date_column_name]
  xx_df$value <- xx_df[,value_column_name]

  #set to datetime
  xx_df$date <- as.POSIXct (xx_df$date)

  #get first and last date in df
  first_time <- min(xx_df$date, na.rm = T)
  last_time <- max(xx_df$date, na.rm = T)


  n_jahre_width_window <- time_span_years*365*24*60*60 # breite moving window =  Anzahl der jahre in denen vergleichbare ganglinien gesucht werden
  n_tage_step_window <-  moving_window_width_days*24*60*60 # anzahl der Tage die das movong window vorr?ckt


  xx_zeitraumliste <- seq(from = first_time, to = last_time, by = n_tage_step_window)


  #iterate over list of dates
  xx_ouput_number_ts <- list()
  for (i in seq_along(1:(length(xx_zeitraumliste)-((n_jahre_width_window/(365*24*60*60)*12))))){

    start_zeitpunkt <- xx_zeitraumliste[i]
    end_zeitpunkt <- xx_zeitraumliste[i+(n_jahre_width_window/(365*24*60*60)*12)]

    #Auswahl der Zeitr?ume
    if(i == 1){
      xx_sub_df <- xx_df[which(xx_df$date < end_zeitpunkt),]
    }
    if(i > 1 && i < (length(xx_zeitraumliste)-1)){
      xx_sub_df <- xx_df[which(xx_df$date > start_zeitpunkt),]
      xx_sub_df <- xx_sub_df[which(xx_sub_df$date < end_zeitpunkt),]
    }
    if(i == (length(xx_zeitraumliste)-(n_jahre_width_window/(365*24*60*60)*12))){
      xx_sub_df <- xx_df[which(xx_df$date >start_zeitpunkt),]
    }

    xx_sub_df <- as.data.frame(xx_sub_df)







    xx_sub_df <- xx_sub_df[,c("date",id_column_name,"value")]

    xx_sub_df <- unique(xx_sub_df)

    #spread data to get NA values and homogenous time series
    xx_sub_df_spread <- spread(xx_sub_df, id_column_name, value,fill = NA)



    #funktion f?r bestimmung anteil NA
    MG_FUN_ratio_na <- function(x){
      sum(is.na(x))/length(x)
    }


    #Bestimmung anteil na
    xx_anteil_na <- sapply(xx_sub_df_spread[,2:length(names(xx_sub_df_spread))],  MG_FUN_ratio_na)



    # Auswahl der Messstellen die einen kleineren anteil als na als festgelegt in fraction_of_NAs_allowed
    xx_sub_df_spread1 <-  xx_sub_df_spread[,(which(xx_anteil_na < fraction_of_NAs_allowed)+1)]

    if(is.data.frame(xx_sub_df_spread1)){


      #hier wieder datum hinzuf?gen
      xx_sub_df_spread2 <-  cbind(xx_sub_df_spread[,1], xx_sub_df_spread1)

      names(xx_sub_df_spread2)[names(xx_sub_df_spread2) == 'xx_sub_df_spread[, 1]'] <- 'mean_date_two_weeks'  #

      print(paste0("start_time: ",start_zeitpunkt,";   end_time: ",end_zeitpunkt, ";  number of valid time series: ",length(names(xx_sub_df_spread2))))

      xx_number_ts <- data.frame(start_zeitpunkt = start_zeitpunkt,end_zeitpunkt = end_zeitpunkt,anzahl_ts = length(names(xx_sub_df_spread2)))
    } else {
      xx_number_ts <- data.frame(start_zeitpunkt,end_zeitpunkt,anzahl_ts = NA)
    }



    xx_ouput_number_ts[[i]] <- xx_number_ts
  }

  number_of_timeseries<- do.call(rbind,xx_ouput_number_ts)

  output[["number_of_timeseries"]] <- number_of_timeseries

  input_settings <- data.frame(value_column_name = value_column_name,
                               date_column_name = date_column_name,
                               id_column_name = id_column_name,
                               time_span_years = time_span_years,
                               moving_window_width_days = moving_window_width_days,
                               fraction_of_NAs_allowed = fraction_of_NAs_allowed)

  output[["input_settings"]] <- input_settings

  return(output)
}


