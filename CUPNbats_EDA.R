
### CUPN Bats EDA

### NAMING RULES for scripts ----
# Lists, data frames, vectors, variables...: list_station_files, df_station_inf
# Data frame cols: df_station_info$col_this_one, df_station_info$col_that_one
# List elements: list_stations$element_one
# Functions: FuncTest(dat1_there, dat2_here)

### QUESTIONS ----
# > Entry counts >= Exit counts for 637 records. By procedure (subtract entry from exit), these counts should be 0 or negative but this would not reflect reality b/c we know there is AT LEAST 1 bat. In cases where Entry or Exit count is more than twice the other number, we can set a minimum number known alive (MNKA) > 1. For example, if 2 bats enter and 0 exit, we can say MNKA = 2. It may be important to distinguish between a cave with MNKA = 0 and MNKA = non-0, even if the latter is a very small number
# > For an additional 137 records (mostly MACA), Entry counts are more than half of Exit counts. Since the current approach (subtract entry from exit) tends to undercount bats, these instances when Entry counts are a large proportion of Exit counts can lead to numbers that are quite "off"
# > Gap Cave--on these dates, no counts for Packrat Entrance (but counts available for all other entrances): 6/28/16, 7/26/16
# > In a single survey night with multiple entrances simultaneously surveyed, the methods may differ among the entrances (explained in Steve's notes)--typically it seems each entrance will have a night vision goggle count and then one of the two types of cameras. Sometimes the number of methods will differ among entrances in a single survey night. If any methods are biased (e.g., a camera undercounts), then these different methods at different entrances can be problematic. BUT in most cases, there is a night vision goggle count. May need to analyze these data as would a metapopulation, with movement among subpops
# > 2 duplicated covariate records. See cov_dupes.csv

### TO DO, TO CHECK, IMPORTANT NOTES ---
# 2018 data are provisional -- DATA QUALITY INFO MISSING for most of the 2018 records. Leave the 2018 data out.
# > Not sure how to deal with multiple entrance caves--just add the entrance means together? Can't do it by method because different methods may be used at different entrances. Perhaps first do the single entrance cave analyses to see if/how methods differ.
# > Should do a text search through data Notes column to find out how many nominally full intervals are actually partial intervals (less than 5 min) and whether or not this is more common for certain methods
# > Is night-to-night variability greater (per entrance) for multi-entrance caves than for single entrance caves?
# > There are some instances of huge differences among methods (as a % of mean count for the night) during a single survey event -- should check back in the data for notes that may warrant excluding some of the data (e.g., Dixon Cave 7/13/2016; Blue Spring Hollow - Upper Entrance 6/16/2016; Long Cave June-July 2016 counts)--perhaps methods did not start/end same time or many partial intervals for some methods?

### FOR THIS INITIAL EDA, THE FINAL DATA...
# > Excluded incomplete surveys (<15 intervals)
# > Only included data rated as "good" quality
# > Only included data starting 2016
# > Omitted two duplicate covariate records
# > Omitted records with NA for both entry and exit count
# > Converted to zero (0) any NA in entry/exit count for which the other count is non-NA
# > Excluded caves with multiple entrances (need to figure out best way to treat these)

### Load packages ----
rm(list=ls())
pkgs <- c("readxl", "readr", "tidyverse", "here", "janitor", "lubridate", "magrittr", "unmarked", "MuMIn", "data.table", "lubridate", "ggthemes", "gridExtra", "grid", "ggforce", "scales")
package.check <- lapply(pkgs, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE, repos = "http://cran.us.r-project.org")
    library(x, character.only = TRUE)
  }
})

### Functions and Themes----

FuncImport <- function(filenam) {
  read_csv(here::here("Data_in", filenam)) %>%
    janitor::clean_names("snake") %>%
    dplyr::mutate(event_date = lubridate::mdy(event_date),
                  across(c("interval_number", "interval_duration"), as.integer),
                  across(contains(c("person")), as.character),
                  across(ends_with(c("entry", "exit")), as.integer))
}

FuncMetrics <- function(dat) {
  # Function to calculate metrics and put results in long format
  #
  # Args:
  #   dat:  The raw data in wide format
  # Returns:
  #   Formatted data with three calculated metrics. Method names should not be distinguished by the observer--instead, a separate column for method and observer. One count per record. Remove records with NA for both Exit and Entry count
  dat$interval_start_time[dat$interval_start_time == "Unknown"] <- NA # if start time is unknown, convert to NA so can assign time class to that column
  
  cols_df <- data.frame(
    rbind(
      c("night_vision_goggles", "night_vision_goggles_1_exit", "night_vision_goggles_1_entry", "method_person_night_vision_goggles_1"),
      c("night_vision_goggles", "night_vision_goggles_2_exit", "night_vision_goggles_2_entry", "method_person_night_vision_goggles_2"),
      c("night_vision_camera", "night_vision_video_camera_1_exit", "night_vision_video_camera_1_entry", "method_person_night_vision_video_camera_1"),
      c("night_vision_camera", "night_vision_video_camera_2_exit", "night_vision_video_camera_2_entry", "method_person_night_vision_video_camera_2"),
      c("thermal_infrared_camera", "thermal_infrared_camera_1_exit", "thermal_infrared_camera_1_entry", "method_person_thermal_infrared_camera_1"),
      c("thermal_infrared_camera", "thermal_infrared_camera_2_exit", "thermal_infrared_camera_2_entry", "method_person_thermal_infrared_camera_2")
    )
  )
  colnames(cols_df) <- c("meth_name", "exit_col", "entry_col", "surveyor_col")
  
  FuncCalcs <- function(dat, meth_name, exit_col, entry_col, surveyor_col) {
    
    df_maxem <- dat %>%
      dplyr::filter(!is.na(get(exit_col)) | !is.na(get(entry_col))) %>% # exclude records with NA for both Exit and Entry counts
      dplyr:: mutate_at(c(exit_col, entry_col), ~replace(., is.na(.), 0)) %>% # if one of these has a number and the other has NA, replace the NA with 0
      dplyr::group_by(park, cave, event_date, entrance) %>%
      dplyr::summarize(
        survey_start_time = switch(all(is.na(interval_start_time))+1, min(chron::times(format(strptime(interval_start_time, "%I:%M:%S %p"), "%H:%M:%S")), na.rm = TRUE), NA),
        surveyor = unique(get(surveyor_col)),
        num_intervals = ifelse(all(is.na(get(exit_col))), NA, length(na.omit(interval_number))),
        total_duration = ifelse(all(is.na(get(exit_col))), NA, sum(interval_duration, na.rm = TRUE)),
        summary_exit = ifelse(all(is.na(get(exit_col))), NA, sum(get(exit_col), na.rm = TRUE)),
        summary_entry = ifelse(all(is.na(get(entry_col))), NA, sum(get(entry_col), na.rm = TRUE)),
        summary_net_exit = ifelse(all(is.na(get(exit_col)), is.na(get(entry_col))), NA, sum(get(exit_col), -get(entry_col), na.rm = TRUE)),
        summary_maxem = ifelse(all(is.na(get(exit_col)), is.na(get(entry_col))), NA, cumsum((get(exit_col) -get(entry_col)) %>% replace_na(., 0)) %>% max())
      ) %>%
      dplyr::mutate(method = meth_name)
    df_maxem[df_maxem == -Inf] <- NA # these are cases where all values were NA for all intervals
    return(df_maxem)
  }
  
  metrics_list <- apply(cols_df, MARGIN = 1, FUN = function(x) {

    out <- FuncCalcs(dat = dat, meth_name = x[["meth_name"]], exit_col = x[["exit_col"]], entry_col = x[["entry_col"]], surveyor_col = x[["surveyor_col"]])
  })
  
  metrics_df <- do.call("rbind", metrics_list) %>%
    dplyr::mutate(
      yr_day = as.integer(lubridate::yday(event_date)),
      mnth = lubridate::month(event_date, label = TRUE, abbr = TRUE),
      yr = as.integer(lubridate::year(event_date)))
  return(metrics_df)
}

FuncFormatCov <- function(func_dat) {
  # Clean individual survey covariate data prior to appending
  #
  # Args:
  #   dat:  The raw covariate data
  # Returns:
  #   Formatted data ready for combining
  
  func_dat %<>% 
    janitor::clean_names("snake") %>% 
    dplyr::mutate(
      method = case_when(
        summer_bat_count_method_id == 1 ~ "night_vision_goggles",
        summer_bat_count_method_id == 2 ~ "night_vision_camera",
        summer_bat_count_method_id == 3 ~ "thermal_infrared_camera"),
      bad_weather = start_weather_condition_id %in% 4:7, # start_weather_condition_id (1-3 is various levels of cloudiness; 4-7 are more severe conditions)
      data_quality = case_when(
        data_quality_flag_id == 1 ~ "good",
        data_quality_flag_id == 2 ~ "questionable",
        data_quality_flag_id == 3 ~ "unreliable",
        data_quality_flag_id == 4 ~ "UNK"
      ),
      event_date = lubridate::ymd(start_date)
    ) %>%
    dplyr::rename(cave = location_name, entrance = entrance_name) %>%
    dplyr::select(-start_date, -data_quality_flag_id, -summer_bat_count_method_id, -start_weather_condition_id) %>%
    distinct() %>%
    dplyr::mutate(
      row_id = row_number())
}

FuncMethodsCompare <- function(func_dat, base_method, compare_method, plot_title) {
  # Function to compare counts for two methods
  #
  # Args:
  #   func_dat:
  #   base_method:
  #   compare_method:
  #   plot_title:
  #
  # Returns:
  #   Point plot
  #
  
  func_dat %<>%
    ungroup() %>%
    dplyr::filter(method %in% c(base_method, compare_method))
  
  # Counts for data starting 2016
  dat_full <- func_dat %>% 
    dplyr::select(park, cave, entrance, yr, event_date, method, metric) %>%
    droplevels()

  dat_plot <- dat_full %>% 
    dplyr::filter(method == base_method) %>% 
    dplyr:: full_join(dat_full %>% dplyr::filter(method == compare_method), by = c("park", "cave", "entrance", "yr", "event_date")) %>% # can't just spread b/c sometimes multiple of same method for a survey
    dplyr::rename(base_count = metric.x, compare_count = metric.y) %>%
    dplyr::select(-method.x, -method.y) %>%
    dplyr::filter(!is.na(base_count) & !is.na(compare_count)) # exclude records with NA for either method count
  
  p1_dat <- dat_plot %>%
    dplyr::filter(base_count > 0 & compare_count > 0) # exclude records where either method recorded zero or negative count (b/c using log-transformed axes)
  
  p1 <- ggplot(data = p1_dat, aes(x = base_count, y = compare_count)) +
    scale_y_log10(limits = range(p1_dat[, c("base_count", "compare_count")])) +    
    scale_x_log10(limits = range(p1_dat[, c("base_count", "compare_count")])) +
    coord_fixed(ratio = 1, expand = TRUE) +
    geom_smooth(se = FALSE) +
    geom_point(aes(fill = as.factor(yr)), shape = 21, size = 3) +
    geom_abline(slope=1, color = "red", linetype = "dashed", size = 1.25) +
    labs(x = paste0("\n", gsub("_", " ", base_method)), y = paste0(gsub("_", " ", compare_method), "\n"), subtitle = "Positive counts for both methods.\nAxes are log-transformed. Blue line is loess smooth.") +
    ggthemes::theme_economist() +
    ggthemes::scale_fill_colorblind(name = "Year")
  
  p2_dat <- dat_plot %>%
    dplyr::filter(base_count <= 0 | compare_count <= 0) # exclude records with positive count for both methods
  
  p2 <- ggplot(data = p2_dat, aes(x = base_count, y = compare_count)) +
    ylim(range(p2_dat[, c("base_count", "compare_count")])) +
    xlim(range(p2_dat[, c("base_count", "compare_count")])) +
    coord_fixed(ratio = 1, expand = TRUE) +
    geom_point(aes(fill = as.factor(yr)), shape = 21, size = 3) +
    geom_abline(slope=1, color = "red", linetype = "dashed", size = 1.25) +
    labs(x = paste0("\n", gsub("_", " ", base_method)), y = paste0(gsub("_", " ", compare_method), "\n"), subtitle = "At least one method had zero or negative count.\nAxes are NOT transformed.") +
    ggthemes::theme_economist() +
    ggthemes::scale_fill_colorblind(name = "Year")
  
  p_page <- gridExtra::grid.arrange(p1, p2, ncol = 2, top = grid::textGrob(plot_title))
  x<-chisq.test(x = c(sum(dat_plot$base_count > dat_plot$compare_count), sum(dat_plot$compare_count > dat_plot$base_count)), p = c(0.5, 0.5))
  return_list <- list(plot = p_page, xtest = x)
  return(return_list)
}

FuncPlotCaves <- function(func_dat, color_by) {
  # Function to plot counts by cave
  #
  # Args:
  #   func_dat: The entrance totals data
  #   color_by: The variable to color points by
  #
  # Returns:
  #   List of point plots, separately for single entrance vs. multi-entrance caves
  #
  
  # Single entrance caves

  plot_dat_single <- func_dat %>% 
    dplyr::rename(color_col = color_by) %>%
    droplevels() %>%
    dplyr::filter(cave %in% setdiff(unique(func_dat$cave), df_multi_entrance$cave)) # exclude multi-entrance caves
  lower_y <- min(plot_dat_single$total_net_count)
  
  p_list <- list()
  for (i in sort(unique(plot_dat_single$cave))) {
    p <- ggplot(data = plot_dat_single %>% dplyr::filter(cave == i), aes(x = event_date, y = total_net_count, group = color_col)) +
      coord_cartesian(clip = "off") +
      geom_point(aes(shape = total_duration==75, color = color_col), alpha = 0.6, size = 3) +
      geom_line(aes(group = event_date), linetype = "dotted") +
      scale_y_continuous(breaks= scales::pretty_breaks(), limits = c(lower_y, NA)) + # set the lower boundary of y-axis consistent across all
    labs(x = "\nSurvey Date", y = "Event Count (Exit minus Entry)\n", title = paste0("Event Counts for ", i), subtitle = "Filled circles are full survey counts; open circles include missing intervals; vertical lines connect same date") +
      scale_shape_manual(values = c("FALSE" = 1, "TRUE" = 16)) +
      theme_bw() +
      theme(axis.text.x = element_text(angle=90))
    
    if(length(levels(plot_dat_single$color_col)) <= 8) {p_list[[i]] <- p + ggthemes::scale_color_colorblind(name = color_by)} else {p_list[[i]] <- p + scale_color_viridis_d(color_by)}
  }
  
  # Multiple entrance caves
  # One cave per page, facet by entrance
  plot_dat_multi <- func_dat %>% 
    dplyr::rename(color_col = color_by) %>%
    droplevels() %>%
    dplyr::filter(cave %in% df_multi_entrance$cave) # only for multi-entrance caves
  
  p_multi_list <- list()
  if(nrow(plot_dat_multi) > 0) {
    lower_y <- min(plot_dat_multi$total_net_count)
    for (i in sort(unique(plot_dat_multi$cave))) {
    p_multi <- ggplot(data = plot_dat_multi %>% dplyr::filter(cave == i), aes(x = event_date, y = total_net_count, group = color_col)) +
      coord_cartesian(clip = "off") +
      geom_point(aes(shape = total_duration==75, color = color_col), alpha = 0.6, size = 3) +
      geom_line(aes(group = event_date), linetype = "dotted") +
      scale_y_continuous(breaks= scales::pretty_breaks(), limits = c(lower_y, NA)) + 
      labs(x = "\nSurvey Date", y = "Event Count (Exit minus Entry)\n", title = paste0("Event Counts for ", i, ", by Entrance"), subtitle = "Filled circles are full survey counts; open circles include missing intervals; vertical lines connect same date") +
      scale_shape_manual(values = c("FALSE" = 1, "TRUE" = 16)) +
      theme_bw() +
      theme(axis.text.x = element_text(angle=90)) +
      facet_grid(entrance ~ .,)
      
      if(length(levels(plot_dat_multi$color_col)) <= 8) {p_multi_list[[i]] <- p_multi + ggthemes::scale_color_colorblind(name = color_by)} else {p_multi_list[[i]] <- p_multi + scale_color_viridis_d(color_by)}
    }
  }
  return_list <-list(single_entr = p_list, multi_entr = p_multi_list)
  return(return_list)

}

FuncSummaryEntryExit <- function(func_dat) {
  # Function to summarize count range by cave, separately  for entry and exit counts
  #
  # Args:
  #   func_dat: The entrance totals data
  #
  # Returns:
  #   Table
  #
  
  # Only using data with full surveys (total_duration = 75)
  df_min_max <- func_dat %>% 
    dplyr::filter(total_duration == 75) %>%
    dplyr::arrange(park, cave, entrance, event_date, method) %>%
    dplyr::group_by(cave, entrance, event_date) 
  
  df_Range <- df_min_max %>%
    dplyr::mutate(
      all_methods = paste0(method, collapse = " / "),
      range_exit_count = paste0(min(total_exit_count, na.rm = TRUE), " - ", max(total_exit_count, na.rm = TRUE)),
      diff_exit_count = max(total_exit_count, na.rm = TRUE) - min(total_exit_count, na.rm = TRUE),
      range_entry_count = paste0(min(total_entry_count, na.rm = TRUE), " - ", max(total_entry_count, na.rm = TRUE)),
      diff_entry_count = max(total_entry_count, na.rm = TRUE) - min(total_entry_count, na.rm = TRUE),
      range_event_count = paste0(min(total_net_count, na.rm = TRUE), " - ", max(total_net_count, na.rm = TRUE)),
      diff_event_count = max(total_net_count, na.rm = TRUE) - min(total_net_count, na.rm = TRUE),
    ) %>% 
    dplyr::select(park, cave, entrance, yr_day, yr, event_date, range_exit_count, diff_exit_count, range_entry_count, diff_entry_count, range_event_count, diff_event_count, all_methods) %>% # present cols in this order
    dplyr::distinct()
    
  # There is a more efficient way to do this but...
  df_min_entry <- df_min_max %>%
    dplyr::slice_min(total_entry_count, n = 1) %>%
    dplyr::select(cave, entrance, event_date, method, total_entry_count) %>%
    dplyr::group_by(cave, entrance, event_date) %>%
    dplyr::mutate(
      min_entry_method = paste0(method, collapse = " / ")) %>% # show multiple if ties
    dplyr::select(-method, -total_entry_count) %>%
    dplyr::distinct()
  
  df_max_entry <- df_min_max %>%
    dplyr::slice_max(total_entry_count, n = 1) %>%
    dplyr::select(cave, entrance, event_date, method, total_entry_count) %>%
    dplyr::group_by(cave, entrance, event_date) %>%
    dplyr::mutate(
      max_entry_method = paste0(method, collapse = " / ")) %>% # show multiple if ties
    dplyr::select(-method, -total_entry_count) %>%
    dplyr::distinct()
  
  df_min_exit <- df_min_max %>%
    dplyr::slice_min(total_exit_count, n = 1) %>%
    dplyr::select(cave, entrance, event_date, method, total_exit_count) %>%
    dplyr::group_by(cave, entrance, event_date) %>%
    dplyr::mutate(
      min_exit_method = paste0(method, collapse = " / ")) %>% # show multiple if ties
    dplyr::select(-method, -total_exit_count) %>%
    dplyr::distinct()
  
  df_max_exit <- df_min_max %>%
    dplyr::slice_max(total_exit_count, n = 1) %>%
    dplyr::select(cave, entrance, event_date, method, total_exit_count) %>%
    dplyr::group_by(cave, entrance, event_date) %>%
    dplyr::mutate(
      max_exit_method = paste0(method, collapse = " / ")) %>% # show multiple if ties
    dplyr::select(-method, -total_exit_count) %>%
    dplyr::distinct()
  
  df_entry_exit <- plyr::join_all(list(df_Range, df_min_exit, df_max_exit, df_min_entry, df_max_entry), by=c("cave", "entrance", "event_date"), type="full")
  df_entry_exit$min_entry_method[df_entry_exit$min_entry_method == df_entry_exit$all_methods] <- NA
  df_entry_exit$max_entry_method[df_entry_exit$max_entry_method == df_entry_exit$all_methods] <- NA
  df_entry_exit$min_exit_method[df_entry_exit$min_exit_method == df_entry_exit$all_methods] <- NA
  df_entry_exit$max_exit_method[df_entry_exit$max_exit_method == df_entry_exit$all_methods] <- NA
  
  return(df_entry_exit)
}

FuncIntervalDistrib <- function(func_dat) {
  # Function to summarize distribution of counts over the 15 intervals of a full survey. For full surveys only. 
  #
  # Args:
  #   func_dat: The final data (by interval)
  #
  # Returns:
  #   Bar plots, faceted by survey night
  #
  # Use only method-surveys with full survey duration
  func_dat %<>% 
    dplyr::mutate(cave_entrance = paste0(cave, "_", entrance)) 
  
  full_surv <- func_dat %>%
    dplyr::group_by(unique_count) %>%
      summarize(
        total_duration = sum(interval_duration)
        )
  
  interval_dat <- func_dat %>%
    dplyr::left_join(full_surv, by = c("unique_count")) %>%
    dplyr::filter(total_duration == 75) %>%
    droplevels()
  
  p_list <- list()
  for (i in sort(unique(interval_dat$cave_entrance))) {
    p_list[[i]] <- ggplot(data = interval_dat %>% dplyr::filter(cave_entrance == i), aes(x = interval_number, y = net_count)) +
      coord_cartesian(clip = "off") +
      geom_point(aes(color = method), alpha = 0.6) +
      geom_line(aes(color = method, group = interaction(method, surveyor))) +
      scale_y_continuous(breaks= scales::pretty_breaks()) +
      ggthemes::scale_color_colorblind() +
      labs(x = "\nInterval", y = "Interval Count (Exit minus Entry)\n", title = paste0("Interval Counts by Survey Date: ", i), subtitle = "Full surveys (75-min) only. Overlapped points may not be visible.") +
      theme_bw() +
      facet_wrap(~event_date)
  }
  return(p_list)
  }
  
### Import and format data ----

# Read in data
dat2018_raw <- FuncImport(filenam = "Summer_2018_Provisional_Data_54Events.csv") # read in 2018 (provisional) data. These were not included in Ingersoll's analysis. Only the finalized 2018 data were included.
dat_raw <- FuncImport(filenam = "Original_data.csv")

# Combine data
df_combined <- dplyr::bind_rows(dat_raw, dat2018_raw)
nrow(df_combined) == sum(nrow(dat_raw), nrow(dat2018_raw)) # make sure no records dropped

df_metrics <- FuncMetrics(dat = df_combined) %>%
  dplyr::filter(yr >=2016) # ONLY SUMMARIZE/ANALYZE DATA STARTING 2016 <<<<<<<<<<<<<<<

# Clean individual survey covariate data prior to appending
dat_cov <- read_excel("Data_in/qry_Rcov.xlsx")
df_cov <- FuncFormatCov(func_dat = dat_cov)

cov_dups <- df_cov %>% janitor::get_dupes(c(cave, entrance, method, event_date)) # find records duplicated for this subset of column names

# I have to remove two particular instances of duplicated records <<<<<<<<<<<<<<<<<<<<<<<<<<<
df_cov %<>%
  filter(!row_id %in% c(130, 895))

# Entrance-Survey totals, with covariate info added
df_final <- df_metrics %>%
  left_join(subset(df_cov, select = -row_id), by = c("cave", "entrance", "method", "event_date")) %>%
  dplyr::filter(data_quality == "good") # 88% (N=520 out of 593) of data were "good" <<<<<<<<<<<<<<<<<

saveRDS(df_final, here::here("Data_out", "df_final.RDS"))

df_multi_entrance <- dat %>%
    dplyr::select(park, cave, entrance) %>%
    dplyr::distinct() %>%
    dplyr::arrange(park, cave) %>%
    dplyr::count(park, cave) %>%
    dplyr::filter(n>1) %>% # these are the caves with multiple entrances
  droplevels()
saveRDS(df_multi_entrance, here::here("Data_out", "df_multi_entrance.RDS"))

### Read in final data ----
df_final <- readRDS(here::here("Data_out", "df_final.RDS"))
df_multi_entrance <- readRDS(here::here("Data_out", "df_multi_entrance.RDS"))

### Check data for possible problems ----
# Check for NA's
summary(df_final) 
# > Highest Entry count (for a method-survey) is 62. Highest Exit count is 4376

# Entry counts >= Exit counts
summary(df_final %>%
  dplyr::filter(
    summary_entry > 0 & summary_net_exit <= 0))
# > 418 records with Entry count >= Exit count (excluding records where both are 0). Highest Exit count was 19, so these are all relatively low overall counts <<<<<<<<<<<<<<<<<<< PICK UP FROM HERE

# Entry counts < Exit counts but more than half the Exit counts
summary(dat %>%
          dplyr::filter(
            net_count > 0 & entry_count > .5*exit_count))
# > 76 records with Entry count more than half the Exit counts

# Check if multiple entrances of a cave are always sampled on the same date
dat %>% 
  dplyr::filter(cave %in% df_multi_entrance$cave) %>%
  dplyr::select(cave, event_date, entrance) %>%
  dplyr::distinct() %>%
  dplyr::mutate(event_date = as.factor(event_date)) %>%
  janitor::tabyl(event_date, entrance, cave, show_missing_levels = FALSE)
# > Gap Cave--on these dates, no counts for Packrat Entrance: 6/28/16, 7/26/16. Checked with Steve and he confirmed no data available for that entrance on those dates (problems occurred during surveys)

### Data Summaries ----
# In all summaries, Entry counts are subtracted from Exit counts, per protocol (this is likely to undercount)
# Only summarizing data starting from 2016

# Summary by surveyor (for data since 2016) ----
# table
dat %>%
  tabyl(surveyor, method, show_missing_levels = FALSE) %>%
  adorn_totals(c("row", "col")) %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 0) %>%
  adorn_ns() %>%
  adorn_title("combined") %>%
  dplyr::arrange(Total)

# graph
ggplot(data = dat, aes(y = forcats::fct_infreq(surveyor), fill = method)) +
  geom_bar(position = position_dodge2(preserve = "single")) +
  labs(y = NULL, x = "# of Survey Events", title = "Distribution of Surveyors by Method") +
  ggthemes::theme_economist(base_size = 8) +
  ggthemes::scale_fill_colorblind()
# > Most surveyors who conducted up to 100 counts (7 of 10 surveyors) only had counts for night vision goggles. Seems possible that this could lead to more noise in the data for night vision goggle counts (less experienced surveyors). Consider including surveyor experience as a covariate to determine how much this might contribute to count discrepancies (but note that for any one surveyor, experience is cumulative--counts may improve based on how many prior surveys they did, so can't just use total survey experience as the covariate)

# Total duration by method (for data since 2016, and ONLY using records where data quality reported as "good") ----
# There may be a couple instances where two different people surveyed with the same method on the same cave-event <<<<<<<<<<<<<<<<<<<<<<<<<<< check for this
df_final %>%
  dplyr::filter(yr >= 2016) %>%
  dplyr::mutate(total_duration = factor(total_duration, levels = seq(5, 75, by = 5))) %>%
  tabyl(method, total_duration) %>%
  adorn_totals("col") %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 0) %>%
  adorn_ns() %>%
  adorn_title("combined")
 # > Thermal infrared camera was more likely to miss at least one 5-min interval (for the other two methods, 94% and 93% of surveys had full 75-min surveys; for thermal infrared, 81% had the full 75-min surveys)

# Comparison of INTERVAL AND TOTAL EVENT COUNTS for surveys with a single method duplicated <<<<<<<<<<<<<<<<<<<<<< STILL NEED TO DO

# Comparison of TOTAL EVENT COUNTS for FULL SURVEY counts ---- 
# No missing intervals, though may have partial intervals
(out <- FuncMethodsCompare(func_dat = df_final %>% dplyr::rename(metric = total_net_count) %>% dplyr::filter(total_duration == 75), base_method = "night_vision_goggles", compare_method= "thermal_infrared_camera", plot_title = "EVENT Counts for FULL Surveys: Goggles vs. Thermal Camera"))
# > Nearly significant (p = 0.069), thermal camera had higher EVENT counts than goggles more often than expected (thermal camera = 32 times, goggles = 19 times). But they fall along a pretty straight 1:1 line.

(out <- FuncMethodsCompare(func_dat = df_final %>% dplyr::rename(metric = total_net_count) %>% dplyr::filter(total_duration == 75), base_method = "night_vision_goggles", compare_method= "night_vision_camera", plot_title = "EVENT Counts for FULL Surveys: Goggles vs. Night Vision Camera"))
# > NOT significant (p = 0.62)

(out <- FuncMethodsCompare(func_dat = df_final %>% dplyr::rename(metric = total_net_count) %>% dplyr::filter(total_duration == 75), base_method = "night_vision_camera", compare_method= "thermal_infrared_camera", plot_title = "EVENT Counts for FULL Surveys: Night Vision Camera vs. Thermal Camera"))
# > NOT significant (p = 0.74)
# > The methods seem to give pretty similar results, so may not need to allow for different detectability by method

# Comparison of TOTAL EVENT COUNTS for ALL SURVEY counts ----
# May have missing intervals
(out <- FuncMethodsCompare(func_dat = df_final %>% dplyr::rename(metric = total_net_count), base_method = "night_vision_goggles", compare_method= "thermal_infrared_camera", plot_title = "EVENT Counts for ALL Surveys: Goggles vs. Thermal Camera"))

(out <- FuncMethodsCompare(func_dat = df_final %>% dplyr::rename(metric = total_net_count), base_method = "night_vision_goggles", compare_method= "night_vision_camera", plot_title = "EVENT Counts for ALL Surveys: Goggles vs. Night Vision Camera"))

(out <- FuncMethodsCompare(func_dat = df_final %>% dplyr::rename(metric = total_net_count), base_method = "night_vision_camera", compare_method= "thermal_infrared_camera", plot_title = "EVENT Counts for ALL Surveys: Night Vision Camera vs. Thermal Camera"))

# Comparison of EXIT COUNTS for FULL SURVEY counts ---- 
# No missing intervals, though may have partial intervals
(out <- FuncMethodsCompare(func_dat = df_final %>% dplyr::rename(metric = total_exit_count) %>% dplyr::filter(total_duration == 75), base_method = "night_vision_goggles", compare_method= "thermal_infrared_camera", plot_title = "EXIT Counts for FULL Surveys: Goggles vs. Thermal Camera"))
# > Nearly statistically significant (p = 0.052), thermal camera had higher exit counts than goggles more often than expected (thermal camera = 33 times, goggles = 19 times) but the actual counts fall pretty well along a 1:1 line

(out <- FuncMethodsCompare(func_dat = df_final %>% dplyr::rename(metric = total_exit_count) %>% dplyr::filter(total_duration == 75), base_method = "night_vision_goggles", compare_method= "night_vision_camera", plot_title = "EXIT Counts for FULL Surveys: Goggles vs. Night Vision Camera"))

(out <- FuncMethodsCompare(func_dat = df_final %>% dplyr::rename(metric = total_exit_count) %>% dplyr::filter(total_duration == 75), base_method = "night_vision_camera", compare_method= "thermal_infrared_camera", plot_title = "EXIT Counts for FULL Surveys: Night Vision Camera vs. Thermal Camera"))

# Comparison of ENTRY COUNTS for FULL SURVEY counts ---- 
# No missing intervals, though may have partial intervals
(out <- FuncMethodsCompare(func_dat = df_final %>% dplyr::rename(metric = total_entry_count) %>% dplyr::filter(total_duration == 75), base_method = "night_vision_goggles", compare_method= "thermal_infrared_camera", plot_title = "ENTRY Counts for FULL Surveys: Goggles vs. Thermal Camera"))

(out <- FuncMethodsCompare(func_dat = df_final %>% dplyr::rename(metric = total_entry_count) %>% dplyr::filter(total_duration == 75), base_method = "night_vision_goggles", compare_method= "night_vision_camera", plot_title = "ENTRY Counts for FULL Surveys: Goggles vs. Night Vision Camera"))

(out <- FuncMethodsCompare(func_dat = df_final %>% dplyr::rename(metric = total_entry_count) %>% dplyr::filter(total_duration == 75), base_method = "night_vision_camera", compare_method= "thermal_infrared_camera", plot_title = "ENTRY Counts for FULL Surveys: Night Vision Camera vs. Thermal Camera"))

# Time series plots by cave entrance (for data since 2016) ----
# Color by method
(tots <- FuncPlotCaves(func_dat = df_final, color_by = "method")) 
# Color by observer
(tots <- FuncPlotCaves(func_dat = df_final, color_by = "surveyor"))
# Color by weather
(tots <- FuncPlotCaves(func_dat = df_final, color_by = "bad_weather"))
# Color by month
# Color by weather
(tots <- FuncPlotCaves(func_dat = df_final, color_by = "mnth"))
 
# > Month/week/day of survey sometimes made a HUGE difference in counts -- See Luna Cave [seems to be due to month of survey], Long Cave [several nights' counts seemingly ended prematurely though that doesn't account for the night-to-night discrepancy], Historic Entrance, which are all MACA (there may be other caves not evaluated in the 2016 study). For these caves, seems particularly important to conduct surveys across multiple nights to find the "peak" period. OR could it be because they are sometimes not getting full emergence period in their surveys (true, but this doesn't seem to be accounting for the high night-to-night differences)? Perhaps some automated software can identify which videos (roughly) have the highest bat counts, then actual counts can focus on those particular video nights. This information would also be really useful to identify factors that correlate with bat emergence, and trends in timing (what day-of-year) of peak emergence over the long-term. Can emergence be explained by weather conditions? Apparent year-to-year trends may totally be masked if some years catch the peak and others don't
# > Seems that 2016 study showed highest (by far) Luna counts in July, but 2017 surveys were still conducted in June (even though 2016 report recommended changing the timing of Luna counts). Month of survey seems to matter, so would want to catch the peak each year--it's not clear how much the timing of the distribution peak shifts year-to-year, and what (weather or other) factors influence this timing. Maybe related to food availability?
# > So far the data seem to argue for changing approach to get more sampling nights per cave, e.g., by getting more night vision cameras that can be set up and run perhaps by volunteers so not limited by availability of personnel
# > Not seeing any obvious patterns/biases related to surveyor or method or even complete (75-min) vs incomplete surveys
# > For analyses, it seems we need to allow for different detection probabilities per night (though it's really different emergence probabilities, not different detection probabilities) and by 'month' --though really survey set. Also limit it to caves for which there are multiple nights of surveys (so generate a table of # of surveys by cave entrance- month-year). For survey set, here are ones that crossover months: Olde Formation Pit (5/30/2016 - 6/3/2016, 6/28/2016 - 7/1/2016); Misty Hole Cave (5/31/2016 - 6/2/2016 which seems to be supposed to align with following year's 6/7/2017 - 6/8/2017); Luna Cave (5/31/2016 - 6/9/2016 which seems to supposedly align with next year's 6/7/2017 - 6/8/2017); Gap Cave (multiple entrances) (5/31/2016 - 6/3/2016, 6/28/2016 - 7/1/20160); Coke Cave (5/31/ 2016 seems to supposedly align with next year's 6/7/2017 - 6/8/2017); Anarchy Shelter(6/28/17 seems to supposedly align with next year's 7/2/2018 - 7/3/2018) -- I think to deal with this I need to use yday and then count out (2wks?) from each and across years


# # Table of min/max exit/entrance counts and methods
# > There are some instances of huge differences among methods (as a % of mean count for the night) during a single survey event -- should check back in the data for notes that may warrant excluding some of the data
dat_exit_entry <- FuncSummaryEntryExit(func_dat = df_final) # Output only for full surveys (75 min) 
table(dat_exit_entry$all_methods) # One case (cave-entrance-survey night) with two night vision goggle counts, and one case with two thermal infrared camera counts

# # Table of range of survey ydays for an entrance
df_RangeYday <- dat_exit_entry %>% 
  dplyr::group_by(park, cave, entrance) %>% 
  dplyr::summarize(
    range_yr_day = paste0(min(yr_day, na.rm = TRUE), " - ", max(yr_day, na.rm = TRUE)),
    diff_yr_day = max(yr_day, na.rm = TRUE) - min(yr_day, na.rm = TRUE),
    range_yr = paste0(min(yr, na.rm = TRUE), " - ", max(yr, na.rm = TRUE))
    )


# Distribution of counts by 5-min intervals ----
# Only
(interval <- FuncIntervalDistrib(func_dat = dat))
# > For Long Cave, it seems like on several nights the count ended in middle of emergence!! I think this argues for using some ecological criterion for when to stop counting. Weather or other environmental factors can affect the timing 

# PLot with day-of-year on x-axis, separate plot per cave <<<<<<<<<<<<< PICK UP FROM HERE
### QAQC analyses ----
# Regression classification with: cave, surveyor experience (prior experience of least experienced surveyor), surveyor name, method, median bat count



