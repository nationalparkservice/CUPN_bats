### Bats Analysis

### NOTES ----
# > Looks like for caves with multiple entrances, Tom only used data where a method was applied across all the entrances (for a survey night)--it was the sum of entrance counts for that particular method [ACTUALLY I'M NOT SURE HE EXCLUDED METHODS THAT WERE MISSING FROM SOME ENTRANCES--I SHOULD CHECK ON THAT]
# > UPSHOT: These are the changes I would make to Tom's analysis
#   - Unless we know how he dealt with incomplete surveys, only use complete (75-min) surveys. Until we know how he calculated the final numbers, use my event counts with my details on how they were calculated
#   - ?? Figure out how different nights were analyzed within a year, in Tom's analyses
#   - Bootstrap CI's instead of assuming normal distribution? But maybe not--because would bootstrapping account for possibility of false detection?
#   - Don't analyze data with very small counts (avg N < 5)
#   - I'm not sure how Tom dealt with the same method but different observers--I think he called them different independent counts. I assume those were only available for single-entrance caves, so that should be fine (no questions about how data were combined across cave entrances then)
#   - Calculate fit stats (modify from JELA function) and evaluate potential weather covariates for the nightly surveys (plot relationships to see what might make sense)
# > TRY AN ALTERNATIVE ANALYSIS AND COMPARE RESULTS:
#   - Temporary emigration model with multiple observers (method = observers) and year as the "site"
#   - For multiple-entrance caves, would a metapopulation model work?
# > COMPARE TO BASIC GLMM
#   - predictors include year, method and night (within year). Compare to a model with predictors of just year and night within year. Compare to models that are separate for each method.

### QUESTIONS ----
# > I don't understand how Tom calculated some of the numbers, though. FOr example, Blue_Spring_Hollow_Sandstone_Shelter night vision camera 6/13/2016 he had 1 bat
# > I don't think it was appropriate for Tom to calculate confidence interval (of estimated abundance) as he did--his lower CI was turning out lower than the lowest reported count. I think it should be a bootstrap CI
# Analysis 1 MACA Anarchy Shelter estimates--reinforces that we need to decide what to "do" about counts of bats entering, and cases where night vision goggles pick up a lot more entering bats than the night vision cameras do

### NAMING RULES for scripts ----
# Lists, data frames, vectors, variables...: list_StationFiles, df_StationInfo, vec_StationNames, var_Station
# Data frame cols: df_StationInfo$ColThisOne, df_StationInfo$ColThatOne
# List elements: list_Stations$ElementOne, list$ElementTwo
# Functions: FuncTest(dat1_there, dat2_here)

### Load packages ----
rm(list=ls())
pkgs <- c("readr", "tidyverse", "here", "magrittr", "unmarked", "MuMIn", "data.table", "lubridate")
lapply(pkgs, library, character.only = TRUE, quietly = TRUE)

### Modified from Tom ----
# # Read in data
# # Original_data <- read_csv(here::here("Data_in", "Original_data.csv"))
# temp_dat <- read_csv(here::here("Data_in", "All_Counts_and_methods.csv"), col_names = FALSE)
# names(temp_dat) <- c("Name", "Date", "Day", "Year", "C1","C2","C3","C4","C5","C6","M1","M2","M3","M4","M5","M6") # Cols starting with "C" gives the counts for the corresponding methods (cols starting with "M")
# temp_dat$Day <- NULL
# temp_dat$Date <- as.Date(temp_dat$Date, "%m/%d/%Y")
# counts <- subset(temp_dat, select = Name:C6) %>%
#   gather(CountCol, Count, -Name, -Date, -Year) %>%
#   dplyr::arrange(Name, Date, CountCol)
# methods <- subset(temp_dat, select = M1:M6) %>%
#   gather(MethodCol, Method) %>%
#   filter(complete.cases(.)) %>%
#   distinct() %>%
#   dplyr::arrange(MethodCol)
# long_dat <- data.frame(counts, Method = methods$Method)
# long_dat$CountCol <- NULL
# long_dat %<>% dplyr::filter(complete.cases(.))



### Ellen's version with subset of data ----

# Read in and subset the data ----
dat_EntranceTots <- readRDS(here::here("Data_out", "dat_EntranceTots.RDS"))
df_MultiEntrance <- readRDS(here::here("Data_out", "df_MultiEntrance.RDS"))

# Limit to caves that average at least 5 bats per survey and have only a single entrance
sub_caves <- dat_EntranceTots %>%
  dplyr::filter(!Cave %in% df_MultiEntrance$Cave) %>% # only analyze single-entrance caves
  dplyr::group_by(Cave, Entrance) %>%
  dplyr::summarize(MeanTotCount = mean(TotalEventCount, na.rm = TRUE)) %>%
  dplyr::filter(MeanTotCount >=5) %>%
  dplyr::distinct(Cave)

# ...and only keep data for full surveys (75 min), total event count >=0
subdat <- dat_EntranceTots %>%
  dplyr::filter(
    Cave %in% sub_caves$Cave,
    TotalDuration == 75,
    TotalEventCount >=0) %>%
  droplevels()
saveRDS(subdat, here::here("Data_out", "subdat.RDS"))

# ...initially, limit trend analysis to surveys conducted within a similar span of Yday. This means omitting some 2016 data for MACA caves Luna (limit to Yday 154 - 161), Historic (limit to 187 - 200, there wasn't a good 2016 yday match), and Long (187 - 200) --but it would be useful to also analyze with all the data and compare results
df_RangeYday <- subdat %>% 
  dplyr::group_by(Park, Cave, Entrance) %>% 
  dplyr::summarize(
    RangeYday = paste0(min(Yday, na.rm = TRUE), " - ", max(Yday, na.rm = TRUE)),
    DiffYday = max(Yday, na.rm = TRUE) - min(Yday, na.rm = TRUE) +1,
    RangeYr = paste0(min(Yr, na.rm = TRUE), " - ", max(Yr, na.rm = TRUE))
  )

subdat_lim <- subdat %>%
 dplyr::filter(
   !(Cave == "Luna Cave" & (Yday < 154 | Yday > 161)),
   !(Cave == "Long Cave" & (Yday < 187 | Yday > 200)),
   !(Cave == "Historic Entrance" & (Yday < 187 | Yday > 200))
   ) %>%
  droplevels() 
    
# Run the limited analysis data through summary functions
(out <- FuncPlotCaves(func_dat = subdat_lim, color_by = "Method"))

final_dat <- subdat_lim %>%
  dplyr::ungroup() %>%
  dplyr::mutate(ParkCave = paste(Park, Cave, sep = "_")) %>%
  dplyr::select(ParkCave, Yr, Yday, EventDate, Method, TotalEventCount)
saveRDS(final_dat, here::here("Data_out", "final_dat.RDS"))

# Analysis 1 is N-mixture, this one uses PCount (like Ingersoll's) but modified ----
pcount_summary <- pcount_list <- list()
for(i in sort(unique(final_dat$ParkCave))) {
  cat(i)
  sub_dat_cave <- final_dat %>%
    dplyr::filter(ParkCave == i) 
  
  template_sessID <- sub_dat_cave %>%
    dplyr::select(Yr, Yday) %>%
    dplyr::distinct() %>%
    dplyr::arrange(Yr, Yday) %>%
    dplyr::group_by(Yr) %>%
    dplyr::mutate(Sess2 = row_number()) 
  
  sub_dat_cave %<>%
    dplyr::left_join(template_sessID, by = c("Yr", "Yday")) %>%
    dplyr::mutate(
      YrIndex = Yr - min(Yr)) %>%
    dplyr::select(-Yr, Sess2, Method)
    
  for(j in sort(unique(sub_dat_cave$YrIndex))) { # temporary 
    sub_dat <- sub_dat_cave %>%
      dplyr::filter(YrIndex == j) 
    
    if(sum(sub_dat$TotalEventCount)> 0) {
      
      UM_dat <- sub_dat %>%
        dplyr::mutate(UM_cols = paste0(Sess2, "//", Method)) %>%
        # dplyr::select(YrIndex, UM_cols, TotalEventCount) %>%
        dplyr::select(UM_cols, TotalEventCount) %>%
        tidyr::spread(key=UM_cols, value=TotalEventCount)
      
      bats_umf <- unmarkedFramePCount(
        y=matrix(as.numeric(UM_dat), nrow = 1),
        siteCovs=data.frame(site = i),
        obsCovs=list(
          # yr = matrix(as.character(sub_dat$YrIndex), nrow = 1),
          yday = matrix(as.character(sub_dat$Sess2), nrow = 1),
          method = matrix(sub_dat$Method, nrow = 1)
        )
      )
      
      
      # summary(bats_umf)
      
      # # Fit a model -- multiple years
      # if(length(unique(na.omit(sub_dat$YrIndex)))>1 & length(unique(na.omit(sub_dat$Sess2)))>1 & length(unique(na.omit(sub_dat$Method)))>1) {
      #   bats_est <- pcount(~ method + yday ~ yr, bats_umf, K=10000, mixture="P", method="BFGS", se=TRUE, control=list(maxit=1000, trace=TRUE, REPORT=1))
      # } 
      # 
      # # If only one date level
      # if(length(unique(na.omit(sub_dat$YrIndex)))>1 & length(unique(na.omit(sub_dat$Sess2)))==1 & length(unique(na.omit(sub_dat$Method)))>1) { 
      #   bats_est <- pcount(~ method ~ yr, bats_umf, K=10000, mixture="P", method="BFGS", se=TRUE, control=list(maxit=1000, trace=TRUE, REPORT=1))
      # } 
      # 
      # # If only one method level
      # if(length(unique(na.omit(sub_dat$YrIndex)))>1 & length(unique(na.omit(sub_dat$Sess2)))>1 & length(unique(na.omit(sub_dat$Method)))==1) {
      #   bats_est <- pcount(~ yday ~ yr, bats_umf, K=10000, mixture="P", method="BFGS", se=TRUE, control=list(maxit=1000, trace=TRUE, REPORT=1))
      # } 
      # 
      # # If only one date level and only one method level
      # if(length(unique(na.omit(sub_dat$YrIndex)))>1 & length(unique(na.omit(sub_dat$Sess2)))==1 & length(unique(na.omit(sub_dat$Method)))==1) {
      #   bats_est <- pcount(~ 1 ~ yr, bats_umf, K=10000, mixture="P", method="BFGS", se=TRUE, control=list(maxit=1000, trace=TRUE, REPORT=1))
      # } 
      # 
      # Fit  a model -- only one year
      if(length(unique(na.omit(sub_dat$YrIndex)))==1 & length(unique(na.omit(sub_dat$Sess2)))>1 & length(unique(na.omit(sub_dat$Method)))>1) {
        bats_est <- pcount(~ method + yday ~ 1, bats_umf, K=10000, mixture="P", method="BFGS", se=TRUE, control=list(maxit=1000, trace=TRUE, REPORT=1))
      } 
      
      # If only one date level
      if(length(unique(na.omit(sub_dat$YrIndex)))==1 & length(unique(na.omit(sub_dat$Sess2)))==1 & length(unique(na.omit(sub_dat$Method)))>1) { 
        bats_est <- pcount(~ method ~ 1, bats_umf, K=10000, mixture="P", method="BFGS", se=TRUE, control=list(maxit=1000, trace=TRUE, REPORT=1))
      } 
      
      # If only one method level
      if(length(unique(na.omit(sub_dat$YrIndex)))==1 & length(unique(na.omit(sub_dat$Sess2)))>1 & length(unique(na.omit(sub_dat$Method)))==1) {
        bats_est <- pcount(~ yday ~ 1, bats_umf, K=10000, mixture="P", method="BFGS", se=TRUE, control=list(maxit=1000, trace=TRUE, REPORT=1))
      } 
      
      # If only one date level and only one method level
      if(length(unique(na.omit(sub_dat$YrIndex)))==1 & length(unique(na.omit(sub_dat$Sess2)))==1 & length(unique(na.omit(sub_dat$Method)))==1) {
        bats_est <- pcount(~ 1 ~ 1, bats_umf, K=10000, mixture="P", method="BFGS", se=TRUE, control=list(maxit=1000, trace=TRUE, REPORT=1))
      } 
      
      # plogis(coef(bats_est, type="det")) # Should be close to p
      
      # Empirical Bayes estimation of random effects
      # (bats_est_re <- ranef(bats_est))
      # plot(bats_est_re)
      # sum(bup(bats_est_re))         # Estimated population size
      pcount_list[[paste(i, j, sep = "_")]] <- c("Site" = i, "Est_N" = round(exp(coef(bats_est))[1], 2), "CI_2.5" = round(exp(coef(bats_est)[1]-2*SE(bats_est)[1]), 2), "CI_97.5" = round(exp(coef(bats_est)[1]+2*SE(bats_est)[1]), 2), "ActualCounts" = paste(sort(as.numeric(UM_dat)), collapse = " "))
    } else {
      pcount_summary[[paste(i, j, sep = "_")]] <- bats_est
      pcount_list[[paste(i, j, sep = "_")]] <- c("Site" = i, "Year" = j, "Est_N" = NA, "CI_2.5" = NA, "CI_97.5" = NA, "ActualCounts" = "All zero")
    }
  }
}

pcount_est <- data.frame(do.call("rbind", pcount_list))
saveRDS(pcount_est, "bat_pcount_est.RDS")
saveRDS(pcount_summary, "bat_pcount_summaries.RDS")


# Analyze final_dat. Analyze each cave separately. Detection can vary by Yday and Method. Abundance can vary by Yr. Not all caves have >1 year of data.

# FuncCreateUnmarkedPCO <- function(func_dat) {
#   # Function to create the 'unmarked' data
#   #
#   # Args:
#   #   func_dat:  The event-count data frame for single-entrance caves, with columns for ParkCave, TotalEventCount, Method, Yr, and EventDate
#   #
#   # Returns:
#   #   As a list of formatted 'unmarked' data: um_y, um_siteCovs, um_ObsCovs, um_yearlySiteCovs
# 
#   # create index column for secondary samples
#   index2 <- func_dat %>%
#     dplyr::select(ParkCave, Yr, Yday) %>%
#     dplyr::arrange(ParkCave, Yr, Yday) %>%
#     dplyr::distinct() %>%
#     dplyr::group_by(ParkCave, Yr) %>%
#     mutate(Index2 = row_number())
# 
#   indexed_dat <- func_dat %>%
#     dplyr::left_join(index2, by = c("ParkCave", "Yr", "Yday")) %>%
#     dplyr::mutate(Yr = Yr - min(Yr)) %>%
#     dplyr::select(-ParkCave, -Yday)
# 
#   unique_method <- sort(unique(func_dat$Method))
#   unique_yrs <- sort(unique(func_dat$Yr))
#   max_rep <- max(indexed_dat$Index2, na.rm = TRUE) # maximum number of survey dates in a year
# 
#   # Create MxJT matrix of the repeated count data, where M is the number of sites, J is the maximum number of secondary sampling periods per site and T is the maximum number of primary sampling periods per site
#   um_y <- indexed_dat %>%
#     ungroup() %>%
#     tidyr::complete(Yr, Index2, Method) %>%
#     dplyr::arrange(Yr, Index2, Method) %>%
#     dplyr::mutate(Index2_Method = paste(Index2, Method, sep = "_")) %>%
#     dplyr::select(-Index2, -Method) %>%
#     tidyr::spread(key = Index2_Method, value = TotalEventCount)
#  
#   um_Y <- data.table(Method = sort(unique(dat$Method)))
#   
#   setkey(dat, Yr)
#   for (i in unique(dat$Yr)) {
#     dat_i <- dat[.(i)]  # just the data for the specified Yr
#     surveys_blank <- max_rep - length(unique(dat_i$EventDate))
#     setkey(dat_i, EventDate, Method)  # sort by date and Method
#     
#     dt_i2 <- dcast.data.table(dat_i, Method ~ EventDate, value.var="TotalEventCount")
#     
#     setkey(dt_i2, NULL)
#     if (surveys_blank > 0) { # add NA-columns as necessary for each monitoring year
#       df_YrDay <- cbind(df_YrDay, t(replicate(num_method, rep(NA, surveys_blank))))
#       dt_i2[, as.character(paste(i, "x", 1:surveys_blank, sep="")) := NA] 
#     }
#     setkey(dt_i2, Method)
#     um_Y <- merge(um_Y, dt_i2, by="Method") # add to the master 'y'
#     print(dim(um_Y))
#     print(dim(df_YrDay))
#   }
#   um_Y[,Method := NULL]  # delete the first column
#   rownames(um_Y) <- 1:num_method
#   df_YrDay <- df_YrDay[-1] # delete the first column--this one is a dataframe
#   colnames(um_Y) <- rep(as.character(unique(dat$Yr)), each=max_rep) # general column names
#   colnames(df_YrDay) <- rep(as.character(unique(dat$Yr)), each=max_rep)
#   
#   um_ObsCovs <- list("yr_eventdate"=df_YrDay)
#   
#   # create the unmarked 'siteCovs' data with cov=Method
#   dat <- data.frame(dat)
#   um.siteCovs <- unique(dat["Method"])
#   um.siteCovs <- um.siteCovs[order(um.siteCovs$Method),]
#   um.siteCovs <- droplevels(um.siteCovs)
#   
#   # create the unmarked 'yearlySiteCovs' data with mean monthly precip
#   yr.dat <- data.frame(yr.dat)
#   yr.vec <- yr.dat$mean.precip[yr.dat$Yr %in% unique(dat$Yr)]
#   if(num_yrs > length(yr.vec)) yr.vec <- c(yr.vec, NA)  # need to add NA for 2016 precipitation data
#   p.mat = matrix(rep(yr.vec, num_method), nrow=num_method, ncol=length(yr.vec), byrow=TRUE)
#   um.yearlySiteCovs <- list("rain"=p.mat)
#   
#   return.list <- list("um_Y"=um_Y, "um.siteCovs"=um.siteCovs, "um_ObsCovs"=um_ObsCovs, "um.yearlySiteCovs"=um.yearlySiteCovs)
#   return(return.list)
# }
# 
# 
# test <- csvToUMF(file = here::here("Data_out", "final_dat.csv"),  long = TRUE, type = "unmarkedFramePCO", numPrimary = 3)


# Analysis 2, this one accounts for temporary emigration and that affects nightly abundance (instead of having detection differ by night) ---- 
# >>>>>>>>>>>>>>>>>> PICK UP FROM HERE

# Analysis 3 is GLMM
Citizen science reveals trends in bat populations: The National Bat Monitoring Programme in Great Britain++-