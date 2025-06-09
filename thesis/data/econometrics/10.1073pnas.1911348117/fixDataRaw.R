# ----------------------------------------------------------------------
# Information
# ----------------------------------------------------------------------

# Creates a raw fixation event data file that would be publicly available,
# with minimal processing.

# Author: Hrvoje Stojic
# Last updated: 2019-11-22


# ----------------------------------------------------------------------
# Loading libraries and functions
# ----------------------------------------------------------------------

# specify your path here if you want to use this script interactively, 
# and uncomment the line
# setwd("/home/hstojic/Research/project/gas_MABeye/dRaw")

# housekeeping 
rm(list = ls())

# directory paths
dirRaw <- '/media/hstojic/dataneuro/MABeye/exp/'
dirOut <- "../dRaw/"

# loading packages 
source("utils.R")

# reading in the HDF path and specific file 
dataset1 <- paste0(dirRaw, "dataTracking_201804.hdf5")
dataset2 <- paste0(dirRaw, "dataTracking_201806.hdf5")
# h5ls(dataset1)
# h5ls(dataset2)

# loading the behavioral data
behData <- read_csv(paste0(dirOut, "behData.csv"), guess_max = 10000)

# filenames
filePathOut <- paste0(dirOut, "fixData")


# ----------------------------------------------------------------------------
# Processing the data
# ----------------------------------------------------------------------------

# a helper function, since we have multiple eye tracking datasets
getFixData <- function(hdf5file, valid) {
    
    # load the data
    fixEvents <- h5read(
        hdf5file, 
        "/data_collection/events/eyetracker/FixationEndEvent"
    )
    messageEvents <- h5read(
        hdf5file, 
        "/data_collection/events/experiment/MessageEvent"
    )
    
    # extract session_id and fixation time
    # careful, we want fixation start time to be logged as events
    # it creates some additional duplicates?   
    timeFixations <- fixEvents %>% 
        mutate(timestart = time - duration) %>%
        select(session_id, timestart) %>% 
        mutate(text = NA)

    # bind fixation time to eventdata and order by session_id and time
    messageEvents <- messageEvents %>% 
        select(session_id, timestart = time, text) %>%
        bind_rows(., timeFixations) %>% 
        arrange(session_id, timestart)

    # merge two data sets
    mergedEvents <- fixEvents %>% 
        mutate(timestart = time - duration) %>%
        full_join(messageEvents, by = c("session_id", "timestart")) %>% 
        arrange(session_id, timestart)

    # replace NAs with lastest non-NA value
    mergedEvents$text <- na.locf(mergedEvents$text)

    # extract subjectID variable, keep valid subjects
    fixData <- mergedEvents %>% 
        mutate(subjectID = sapply(strsplit(text, split = "_"), "[", 1)) %>% 
        filter(subjectID %in% valid) 

    # keep a selected range of data 
    fixData <- select(fixData,
        subjectID, 
        time, 
        timestart,
        duration, 
        text, 
        eye, 
        gazex = average_gaze_x, 
        gazey = average_gaze_y, 
        pupil = average_pupil_measure1
    )
    return(fixData)
}

# getting data for both datasets and combining them
fixData1 <- getFixData(dataset1, unique(behData$subjectID))
fixData2 <- getFixData(dataset2, unique(behData$subjectID))
fixData <- rbind(fixData1, fixData2)

# closing the connections
H5close()


# ----------------------------------------------------------------------
# Saving
# ----------------------------------------------------------------------

write.csv(
    fixData, 
    file = paste0(filePathOut, ".csv"), 
    row.names = FALSE
)