# ----------------------------------------------------------------------
# Information
# ----------------------------------------------------------------------

# Creates a raw behavioral data file that will be made publicly available,
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

# reading in the path and list files in the folder
# assumes that csv file with behavioral data for each participant 
# is placed there
dataBehPath <- paste0(dirRaw, "beh")
filenames <- list.files(path = dataBehPath)

# out file
filePathOut <- paste0(dirOut, "behData")


# ----------------------------------------------------------------------------
# Processing the data
# ----------------------------------------------------------------------------

# empty data frame for storing all individual files
behData <- data.frame()

# going one by one, adding to the big file
for(f in filenames[filenames %nin% invalid]) {
    # f <- filenames[1]; print(f)

    # reading in individual file
    subjectFile <- read.table(
        paste0(dataBehPath, "/", f), 
        header = TRUE,
        sep = ",", 
        row.names = NULL,
        stringsAsFactors = FALSE
    )  
    noArms <- 
        length(colnames(subjectFile)[grep("posArm", colnames(subjectFile))])

    # rearranging the data 
    orderedNames <- c(
        "subjectID", "age", "gender", "glasses", "lenses",
        "psychopyVers", "codeVersion",
        "expID", "expCond",  
        # game specs
        "noArms",
        paste0("idArm", 1:noArms),
        paste0("imgArm", 1:noArms),
        paste0("posArm", 1:noArms),
        paste0("valExpArm", 1:noArms),
        paste0("valSdArm", 1:noArms),
        paste0("inoSdArm", 1:noArms),
        paste0("decayArm", 1:noArms),
        "exchangeRate", "scalingFactor", "units", "seed", 
        # data from the task       
        "game", "gameType", "trial", "calibration",
        "ITI", "timeFixStim", "timeFixChoice",
        "choiceMade", "chosenArm", "chosenArmId", 
        "chosenArmPos", "choiceRT", 
        "IFI", "reward", "rewardScaled", "rewardExp", "rewardMaxExp", 
        "regret", "regretExp", "correct", "correctArm", "correctArmId",
        "chosenRankExp", "rewardTotal", "rewardTotalScaled",  "feedbackRT",
        "timeTrial", "instTime", "timeTotal"         
    )
    orderedFile <- subjectFile[orderedNames]

    # Adding subject file to the big file
    behData <- rbind(behData, orderedFile)
}


# ----------------------------------------------------------------------
# Saving
# ----------------------------------------------------------------------

write.csv(
    behData, 
    file = paste0(filePathOut, ".csv"), 
    row.names = FALSE
)