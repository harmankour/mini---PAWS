library(camtrapR)

setwd("C:/Users/user1/Documents/USL 2017/2017")
ctfile<-list.files(pattern = "*trap_info*.csv")
ctfile

ct_dat<-read.csv(ctfile, header=T)

fixDateTimeOriginal(inDir = getwd(), recursive = TRUE) 

imageRename(inDir = getwd(),
            outDir = dirname(getwd()),
            hasCameraFolders = FALSE, keepCameraSubfolders = FALSE, copyImages = TRUE, writecsv = FALSE)

