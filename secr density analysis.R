##Install and load packages CamtrapR, stringr and secr
#options(repos = c(CRAN = "http://cran.rstudio.com"))
#install.packages("Rcpp")

library(camtrapR)
library(secr)
library(stringr)
library(rgdal)

#1 Set your working directory
directory1<-"I:/Camera database SECR (backup 03-12-18)/USL 2017/2017"
setwd(directory1)
getwd()

ctfile<-list.files(pattern = "*trap_info*.csv")
ctfile

ct_dat<-read.csv(ctfile, header=T)
View(ct_dat)

#bring dates in the right format
ct_dat$Setup_date <- as.Date(ct_dat$Setup_date)
ct_dat$Retrieval_date <- as.Date(ct_dat$Retrieval_date)
View(ct_dat)

a<-regexpr(pattern="[0-9]", ctfile)
location<-str_sub(ctfile, start=1, end=a-2)
year<-str_sub(ctfile, start=a, end=a+3)


species <-  recordTable(inDir= getwd(),
            IDfrom = "metadata",
            cameraID = "filename",
            camerasIndependent = TRUE,
            exclude = c("Unidentified","Chukar","Snowcock","Tibetan Snowcock","Chukar Patridge","Chukar Partridge","SLK","Other Birds","Rodent","People", "empty","Livestock"),
            minDeltaTime = 60,
            deltaTimeComparedTo = "lastIndependentRecord",
            timeZone = "Asia/Kolkata",
            stationCol = "Station",
            writecsv = FALSE,
            outDir = getwd(),
            metadataHierarchyDelimitor = "|",
            metadataSpeciesTag = "Species",
            removeDuplicateRecords = TRUE)

write.csv(species, paste("species", location, year, "csv", sep="."))

CamMatrix<-cameraOperation(CTtable = ct_dat, stationCol = "Station", setupCol = "Setup_date", retrievalCol = "Retrieval_date", 
                           byCamera = FALSE, allCamsOn = TRUE, dateFormat = "%Y-%m-%d", hasProblems = F, writecsv = FALSE, 
                           outDir = getwd())

getSpeciesImages(species = "Snow leopard",
                 recordTable= species,
                 speciesCol = "Species",
                 stationCol = "Station",
                 outDir= paste(dirname(getwd()), "Snow leopard", sep="/"),
                 createStationSubfolders = FALSE,
                 IDfrom = "Species",
                 metadataSpeciesTag = "Snow leopard",
                 metadataHierarchyDelimitor = "|")

directory2<-paste(dirname(getwd()), "Snow leopard", sep = "/")
setwd(directory2)          

individuals <- recordTableIndividual(inDir = getwd(),
                                      minDeltaTime = 60, deltaTimeComparedTo = "lastIndependentRecord", 
                                      hasStationFolders = FALSE, IDfrom = "metadata", writecsv = FALSE, 
                                      outDir = getwd(), 
                                      metadataHierarchyDelimitor = "|", metadataIDTag = "Individuals", 
                                      timeZone = "Asia/Kolkata", removeDuplicateRecords =  TRUE)

#remove unidentified individuals
individuals<-individuals[!(individuals$Individual %in% c("unidentified", "Unidentified" )), ]
View(individuals)

setwd(directory1)
write.csv(individuals, paste("individuals", location, year, "csv", sep="."))

input_secr<-spatialDetectionHistory(recordTableIndividual =  individuals, species = "Snow leopard", camOp = CamMatrix, CTtable = ct_dat, output = "binary",
                                    stationCol = "Station", speciesCol = "Species", Xcol = "utm_x", Ycol = "utm_y", individualCol = "Individual", 
                                    recordDateTimeCol = "DateTimeOriginal", recordDateTimeFormat = "%Y-%m-%d %H:%M:%S", occasionLength = 1, occasionStartTime = 12, 
                                    day1 = "survey", includeEffort = TRUE, binaryEffort = TRUE, timeZone = "Asia/Bishkek")

summary(input_secr, terse = TRUE)

mask <- make.mask(traps(input_secr), buffer = 28000, type = 'trapbuffer')
fit<-secr.fit(input_secr, mask = mask, model = g0~1, start = list(D=.00002, g0= 0.02, sigma= 25000), trace = FALSE)
#esa.plot(fit, ylim=c(0,0.006), xlim=c(0,4000))
predict(fit)
print(fit)

##########################################################################################
##########################################################################################
getwd()
poly <- readOGR(dsn= "C:/Users/user1/Documents/USL 2017/2017", layer="HP_3200_5200UTM")
plot(poly)


# Create habitat Mask
habitat_mask <- make.mask(traps(input_secr), buffer = 28000, type = "trapbuffer", poly = poly, poly.habitat = FALSE)
#plot(habitat_mask, ppoly = FALSE, col = "grey", pch = 16, cex = 0.6, add = T)
#plot(traps(input_secr), add = T)

# SECR Analysis with habitat mask 
fit2<-secr.fit(input_secr, mask = habitat_mask, model = g0~1, trace = FALSE)
predict(fit2)
print(fit2)
