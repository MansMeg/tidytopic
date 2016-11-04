# Create State of the Union
library(dplyr)

# Presidents
uspresidents <- read.table("data-raw/presidents.csv", sep = ";", header = TRUE)
uspresidents$presidency_start <- lubridate::ymd(uspresidents$presidency_start)
uspresidents$presidency_end <- lubridate::ymd(uspresidents$presidency_end)
uspresidents$no <- 1:nrow(uspresidents)
uspresidents <- uspresidents[, c(7,1:6)]

devtools::use_data(uspresidents, overwrite = TRUE)

# State of the Union Adresses
sotu <- readLines("data-raw/state_of_the_union_1790_2009.txt")
sotu <- stringr::str_split(sotu, pattern = "\t")
sotu <- data_frame(V1 = unlist(lapply(sotu, function(x) x[1])),
                   V2 = unlist(lapply(sotu, function(x) x[2])),
                   V3 = unlist(lapply(sotu, function(x) x[3])))
sotu$V1 <- as.numeric(unlist(lapply(stringr::str_split(sotu$V1, "-"), function(x) x[2])))
sotu$doc <- 1:nrow(sotu)
sotu <- sotu[, c(4,2,1,3)] 
names(sotu) <- c("doc", "year", "paragraph", "text")
sotu$paragraph <- as.integer(sotu$paragraph)

sotuspeaker <- read.csv("data-raw/sotuspeaker.csv")
sotuspeaker$year <- as.integer(sotuspeaker$year)
sotu$year <- as.integer(sotu$year)
sotu <- left_join(sotu, sotuspeaker, by = "year")
sotu <- sotu[,c(5,2,3,1,4)]

devtools::use_data(sotu, overwrite = TRUE)

# SOTU50
sotu50 <- tidytopics::read_mallet_statefile("data-raw/sotu50.txt.gz")
sotu50$pos <- NULL

devtools::use_data(sotu50, overwrite = TRUE)


# Stats dataset
stats_txt <- readLines("data-raw/stats.txt")
stats_txt <- stringr::str_trim(stats_txt)
statse <- data_frame(text = stats_txt)

devtools::use_data(statse, overwrite = TRUE)
