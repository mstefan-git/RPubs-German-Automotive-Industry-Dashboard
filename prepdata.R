# Replication file for: "German Automotive Industry Dashboard"
# RPubs-link: https://rpubs.com/mstefan-rpubs/cars
# (c) Martin Stefan, July 2020

rm(list = ls())

library(readxl)
library(tidyverse)
'%!in%' <- function(x,y)!('%in%'(x,y))

# import excel files
df1 <- read_excel("data/raw_total.xlsx", sheet="FZ 2.3", range = "C8:V89", na="-",
                  col_types = c("text","skip", rep("numeric",18)))

df2 <- read_excel("data/raw_new.xlsx", sheet="FZ 4.3", range = "C8:V73", na="-",
                  col_types = c("text","skip", rep("numeric",18)))

# remove empty row
df1 <- df1[-1,]
df2 <- df2[-1,]

# simpler variable names
names(df1) <- c("make","BW","BY","BE","BB","HB","HH","HE","MV","NI","NW",
                "RP","SL","SN","ST","SH","TH","other","total")
names(df2) <- names(df1)

# remove country codes from brand names
df1$make <- gsub("\\s*\\([^\\)]+\\)","",df1$make)
df2$make <- gsub("\\s*\\([^\\)]+\\)","",df2$make)

# replace NA entries with zeros
df1[is.na(df1)] <- 0
df2[is.na(df2)] <- 0

# simplify brand names: total registrations
df1$make[df1$make == "AUTOMOB-EISENACH-AWE"] <- "AWE"
df1$make[df1$make == "DAIMLER"] <- "MERCEDES"
df1$make[df1$make == "MERCEDES-BENZ"] <- "MERCEDES"
df1$make[df1$make == "FCA"] <- "FCA (FIAT)"
df1$make[df1$make == "GENERAL MOT-GMC"] <- "GENERAL MOTORS"
df1$make[df1$make == "HONDA MOTOR"] <- "HONDA"
df1$make[df1$make == "HYUNDAI MOTOR"] <- "HYUNDAI"
df1$make[df1$make == "KIA MOTOR"] <- "KIA"
df1$make[df1$make == "KIA MOTORS"] <- "KIA"
df1$make[df1$make == "MAGYAR SUZUKI"] <- "SUZUKI"
df1$make[df1$make == "NISSAN EUROPE"] <- "NISSAN"
df1$make[df1$make == "PSA AUTOMOBILES"] <- "PSA"
df1$make[df1$make == "TOYOTA EUROPE"] <- "TOYOTA"
df1$make[df1$make == "SAAB,-SCANIA"] <- "SAAB-SCANIA"
df1$make[df1$make == "SUBARU-FUJI HEAVY"] <- "SUBARU"
df1$make[df1$make == "VAZ-LADA"] <- "LADA"
df1$make[df1$make == "VOLKSWAGEN"] <- "VW"
df1$make[df1$make == "SONSTIGE HERSTELLER"] <- "OTHER"

# simplify brand names: new registrations
df2$make[df2$make == "DAIMLER"] <- "MERCEDES"
df2$make[df2$make == "FCA"] <- "FCA (FIAT)"
df2$make[df2$make == "HONDA MOTOR"] <- "HONDA"
df2$make[df2$make == "HYUNDAI MOTOR"] <- "HYUNDAI"
df2$make[df2$make == "KIA MOTOR"] <- "KIA"
df2$make[df2$make == "KIA MOTORS"] <- "KIA"
df2$make[df2$make == "MAGYAR SUZUKI"] <- "SUZUKI"
df2$make[df2$make == "MAGYAR SUZUKI"] <- "SUZUKI"
df2$make[df2$make == "MAN TRUCK & BUS"] <- "MAN"
df2$make[df2$make == "PSA AUTOMOBILES"] <- "PSA"
df2$make[df2$make == "RENAULT TRUCKS"] <- "RENAULT"
df2$make[df2$make == "SUBARU-FUJI HEAVY"] <- "SUBARU"
df2$make[df2$make == "VAZ-LADA"] <- "LADA"
df2$make[df2$make == "TOYOTA EUROPE"] <- "TOYOTA"
df2$make[df2$make == "VOLKSWAGEN"] <- "VW"
df2$make[df2$make == "VOLKSWAGEN-VWOA"] <- "VW"
df2$make[df2$make == "SONSTIGE HERSTELLER"] <- "OTHER"

# aggregate over different subsidiaries of the same brand
df1 <- aggregate(df1 %>% select(-make), by=list(df1$make), FUN=sum)
df2 <- aggregate(df2 %>% select(-make), by=list(df2$make), FUN=sum)
names(df1)[1] <- "make"
names(df2)[1] <- "make"

# select brands of interest
brandnames <- c("VW","AUDI","BMW","MERCEDES","PORSCHE","OPEL",
                "CITROEN","PEUGEOT","RENAULT",
                "HONDA","TOYOTA","MAZDA","NISSAN","MITSUBISHI","SUZUKI","SUBARU",
                "FCA (FIAT)","FORD","SEAT","VOLVO","KIA","HYUNDAI","DACIA","LADA",
                "JAGUAR LAND ROVER","ASTON MARTIN","BENTLEY","FERRARI","MASERATI","TESLA")
df1 <- df1 %>% filter(make %in% brandnames)
df2 <- df2 %>% filter(make %in% brandnames)

# empty array
data <- array(0, dim=c(length(brandnames),ncol(df1)-1,2))
dimnames(data) <- list(
  brandnames, colnames(df1)[-1], c("total","new")
)

# fill array
for(b in brandnames){
  if(b %in% df1$make) data[b,,"total"] <- as.numeric(df1[df1$make == b, -1])
  if(b %in% df2$make) data[b,,"new"] <- as.numeric(df2[df2$make == b, -1])
}

# export data
saveRDS(data,"data/registrations.RDS")
