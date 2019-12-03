library(tidyverse)
library(Package4)
# setup values
halls <- 1:8
doorSymbols <- c("Surf",
                 "Turf",
                 "Fin",
                 "Feather",
                 "Hook",
                 "Crook")

# functions
makeEncounter <- function(hall = NULL){
        # Room Encounter
        encounter <- dice(sides = 12)
        if(encounter <= 4){
                RoomEnc <- "Yes"
                RoomEncTyp <- sample(c(1:10),1)
        } else {
                RoomEnc <- "No"
                RoomEncTyp <- " "
        }
        # Hall Encounters
        if(hall == 1){
                encounter <- dice(sides = 6)
                if(encounter <= 4){
                        HallEnc <- "Yes"
                        HallEncType <- sample(c(1:10),1)
                } else {
                        HallEnc <- "No"
                        HallEncType <- " "
                }
        }
        if(hall == 2){
                encounter <- dice(sides = 12)
                if(encounter <= 4){
                        HallEnc <- "Yes"
                        HallEncType <- sample(c(1:10),1)
                } else {
                        HallEnc <- "No"
                        HallEncType <- " "
                }
        }
        if(hall == 3){
                encounter <- dice(sides = 12)
                if(encounter <= 6){
                        HallEnc <- "Yes"
                        HallEncType <- sample(c(1:10),1)
                } else {
                        HallEnc <- "No"
                        HallEncType <- " "
                }
        }
        if(hall == 4){
                encounter <- dice(sides = 12)
                if(encounter <= 3){
                        HallEnc <- "Yes"
                        HallEncType <- sample(c(1:10),1)
                } else {
                        HallEnc <- "No"
                        HallEncType <- " "
                }
        }
        if(hall == 5){
                encounter <- dice(sides = 12)
                if(encounter <= 5){
                        HallEnc <- "Yes"
                        HallEncType <- sample(c(1:10),1)
                } else {
                        HallEnc <- "No"
                        HallEncType <- " "
                }
        }
        if(hall == 6){
                HallEnc <- "No"
                HallEncType <- " "
        }
        if(hall == 7){
                encounter <- dice(sides = 12)
                if(encounter <= 4){
                        HallEnc <- "Yes"
                        HallEncType <- sample(c(1:10),1)
                } else {
                        HallEnc <- "No"
                        HallEncType <- " "
                }
        }
        if(hall == 8){
                encounter <- dice(sides = 12)
                if(encounter <= 2){
                        HallEnc <- "Yes"
                        HallEncType <- sample(c(1:10),1)
                } else {
                        HallEnc <- "No"
                        HallEncType <- " "
                }
        }
        values <- data.frame(
                hallEncounter = HallEnc,
                hallEncounterType = HallEncType,
                roomEncounter = RoomEnc,
                roomEncounterType = RoomEncTyp,
                stringsAsFactors = FALSE
        )
        return(values)
}
makeRoom <- function(){
     doors <- sample(doorSymbols, 6)
     outHall <- sample(halls,1)
     fountainPresent <- sample(c("Yes","No"),1)
     if(fountainPresent == "Yes"){
          fountainPosion <- sample(c("Yes","No"),1)
     } else {
          fountainPosion <- " "
     }
     encounter <- makeEncounter(hall = outHall)
     hold <- NULL
     hold <- data.frame(RoomName = " ",
                        OutHallway = outHall,
                        FountainPresent = fountainPresent,
                        FountainPosioned = fountainPosion,
                        Door1 = doors[1],
                        Door2 = doors[2],
                        Door3 = doors[3],
                        Door4 = doors[4],
                        Door5 = doors[5],
                        Door6 = doors[6],
                        RoomEncounter = encounter$roomEncounter,
                        RoomEncounterType = encounter$roomEncounterType,
                        HallwayEncounter = encounter$hallEncounter,
                        HallwayEncounterType = encounter$hallEncounterType,
                        stringsAsFactors = FALSE)
     return(hold)
}

# initial sequence for first 9
RoomName <- c("Initial",
              rep_len(" ", 8))
OutHallway <- c(1,2,3,4,5,6,7,8, sample(c(1:8), 1))
InitHalls <- NULL
fountainPresent <- c("No",
                     rep_len(c("No","Yes"), 8))
fountainPosion <- c(" ",
                    rep_len(c(" ","No"), 8))
RoomInitEncounters <- rep_len(" ",9)
RoomInitEncType <- rep_len(" ",9)
HallInitEncounters <- rep_len(" ",9)
HallInitEncType <- rep_len(" ",6)
for(i in 1:8){
        doors <- sample(doorSymbols, 6)
        hold <- NULL
        hold <- data.frame(RoomName = RoomName[i],
                           OutHallway = OutHallway[i],
                           FountainPresent = fountainPresent[i],
                           FountainPosioned = fountainPosion[i],
                           Door1 = doors[1],
                           Door2 = doors[2],
                           Door3 = doors[3],
                           Door4 = doors[4],
                           Door5 = doors[5],
                           Door6 = doors[6],
                           RoomEncounter = RoomInitEncounters[i],
                           RoomEncounterType = RoomInitEncType[i],
                           HallwayEncounter = HallInitEncounters[i],
                           HallwayEncounterType = HallInitEncType[i],
                           stringsAsFactors = FALSE)
        InitHalls <- rbind(InitHalls, hold)
}
rm(hold)
rm(RoomName)
rm(OutHallway)
rm(fountainPresent)
rm(fountainPosion)
rm(doors)
rm(HallInitEncounters)
rm(HallInitEncType)
rm(RoomInitEncounters)
rm(RoomInitEncType)

# rooms/halls after initial 9
roomList <- NULL
for(i in 1:31){
     currentRoom <- makeRoom()
     roomList <- rbind(roomList,currentRoom)
}
rm(currentRoom)
roomList$FountainPresent[1] <- "Yes"
roomList$FountainPosioned[1] <- " "
hallList <- rbind(InitHalls, roomList)
rm(InitHalls)
rm(roomList)
# set factors
hallList$Door1 <- factor(hallList$Door1, doorSymbols)
hallList$Door2 <- factor(hallList$Door2, doorSymbols)
hallList$Door3 <- factor(hallList$Door3, doorSymbols)
hallList$Door4 <- factor(hallList$Door4, doorSymbols)
hallList$Door5 <- factor(hallList$Door5, doorSymbols)
hallList$Door6 <- factor(hallList$Door6, doorSymbols)

# started programing

# Final Clean Up
rm(i)
rm(doorSymbols)
rm(halls)
rm(makeRoom)
rm(makeEncounter)
