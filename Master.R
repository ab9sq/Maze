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
        encounter <- dice(sides = 12)


        values <- data.frame(
                hallEncounter = " ",
                hallEncounterType = " ",
                roomEncounter = " ",
                roomEncounterType = " ",
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
     hold <- data.frame(roomName = " ",
                        outHallway = outHall,
                        FountainPresent = fountainPresent,
                        FountainPosioned = fountainPosion,
                        door1 = doors[1],
                        door2 = doors[2],
                        door3 = doors[3],
                        door4 = doors[4],
                        door5 = doors[5],
                        door6 = doors[6],
                        RoomEncounter = encounter$roomEncounter,
                        RoomEncounterType = encounter$roomEncounterType,
                        HallwayEncounter = encounter$hallEncounter,
                        HallwayEncounterType = encounter$roomEncounterType,
                        stringsAsFactors = FALSE)
     return(hold)
}

# initial sequence
RoomName <- c("Initial",
              rep_len(" ", 7))
OutHallway <- c(1,2,3,4,5,6,7,8)
InitHalls <- NULL
fountainPresent <- c("No",
                     rep_len(c("No","Yes"), 7))
fountainPosion <- c(" ",
                    rep_len(c(" ","No"), 7))
InitEncounters <- rep_len(" ",7)
InitEncType <- rep_len(" ",7)
for(i in 1:8){
        doors <- sample(doorSymbols, 6)
        hold <- NULL
        hold <- data.frame(roomName = RoomName[i],
                           outHallway = OutHallway[i],
                           FountainPresent = fountainPresent[i],
                           FountainPosioned = fountainPosion[i],
                           door1 = doors[1],
                           door2 = doors[2],
                           door3 = doors[3],
                           door4 = doors[4],
                           door5 = doors[5],
                           door6 = doors[6],
                           RoomEncounter = InitEncounters[i],
                           RoomEncounterType = InitEncType[i],
                           HallwayEncounter = InitEncounters[i],
                           HallwayEncounterType = InitEncType[i],
                           stringsAsFactors = FALSE)
        InitHalls <- rbind(InitHalls, hold)
}
rm(hold)
rm(RoomName)
rm(OutHallway)
rm(fountainPresent)
rm(fountainPosion)
rm(doors)
rm(InitEncounters)
rm(InitEncType)

# rooms/halls after initial 8
roomList <- NULL
for(i in 1:32){
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
hallList$door1 <- factor(hallList$door1, doorSymbols)
hallList$door2 <- factor(hallList$door2, doorSymbols)
hallList$door3 <- factor(hallList$door3, doorSymbols)
hallList$door4 <- factor(hallList$door4, doorSymbols)
hallList$door5 <- factor(hallList$door5, doorSymbols)
hallList$door6 <- factor(hallList$door6, doorSymbols)

# started programing

# Final Clean Up
rm(i)
rm(doorSymbols)
rm(halls)
rm(makeRoom)
rm(makeEncounter)
