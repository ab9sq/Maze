library(tidyverse)
source(file = "./Generation of setup values.R")
makeRoom <- function(){
     doors <- sample(doorSymbols, 6)
     outHall <- sample(halls,1)
     fountainPresent <- sample(c(TRUE,FALSE),1)
     if(fountainPresent){
          fountainPosion <- sample(c("Yes","No"),1)
     } else {
          fountainPosion <- " "
     }
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
                        stringsAsFactors = FALSE)
     return(hold)
     }
roomList <- NULL
for(i in 1:32){
     currentRoom <- makeRoom()
     roomList <- rbind(roomList,currentRoom)
}

startState <- NULL
RoomName <- c("Initial",
              rep_len(" ", 7))
OutHallway <- c(1,2,3,4,5,6,7,8)
InitHalls <- NULL
fountainPresent <- c("No",
                     rep_len(c("No","Yes"), 7))
fountainPosion <- c(" ",
                    rep_len(c(" ","No"), 7))

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
                        stringsAsFactors = FALSE)
     InitHalls <- rbind(InitHalls, hold)
}

hallList <- rbind(InitHalls, roomList)

roomList$FountainPresent[1] <- "Yes"
roomList$FountainPosioned[1] <- " "

hallList$door1 <- factor(hallList$door1, doorSymbols)
hallList$door2 <- factor(hallList$door2, doorSymbols)
hallList$door3 <- factor(hallList$door3, doorSymbols)
hallList$door4 <- factor(hallList$door4, doorSymbols)
hallList$door5 <- factor(hallList$door5, doorSymbols)
hallList$door6 <- factor(hallList$door6, doorSymbols)

