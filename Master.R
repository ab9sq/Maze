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
                        door6 = doors[6])
     return(hold)
     }
roomList <- NULL
for(i in 1:1000){
     currentRoom <- makeRoom()
     roomList <- rbind(roomList,currentRoom)
}
