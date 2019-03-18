# Specify necesssary urls from which the data will be extracted
urlref <- "https://www.extonpokemap.com/"
urlreward <- "https://www.extonpokemap.com/static/data/en.json"
urldata <- "https://www.extonpokemap.com/api/get_data"
library(httr)
library(jsonlite)

# Definitions for the while loop
statuscodes <- 0
i <- 0
questdata <- data.frame()
getcookie <- ""

while(statuscodes != 200 && i <= 1){
  i = i+1
  
  # Reset the Curl handle
  detach("package:httr", unload=TRUE)
  library(httr)
  h <- handle('')
  
  # Before we can extract the data from the website we need to perform a GET request 
  # in order to get the necessary cookie
  try(getcookie <- GET(urlref,handle=h,config(ssl_verifypeer = 0L)),silent=TRUE)
  
  if (getcookie != "" && status_code(getcookie) == 200) {
    # Save the cookie for late website requests
    getcookie <- getcookie$headers[names(getcookie$headers) %in% "set-cookie"]
    sessiontoken <- unname(unlist(strsplit(unlist(getcookie)[1],";"))[1])
    csrftoken <- unname(unlist(strsplit(unlist(getcookie)[2],";"))[1])
    
    # Specify the header for the POST request in order to get the quest data
    ua <- "Mozilla/5.0 (Windows NT 6.1; Win64; x64; rv:63.0) Gecko/20100101 Firefox/63.0"
    hd <- c("Accept" = "*/*",
            "Accept-Language" = "de,en-US;q=0.7,en;q=0.3",
            "Referer" = "https://www.extonpokemap.com/",
            "Content-Type" = "application/x-www-form-urlencoded; charset=UTF-8",
            "X-Requested-With" = "XMLHttpRequest",
            "Connection" = "Close",
            "Cookie" = paste(sessiontoken,csrftoken,sep="; "))
    
    # Specify the body request for the POST request in order to get the quest data
    bdy <- list ("_"= round(unclass(Sys.time())),
                 "min_lat" = "38.59540719940386", 
                 "max_lat"= "41.14970617453726", 
                 "min_lon" = "-78.739013671875",
                 "max_lon" = "-71.23535156250001",
                 "show_gyms" = "false",
                 "show_raids" = "false",
                 "show_pokestops" = "false",
                 "show_quests" = "true",
                 "show_pokemon"= "false",
                 "pokemon_filter_exclude"= "[]",
                 "quest_filter_exclude" = "[]",
                 "pokemon_filter_iv" = "{}",
                 "show_spawnpoints" = "false",
                 "show_cells" = "false",
                 "last_update" = "0",
                 "_csrf" = unlist(strsplit(csrftoken,"="))[2])
    bdy <- paste(names(bdy),bdy,sep="=",collapse="&")
    
    # Perform the POST request in order to get the quest data
    try({questdatacontent <- RETRY("POST",urldata,body=bdy,add_headers(.headers=hd),config(ssl_verifypeer = 0L),
                                   user_agent(ua),handle=h,times=5)
         statuscodes <- status_code(questdatacontent)},silent=TRUE)
    
    if (statuscodes == 200) {
      questdata <- jsonlite::fromJSON(content(questdatacontent, as="text",encoding = "UTF-8"))
      questdata <- data.frame(questdata$data$pokestops)
      
      #Use only new questdata 
      if (input$location %in% "Philadelphia") {
        resettime <- as.numeric(trunc(as.POSIXlt(Sys.time(), tz="America/New_York"),"days"))
      } else {
        resettime <- 0L
      }
      questdata <- questdata[questdata$quest_timestamp > resettime,]
      
      # Download the reward and quest condition in order to decrypt them in questdata
      try({
        questdef <- data.frame(jsonlite::fromJSON(content(GET(urlreward,config(ssl_verifypeer = 0L)),type="text",encoding="UTF-8")))
        names(questdef) <- sapply(strsplit(names(questdef),split="values.",fixed=TRUE), function(x) (x[2]))
        
        # Function to decrypt the quest rewards
        getReward <- function(item){
          id <- item$type
          info <- item$info
          
          if (id == 1 && !is.null(info) && !is.null(info$amount)) {
            strvalue <- as.character(questdef$quest_reward_1_formatted)
            strvalue <- sub("%\\{amount\\}",info$amount,strvalue)
          } else if (id == 2 && !is.null(info) && !is.null(info$amount) && !is.null(info$item_id)) {
            strvalue <- as.character(questdef$quest_reward_2_formatted)
            strvalue <- sub("%\\{amount\\}",info$amount,strvalue)
            strvalue <- sub("%\\{item\\}",questdef[[paste0("item_",info$item_id)]],strvalue)
          } else if (id == 3 && !is.null(info) && !is.null(info$amount)) {
            strvalue <- as.character(questdef$quest_reward_3_formatted)
            strvalue <- sub("%\\{amount\\} ",info$amount,strvalue)
          } else if (id == 4 && !is.null(info) && !is.null(info$amount) && !is.null(info$pokemon_id)) {
            strvalue <- as.character(questdef$quest_reward_4_formatted)
            strvalue <- sub("%\\{amount\\}",info$amount,strvalue)
            strvalue <- sub("%\\{type\\}",questdef[[paste0("poke_",info$pokemon_id)]],strvalue)
          } else if (id == 7 && !is.null(info) && !is.null(info$pokemon_id)) {
            strvalue <- as.character(questdef$quest_reward_7_formatted)
            strvalue <- sub("%\\{pokemon\\}",questdef[[paste0("poke_",info$pokemon_id)]],strvalue)
          } else {
            strvalue <- as.character(questdef[[paste0("quest_reward_",id)]])
          }
          return(as.character(strvalue))
        }
        
        # Decrypt the quest rewards
        questdata$reward <- sapply(questdata$quest_rewards,getReward)
        
        # Functions to decrypt the quest conditions
        getQuest <- function(item) {
          queststr <- ""
          raidLevel <- 0
          
          type <- as.numeric(item[["quest_type"]])
          target <- as.numeric(item[["quest_target"]])
          condition <- item[["quest_conditions"]]
          
          if (!is.null(condition$type)) {
            queststr <- questdef[[paste0("quest_",type)]]
            queststr <- sub("%\\{amount\\}",target,queststr)
            
            if (condition$type[1] == 1) {
              tstr <- paste(unlist(questdef[paste0("poke_type_",unlist(condition$info$pokemon_type_ids))]),collapse=", ")
              queststr = sub("Pokemon", paste(tstr,"Pokemon",collapse=""),queststr)
            } else if (condition$type[1] == 2) {
              pstr <- paste(unlist(questdef[paste0("poke_",unlist(condition$info$pokemon_ids))]),collapse=", ")
              queststr <- sub("Pokemon",pstr,queststr)
            } else if (condition$type[1] == 3) {
              queststr <- sub("Pokemon","Pokemon with Weather Boost",queststr)
            } else if (condition$type[1] == 6) {
              queststr = sub("Complete","Win",queststr)
            } else if (condition$type[1] == 7) {
              raidLevel <- min(unlist(condition$info$raid_levels))
              if (raidLevel > 1) {
                queststr <- sub("Raid Battle\\(s\\)",paste("Level",raidLevel,"Raid or higher"),queststr)
              }
              if (!is.na(condition$type[2]) && condition$type[2] == 6) {
                queststr <- sub("Complete","Win",queststr)
              }
            } else if (condition$type[1] == 8) {
              queststr <- sub('Throw\\(s\\)',paste(questdef[[paste0("throw_type_",condition$info$throw_type_id[!is.na(condition$info$throw_type_id)])]],
                                                   "Throw\\(s\\)"),queststr)
              if (!is.na(condition$type[2]) && condition$type[2] == 15) {
                queststr <- sub("Throw\\(s\\)","Curveball Throw\\(s\\)",queststr)
              }
            } else if (condition$type[1] == 9) {
              queststr <- sub("Complete","Win",queststr)
            } else if (condition$type[1] == 10) {
              queststr <- sub("Complete","Use a Super Effective Charge Move in",queststr)
            } else if (condition$type[1] == 11 && is.null(condition$info)) {
              istr <- questdef$quest_condition_11
              queststr <- paste(queststr,paste0("(",istr,")"))
            } else if (condition$type[1] == 11 && !is.null(condition$info)) {
              istr <- questdef$quest_condition_11_formatted
              istr <- sub("%\\{item\\}",paste(unlist(questdef[paste0("item_",unlist(condition$info$item_id))]),collapse=", "),istr)
              queststr <- paste(queststr,paste0("(",istr,")"))
            } else if (condition$type[1] == 14 && is.null(condition$info$throw_type_id)) {
              queststr <- sub("Throw\\(s\\)","Throw\\(s\\) In a Row",queststr)
              if (!is.na(condition$type[2]) && condition$type[2] == 15) {
                queststr <- sub("Throw\\(s\\)","Curveball Throw\\(s\\)",queststr)
              }
            } else if (condition$type[1] == 14) {
              queststr <- sub("Throw\\(s\\)",paste(questdef[[paste0("throw_type_",condition$info$throw_type_id[!is.na(condition$info$throw_type_id)])]],
                                                   "Throw\\(s\\) In a Row"),queststr)
              if (!is.na(condition$type[2]) && condition$type[2] == 15) {
                queststr <- sub("Throw\\(s\\)","Curveball Throw\\(s\\)",queststr)
              }
            } else if (condition$type[1] != 0) {
              queststr <- paste("Undefined condition",condition$type[1])
            }
          } else if (!is.null(type)) {
            queststr <- questdef[[paste0("quest_",type)]]
            queststr <- sub("%\\{amount\\}",target,queststr)
          }
          return(as.character(queststr))
        }
        
        questdata$condition <- apply(questdata[c("quest_conditions","quest_type","quest_target")],1,getQuest)
        
        # Format the Quest data in a way that we have only the variables: Lat,Lng,Condition,Reward
        questdata <- questdata[c("lat","lon","reward","condition")]
        names(questdata) <- c("lat","lng","reward","condition")
      },silent=TRUE)
    }
  }
}
