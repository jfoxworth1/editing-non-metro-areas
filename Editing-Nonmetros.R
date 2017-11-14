# ```` Nonmetro Annual rates

WorkDir <- "/home/me/Documents/Uni-Jena/Work/HIWI/HIWI-2017-11-08-nonmetro-unemployment-rates"
setwd(WorkDir)

#dependencies
library(tidyverse)
library(stringr)
library(readxl)
library(openxlsx)

#select files
counties <- read_xls("Nonmetro_county_unemployment.xls")
files <- list.files()
files.annual.unemployment <- files[str_detect(files, "Annual")]
files.README <- files[str_detect(files, "README")]

#Make list of names
states.names <- str_split_fixed(files.annual.unemployment, "_", 4)
states.names <- states.names[,c(-1, -4)]
states.names[str_detect(states.names[,2], ".txt$"),2] <- ""
for(i in 1:2){
  states.names[,i] <- str_to_title(states.names[,i])
}
states.names <- paste(states.names[,1], states.names[,2], sep = " ")
states.names <- str_replace(states.names, " $", "")

given.states <- counties$state %>%
  unique()%>%
  str_replace(" .Nonmetropolitan Portion.", "")
given.states <- given.states[-c(1:3)]
givenstates <- states.names %in% given.states
#create list. Every entry is a state while the sublists are the entries for the state. Each element in 
#the main list will become its own excel file




#````
states <- list()
#tmp.count <- 0
for (i in 1:length(states.names)){
  if(!givenstates[i]){
    next()
  }
  #tmp.count <- tmp.count+1
  #i <- tmp.count
  x <- read_file(files.README[i]) %>%
    str_split(pattern = "\r\n")
  x <- x[[1]]
  x <- x[x != ""]
  
  y <- read_tsv(files.annual.unemployment[i])
  y <- y[y$DATE >= as.Date("1991-01-01") & y$DATE <= as.Date("2016-01-01"),]
  
  tmp1 <- names(y)[-1]
  tmp2 <- NULL
  for(j in 1:length(tmp1)){
    tmp2 <- c(tmp2, which(str_detect(x, tmp1[j])))
  }
  tmp2 <- unique(tmp2)
  tmp2 <- tmp2 + 3
  
  tmp2 <- x[tmp2]
  
  state.abbreviation <- tmp2 %>%
    str_trim() %>%
    str_extract(", ..$")
  unique.state.abbreviation <- unique(state.abbreviation)
  counter <- 1
  for(k in 1:length(unique.state.abbreviation)){
    if(sum(state.abbreviation %in% unique.state.abbreviation[k]) > sum(state.abbreviation %in% unique.state.abbreviation[counter])){
      counter <- k
    }
  }
  state.abbreviation <- unique.state.abbreviation[counter]
  
  tmp2 <- tmp2 %>%
    str_replace("^Unemployment Rate in ", "") %>%
    str_trim() %>%
    str_replace(" (County|Parish), ..", state.abbreviation)
  tmp3 <- tmp2 %in% counties$county
  
  y <- y[,c(TRUE, tmp3)]
  
  z <- y %>%
    gather(key = var_name, value = value, -1) %>% 
    spread_(key = names(y)[1],value = 'value')
  
  
  
  states[[i]] <- list(x, y, z)
  names(states[[i]]) <- c(paste(states.names[i], "README" , sep = "_"), 
                          paste("nonmetro", states.names[i],  "Annual" , sep = "_"), 
                          paste("nonmetro", states.names[i], "Transpose", sep = "_"))
}

#names(states) <- paste("nonmetro", states.names, sep = "_")
names(states) <- states.names
rm(list = c("states.names", "files.README", "files.annual.unemployment", "x",
            "y", "z", "i", "tmp", "num", "j", "tmp1", "tmp2", "tmp3", "unique.state.abbreviation", "k",
            "given.states", "givenstates", "counter", "state.abbreviation"))

for(i in 1:length(states)){
  if(is.null(states[[i]])){next()}
  write.xlsx(states[[i]], file = paste0(WorkDir, "/Saves/nonmetro_", names(states[i]), ".xlsx"))
}

rm(i)

