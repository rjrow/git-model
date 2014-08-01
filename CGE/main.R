Sys.setenv(MYSQL_HOME = 'C:/Program Files/R/R-3.0.2/library/RMySQL')
library(gdxrrw)
library(lubridate)
library(RMySQL)


# Run a config file containing all constants for modeling. 
# THESE FILES WILL NEED TO BE EDITED IF WE ADD/CHANGE VARIABLES, not a permanent solution.

wd <- getwd()
cur.source <- paste0(wd, "/","CGE","/")
sectors.source <- paste0(cur.source,"sectors.R")

source(sectors.source)

####################################################################################################

# Run the cge model, it outputs a gdx file
run <- function()
{
  setwd("C:/Users/rjrow.ASURITE/Desktop/model production/model")
  system("GAMS mrtmcp.gms")
}

# This function takes in data from the web interface, and perturbs the main input data
# main_data.gdx and outputs main_data_out.gdx. The main_data_out.gdx is the file that is used
# as input to mrtmcp.gms (the actual cge model)



# TODO: This function needs an update for how it handles slots, it really inhibits the possibilities
# of model inputs. not a huge fan, but it works and will be re-wored in next iteration run through
perturb_data <- function(file.name, slots, value, prm.name)
{
  
  data.source <- paste0(cur.source, "data")
  setwd(data.source)
  prm.list <- list()
  slots.df <- data.frame(Reduce(rbind,slots))
  row.names(slots.df) <- NULL
  names(slots.df) <- c("slot.names","item")
  prm.name <- paste(prm.name, collapse = '') 
  file.name <- paste(file.name, collapse = '')
  value <- value[-length(value)] # get rid of the zero from control.py same function name 
  
  
  for(i in 1:length(all.gdx.objects))
  {
    gdx.item <- all.gdx.objects[i]
    prms.lst <- list(name = gdx.item, form = "sparse")
    prm <- rgdx(file.name, prms.lst)
    prm.list[[i]] <- prm
  }

  
  for(i in 1:length(all.gdx.objects))
  {
    gdx <- all.gdx.objects[i]
    if(gdx == prm.name)
    {  
      param.list <- prm.list[[i]]
      uel.list <- param.list$uels
      domains <- param.list$domains
      domains.value <- c(domains, "value")
      values <- data.frame(param.list$val)
      uels <- data.frame()
      metrics <- list()
      names(uel.list) <- domains
      
      
      for(i in 1:length(uel.list))
      {
        item.i <- uel.list[i]
        slot.name <- as.character(names(item.i))
        metrics[[slot.name]] <- !(logical(length(uel.list[[i]])))
      }
      
      
      for(j in 1:length(slots))
      {
        item.j <- uel.list[j]
        slot.name <- as.character(slots[[j]][1])
        item <- as.character(unlist(slots.df[slots.df$slot.names == slot.name, "item"]))
        matched <- match(item,uel.list[[slot.name]])
        uels <- data.frame(c(uels, match(item,uel.list[[slot.name]])))
        metrics[[slot.name]] <- logical(length(metrics[[slot.name]]))
        metrics[[slot.name]][matched] <- TRUE
        names(uels)[j] <- slot.name
      }
      
      names(values) <- domains.value
      
      if(length(slots) == 1)
      {
        slot.name <- as.character(names(uels[1]))
        values[which(values[[slot.name]] == uels[[slot.name]]), length(values)] <- value
      }
      
      if(length(slots) == 2)
      {
        slot.name1 <- as.character(names(uels[1]))
        slot.name2 <- as.character(names(uels[2]))
        print(values[[slot.name1]] == uels[[slot.name1]])
        values[which(values[[slot.name1]] == uels[[slot.name1]] &
                       values[[slot.name2]] == uels[[slot.name2]]), length(values)] <- value
        print(uel.list)
      }
      
      if(length(slots) == 3)
      {
        slot.name1 <- as.character(names(uels[1]))
        slot.name2 <- as.character(names(uels[2]))
        slot.name3 <- as.character(names(uels[3]))
        values[which(values[[slot.name1]] == uels[[slot.name1]] &
                       values[[slot.name2]] == uels[[slot.name2]] &
                       values[[slot.name3]] == uels[[slot.name3]]), length(values)] <- value
      }
      
      
      if(length(slots) == 4)
      {
        slot.name1 <- as.character(names(uels[1]))
        slot.name2 <- as.character(names(uels[2]))
        slot.name3 <- as.character(names(uels[3]))
        slot.name4 <- as.character(names(uels[4]))
        values[which(values[[slot.name1]] == uels[[slot.name1]] &
                       values[[slot.name2]] == uels[[slot.name2]] &
                       values[[slot.name3]] == uels[[slot.name3]] &
                       values[[slot.name4]] == uels[[slot.name4]]), length(values)] <- value
      }
      
      print(values) 
      prm.list[[i]]$val <- as.matrix(values)
      
    }
    
    
  } 
  #return(prm.list)
  wgdx.lst("main_data_out.gdx", prm.list, squeeze = 'y')
  setwd(wd)
}


