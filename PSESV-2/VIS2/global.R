require(shiny)
require(shinydashboard)
require(dplyr)
require(reshape2)
require(ggplot2)
require(xtable)
require(scales)
require(ggthemes)
require(psesv)

# rm(list=ls())
# # direc <- "/x/projects/afsc/shiny_econsafe/effort2016"
# direc <- "x:/projects/afsc/shiny_econsafe/effort2016"
# setwd(direc)

eff <- readRDS("./rds/eff.rds")


namedList <- function(x){
  y <- as.list(x)
  names(y) <- x
  return(y)
}

# ## compute the "All lengths" strata for fishingweeks
# tmp <- eff$fishingweeks %>% group_by(year, gear, target, zone, sector) %>%
#   summarize(length = factor("All lengths"), weeks = sum(weeks)) %>% data.frame
# eff$fishingweeks <- rbind(eff$fishingweeks, tmp)
# 
# ## some NAs in months that shold be All Month in early year and
# ## All Month includes what should be the aggreaget so two times too large in later years
# ## dropping NA and All Month and recalculating fixes
# eff$crewweeks <- subset(eff$crewweeks, month!="All Months")
# tmp <- eff$crewweeks %>% group_by(zone, sector, year) %>%
#   summarize(crew.weeks = sum(crew.weeks), month="All Months") %>% data.frame
# eff$crewweeks <- rbind(eff$crewweeks, tmp)
# eff$crewweeks <- transform(eff$crewweeks, crew.weeks = as.integer(round(crew.weeks)))
# saveRDS(eff, file = "./rds/eff.rds")

zone.list <- namedList(levels(eff$vesscounts$nvescat_ak$zone))
sector.list <- namedList(levels(eff$vesscounts$nvescat_ak$sector))
target.list <- namedList(levels(eff$vesscounts$nvescat_ak$target))
gear.list <- namedList(levels(eff$vesscounts$nvesmonth$gear))
length.list <- namedList(levels(eff$fishingweeks$length))
month.list <- namedList(levels(eff$vesscounts$nvesmonth$month))

