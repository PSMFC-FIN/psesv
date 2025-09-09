require(shiny)
require(shinydashboard)
require(dplyr)
require(reshape2)
require(ggplot2)
require(xtable)
require(scales)
require(ggthemes)
require(psesv)

# # direc <- "/x/projects/afsc/shiny_econsafe/econ2016"
# direc <- "x:/projects/afsc/shiny_econsafe/econ2016"
# setwd(direc)

econ <- readRDS("rds/econ.rds")

namedList <- function(x){
  y <- as.list(x)
  names(y) <- x
  return(y)
}

ak.wsl.spec.list <- namedList(unique(as.character(econ$wsl$species)))
bsai.wsl.spec.list <- namedList(unique(as.character(subset(econ$wsl, zone == "BSAI")$species)))
goa.wsl.spec.list <- namedList(unique(as.character(subset(econ$wsl, zone == "GOA")$species)))
wsl.zone.list <- namedList(unique(as.character(econ$wsl$zone)))
wsl.sector.list <- namedList(unique(as.character(econ$wsl$sector)))
ak.wsl.product.list <- namedList(as.character(unique(econ$wsl$product)))
bsai.wsl.product.list <- namedList(as.character(unique(subset(econ$wsl, zone == "BSAI")$product)))
goa.wsl.product.list <- namedList(as.character(unique(subset(econ$wsl, zone == "GOA")$product)))
wsl.vars <- c("zone", "species", "sector", "product", "year")

ak.exv.spec.list <- namedList(unique(as.character(econ$exv$specgrp)))
bsai.exv.spec.list <- namedList(unique(as.character(subset(econ$exv, zone == "BSAI")$specgrp)))
goa.exv.spec.list <- namedList(unique(as.character(subset(econ$exv, zone == "BSAI")$specgrp)))
exv.zone.list <- namedList(unique(as.character(econ$exv$zone)))
exv.sector.list <- namedList(unique(as.character(econ$exv$sector)))
exv.area.list <- namedList(as.character(unique(econ$exv$area)))
exv.gear.list <- namedList(as.character(unique(econ$exv$gear)))
exv.vars <- c("zone", "area", "specgrp", "sector", "gear", "year")
