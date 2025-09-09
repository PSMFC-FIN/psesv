####ui.r

#set article number and viz number to pull in metadata
articleno <-2  #replace with article number
vizno <- 1 #replace with viz number

#source("psesv.R")  #### remove when psesv.R is converted to a package; this will need to be intializedor 
source("appcode.R")

psesvdashboard(articleno, vizno, app)




