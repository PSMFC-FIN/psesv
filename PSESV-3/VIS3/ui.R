#set article number and viz number to pull in metadata
articleno <- 3  #replace with article number
vizno <- 3 #replace with viz number

#source("psesv.R")  #### remove when psesv.R is converted to a package; this will need to be intializedor 
source("appcode.R")

psesvdashboard(articleno, vizno, app)
