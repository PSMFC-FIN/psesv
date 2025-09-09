### deploy psesv-1 visualization to NMFS Posit Connect


api_file <- rstudioapi::selectFile(
  caption = "Select API key file",
  filter = "Text files (*.txt);;All files (*)"
)

api_key <- readLines(api_file, n = 1)



# Register your Connect server
rsconnect::addServer(
  url = "https://connect.fisheries.noaa.gov/",
  name = "nmfspositconnect"
)

# Register yourself as a user with the API key
rsconnect::connectApiUser(
  server = "nmfspositconnect",
  apiKey = api_key
)


rsconnect::deployApp(
  appDir = ".",
  appName = "psesv-2b",
  account = "jean.lee"
)



