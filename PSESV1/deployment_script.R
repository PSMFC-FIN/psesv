### deploy psesv-1 visualization to NMFS Posit Connect

api_file <- tcltk::tk_choose.files(caption = "Select API key file")
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


deployApp(
  appDir = ".",
  appName = "psesv-1",
  account = "jean.lee"
)


