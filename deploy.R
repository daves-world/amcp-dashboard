# One-time: connect your shinyapps.io account (token from Account > Tokens)
# rsconnect::setAccountInfo(name = "<ACCOUNT>", token = "<TOKEN>", secret = "<SECRET>")

# Deploy (run from the project folder)
rsconnect::deployApp(
  appDir   = ".",
  appName  = "amcp-dashboard",
  appFiles = c("app.R", "data")
)
