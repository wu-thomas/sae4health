# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file
pkgload::load_all(export_all = FALSE,helpers = FALSE,attach_testthat = FALSE)
options( "golem.app.prod" = TRUE)
#test on local server
#sae4health::run_app(server_link='http://localhost:8000/')
#version to deploy to stat server
sae4health::run_app(server_link='https://sites.stat.washington.edu/sae4health/')