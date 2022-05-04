# Setup fonts. This function will ensure that fonts are available for plotting
setup_fonts <- function(init= FALSE, font_dir = file.path(here::here(), "fonts")) {
  
  if (init) {
    remotes::install_version("Rttf2pt1", version = "1.3.8")
    extrafont::ttf_import(font_dir)
  }
  
  # Fonts are a big problem in these figures. They have to be Unicode compatible (because of the special characters)
  # and ideally multi-platform. So I'm using some google fonts, in addition to Arial Unicode. Set the font name here
  # to use throughout
  # font_name<-"Arial Unicode MS"
  library(extrafont)
  extrafont::loadfonts(device = "pdf")

}


