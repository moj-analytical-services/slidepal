# Functions for unpacking/parsing JSON-stored themes

#' Unpack named list of hex colours from json format
#'
#' @param path Path to json theme to unpack (from project root)
#'
unpack_pal <- function(path){
  theme <- jsonlite::read_json(path)
  
  pal_vec <- unlist(lapply(theme$palette, "[[", "hex"))
  pal_names <- unlist(lapply(theme$palette, "[[", "name"))
  names(pal_vec) <- pal_names
  
  pal_vec <- pal_vec[unique(names(pal_vec))]
  return(pal_vec)
}

#' Extract colors as hex codes
#'
#' @param ... Character names of theme colours
#'
#' @export
#' 
moj_colours <- function(..., theme = default_theme()) {
  if (missing(theme)) {message("Using default theme: ", theme)}
  themefile <- match.arg(theme, 
                         c("dummy-theme.json",
                         list.files(here::here("themes"), pattern="*.json")))
  if(themefile=="dummy-theme.json"){
    themepath <- here::here("tests", "testthat", themefile)
  } else {
    themepath <- here::here("themes", themefile)
  }
  
  themepal <- unpack_pal(themepath)
  
  cols <- c(...)
  if (is.null(cols)){
    return(themepal)
  } else {
    return(themepal[cols])
  }
}

#' Set default theme if unspecified
#' 
#' @return String of default theme name (without file ext)
#' 
default_theme <- function(){
  default <- "mojlight"
  stopifnot(file.exists(here::here("themes", paste0(default, ".json"))))
  return(default)
}
