#function to clean the output folders from old files generated automatically with apollo

moveold <- function(){
  oldfiles <- list.files(pattern = "OLD[1-9]", recursive = TRUE)


  for(file in oldfiles) {

    file.rename(from = file, to = paste0("OLD/",gsub("^.*/", "",file)))

  }

}
