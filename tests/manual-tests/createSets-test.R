rm(list = ls())
devtools::load_all()

model_path <- system.file("extdata", "csasimdata.csv", package = "choiceTools")

data <- read.csv(model_path)


sets <- createSets(data, choice = "CHOICE", uniquerow = "Choice_situation", attributes = c("alt1_x1", "alt2_x1", "alt1_x2", "alt2_x2"), prefix = "alt")
