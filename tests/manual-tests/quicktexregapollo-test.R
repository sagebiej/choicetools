rm(list = ls())
devtools::load_all()

model_path <- system.file("extdata", "mixlogitInt_bootstrap.RDS", package = "choiceTools")

est_model <- readRDS(model_path)


different_se <- list()

different_se[["normalse"]] <- quicktexregapollo(est_model, se = "normal")

different_se[["robustse"]] <- quicktexregapollo(est_model, se = "rob")

different_se[["bootstrapse"]] <- quicktexregapollo(est_model, se = "bs")

texreg::screenreg(different_se)

janitor::clean_names(apollo::apollo_modelOutput(est_model))
