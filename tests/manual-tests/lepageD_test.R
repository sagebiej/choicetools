model <- readRDS(system.file("extdata", "mixlogitInt_bootstrap.RDS", package = "choiceTools"))

model1 <- list(estimate = c(a = 0.5, b = 0.2), varcov = matrix(c(0.001, 0.0, 0.0, 0.001), nrow = 2))
model2 <- list(estimate = c(a = 0.5, b = 0.2), varcov = matrix(c(0.001, 0.0, 0.0, 0.001), nrow = 2))

res <- lapage(sim = 10, n = 1000, modelname1 = model1, modelname2 = model2, meanname = "a", sdname = "b")


x <- rnorm(100, 0.5, 0.2)
y <- rnorm(100, 0.5, 0.1)

lepage.test(x, y)
