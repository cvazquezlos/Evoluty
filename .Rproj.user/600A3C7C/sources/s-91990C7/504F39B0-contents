kBaseCharacter <- ""
kBaseNumeric <- 0.000000
kSplitCriteria <- c(train = 0.7, validation = 0.2, test = 0.1)

setClass("NeuralNetwork", representation(id = "numeric", architecture = "character",
                                         evaluated = "logical", loss = "numeric",
                                         metric = "numeric", model = "character"),
                          prototype(id = kBaseNumeric, architecture = kBaseCharacter,
                                    evaluated = FALSE, loss = kBaseNumeric,
                                    metric = kBaseNumeric, model = kBaseCharacter))

CleanArchitecture <- function(architecture) {
  return(toString(gsub("\"", "", toString(architecture))))
}

Evaluation <- function(individual, mode, input, output) {
  hidden.letters <- head(tail(strsplit(individual@architecture, "/")[[1]], -1), -1)
  hidden.count <- unlist(lapply(hidden.letters, function(x) nchar(x)))
  model <- keras::keras_model_sequential()
  model %>% keras::layer_dense(units = hidden.count[1], input_shape = c(input), activation = "relu")
  invisible(lapply(hidden.count, function(x) model %>% keras::layer_dense(units = x, activation = "relu")))
  if (mode == 0) {
    model %>% keras::layer_dense(untis = output, activation = "softmax")
    model %>% keras::compile(optimizer = "",
                             loss = "categorical_crossentropy",
                             metrics = c("accuracy"))
  } else {
    # Regression.
  }
  history <-model %>% keras::fit()
}

Main <- function(population.size, mode, data) {
  population.individuals <- lapply(population.size, function(x) {
    new("NeuralNetwork", id = x, architecture = CleanArchitecture(Standard(1)))
  })
  n <- nrow(data)
  sample(cut(
    seq(n),
    nrow(data)*cumsum(c(0, kSplitCriteria)),
    labels = names(kSplitCriteria)
  ))
}
# BASE: http://r-pkgs.had.co.nz/package.html
# http://r-pkgs.had.co.nz/r.html
