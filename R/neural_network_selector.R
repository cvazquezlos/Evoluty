data <- NULL

kBaseCharacter <- ""
kBaseNumeric <- 0.000000

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
    model %>% keras::compile(optimizer = "Adam",
                             loss = "categorical_crossentropy",
                             metrics = c("accuracy"))
  } else {
    # Regression.
  }
  history <- model %>% keras::fit(X_train, y_train, validation_data = (X_validation, y_validation), epochs = , batch_size =, verbose = 0, callbacks =)
  history <- model %>% fit(rbind(X_train, X_validation), rbind(y_train, y_validation), validation_split = 0.235294, epochs = 500, batch_size = 150, verbose = 0,
                           callbacks = list(
                             callback_early_stopping(monitor = "val_loss", patience = 50, verbose = 0, mode ="auto")
                           ))
}

Main <- function(population.size, mode, data) {
  population.individuals <- lapply(population.size, function(x) {
    new("NeuralNetwork", id = x, architecture = CleanArchitecture(Standard(1)))
  })

}


# BASE: http://r-pkgs.had.co.nz/package.html
# http://r-pkgs.had.co.nz/r.html
