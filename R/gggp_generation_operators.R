grammar <- NULL

SetGrammar <- function(g) {
  grammar <- CreateGrammar(g)
}

Standard <- function() {
  return(GrammarRandomExpression(grammar, 1))
}

GrammarBased <- function() {
}
