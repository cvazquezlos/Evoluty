grammar <- NULL

SetGrammar <- function(g) {
  grammar <- gramEvol::CreateGrammar(g)
}

Standard <- function(population.size) {
  population.individuals <- gramEvol::GrammarRandomExpression(grammar, population.size)
  return(population.individuals)
}

GrammarBased <- function() {
}
