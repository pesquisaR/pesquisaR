#' Funções auxiliares
#' 
#' Funções auxiliares para manipulação de textos e números.
#'
#' A função \code{wrap.it} é usada para gerar os nomes em eixos de gráficos, quebrando a linha em 
#' blocos de no máximo \code{len} caracteres.
#' A função \code{capitalize} transforma um vetor de texto para Iniciais Maiúsculas.
#' A função \code{trim} remove espaços no início e final de textos.
#' A função \code{split} separa cada item de um vetor usando um padrão regular, e devolve um vetor
#' contendo todos os elementos constituintes.
#' A função \code{to.p} formata um vetor numérico como porcentagem.
#' @param x Vetor de entrada; character para \code{wrap.it}, \code{trim} e \code{capitalize}, numeric para
#' \code{to.p}.
#' @param len Número máximo de caracteres para cada linha
#' @examples
#' wrap.it("Texto muito muito extremamente longo e desnecessariamente comprido", 10)
#' capitalize("texto em minúsculas")
#' trim("     espaços     ")
#' split("Um item e outro item, finalmente/no entanto")
#' @export
#' @encoding utf-8
#' @rdname auxiliar
wrap.it <- function(x, len = 12)
{ 
	sapply(x, function(y) paste(strwrap(y, len), 
								collapse = "\n"), 
		   USE.NAMES = FALSE)
}

#' @export
#' @rdname auxiliar
capitalize <- function(x) {
	  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
		      sep="", collapse=" ")
}
capitalize <- Vectorize(capitalize)

#' @export
#' @rdname auxiliar
trim <- function(x) return(gsub("^\\s+", "", gsub("\\s+$", "", x)))

#' @export
#' @param pattern Padrão regular usado para separar os elementos
#' @rdname auxiliar
split <- function(x, pattern="(, )|( e )|/") {
	res <- list()
	j = 1
	for (i in 1:length(x)) {
		res[i]  = strsplit(trim(x[i]), pattern)
	}
	return(unlist(res))
}
#' @export
#' @rdname auxiliar
to.p <- function(x) { return(round(x/sum(x)*100,1)) }

#' @export
#' @rdname auxiliar 
#' @import utils
rname <- function(x, wrap=12, dictionary="dictionary.txt") {
  dict <- read.csv(dictionary, header=FALSE, stringsAsFactors=FALSE)
	x <- gsub("\\.", " ", x)
	if (x=="") x <- "não respondeu / nenhum"
	# Substitui handles
  if (x %in% dict[,1])
    x <- dict[which(x == dict[,1])  ,2]
	x <- wrap.it(x, wrap)
	return(x[[1]]) # BUGFIX, as vezes esta retornando uma lista e nao sei pq
}
rname <- Vectorize(rname)
