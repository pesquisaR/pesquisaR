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
#' @rdname aux
wrap.it <- function(x, len = 10)
{ 
	sapply(x, function(y) paste(strwrap(y, len), 
								collapse = "\n"), 
		   USE.NAMES = FALSE)
}

#' @export
#' @rdname aux
capitalize <- function(x) {
	  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
		      sep="", collapse=" ")
}
capitalize <- Vectorize(capitalize)

#' @export
#' @rdname aux
trim <- function(x) return(gsub("^ .", "", gsub(". $", "", x)))

#' @export
#' @param pattern Padrão regular usado para separar os elementos
#' @rdname aux
split <- function(x, pattern="(, )|( e )|/") {
	res <- list()
	j = 1
	for (i in 1:length(data)) {
		res[i]  = strsplit(trim(data[i]), )
	}
	return(unlist(res))
}
#' @export
#' @rdname aux
to.p <- function(x) round(x/sum(x)*100,1)

graf <- function(idx, col, main, under=FALSE) {
	d <- dados[,idx]
	l <- length(idx); n <- length(levels(d[,1])) #Precisam ter todos o mesmo n de levels!!
	mt <- matrix(rep(0, n*l), ncol=l)
	for (i in 1:l)
		mt[,i] <- table(dados[,idx[i]])
	mt = mt[-1,]
	par(mar=c(4,7,3,2), xpd=TRUE)
	if (under) {
		barplot(mt, col=col, main=main, horiz=TRUE, axes=FALSE, las=1, space=0.8); text(dim(dados)[1]/2, 0.2+1.8*1:l, rname(names(dados)[idx],999))
	} else {
		mt <- apply(mt, 2, to.p)
		barplot(mt, col=col, names.arg=rname(names(dados)[idx]), main=main, horiz=TRUE, axes=FALSE, las=1)
	}
	legend(x=1, y=-.5, legend= levels(d[,1])[-1], fill=col,  bty='n', ncol=n )
}
porc <- function(idx, name, lev=NULL) {
	d <- dados[,idx]
	l <- length(idx);
	if (is.null(dim(d)))	{
		n <- length(levels(d)) #Precisam ter todos o mesmo n de levels!!
		lev <- levels(d)
	} else {
		n <- length(levels(d[,1])) #Precisam ter todos o mesmo n de levels!!
		lev <- levels(d[,1])
	}
	mt <- matrix(rep(0, n*l), ncol=l)
	for (i in 1:l)
		mt[,i] <- table(dados[,idx[i]])
	data <-data.frame(apply(mt, 2, to.p), row.names=lev)
	colnames(data)<-rname(names(dados)[idx], 999)
  rownames(data) <-rname(rownames(data), 999)
	return(data)
}
