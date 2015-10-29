#' Gráficos e tabelas
#' 
#' Gráficos e tabelas padrão.
#'
#' A função \code{graf} gera um gráfico de barra com as porcentagens de cada nível de um fator.
#' Essa função deve ser usada apenas em perguntas relacionadas, ou seja, todos os fatores devem ter
#' exatamente os mesmos níveis.
#' A função \code{porc} gera uma tabela de porcentagem para ser mostrada juntamente com o gráfico acima.
#' @param dados Tabela de dados gerada pela função \code{\link{carga}}
#' @param idx índice das colunas desejadas na variável dados
#' @param col Cores a serem utilizadas
#' @param main Título do gráfico
#' @param under Lógico. O label de cada questão deve ser mostrado sob a barra?
#' @encoding utf-8
#' @export
#' @import graphics
graf <- function(dados, idx, col, main, under=FALSE) {
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
#' @export
#' @rdname graf
porc <- function(dados, idx) {
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
