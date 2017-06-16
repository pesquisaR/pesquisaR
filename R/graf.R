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
graf <- function(dados, idx, col = paleta, main = "", under=FALSE, y.leg = -0.5) {
	d <- dados[,idx]
	l <- length(idx); n <- length(levels(d[,1])) #Precisam ter todos o mesmo n de levels!!
	mt <- matrix(rep(0, n*l), ncol=l)
	for (i in 1:l)
		mt[,i] <- table(dados[,idx[i]])
    leg = levels(d[,1])
    if (names(dados)[1] == "") {
    	mt = mt[-1,]
        leg = leg[-1]
    }
  mt <- apply(mt, 2, to.p)
	par(mar=c(4,7,3,2), xpd=TRUE)
	if (under) {
		barplot(mt, col=col, main=main, horiz=TRUE, axes=FALSE, las=1, space=0.8)
    text(50, 0.2+1.8*1:l, rname(names(dados)[idx],999))
	} else {
		barplot(mt, col=col, names.arg=rname(names(dados)[idx]), main=main, horiz=TRUE, axes=FALSE, las=1)
	}
	legend(x=1, y=y.leg, legend= leg, fill=col,  bty='n', ncol=n )
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


#' @export
#' @rdname graf
plivre <- function(dados, idx, corte = 5, main, col=paleta[1], wrap=12) {
    n = dim(dados)[1]
    data <- rname(capitalize(split(dados[,idx])), 999)
    data <- data[data != ""] 
    toplot <- table(data)
    toplot["Outros"] = sum(toplot[toplot < corte])
    toplot <- sort(toplot[toplot >= corte])/n*100
    par(mar=c(4,10,3,2))
    barplot(as.numeric(toplot), horiz=TRUE, las=1, names.arg=wrap.it(names(toplot), wrap), main=main, col=col)
}

#' @export
#' @rdname graf
tlivre <- function(dados, idx, corte = 5) {
    n = dim(dados)[1]
    data <- rname(capitalize(split(dados[,idx])), 999)
    data <- data[data != ""] 
    toplot <- table(data)
    toplot["Outros"] = sum(toplot[toplot < corte])
    toplot <- rev(round(sort(toplot[toplot >= corte])/n*100,2))
    x = data.frame(Porcentagem=as.numeric(toplot))
    rownames(x) = names(toplot)
    return(x)
}

#' @export
#' @rdname graf
paleta <- c("#8facbc", "#029db1", "#1f3f68", "#72ba94", "#d4c165", "#e17834", "#ce6c6d", "#874137", "#be9776")

#' acept.plot faz uma plot de aceitação ou rejeição de uma pauta, semelhante ao gerado pela tabela acept.table
#' @param var Número da variável na tabela de dados
#' @param cols Colunas a serem mantidas (em ordem) na tabela de dados
#' @export
#' @rdname graf
acept.plot = function(dados, var, cols = NULL, ...) {
    NN = c( "Progressista","Conservador")
    par(mar=c(4,6,3,2))
    tt = acept.table(dados, var)
    name = rname(var, 15)
    if (!is.null(cols))
            tt = tt[,cols]
    nl = ncol(tt)
p = paleta[c(7, 2, 1, 5, 9, 4, 3, 6) ]
plot(1:nl,tt[1,], type='l', col=p[1], lwd=2, ylim=c(-100,100), las=1, xaxt='n', main=paste("Adesão por", name), xlab="", yaxt='n', ylab='', ...)
for (i in 2)
    lines(1:nl, tt[i,], col=p[i], lwd=2)
axis(1, 1:nl, colnames(tt), mgp=c(3,2.5,0), ...)
axis(2, c(-100,0,100), c("Rejeição", "Neutro", "Aceitação"), las=1)
legend("bottomleft", legend=NN, fill=p, bty='n', ncol=2)
}

#' rel.plot faz uma plot de relacionamento entre duas variáveis
#' @param icols Colunas a serem mantidas na variável independente
#' @param dcols Colunas a serem mantidas na variável dependente
#' @param indep Número da coluna da variável independente
#' @param dep Número da coluna da variável dependente
#' @export
#' @rdname graf
#' @import grDevices

rel.plot = function(dados, indep, dep, icols = NULL, dcols = NULL, ...) {
        rel.table = function(dados, indep, dep) {
                apply(table(dados[,dep], dados[,indep]),2, to.p)
        }
        ni = rname(names(dados)[indep],99)
        nd = rname(names(dados)[dep],99)
        tt = rel.table(dados,indep,dep)
        if (!is.null(icols))
                tt = tt[ , icols ]
        if (!is.null(dcols))
                tt = tt[ dcols , ]
        mx=min(max(tt)*1.2,100)
        p = c(paleta[c(1, 4, 5, 6, 7) ], rainbow(7))
        plot(tt[1,], type='l', col=p[1], lwd=2, ylim=c(0,mx), las=1, xaxt='n', main=wrap.it(paste0(nd," por ",ni),40), xlab="",ylab="", ...)
        for (i in 2:nrow(tt))
                lines(tt[i,], col=p[i], lwd=2)
        ax = rname(colnames(tt))
        axis(1, 1:length(ax), ax, ...)
        legend("topleft", legend=rownames(tt), fill=p, bty='n')
}

