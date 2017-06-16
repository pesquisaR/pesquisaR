#' Funções para carregar e tratar dados
#' 
#' Funções padrão para carregar dados a partir de arquivos csv.
#' 
#' A função \code{carga} carrega todos os arquivos com um determinado padrão (por padrão *.csv)
#' e devolve um data.frame com todos os dados. Um erro pode ser emitido caso os arquivos não contenham
#' os mesmos nomes de colunas.
#' @export
#' @rdname dados
#' @encoding utf-8
#' @param pattern Padrão de busca para o nome dos arquivos.
#' @param \dots Demais argumentos para a função \code{read.csv}.
#' @param remove Lógico. As colunas devem ser tratadas para remover espaços em branco e 1-, 2-, 3-, etc?
carga <- function(pattern = "*.csv", remove = TRUE, ...) {
  dots <- list(...)
  if (! "header" %in% names(dots)) dots$header = TRUE
  if (! "stringsAsFactors" %in% names(dots)) dots$stringsAsFactors = FALSE
  dados <- NULL
  for (file in list.files(pattern=pattern))
  {
    if(is.null(dados)) 
      dados <- do.call(read.csv, c(list(file=file), dots))
    else
      dados <- rbind(dados, do.call(read.csv, c(list(file=file), dots)))
  }
  if (remove) {
    dados <- as.data.frame(lapply(dados, function(x) gsub("[1234567890]*-", "", x)), stringsAsFactors=FALSE)
    dados <- as.data.frame(lapply(dados, trim), stringsAsFactors=FALSE)
  }
  cat(paste("Entrevistas:", dim(dados)[1], "\n"))
  return(dados)
}

#' Ambiente shiny para geração do relatório
#' @export
#' @rdname dados
shinyEnv <- new.env()

#' Registra um conjunto de colunas para geração de gráficos e tabelas
#' @export
#' @param dados Dados. Como retornado por carga().
#' @param idx Vetor numerico contendo os indices das colunas de dados
#' @param plot.f Nome da funcao que sera chamada para fazer o grafico (ex: graf; pie; truehist)
#' @param col Vetor com as cores a serem utilizadas
#' @param main Titulo do grafico
#' @param \dots Demais parametros graficos (inclusive under, se aplicavel)
#' @import shiny
#' @rdname relatorio
registrar <- function(dados, idx, plot.f, col, main, levels, ...) {
    dots <- list(...)
    outlist <- tryCatch(get("outlist", envir=shinyEnv), error=function(x) return(list()))
    if (!missing(dados))
        for (i in idx)
            dados[,i] <- factor(dados[,i], levels=levels)
    id <- outlist$maxid; if (is.null(id)) id=1
    outlist[[paste0("g", id)]] <- renderPlot({
        do.call(plot.f, c(list(dados=dados, idx=idx, col=col, main=main), dots))
    })
    outlist[[paste0("p", id)]] <- renderTable({porc(dados,rev(idx))})
    outlist$maxid <- id+1
    assign("outlist", outlist, envir=shinyEnv)
}

#' Abre o relatorio com todos os campos registrados
#' @export
#' @import shiny
relatorio <- function() {
    server <- function(input, output) {
        outlist <- tryCatch(get("outlist", envir=shinyEnv), error=function(x) return(list()))
        for (i in names(outlist)) if(i != "maxid")
            output[[i]] <- outlist[[i]]
    }
    # GERAR AUTOMATICAMENTE?????
    ui <- fluidPage(
                    plotOutput("g1"),
                    tableOutput("p1")
                    )
    shinyApp(ui=ui, server=server)
}
# Carregar dados INTERMEDIARIOS

# Remove os 1- 2- 3-
# "Factor"ization
#dados$IdadeB <- cut(as.numeric(dados$Idade), c(16,2:9*10), include.lowest=TRUE)
#dados[,5] <- factor(dados[,5], levels=c("", "Feminino", "Masculino"))
#dados[,6] <- factor(dados[,6], levels=c("", "até R$ 1.576", "de R$ 1.576 a R$ 2.364", 
#	"de R$ 2.364 a R$ 3.940","de R$ 3.940 a R$ 7.880", "de R$ 7.880 a R$ 15.760", "acima de R$ 15.760"))
#dados[,7] <- factor(dados[,7], levels=c("", "fundamental incompleto", "fundamental completo", 
#	"médio incompleto", "médio completo", "superior incompleto", "superior completo"))
#dados[,8] <- factor(dados[,8] ,levels=c("", "branca", "parda", "preta", "amarela", "indígena", "outra"))
#dados[,10] <- factor(dados[,10] ,levels=c("", "católica", "evangélica", "matriz africana", "sem religião", "outras"))
## Grupos de questoes
#for (i in c(11:22))
#	dados[,i] <- factor(dados[,i] ,levels=c("", "sim", "não sei", "não"))
## VALIDACAO:
#err <- apply(dados, 2, function (x) sum(is.na(x)))
#print(err [ err > 0 ] )
#
## Funcoes auxiliares
#rname <- function(x, wrap=12) {
#	x <- gsub("\\.", " ", x)
#	x <- gsub("\\.", " ", x)
#	# Substitui handles
#	if (x=="") x <- "não respondeu / nenhum"
#	if (x=="IdadeB") x <- "Idade"
#	if (x=="Violenta") x <- "Sua região é muito violenta?"
#	if (x=="Leis duras") x <- "Devemos ter leis mais duras para combater o crime?"
#	if (x=="Matar bandido") x <- "A polícia deve matar bandido?"
#	if (x=="Linha dura") x <- "Votaria num político que fosse linha dura na luta contra o crime?"
#	if (x=="Maioridade") x <- "É a favor da redução da maioridade penal?"
#	if (x=="Presença") x <- "É favor de aumentar a presença da polícia na rua?"
#	if (x=="Portando arma") x <- "Você se sentiria mais seguro portando uma arma?"
#	if (x=="Lei igual") x <- "Acha que a lei é igual para rico que para pobre?"
#	if (x=="Cadeia") x <- "O bandido merece o tratamento da cadeia?"
#	if (x=="Racista") x <- "A polícia é racista?"
#	if (x=="Problema") x <- "Você já teve ou conhece alguém que teve problema com a polícia?"
#	if (x=="PoliciaViolenta") x <- "Você acha que a polícia é violenta?"
#	x <- wrap.it(x, wrap)
#	return(x)
#}
#rname <- Vectorize(rname)
#

#' acept.table gera uma tabela de aceitação ou rejeição de uma pauta. Na versão atual, só funciona com as categorias
#' 'progressista' e 'conservador'.
#' @param dados tabela de dados
#' @param var número da variável na tabela de dados
#' @export
acept.table = function(dados, var) {
    NN = c( "Progressista","Conservador")
    dv = dados[,var]
    to.m = function(x,full) 100*mean(x)/max(abs(full))
    tt = tapply(dados$progressista, dv, to.m, full=dados$progressista)
    v1 = data.frame(t(tt))
    v1 = rbind(v1, tapply(dados$conservador, dv, to.m, full=dados$conservador))
    rownames(v1) = NN
    colnames(v1) = rname(colnames(v1))
    v1
}

