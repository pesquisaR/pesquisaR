% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dados.R
\name{registrar}
\alias{registrar}
\alias{relatorio}
\title{Registra um conjunto de colunas para geração de gráficos e tabelas}
\usage{
registrar(dados, idx, plot.f, col, main, levels, ...)

relatorio()
}
\arguments{
\item{dados}{Dados. Como retornado por carga().}

\item{idx}{Vetor numerico contendo os indices das colunas de dados}

\item{plot.f}{Nome da funcao que sera chamada para fazer o grafico (ex: graf; pie; truehist)}

\item{col}{Vetor com as cores a serem utilizadas}

\item{main}{Titulo do grafico}

\item{\dots}{Demais parametros graficos (inclusive under, se aplicavel)}
}
\description{
Registra um conjunto de colunas para geração de gráficos e tabelas

Abre o relatorio com todos os campos registrados
}
