# kappa.R -- concordancia de julgamento: kappa de Cohen
# Projeto Integrador III -- Motor de Busca
#
# DOIS USOS
#   (a) ENTRE JUIZES: Ana e Bruno julgaram os mesmos itens, cada um no seu
#       navegador. Compara Ana com Bruno.
#   (b) CONSIGO MESMO: a mesma pessoa julgou de novo, dias depois, no modo
#       "segunda passada" do julgar.html (juiz gravado como "ana_p2").
#       Compara "ana" com "ana_p2".
#
# ENTRADA
#   Um ou mais qrels_*.csv exportados pelo julgar.html. Podem ser o arquivo de
#   cada juiz separado: o script junta.
#
# COMO RODAR (no R, no RStudio ou no Colab)
#   arquivos <- c("qrels_ana_2026-10-08.csv", "qrels_bruno_2026-10-08.csv")
#   source("kappa.R")
#
#   Se o script nao conseguir adivinhar quem comparar, ele lista os juizes
#   que encontrou e pede que voce diga:
#   juiz_a <- "ana"; juiz_b <- "bruno"        # entre juizes
#   juiz_a <- "ana"; juiz_b <- "ana_p2"       # consigo mesmo
#   source("kappa.R")
#
#   No terminal:  Rscript kappa.R qrels_ana.csv qrels_bruno.csv
#
# SAIDA
#   a matriz 3x3, po, pe, o kappa, a leitura, e a lista de itens em que os
#   dois julgamentos discordaram.

# ---------------------------------------------------------------------------
# 1. ler e juntar os arquivos
# ---------------------------------------------------------------------------
if (!exists("arquivos")) {                                   # nao definido no R:
  args <- commandArgs(trailingOnly = TRUE)                   #   tenta a linha de comando
  arquivos <- if (length(args) > 0) args else "qrels.csv"    #   ou o nome padrao
}

faltando <- arquivos[!file.exists(arquivos)]                 # algum arquivo nao existe?
if (length(faltando))
  stop("arquivo(s) nao encontrado(s): ", paste(faltando, collapse = ", "),
       "\nVoce esta em: ", getwd(), call. = FALSE)

ler <- function(f)                                           # o julgar.html grava com BOM:
  read.csv(f, stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM")
q <- do.call(rbind, lapply(arquivos, ler))                   # um data frame com tudo

precisa <- c("consulta", "documento", "grau", "juiz")        # colunas que o script usa
if (!all(precisa %in% names(q)))
  stop("colunas esperadas: ", paste(precisa, collapse = ", "),
       "\nencontradas: ", paste(names(q), collapse = ", "), call. = FALSE)

q$juiz  <- trimws(q$juiz)                                    # " ana" e "ana" sao o mesmo juiz
q$chave <- paste(q$consulta, q$documento)                    # um item = (consulta, documento)

# se um juiz julgou o mesmo item mais de uma vez (arquivos repetidos,
# exportou duas vezes), fica a ULTIMA nota dele para aquele item
q <- q[!duplicated(paste(q$juiz, q$chave), fromLast = TRUE), ]

# ---------------------------------------------------------------------------
# 2. decidir quem comparar com quem
# ---------------------------------------------------------------------------
juizes <- sort(unique(q$juiz))                               # todos os nomes no arquivo

if (!exists("juiz_a") || !exists("juiz_b")) {                # nao foi dito: tenta adivinhar
  base <- unique(sub("_p2$", "", juizes))                    # nomes sem o sufixo _p2
  com_p2 <- juizes[grepl("_p2$", juizes)]                    # quem fez segunda passada
  if (length(base) == 1 && length(com_p2) == 1) {            # uma pessoa, duas passadas
    juiz_a <- base; juiz_b <- com_p2
  } else if (length(juizes) == 2 && length(com_p2) == 0) {   # duas pessoas, uma passada cada
    juiz_a <- juizes[1]; juiz_b <- juizes[2]
  } else {                                                   # ambiguo: pergunta
    stop("encontrei estes juizes: ", paste(juizes, collapse = ", "),
         "\nDiga quem comparar, por exemplo:",
         "\n  juiz_a <- \"", juizes[1], "\"; juiz_b <- \"", juizes[length(juizes)], "\"",
         "\ne rode source(\"kappa.R\") de novo.", call. = FALSE)
  }
}

for (j in c(juiz_a, juiz_b))                                 # os nomes existem mesmo?
  if (!j %in% juizes)
    stop("juiz \"", j, "\" nao esta nos arquivos. Encontrei: ",
         paste(juizes, collapse = ", "), call. = FALSE)

mesmo <- sub("_p2$", "", juiz_a) == sub("_p2$", "", juiz_b)  # consigo mesmo ou entre juizes?
cat("\nmodo:", if (mesmo) "CONSIGO MESMO" else "ENTRE JUIZES",
    "--", juiz_a, "x", juiz_b, "\n")

# ---------------------------------------------------------------------------
# 3. parear os MESMOS itens
# ---------------------------------------------------------------------------
pa <- q[q$juiz == juiz_a, ]                                  # julgamentos do juiz A
pb <- q[q$juiz == juiz_b, ]                                  # julgamentos do juiz B
comuns <- intersect(pa$chave, pb$chave)                      # itens que os dois julgaram

if (length(comuns) < 5)
  stop("so ", length(comuns), " itens julgados pelos dois. Sao precisos pelo menos 5.",
       call. = FALSE)

a <- pa$grau[match(comuns, pa$chave)]                        # nota de A, na ordem de comuns
b <- pb$grau[match(comuns, pb$chave)]                        # nota de B, na MESMA ordem

# ---------------------------------------------------------------------------
# 4. matriz, po, pe, kappa
# ---------------------------------------------------------------------------
# o factor com levels garante 3 linhas e 3 colunas, mesmo se algum grau nao foi usado
m <- table(factor(a, levels = 0:2), factor(b, levels = 0:2))
dimnames(m) <- list(paste0(juiz_a, "=", 0:2), paste0(juiz_b, "=", 0:2))

n  <- sum(m)                                                 # itens comparados
po <- sum(diag(m)) / n                                       # OBSERVADA: a diagonal
pe <- sum(rowSums(m) * colSums(m)) / n^2                     # ESPERADA por acaso: marginais
kappa <- if (pe < 1) (po - pe) / (1 - pe) else NA            # pe = 1: todos sempre iguais

cat("itens comparados:", n, "\n\n")
print(m)
cat("\n")
print(round(c(po = po, pe = pe, kappa = kappa), 3))

# ---------------------------------------------------------------------------
# 5. leitura
# ---------------------------------------------------------------------------
cat("\nleitura: ")
if (is.na(kappa)) {
  cat("INDEFINIDO -- os dois deram sempre o mesmo grau; nao ha como medir.\n")
} else {
  cat(if (kappa < 0.4)      "FRACA -- o guia de julgamento esta ambiguo. Reescrever e rejulgar."
      else if (kappa < 0.6) "MODERADA -- aceitavel, mas registrar a limitacao no relatorio."
      else                  "BOA -- seguir adiante.", "\n")
}
if (mesmo && !is.na(kappa) && kappa > 0.9)
  cat("atencao: kappa > 0,9 consigo mesmo costuma ser memoria, nao criterio.",
      "Esperou pelo menos um dia entre as passadas?\n")

# ---------------------------------------------------------------------------
# 6. onde discordaram -- aponta o caso de fronteira que faltou no guia
# ---------------------------------------------------------------------------
dif <- a != b
if (any(dif)) {
  cat("\nitens em que os dois julgamentos discordaram (", sum(dif), "):\n", sep = "")
  print(data.frame(item = comuns[dif], juiz_a = a[dif], juiz_b = b[dif],
                   row.names = NULL))
}

rm(list = intersect(c("juiz_a", "juiz_b"), ls()))           # limpa: a proxima rodada pergunta de novo
