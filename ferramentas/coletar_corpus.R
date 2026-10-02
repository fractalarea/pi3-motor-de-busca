# =====================================================================
# coletar_corpus.R  --  Projeto Integrador III, Aula 05
#
# Monta o arquivo corpus.csv que o julgar.html le.
#
# COMO USAR
#   1. Va ate a secao 5, escolha UMA fonte e descomente as linhas dela.
#   2. Rode o script inteiro.
#   3. Leia a ficha tecnica que ele imprime. Se houver ERRO, conserte antes de julgar.
#   4. O arquivo corpus.csv sai na pasta de trabalho.
#
# SAIDA: id, titulo, texto (+ url, data, fonte quando existirem)
#
# Pacotes: so sao necessarios para a fonte que voce escolher.
#   RSS       -> install.packages("xml2")
#   Wikipedia -> install.packages("jsonlite")
#   .txt/.csv -> nenhum
# =====================================================================


# ---------------------------------------------------------------------
# 1. LIMPEZA
# ---------------------------------------------------------------------
# Tira marcacao HTML e, sobretudo, quebras de linha: um \n solto dentro
# de um campo parte o CSV em duas linhas e estraga tudo mais adiante.

limpar_campo <- function(x) {   # (nome proprio: o motor do curso tem outra funcao chamada limpar)
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- gsub("<[^>]*>", " ", x)                 # tags HTML
  x <- gsub("&nbsp;", " ", x, fixed = TRUE)
  x <- gsub("&amp;",  "&", x, fixed = TRUE)
  x <- gsub("&quot;", '"', x, fixed = TRUE)
  x <- gsub("&#39;",  "'", x, fixed = TRUE)
  x <- gsub("[\r\n\t]+", " ", x)               # quebras de linha
  x <- gsub("[[:space:]]+", " ", x)            # espacos repetidos
  trimws(x)
}


# ---------------------------------------------------------------------
# 2. IDENTIFICADOR ESTAVEL
# ---------------------------------------------------------------------
# O id amarra corpus -> pool -> qrels. Ele NAO pode mudar se voce
# reordenar as linhas ou refizer a coleta: se mudar, o gabarito inteiro
# passa a apontar para os documentos errados, e em silencio.
#
# Por isso o id NAO e o numero da linha. Ele vem do proprio conteudo:
# o mesmo texto gera sempre o mesmo id. (Hash FNV-1a de 32 bits,
# em R base -- nao precisa de pacote.)

mul32 <- function(a, b) {
  # multiplicacao modulo 2^32 sem estourar a precisao do double
  lo <- bitwAnd(a, 0xFFFF)
  hi <- a %/% 65536
  ((lo * b) %% 4294967296 + ((hi * b) %% 65536) * 65536) %% 4294967296
}

hash_id <- function(s, prefixo = "d") {
  h <- vapply(as.character(s), function(x) {
    if (is.na(x) || !nzchar(x)) x <- " "
    v <- 2166136261
    for (b in utf8ToInt(x)) {
      v <- bitwXor(v, b %% 256)
      v <- mul32(v, 16777619)
    }
    sprintf("%s%08x", prefixo, v)
  }, character(1), USE.NAMES = FALSE)

  # colisao de hash e rara, mas se acontecer desempatamos aqui
  dup <- duplicated(h)
  if (any(dup)) h[dup] <- paste0(h[dup], "_", seq_len(sum(dup)))
  h
}


# ---------------------------------------------------------------------
# 3. NORMALIZAR
# ---------------------------------------------------------------------
# Recebe o que voce conseguiu coletar e devolve o formato do julgar.html.
#   col_texto  : nome da coluna com o texto (obrigatorio)
#   col_titulo : nome da coluna com o titulo (opcional)
#   col_id     : se voce JA tem um id estavel (uma URL, por exemplo).
#                Deixe NULL para gerar pelo conteudo.

normalizar <- function(df, col_texto, col_titulo = NULL, col_id = NULL) {
  if (!col_texto %in% names(df))
    stop("a coluna de texto '", col_texto, "' nao existe. Colunas: ",
         paste(names(df), collapse = ", "))

  texto  <- limpar_campo(df[[col_texto]])
  titulo <- if (is.null(col_titulo) || !col_titulo %in% names(df))
              rep("", nrow(df)) else limpar_campo(df[[col_titulo]])

  id <- if (is.null(col_id) || !col_id %in% names(df)) {
          hash_id(paste(titulo, texto))
        } else {
          limpar_campo(df[[col_id]])
        }

  out <- data.frame(id = id, titulo = titulo, texto = texto,
                    stringsAsFactors = FALSE)

  # carrega junto as colunas extras que o julgar.html sabe exibir
  for (extra in c("data", "fonte", "autor", "url", "secao")) {
    if (extra %in% names(df)) out[[extra]] <- limpar_campo(df[[extra]])
  }
  out
}


# ---------------------------------------------------------------------
# 4. VALIDAR
# ---------------------------------------------------------------------
# Imprime a ficha tecnica e aponta o que vai dar problema no julgamento.

validar <- function(corpus) {
  cat("\n=============== FICHA TECNICA DO CORPUS ===============\n")
  cat("documentos :", nrow(corpus), "\n")
  cat("colunas    :", paste(names(corpus), collapse = ", "), "\n\n")

  for (col in names(corpus)) {
    v     <- corpus[[col]]
    cheio <- sum(!is.na(v) & trimws(as.character(v)) != "")
    cat(sprintf("  %-10s %5.1f%% preenchido\n", col, 100 * cheio / nrow(corpus)))
  }

  palavras <- lengths(strsplit(corpus$texto, "[[:space:]]+"))
  cat("\npalavras por documento:\n")
  print(summary(palavras))

  problemas <- 0

  dups <- sum(duplicated(corpus$id))
  if (dups > 0) {
    cat("\nERRO: ", dups, " id(s) duplicado(s). O gabarito nao vai parear.\n", sep = "")
    problemas <- problemas + 1
  }

  vazios <- sum(!nzchar(corpus$texto))
  if (vazios > 0) {
    cat("\nERRO: ", vazios, " documento(s) sem texto. Remova-os antes de julgar.\n", sep = "")
    problemas <- problemas + 1
  }

  med <- stats::median(palavras)
  if (med < 15) {
    cat("\nAVISO: mediana de", med, "palavras. Documentos curtos demais:",
        "\n  o julgamento fica raso e o parametro b do BM25 quase nao atua.",
        "\n  Considere coletar o corpo do texto, nao so o titulo.\n")
  }
  if (med > 600) {
    cat("\nAVISO: mediana de", med, "palavras. Documentos longos:",
        "\n  cada julgamento vai levar minutos. Considere dividir por secao ou paragrafo.\n")
  }
  if (nrow(corpus) < 30) {
    cat("\nAVISO: menos de 30 documentos. Serve para testar a ferramenta,",
        "\n  nao para uma avaliacao que signifique alguma coisa.\n")
  }

  cat("\n", if (problemas == 0) "Sem erros bloqueantes." else "CONSERTE OS ERROS ACIMA.",
      "\n=======================================================\n\n", sep = "")
  invisible(problemas == 0)
}


# ---------------------------------------------------------------------
# 5. FONTES  --  escolha UMA e descomente
# ---------------------------------------------------------------------

## ---- A) Feed RSS (jornal, blog, Google News) ------------------------
# Devolve titulo + resumo. ATENCAO: muitos feeds trazem so o titulo,
# o que da documentos curtissimos. Confira a ficha tecnica.

coletar_rss <- function(url) {
  if (!requireNamespace("xml2", quietly = TRUE))
    stop('rode antes: install.packages("xml2")')
  x     <- xml2::read_xml(url)
  itens <- xml2::xml_find_all(x, "//item")
  if (!length(itens)) stop("nenhum <item> encontrado. O endereco e mesmo um RSS?")
  data.frame(
    titulo = xml2::xml_text(xml2::xml_find_first(itens, "title")),
    texto  = xml2::xml_text(xml2::xml_find_first(itens, "description")),
    url    = xml2::xml_text(xml2::xml_find_first(itens, "link")),
    data   = xml2::xml_text(xml2::xml_find_first(itens, "pubDate")),
    stringsAsFactors = FALSE
  )
}

# feed <- "https://news.google.com/rss/search?q=site:atribuna.com.br&hl=pt-BR&gl=BR&ceid=BR:pt-419"
# bruto  <- coletar_rss(feed)
# corpus <- normalizar(bruto, col_texto = "texto", col_titulo = "titulo", col_id = "url")


## ---- B) Wikipedia em portugues --------------------------------------
# Texto completo dos artigos. Bom para comecar no mesmo dia.

coletar_wikipedia <- function(titulos, idioma = "pt") {
  if (!requireNamespace("jsonlite", quietly = TRUE))
    stop('rode antes: install.packages("jsonlite")')
  linhas <- lapply(titulos, function(t) {
    u <- paste0("https://", idioma, ".wikipedia.org/w/api.php",
                "?action=query&prop=extracts&explaintext=1&format=json&redirects=1",
                "&titles=", utils::URLencode(t, reserved = TRUE))
    r <- try(jsonlite::fromJSON(u, simplifyVector = FALSE), silent = TRUE)
    if (inherits(r, "try-error")) {
      warning("falhou: ", t); return(NULL)
    }
    p <- r$query$pages[[1]]
    if (is.null(p$extract) || !nzchar(p$extract)) {
      warning("sem texto: ", t); return(NULL)
    }
    data.frame(
      titulo = if (is.null(p$title)) t else p$title,
      texto  = p$extract,
      url    = paste0("https://", idioma, ".wikipedia.org/wiki/",
                      utils::URLencode(t, reserved = TRUE)),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, linhas)
}

# Para artigos longos, considere dividir por secao: um artigo inteiro
# como um unico documento leva minutos para julgar.
dividir_em_paragrafos <- function(df, minimo = 40) {
  linhas <- lapply(seq_len(nrow(df)), function(i) {
    ps <- unlist(strsplit(df$texto[i], "\n{2,}"))
    ps <- ps[lengths(strsplit(ps, "[[:space:]]+")) >= minimo]
    if (!length(ps)) return(NULL)
    data.frame(titulo = paste0(df$titulo[i], " (", seq_along(ps), ")"),
               texto  = ps,
               url    = df$url[i],
               stringsAsFactors = FALSE)
  })
  do.call(rbind, linhas)
}

# assuntos <- c("Recuperação de informação", "Modelo de espaço vetorial",
#               "Okapi BM25", "Índice invertido", "Word embedding",
#               "Motor de busca", "Precisão e revocação")
# bruto  <- coletar_wikipedia(assuntos)
# bruto  <- dividir_em_paragrafos(bruto)      # opcional, recomendado
# corpus <- normalizar(bruto, col_texto = "texto", col_titulo = "titulo")


## ---- C) Pasta com arquivos .txt -------------------------------------

coletar_txt <- function(pasta) {
  arqs <- list.files(pasta, pattern = "\\.txt$", full.names = TRUE)
  if (!length(arqs)) stop("nenhum .txt em ", pasta)
  data.frame(
    titulo = tools::file_path_sans_ext(basename(arqs)),
    texto  = vapply(arqs, function(a)
               paste(readLines(a, warn = FALSE, encoding = "UTF-8"), collapse = " "),
               character(1), USE.NAMES = FALSE),
    fonte  = basename(arqs),
    stringsAsFactors = FALSE
  )
}

# bruto  <- coletar_txt("textos/")
# corpus <- normalizar(bruto, col_texto = "texto", col_titulo = "titulo")


## ---- D) CSV ou planilha que voce ja tem -----------------------------
# Se a primeira coluna vier como "i..id", troque para fileEncoding = "UTF-8".

# bruto  <- read.csv("meus_dados.csv", stringsAsFactors = FALSE,
#                    fileEncoding = "UTF-8-BOM")
# corpus <- normalizar(bruto, col_texto = "conteudo", col_titulo = "manchete")


# ---------------------------------------------------------------------
# 6. VALIDAR E GRAVAR
# ---------------------------------------------------------------------

if (exists("corpus")) {
  ok <- validar(corpus)
  write.csv(corpus, "corpus.csv", row.names = FALSE, fileEncoding = "UTF-8")
  cat("gravado: corpus.csv  (", nrow(corpus), " documentos )\n", sep = "")
  if (!ok) cat("ATENCAO: ha erros na ficha tecnica. Nao comece a julgar ainda.\n")
} else {
  cat("\nNenhuma fonte foi escolhida.\n",
      "Va ate a secao 5, descomente as linhas de UMA fonte e rode de novo.\n\n", sep = "")
}
