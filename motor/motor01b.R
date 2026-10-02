# ==========================================================================
#  motor01b.R  --  Projeto Integrador III -- Motor de Busca
#  Para a Aula 1,5: as funcoes das aulas ANTERIORES (00, 01).
#  Gerado de motor_mestre.R em 2026-10-01. NAO EDITE ESTE ARQUIVO: edite o mestre.
# ==========================================================================
#
#  COMO CARREGAR
#    Colab:    source("https://raw.githubusercontent.com/fractalarea/pi3-motor-de-busca/main/motor/motor01b.R")
#    RStudio:  source("motor01b.R")
#
#  DEPOIS (teoria, com os 8 documentos do curso):
#    docs <- docs_aula();  cfg <- cfg_aula();  ix <- montar(docs, cfg)
#  DEPOIS (Parte D, com o corpus do grupo, no Colab):
#    REPO <- "https://raw.githubusercontent.com/<usuario>/projeto-<grupo>/main/"
#    download.file(paste0(REPO, "estrutura/codigo/config.R"), "config.R")
#    source("config.R")     # cria cfg
#    docs <- readRDS(gzcon(url(paste0(REPO, "estrutura/banco-de-dados/docs.rds"))))
#    ix   <- montar(docs, cfg)
#
#  FUNCOES: tokenizar, docs_aula, matriz_tf, busca_booleana, idf_classico, cfg_aula, montar
#           + estado(), anexar_estado()
#  O que cada uma recebe e devolve: motor/CONTRATO.md
# ==========================================================================

MOTOR_VERSAO <- "motor01b v1 2026-10-01"   # aparece no estado()
.MOTOR_AULA  <- 1.5                           # o motor nao adianta aulas >= esta

# --------------------------------------------------------------------------
# Aula 00 -- R basico: tokenizar
# --------------------------------------------------------------------------

# texto -> vetor de termos: minusculas e quebra em um ou mais espacos.
# (Igual a da Aula 00; aqui tambem descarta o "" que sobra de espaco na ponta.)
tokenizar <- function(texto) {
  t <- unlist(strsplit(tolower(texto), "\\s+"))   # minusculas; quebra em espacos
  t[nzchar(t)]                                    # tira tokens vazios
}

# --------------------------------------------------------------------------
# Aula 01 -- corpus, matriz termo-documento, busca booleana, TF-IDF
# --------------------------------------------------------------------------

# Os 8 documentos do curso (o exemplo base).
docs_aula <- function() c(
  d1 = "recuperacao de informacao ordena documentos por relevancia",
  d2 = "o modelo de espaco vetorial representa documentos como vetores",
  d3 = "bm25 e um modelo probabilistico de ranqueamento de texto",
  d4 = "aprendizado estatistico fundamenta a recuperacao moderna",
  d5 = "o indice invertido acelera a busca em muitos documentos",
  d6 = "embeddings capturam a semantica de palavras e documentos",
  d7 = "a avaliacao mede a relevancia dos resultados da busca",
  d8 = "ciencia de dados combina estatistica e programacao"
)

# lista de tokens (um vetor por documento) -> matriz termos x documentos.
# O factor com levels = vocab garante a mesma linha para o mesmo termo em
# todos os documentos (Aula 01, Modulo 6).
matriz_tf <- function(tokens, vocab = sort(unique(unlist(tokens)))) {
  m <- sapply(tokens, function(t) as.integer(table(factor(t, levels = vocab))))
  matrix(m, nrow = length(vocab),                  # garante matriz mesmo com 1 termo
         dimnames = list(vocab, names(tokens)))    #   ou 1 documento
}

# Quais documentos tem o termo? Sem ordem (Aula 01, Modulo 7).
busca_booleana <- function(termo, tf) {
  termo <- tolower(termo)                                    # mesma normalizacao
  if (!termo %in% rownames(tf)) return(character(0))         # termo fora do vocabulario
  colnames(tf)[tf[termo, ] > 0]                              # colunas com contagem > 0
}

# IDF da Aula 01: log(N / df). Termo em todos os documentos -> 0.
idf_classico <- function(tf) log(ncol(tf) / rowSums(tf > 0))

# As decisoes canonicas da aula, no mesmo formato do config.R dos grupos.
# Os campos aparecem conforme a aula: o motor nao adianta conteudo.
cfg_aula <- function() {
  cfg <- list()
  if (.MOTOR_AULA > 3)                                   # Aula 03: limpeza
    cfg <- c(cfg, list(limpar    = TRUE,
                       acentos   = "manter",
                       stopwords = character(0)))       # os modelos canonicos mantem o "de"
  if (.MOTOR_AULA > 4)                                   # Aula 04: BM25
    cfg <- c(cfg, list(k1 = 1.2, b = 0.75))
  if (.MOTOR_AULA > 5.5)                                 # Aula 5,5: metricas
    cfg <- c(cfg, list(limiar = 2))
  cfg
}

# Tokens de um texto segundo cfg: antes da Aula 03, so tokenizar;
# depois, o pipeline de limpeza e stopwords (preparar).
.tokens_de <- function(texto, cfg) {
  if (.MOTOR_AULA > 3) preparar(texto, cfg) else tokenizar(texto)
}

# Monta, de uma vez, tudo que as aulas anteriores calcularam sobre o corpus.
# Devolve a lista ix. Nada fica solto no ambiente.
montar <- function(docs, cfg = cfg_aula()) {
  if (is.null(names(docs)))                                # documentos sem nome:
    names(docs) <- paste0("d", seq_along(docs))            #   d1, d2, ...
  ix <- list()
  ix$tokens    <- lapply(docs, .tokens_de, cfg = cfg)      # Aula 01 (03: com limpeza)
  ix$vocab     <- sort(unique(unlist(ix$tokens)))          # Aula 01
  ix$tf        <- matriz_tf(ix$tokens, ix$vocab)           # Aula 01: a TDM
  ix$N         <- ncol(ix$tf)                              # Aula 01
  ix$df        <- rowSums(ix$tf > 0)                       # Aula 01
  ix$idf_tfidf <- log(ix$N / ix$df)                        # Aula 01
  ix$w         <- ix$tf * ix$idf_tfidf                     # Aula 01: TF-IDF
  if (.MOTOR_AULA > 2)
    ix$wn <- norm_cols(ix$w)                               # Aula 02: colunas unitarias
  if (.MOTOR_AULA > 3)
    ix$postings <- indice_invertido(ix$tokens)             # Aula 03
  if (.MOTOR_AULA > 4) {
    ix$dl       <- colSums(ix$tf)                          # Aula 04: tamanhos
    ix$avgdl    <- mean(ix$dl)                             # Aula 04
    ix$idf_bm25 <- idf_bm25(ix$tf)                         # Aula 04: nome proprio
  }
  ix
}

# ==========================================================================
#  ESTADO -- fotografia da sessao (motor/estado.R)
# ==========================================================================

# --- marcadores que delimitam a secao gerada no arquivo .md --------------
.ESTADO_INI <- "<!-- estado-R:inicio -->"   # linha que abre a secao
.ESTADO_FIM <- "<!-- estado-R:fim -->"      # linha que fecha a secao


# --- descreve UM objeto em uma linha de tabela ---------------------------
# Devolve um vetor de 3 textos: tipo, tamanho, amostra (o nome vem de fora).
.descrever <- function(x, max_itens = 6) {
  tipo <- class(x)[1]                                   # primeira classe: "numeric", "matrix"...
  if (!is.null(names(x)) && is.atomic(x))               # vetor atomico com nomes?
    tipo <- paste0(tipo, ", nomeado")                   #   avisa: o nome "gruda" (Aula 00)

  tamanho <- if (!is.null(dim(x)))                      # matriz ou data frame:
    paste(dim(x), collapse = " \u00d7 ")                #   "45 x 8" (o x e o sinal de vezes)
  else as.character(length(x))                          # senao, o comprimento

  amostra <- ""                                         # comeca vazia
  if (is.data.frame(x)) {                               # data frame: mostra as colunas
    amostra <- paste("colunas:", paste(names(x), collapse = ", "))
  } else if (is.matrix(x)) {                            # matriz: primeiras linhas e colunas
    amostra <- paste0("linhas: ", .lista(rownames(x), 3),
                      " | colunas: ", .lista(colnames(x), 4))
  } else if (is.atomic(x) && length(x) > 0) {           # vetor de numeros ou textos
    v <- if (is.numeric(x)) signif(x, 4) else x         #   numeros com 4 algarismos
    v <- head(v, max_itens)                             #   so os primeiros
    txt <- if (is.character(v))                         #   textos: corta os longos
      ifelse(nchar(v) > 30, paste0(substr(v, 1, 27), "..."), v)
    else as.character(v)
    if (!is.null(names(v)))                             #   com nomes: "d1 0.254"
      txt <- paste(names(v), txt)
    amostra <- paste(txt, collapse = ", ")              #   tudo numa linha
    if (length(x) > max_itens)                          #   avisa que ha mais
      amostra <- paste0(amostra, ", ...")
  } else if (is.list(x)) {                              # lista que nao foi expandida
    amostra <- paste("elementos:", .lista(names(x), 8))
  }

  amostra <- gsub("|", "\\|", amostra, fixed = TRUE)    # | quebraria a tabela Markdown
  amostra <- gsub("\n", " ", amostra, fixed = TRUE)     # quebra de linha tambem
  c(tipo, tamanho, amostra)                             # os tres campos
}


# --- junta os primeiros n nomes com virgula ------------------------------
.lista <- function(nomes, n) {
  if (is.null(nomes)) return("(sem nomes)")             # matriz sem dimnames
  out <- paste(head(nomes, n), collapse = ", ")         # os primeiros n
  if (length(nomes) > n) out <- paste0(out, ", ...")    # e reticencias se houver mais
  out
}


# --- estado(): a fotografia da sessao, em Markdown -----------------------
# Devolve (invisivel) as linhas e as imprime na tela.
# Listas como cfg e ix sao abertas um nivel: cfg$k1, ix$vocab...
estado <- function(env = globalenv(), max_itens = 6, imprimir = TRUE) {
  nomes <- sort(ls(env))                                # todos os objetos visiveis
  objs  <- mget(nomes, envir = env)                     # os proprios objetos

  e_funcao <- vapply(objs, is.function, logical(1))     # quais sao funcoes
  funcoes  <- nomes[e_funcao]                           # vao para uma lista a parte
  dados    <- objs[!e_funcao]                           # o resto vai para a tabela

  linhas <- c("| objeto | tipo | tamanho | valor / amostra |",   # cabecalho da tabela
              "|---|---|---|---|")
  for (nm in names(dados)) {                            # para cada objeto...
    x <- dados[[nm]]
    if (is.list(x) && !is.data.frame(x) && !is.null(names(x))) {
      for (el in names(x)) {                            # lista nomeada: abre um nivel
        d <- .descrever(x[[el]], max_itens)
        linhas <- c(linhas, sprintf("| `%s$%s` | %s | %s | %s |",
                                    nm, el, d[1], d[2], d[3]))
      }
    } else {                                            # objeto comum: uma linha
      d <- .descrever(x, max_itens)
      linhas <- c(linhas, sprintf("| `%s` | %s | %s | %s |",
                                  nm, d[1], d[2], d[3]))
    }
  }
  if (length(dados) == 0)                               # sessao vazia
    linhas <- c(linhas, "| (nenhum objeto) | | | |")

  versao_motor <- if (exists("MOTOR_VERSAO", envir = env))   # o motor declara a versao
    get("MOTOR_VERSAO", envir = env) else "sem motor"

  out <- c(
    .ESTADO_INI,
    "## Estado do R ao fim da sess\u00e3o",
    sprintf("*gerado por `anexar_estado()` em %s \u00b7 %s \u00b7 %s \u00b7 n\u00e3o edite \u00e0 m\u00e3o*",
            format(Sys.time(), "%Y-%m-%d %H:%M"),       # quando
            R.version.string,                           # qual R
            versao_motor),                              # qual motor
    "",
    "### Objetos",
    "",
    linhas,
    "",
    "### Fun\u00e7\u00f5es definidas",
    "",
    if (length(funcoes)) paste0("`", funcoes, "`", collapse = ", ") else "(nenhuma)",
    .ESTADO_FIM
  )

  if (imprimir) cat(out, sep = "\n")                    # mostra na tela
  invisible(out)                                        # e devolve, para anexar_estado()
}


# --- anexar_estado(): grava a fotografia no fim do arquivo .md -----------
anexar_estado <- function(arquivo, env = globalenv()) {
  if (!file.exists(arquivo))                            # o consolidado tem que existir antes
    stop("arquivo n\u00e3o encontrado: ", arquivo,
         "\nSalve primeiro o consolidado que a tutora gerou, depois rode de novo.",
         "\nVoc\u00ea est\u00e1 em: ", getwd(), call. = FALSE)

  txt <- readLines(arquivo, encoding = "UTF-8", warn = FALSE)   # o arquivo como esta

  ini <- which(txt == .ESTADO_INI)                      # ja havia uma secao gerada?
  fim <- which(txt == .ESTADO_FIM)
  if (length(ini) && length(fim) && fim[1] > ini[1]) {  # sim: apaga a antiga
    txt <- txt[-(ini[1]:fim[1])]
    while (length(txt) && txt[length(txt)] %in% c("", "---"))   # e o separador e as linhas
      txt <- txt[-length(txt)]                                  #   vazias que sobraram
  }

  bloco <- estado(env = env, imprimir = FALSE)          # a fotografia de agora
  novo  <- c(txt, "", "---", "", bloco, "")             # anexa no fim, separada por ---

  # grava os bytes em UTF-8 sem reconverter: preserva acentos, travessoes,
  # simbolos do LaTeX do consolidado, em qualquer versao do R no Windows
  writeLines(enc2utf8(novo), arquivo, useBytes = TRUE)

  n_obj <- sum(startsWith(bloco, "| `"))                # quantas linhas de objeto
  message("Estado do R anexado a ", arquivo, " (", n_obj, " linhas de objetos).")
  invisible(arquivo)
}
