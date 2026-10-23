# ==========================================================================
#  motor07.R  --  Projeto Integrador III -- Motor de Busca
#  Para a Aula 07: as funcoes das aulas ANTERIORES (00, 01, 02, 03, 04, 05, 5,5, 06).
#  Gerado de motor_mestre.R em 2026-10-15. NAO EDITE ESTE ARQUIVO: edite o mestre.
# ==========================================================================
#
#  COMO CARREGAR
#    Colab:    source("https://raw.githubusercontent.com/fractalarea/pi3-motor-de-busca/main/motor/motor07.R")
#    RStudio:  source("motor07.R")
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
#  FUNCOES: tokenizar, docs_aula, matriz_tf, busca_booleana, idf_classico, cfg_aula, montar, cosseno, norm_cols, vetor_consulta, ranking_cosseno, stopwords_aula, limpar, sem_stop, preparar, indice_invertido, busca_AND, idf_bm25, saturacao, bm25, bm25_doc, ranking_bm25, gabarito_aula, kappa_matriz, kappa_cohen, ler_qrels, relevancia, precisao_k, recall_k, ap, rr, dcg, ndcg, metricas, rocchio, ranking_vetor, prf, residual
#           + estado(), anexar_estado()
#  O que cada uma recebe e devolve: motor/CONTRATO.md
# ==========================================================================

MOTOR_VERSAO <- "motor07 v1 2026-10-15"   # aparece no estado()
.MOTOR_AULA  <- 7                           # o motor nao adianta aulas >= esta

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
  if (.MOTOR_AULA > 6)                                   # Aula 06: realimentacao
    cfg <- c(cfg, list(prf_k = 0,                        # 0 = sem pseudo-realimentacao
                       rocchio = c(a = 1, b = 0.75, g = 0.15)))
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

# --------------------------------------------------------------------------
# Aula 02 -- espaco vetorial e cosseno
# --------------------------------------------------------------------------

# Cosseno entre dois vetores. Vetor nulo (consulta fora do vocabulario,
# documento vazio): devolve 0 em vez de 0/0 = NaN.
cosseno <- function(a, b) {
  den <- sqrt(sum(a^2)) * sqrt(sum(b^2))      # produto das normas
  if (den == 0) return(0)                     # nada em comum
  sum(a * b) / den                            # a formula do Modulo 4
}

# Divide cada coluna pela sua norma (coluna nula fica nula).
norm_cols <- function(m) {
  nrm <- sqrt(colSums(m^2))                   # norma de cada coluna
  nrm[nrm == 0] <- 1                          # evita dividir por zero
  sweep(m, 2, nrm, "/")
}

# Termos da consulta -> vetor de pesos no espaco do corpus (Aula 02, Modulo 6).
vetor_consulta <- function(termos, vocab, idf) {
  q <- as.integer(table(factor(termos, levels = vocab)))   # contagens; fora do vocab some
  setNames(q * idf, vocab)                                 # a regua do corpus
}

# Ranking por cosseno: todos os documentos, do mais parecido ao menos.
ranking_cosseno <- function(consulta, ix, cfg = cfg_aula()) {
  qw <- vetor_consulta(.tokens_de(consulta, cfg), ix$vocab, ix$idf_tfidf)
  s  <- apply(ix$w, 2, function(d) cosseno(qw, d))       # um cosseno por coluna
  sort(s, decreasing = TRUE)
}

# --------------------------------------------------------------------------
# Aula 03 -- limpeza, stopwords, indice invertido
# --------------------------------------------------------------------------

# A lista curta da aula.
stopwords_aula <- function() c("de", "o", "a", "e", "um", "por", "como", "que", "da", "do")

# Limpeza da Aula 03. acentos = "manter": acento vira espaco (o
# comportamento da aula); "translit": i com acento vira i, c cedilha vira c.
limpar <- function(x, acentos = "manter") {
  if (identical(acentos, "translit")) {
    y <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")  # tira os acentos
    ok <- !is.na(y)                                        # iconv falhou? mantem o original
    x[ok] <- y[ok]
  }
  x <- tolower(x)                       # 1) minusculas
  x <- gsub("[^a-z0-9 ]", " ", x)       # 2) o que nao e letra, digito ou espaco vira espaco
  x <- gsub("\\s+", " ", x)             # 3) espacos repetidos viram um
  trimws(x)                             # 4) tira espacos das pontas
}

# Texto -> tokens limpos, sem stopwords (Aula 03).
sem_stop <- function(texto, stopwords = stopwords_aula(), acentos = "manter") {
  t <- unlist(strsplit(limpar(texto, acentos), " "))    # limpa e quebra
  t <- t[nzchar(t)]                                     # sem vazios
  t[!t %in% stopwords]                                  # fica quem NAO esta na lista
}

# O pipeline inteiro segundo as decisoes (cfg do grupo ou cfg_aula()).
preparar <- function(texto, cfg = cfg_aula()) {
  acentos <- if (is.null(cfg$acentos)) "manter" else cfg$acentos
  t <- if (isTRUE(cfg$limpar))
         unlist(strsplit(limpar(texto, acentos), " "))   # limpeza da Aula 03
       else tokenizar(texto)                             # ou so a da Aula 01
  t <- t[nzchar(t)]
  t[!t %in% cfg$stopwords]                               # sem stopwords (se houver)
}

# Indice invertido: termo -> documentos que o tem (Aula 03, Modulo 8).
indice_invertido <- function(tokens) {
  postings <- list()
  for (d in names(tokens))                               # para cada documento...
    for (termo in unique(tokens[[d]]))                   # ...cada termo distinto dele...
      postings[[termo]] <- c(postings[[termo]], d)       # ...anexa o documento
  postings
}

# Busca AND: documentos com TODOS os termos (Aula 03, Modulo 9).
busca_AND <- function(consulta, ix, cfg = cfg_aula()) {
  termos <- unique(.tokens_de(consulta, cfg))            # a mesma limpeza do indice
  if (!length(termos) || !all(termos %in% names(ix$postings)))
    return(character(0))                                 # algum termo nao existe: vazio
  Reduce(intersect, ix$postings[termos])                 # intersecta as listas
}

# --------------------------------------------------------------------------
# Aula 04 -- BM25
# --------------------------------------------------------------------------

# IDF do BM25 (Robertson-Sparck Jones suavizado, com +1).
idf_bm25 <- function(tf) {
  N  <- ncol(tf)
  df <- rowSums(tf > 0)
  log((N - df + 0.5) / (df + 0.5) + 1)
}

# A saturacao, sem tamanho (Aula 04, Modulo 9; na aula, sat).
saturacao <- function(f, k1 = 1.2) (f * (k1 + 1)) / (f + k1)

# BM25 de todos os documentos para uma lista de termos.
# Termo ausente do documento contribui 0 -- inclusive com k1 = 0,
# onde a formula daria 0/0 (propriedade w(0) = 0 do Modulo 6).
bm25 <- function(termos, ix, k1 = 1.2, b = 0.75) {
  s <- setNames(numeric(ix$N), colnames(ix$tf))          # um escore por documento
  K <- k1 * (1 - b + b * ix$dl / ix$avgdl)               # o K de cada documento
  for (t in termos) {                                    # soma sobre os termos
    if (!t %in% rownames(ix$tf)) next                    # fora do vocabulario
    f <- ix$tf[t, ]                                      # frequencia em cada documento
    parcela <- ix$idf_bm25[[t]] * (f * (k1 + 1)) / (f + K)
    parcela[f == 0] <- 0                                 # ausente: 0, nunca NaN
    s <- s + parcela
  }
  s
}

# O escore de um documento so (a bm25_doc da aula, sem globais).
bm25_doc <- function(termos, d, ix, k1 = 1.2, b = 0.75) bm25(termos, ix, k1, b)[[d]]

# Ranking BM25 com os parametros do cfg.
ranking_bm25 <- function(consulta, ix, cfg = cfg_aula()) {
  k1 <- if (is.null(cfg$k1)) 1.2 else cfg$k1
  b  <- if (is.null(cfg$b))  0.75 else cfg$b
  sort(bm25(.tokens_de(consulta, cfg), ix, k1, b), decreasing = TRUE)
}

# --------------------------------------------------------------------------
# Aula 05 -- relevancia, gabarito, kappa
# --------------------------------------------------------------------------

# O gabarito canonico da consulta "modelo de recuperacao".
gabarito_aula <- function() c(d1 = 1, d2 = 2, d3 = 2, d4 = 0, d5 = 0, d6 = 1, d7 = 0, d8 = 0)

# po, pe e kappa a partir da matriz de concordancia.
kappa_matriz <- function(m) {
  n  <- sum(m)
  po <- sum(diag(m)) / n                                 # observada: a diagonal
  pe <- sum(rowSums(m) * colSums(m)) / n^2               # esperada: as marginais
  k  <- if (pe < 1) (po - pe) / (1 - pe) else NA         # pe = 1: indefinido
  c(po = po, pe = pe, kappa = k)
}

# kappa a partir de dois vetores de notas, pareados.
kappa_cohen <- function(a, b, niveis = 0:2) {
  m <- table(factor(a, levels = niveis), factor(b, levels = niveis))
  list(matriz = m, medidas = kappa_matriz(m))
}

# Le um ou mais qrels_*.csv do julgar.html e devolve, por consulta, o
# vetor nomeado documento -> grau. A segunda passada (_p2) nao entra.
# juiz: um nome, ou varios em ordem de prioridade (se dois julgaram o
# mesmo item, vale o primeiro da lista).
ler_qrels <- function(arquivos, juiz = NULL) {
  ler <- function(f) read.csv(f, stringsAsFactors = FALSE,
                              fileEncoding = "UTF-8-BOM")   # o julgar.html grava com BOM
  q <- do.call(rbind, lapply(arquivos, ler))
  q$juiz <- trimws(q$juiz)
  q <- q[!grepl("_p2$", q$juiz), ]                      # so a primeira passada
  juizes <- unique(q$juiz)
  if (is.null(juiz)) {
    if (length(juizes) > 1)
      stop("ha mais de um juiz: ", paste(juizes, collapse = ", "),
           "\nDiga qual vale, ou a ordem de prioridade, em juiz = c(...)", call. = FALSE)
    juiz <- juizes
  }
  q <- q[q$juiz %in% juiz, ]
  q <- q[order(match(q$juiz, juiz)), ]                   # prioridade
  q <- q[!duplicated(paste(q$consulta, q$documento)), ]  # um grau por item
  split(setNames(as.numeric(q$grau), q$documento), q$consulta)
}

# --------------------------------------------------------------------------
# Aula 5,5 -- metricas
# --------------------------------------------------------------------------

# Ranking (nomes, ou o vetor de escores ordenado) + graus -> rel, g e R.
relevancia <- function(ranking, grau, limiar = 2) {
  ids <- if (is.character(ranking)) ranking else names(ranking)
  g <- as.numeric(grau[ids])                             # grau de cada posicao
  g[is.na(g)] <- 0                                       # nao julgado = 0 (pooling)
  list(rel = as.integer(g >= limiar), g = g, R = sum(grau >= limiar))
}

precisao_k <- function(rel, k) sum(head(rel, k)) / k                    # P@k
recall_k   <- function(rel, k, R) if (R == 0) NA else sum(head(rel, k)) / R   # R@k
ap <- function(rel, R) {                                                 # AP
  if (R == 0) return(NA)                                 # consulta sem relevante
  p <- cumsum(rel) / seq_along(rel)                      # precisao em cada posicao
  sum(p[rel == 1]) / R                                   # media nos relevantes, sobre R
}
rr <- function(rel) {                                                    # 1 / posicao do 1o
  i <- which(rel == 1)
  if (length(i)) 1 / i[1] else 0
}
dcg  <- function(g) sum(g / log2(seq_along(g) + 1))                      # ganho descontado
ndcg <- function(g, ideal = sort(g, decreasing = TRUE)) {
  d <- dcg(ideal)
  if (d == 0) NA else dcg(g) / d
}

# As cinco metricas de um ranking contra os graus.
# Mesmo nome, argumentos e saida da funcao escrita na Parte D da Aula 5,5.
metricas <- function(ranking, grau, limiar, k = 3) {
  r <- relevancia(ranking, grau, limiar)
  c(P_at_k = precisao_k(r$rel, k), AP = ap(r$rel, r$R), RR = rr(r$rel),
    nDCG_bin = ndcg(r$rel), nDCG_grad = ndcg(r$g))
}

# --------------------------------------------------------------------------
# Aula 06 -- realimentacao de relevancia
# --------------------------------------------------------------------------

# Rocchio: q (vetor no espaco de ix$vocab), Dr e Dnr (nomes de documentos).
rocchio <- function(q, Dr, Dnr = character(0), ix, a = 1, b = 0.75, g = 0.15) {
  cr  <- rowMeans(ix$w[, Dr, drop = FALSE])                # centroide dos relevantes
  cnr <- if (length(Dnr)) rowMeans(ix$w[, Dnr, drop = FALSE]) else 0
  a * q + b * cr - g * cnr                                 # a formula, termo a termo
}

# Ranking de um VETOR de consulta (nao de um texto) pelo cosseno com ix$w.
ranking_vetor <- function(q, ix)
  sort(apply(ix$w, 2, function(d) cosseno(q, d)), decreasing = TRUE)

# Pseudo-realimentacao: os top-k do ranking base viram Dr, sem Dnr.
prf <- function(q0, k, ix, p = c(a = 1, b = 0.75, g = 0.15)) {
  top <- names(ranking_vetor(q0, ix))[1:k]
  ranking_vetor(rocchio(q0, top, ix = ix, a = p[["a"]], b = p[["b"]], g = p[["g"]]), ix)
}

# Metricas na colecao residual: tira 'fora' do ranking e do gabarito.
residual <- function(rk, g, fora, limiar)
  metricas(rk[!names(rk) %in% fora], g[!names(g) %in% fora], limiar)

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
