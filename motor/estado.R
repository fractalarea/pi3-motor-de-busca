# ==========================================================================
#  estado.R  --  fotografa a sessao do R e anexa ao consolidado (ou a ficha)
#  Projeto Integrador III -- Motor de Busca -- Fatec Baixada Santista
# ==========================================================================
#
#  POR QUE EXISTE
#    A LLM tutora nao ve o R. Para a proxima sessao saber que objetos existem,
#    com que nomes e valores, quem escreve isso e o R -- nao a LLM, que erraria
#    os valores de memoria.
#
#  COMO USAR, NO FIM DA SESSAO
#    1. A tutora gera o consolidado; voce salva como, por exemplo,
#         aula04_consolidado.md   e sobe o arquivo ao Colab (/content)
#    2. No MESMO R em que trabalhou (sem fechar), rode:
#         anexar_estado("aula04_consolidado.md")
#       (os motorNN.R ja trazem estas funcoes; so na Aula 00, sem motor:
#        source("https://raw.githubusercontent.com/fractalarea/pi3-motor-de-busca/main/motor/estado.R"))
#    3. Baixe o arquivo: no fim apareceu a secao "Estado do R".
#       Na Parte D, faca o mesmo com a ficha:
#         anexar_estado("00_FICHA_PROJETO.md")
#
#  COMO USAR, NO INICIO DA SESSAO SEGUINTE
#    Depois de carregar tudo, rode  estado()  e compare o que aparece na
#    tela com a secao "Estado do R" do consolidado (ou da ficha).
#
#  Rodar anexar_estado() de novo no mesmo arquivo SUBSTITUI a secao antiga
#  (nao duplica). O resto do arquivo nao e tocado.
#
#  So R base. Strings sem acento de proposito (acentos em \u escape), para
#  funcionar em qualquer locale do Windows.
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
