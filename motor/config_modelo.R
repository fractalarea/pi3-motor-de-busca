# ==========================================================================
#  config.R  --  as decisoes do grupo, para o R
#  Copie para  estrutura/codigo/config.R  no repositorio do grupo.
# ==========================================================================
#
#  A ficha do projeto diz as decisoes em prosa (e o porque).
#  Este arquivo diz as mesmas decisoes para o R. Os dois tem que bater.
#
#  Cada Parte D acrescenta um campo. Nao apague campos antigos: se uma
#  decisao mudar, mude o valor e registre na ficha (secao 6), com o motivo.
#
#  Sem acento nas strings (locale do Colab/Windows).
# ==========================================================================

cfg <- list(
  # --- Aula 01: o que e um documento ------------------------------------
  minimo = 200,                 # paragrafo com mais de 200 caracteres

  # --- Aula 03: limpeza e stopwords (descomente quando decidirem) -------
  # limpar    = TRUE,           # usar a limpeza da Aula 03
  # acentos   = "translit",     # "manter" (acento vira espaco) ou "translit" (i, c, a...)
  # stopwords = c("de", "o", "a", "e", "em", "dos", "com", "para"),

  # --- Aula 04: BM25 ------------------------------------------------------
  # k1 = 1.2,
  # b  = 0.75,

  # --- Aula 5,5: metricas -------------------------------------------------
  # limiar = 2,                 # grau minimo para contar como relevante
  # juizes = c("ana", "bruno"), # quem vale no gabarito, em ordem de prioridade (ler_qrels)

  # --- Aula 06: realimentacao -------------------------------------------
  # prf_k   = 0,                            # 0 = sem pseudo-realimentacao; k = usa os top-k
  # rocchio = c(a = 1, b = 0.75, g = 0.15), # pesos do Rocchio

  # --- Aula 07: LSA e busca hibrida --------------------------------------
  # modelo = "bm25",                        # "bm25", "lsa" ou "hibrido" (RRF de bm25 e lsa)
  # lsa_k  = 20,                            # dimensoes do LSA
  # rrf_c  = 60,                            # constante da fusao RRF

  # --- Aula 08: reordenacao (rerank) ------------------------------------
  # rerank_k = 0,                           # quantos do topo o reordenador reordena (0 = nao)
  # janela   = 3,                           # distancia maxima da proximidade ordenada

  # --- fim ----------------------------------------------------------------
  grupo = "<nome do grupo>"     # ultimo campo, sem virgula depois
)
