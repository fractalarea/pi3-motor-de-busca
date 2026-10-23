# Contrato do motor

*O que cada `motorNN.R` define: nome, o que recebe, o que devolve, de que aula vem. É esta tabela — e não o código — que os guias de estudo mostram à LLM tutora. v1 — 2026-10-01.*

**Regra:** o `motorNN.R` traz as funções das aulas **anteriores** a NN. O código da aula NN é construído na sessão.

| motor | para a aula | traz as aulas |
|---|---|---|
| `motor01.R` | 01 | 00 |
| `motor01b.R` | 1,5 | 00, 01 |
| `motor02.R` | 02 | 00, 01 (a 1,5 não acrescenta função) |
| `motor03.R` | 03 | 00–02 |
| `motor04.R` | 04 | 00–03 |
| `motor05.R` | 05 | 00–04 |
| `motor05b.R` | 5,5 | 00–05 |
| `motor06.R` | 06 | 00–5,5 |
| `motor07.R` | 07 | 00–06 |
| `motor08.R` | 08 | 00–07 |

Todos trazem também `estado()` e `anexar_estado()`.

---

## Como toda sessão começa

**Teoria** (a partir da Aula 1,5):

```r
source("https://raw.githubusercontent.com/fractalarea/pi3-motor-de-busca/main/motor/motorNN.R")   # as funções do curso
docs <- docs_aula()              # os 8 documentos
cfg  <- cfg_aula()               # as decisões canônicas
ix   <- montar(docs, cfg)        # tudo que as aulas anteriores calcularam
```

**Parte D** (a partir da Aula 02):

```r
source("https://raw.githubusercontent.com/fractalarea/pi3-motor-de-busca/main/motor/motorNN.R")   # as funções do curso
REPO <- "https://raw.githubusercontent.com/<usuario>/projeto-<grupo>/main/"           # o repositório do grupo (público)
download.file(paste0(REPO, "estrutura/codigo/config.R"), "config.R")
source("config.R")                                                               # cfg: as decisões do grupo
docs <- readRDS(gzcon(url(paste0(REPO, "estrutura/banco-de-dados/docs.rds"))))  # o corpus do grupo
ix   <- montar(docs, cfg)                                                        # tudo derivado
```

A partir da Parte D da Aula 5,5, o corpus vem do arquivo congelado: `cp <- read.csv(paste0(REPO, "estrutura/banco-de-dados/corpus.csv"), fileEncoding = "UTF-8"); docs <- setNames(cp$texto, cp$id)`.

Quem tem o repositório clonado (RStudio) pode trocar o endereço pelo caminho local: `source("estrutura/codigo/config.R")`, `readRDS("estrutura/banco-de-dados/docs.rds")`.

---

## As funções

| função | desde | recebe | devolve | aula |
|---|---|---|---|---|
| `tokenizar(texto)` | motor01 | um texto | vetor de termos (minúsculas, quebra em espaços) | 00 |
| `docs_aula()` | motor01b | — | os 8 documentos, vetor nomeado `d1`…`d8` | 01 |
| `matriz_tf(tokens, vocab)` | motor01b | lista de tokens por documento | matriz termos × documentos (a TDM) | 01 |
| `busca_booleana(termo, tf)` | motor01b | um termo e a matriz | nomes dos documentos que o têm | 01 |
| `idf_classico(tf)` | motor01b | a matriz | $\log(N/\text{df})$ por termo | 01 |
| `cfg_aula()` | motor01b | — | as decisões canônicas (campos conforme a aula) | — |
| `montar(docs, cfg)` | motor01b | corpus e decisões | a lista `ix` (abaixo) | — |
| `cosseno(a, b)` | motor03 | dois vetores | o cosseno; **0** se um vetor é nulo | 02 |
| `norm_cols(m)` | motor03 | uma matriz | colunas com norma 1 | 02 |
| `vetor_consulta(termos, vocab, idf)` | motor03 | termos da consulta | vetor de pesos no espaço do corpus | 02 |
| `ranking_cosseno(consulta, ix, cfg)` | motor03 | a consulta (texto) | todos os documentos, do maior cosseno ao menor | 02 |
| `stopwords_aula()` | motor04 | — | as 10 stopwords da aula | 03 |
| `limpar(x, acentos)` | motor04 | texto; `"manter"` ou `"translit"` | texto limpo | 03 |
| `sem_stop(texto, stopwords, acentos)` | motor04 | texto | tokens limpos, sem stopwords | 03 |
| `preparar(texto, cfg)` | motor04 | texto e decisões | tokens segundo o `cfg` (limpeza + stopwords) | 03 |
| `indice_invertido(tokens)` | motor04 | lista de tokens | lista termo → documentos | 03 |
| `busca_AND(consulta, ix, cfg)` | motor04 | a consulta | documentos com **todos** os termos | 03 |
| `idf_bm25(tf)` | motor05 | a matriz | IDF do BM25 por termo | 04 |
| `saturacao(f, k1)` | motor05 | frequência(s) | $\frac{f(k_1+1)}{f+k_1}$ | 04 (na aula, `sat`) |
| `bm25(termos, ix, k1, b)` | motor05 | termos já preparados | escore BM25 de cada documento; termo ausente vale **0** (sem `NaN` com $k_1 = 0$) | 04 |
| `bm25_doc(termos, d, ix, k1, b)` | motor05 | termos e um documento | o escore desse documento | 04 (na aula, `bm25_doc(termos, d, k1, b)`, lendo `ix`, `dl`, `avgdl`, `idf_b` da sessão; no motor, `ix` é argumento) |
| `ranking_bm25(consulta, ix, cfg)` | motor05 | a consulta (texto) | todos os documentos, do maior BM25 ao menor (`k1`, `b` do `cfg`) | 04 |
| `gabarito_aula()` | motor05b | — | graus canônicos: d2 = 2, d3 = 2, d1 = 1, d6 = 1, resto 0 | 05 |
| `kappa_matriz(m)` | motor05b | matriz de concordância | `po`, `pe`, `kappa` | 05 |
| `kappa_cohen(a, b, niveis)` | motor05b | dois vetores de notas pareados | a matriz e as medidas | 05 |
| `ler_qrels(arquivos, juiz)` | motor05b | um ou mais CSV do `julgar.html` | lista consulta → graus; tira `_p2`; lê BOM; com vários juízes, pede a prioridade | 05 |
| `relevancia(ranking, grau, limiar)` | motor06 | ranking e graus | `rel` (0/1), `g` (graus na ordem), `R` | 5,5 |
| `precisao_k(rel, k)` · `recall_k(rel, k, R)` | motor06 | | P@k, R@k | 5,5 |
| `ap(rel, R)` · `rr(rel)` | motor06 | | AP; 1 / posição do primeiro relevante | 5,5 |
| `dcg(g)` · `ndcg(g, ideal)` | motor06 | | DCG; nDCG | 5,5 |
| `metricas(ranking, grau, limiar, k = 3)` | motor06 | ranking, graus e o limiar (obrigatório) | vetor nomeado `P_at_k`, `AP`, `RR`, `nDCG_bin`, `nDCG_grad` — igual à função da Parte D da Aula 5,5 | 5,5 |
| `rocchio(q, Dr, Dnr, ix, a, b, g)` | motor07 | vetor da consulta e nomes dos marcados | a consulta modificada $\alpha q + \beta \bar d_r - \gamma \bar d_{nr}$ | 06 |
| `ranking_vetor(q, ix)` | motor07 | um **vetor** de consulta | todos os documentos, do maior cosseno (com `ix$w`) ao menor | 06 |
| `prf(q0, k, ix, p)` | motor07 | vetor, $k$, pesos `c(a, b, g)` | o ranking depois da pseudo-realimentação com os top-$k$ | 06 |
| `residual(rk, g, fora, limiar)` | motor07 | ranking, graus, documentos marcados | as métricas sem os marcados (coleção residual) | 06 |
| `lsa(ix, k)` | motor08 | índice e número de dimensões | lista `U` (termos × k), `S`, `D` (documentos × k) | 07 |
| `ranking_lsa(consulta, ix, cfg, modelo)` | motor08 | consulta (texto) e o que `lsa` devolve | documentos do maior cosseno ao menor, no espaço LSA | 07 |
| `rrf(..., c = 60)` | motor08 | dois ou mais rankings | a fusão RRF, do maior escore ao menor | 07 |
| `estado()` | todos | — | a fotografia da sessão, em Markdown, na tela | — |
| `anexar_estado(arquivo)` | todos | caminho do `.md` | anexa (ou substitui) a seção "Estado do R" no fim do arquivo | — |

---

## A lista `ix` (o que `montar` devolve)

| campo | desde | o que é | na aula, era |
|---|---|---|---|
| `ix$tokens` | motor01b | lista de tokens por documento | `tokens` (Aula 01) |
| `ix$vocab` | motor01b | termos distintos, ordenados | `vocab` |
| `ix$tf` | motor01b | matriz termos × documentos | `tdm` (Aula 01), `tf` (Aula 04) |
| `ix$N` | motor01b | número de documentos | `N` |
| `ix$df` | motor01b | em quantos documentos cada termo aparece | `df` |
| `ix$idf_tfidf` | motor01b | $\log(N/\text{df})$ | `idf` (Aulas 01–02) |
| `ix$w` | motor01b | TF-IDF | `tfidf` (Aula 01), `w` (Aula 02) |
| `ix$wn` | motor03 | TF-IDF com colunas unitárias | `wn` (Aula 02) |
| `ix$postings` | motor04 | índice invertido | `postings` (Aula 03) |
| `ix$dl`, `ix$avgdl` | motor05 | tamanho de cada documento; a média | `dl`, `avgdl` (Aula 04) |
| `ix$idf_bm25` | motor05 | IDF do BM25 | `idf_b` (Aula 04) — **nome diferente de propósito** |

## A lista `cfg` (decisões)

| campo | desde | canônico (`cfg_aula()`) | do grupo (`config.R`) |
|---|---|---|---|
| `limpar` | motor04 | `TRUE` | decidido na Aula 03 |
| `acentos` | motor04 | `"manter"` | `"manter"` ou `"translit"` |
| `stopwords` | motor04 | vazio — os modelos canônicos mantêm o `de` | a lista do grupo |
| `k1`, `b` | motor05 | 1,2 e 0,75 | decididos na Aula 04 |
| `limiar` | motor06 | 2 | decidido na Aula 5,5 |
| `juizes` | — (só do grupo) | — | decidido na Aula 5,5: quem vale no gabarito, em ordem de prioridade; usado em `ler_qrels(arquivos, juiz = cfg$juizes)` |
| `prf_k`, `rocchio` | motor07 | `0` e `c(a = 1, b = 0.75, g = 0.15)` | decididos na Aula 06: pseudo-realimentação (0 = não usar) e pesos $\alpha, \beta, \gamma$ |
| `modelo`, `lsa_k`, `rrf_c` | motor08 | `"bm25"`, `2` e `60` | decididos na Aula 07: `"bm25"`, `"lsa"` ou `"hibrido"` (RRF de BM25 e LSA); dimensões do LSA; constante da RRF |
| `rerank_k` | — (só do grupo, até haver motor09) | — | decidido na Aula 08: quantos do topo o reordenador de proximidade reordena (0 = não reordenar) |
| `janela` | — (só do grupo, até haver motor09) | — | decidida na Aula 08: distância máxima da proximidade ordenada |
| `minimo`, `grupo` | — (só do grupo) | — | Aula 01: critério de documento (caracteres) e nome do grupo; o `montar` os ignora |

Os números canônicos do índice invertido (37 termos) usam `stopwords_aula()`: `montar(docs_aula(), modifyList(cfg_aula(), list(stopwords = stopwords_aula())))`. Os do cosseno e do BM25 usam `cfg_aula()` como está.
