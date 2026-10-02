# PI3 — Motor de Busca · Fatec Baixada Santista

Material de apoio da disciplina **Projeto Integrador III** (Tecnologia em Ciência de Dados).

## O motor

A cada aula você recebe um `motorNN.R` com as funções de todas as aulas **anteriores**. Ele é a base comum: o que a turma já construiu, numa versão única e testada. O código da aula do dia, você escreve na sessão.

No Colab (ambiente **R**), a primeira célula de toda sessão é:

```r
source("https://raw.githubusercontent.com/fractalarea/pi3-motor-de-busca/main/motor/motor04.R")
```

troque `04` pelo número da aula. Na Aula 1,5 é `motor01b`; na 5,5, `motor05b`.

| arquivo | o que é |
|---|---|
| `motor/motorNN.R` | as funções das aulas anteriores a NN, mais `estado()` e `anexar_estado()` |
| `motor/estado.R` | só `estado()` e `anexar_estado()` — para a Aula 00, que não tem motor |
| `motor/CONTRATO.md` | o que cada função recebe e devolve, e a lista `ix` que `montar()` cria |
| `motor/config_modelo.R` | modelo do `config.R` do grupo (as decisões do projeto, para o R) |

## No fim de cada sessão

1. Salve o consolidado que a tutora gerou como `.md`.
2. Suba o arquivo no Colab (pasta à esquerda → upload).
3. Rode `anexar_estado("nome_do_arquivo.md")` — o R acrescenta ao consolidado a fotografia da sessão.
4. Baixe o arquivo de volta e envie ao repositório do seu grupo.

Os arquivos do Colab somem quando a sessão cai: baixe antes de fechar.
