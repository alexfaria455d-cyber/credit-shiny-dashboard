# Credit — dashboard Shiny

Dashboard para exploração estatística do conjunto de dados `Credit` do pacote [ISLR](https://cran.r-project.org/package=ISLR) (400 clientes, saldo do cartão de crédito e características sociodemográficas).

Trabalho desenvolvido no âmbito do Mestrado em Estatística para a Ciência de Dados (Universidade do Minho).

## O que faz

- **Visão geral**: número de clientes, dívida média e mediana, rácio dívida/limite e tabela dos dados.
- **Análise**: histograma com curva de densidade, boxplot por fator e gráfico de dispersão com reta de regressão (opcionalmente por grupo).
- **Testes de hipótese**: teste t (Welch ou variâncias iguais), ANOVA a um fator e teste do qui-quadrado, com tabela de contingência.

Todos os separadores respeitam os filtros da barra lateral (género, estudante, estado civil, etnia, idade, rendimento e limite de crédito). Quando os filtros deixam um teste sem condições de ser executado, a app explica porquê em vez de falhar.

## Executar

```r
install.packages(c("shiny", "shinydashboard", "ISLR", "dplyr", "ggplot2", "DT", "broom"))
shiny::runApp()
```

## Licença

MIT
