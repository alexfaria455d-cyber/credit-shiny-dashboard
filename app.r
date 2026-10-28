# app.R
# Dashboard Shiny - ISLR::Credit com exploração + testes de hipótese
# Instalar (se precisares):
# install.packages(c("shiny","shinydashboard","ISLR","dplyr","ggplot2","DT","broom"))

library(shiny)
library(shinydashboard)
library(ISLR)
library(dplyr)
library(ggplot2)
library(DT)
library(broom)

data("Credit")

num_vars <- setdiff(names(Credit)[sapply(Credit, is.numeric)], "ID")
fac_vars <- names(Credit)[sapply(Credit, is.factor)]

# ----------------------------- UI ------------------------------------

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "Credit"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Visão Geral", tabName = "sumario", icon = icon("sliders")),
      menuItem("Análise", tabName = "analise", icon = icon("magnifying-glass")),
      menuItem("Testes de hipótese", tabName = "testes", icon = icon("flask")),
      hr(),
      # filtros globais
      selectInput("gender",  "Género",  choices = c("Todos", levels(Credit$Gender)),  selected = "Todos"),
      selectInput("student", "Estudante?", choices = c("Sim/Não", levels(Credit$Student)), selected = "Sim/Não"),
      selectInput("married", "Casado?", choices = c("Sim/Não", levels(Credit$Married)), selected = "Sim/Não"),
      selectInput("eth",     "Etnia", choices = c("Todas", levels(Credit$Ethnicity)), selected = "Todas"),
      sliderInput("age", "Idade", min(Credit$Age), max(Credit$Age),
                  value = c(min(Credit$Age), max(Credit$Age))),
      sliderInput("income", "Rendimentos", min(Credit$Income), max(Credit$Income),
                  value = c(min(Credit$Income), max(Credit$Income))),
      sliderInput("limit", "Limite de Crédito", min(Credit$Limit), max(Credit$Limit),
                  value = c(min(Credit$Limit), max(Credit$Limit)))
    )
  ),
  dashboardBody(
    tabItems(
      # ------------------------ SUMARIO ------------------------
      tabItem(
        tabName = "sumario",
        fluidRow(
          valueBoxOutput("clientes", width=3), valueBoxOutput("media_divida",width=3),
          valueBoxOutput("mediana_divida",width=3), valueBoxOutput("racio_divida",width=3)
        ),
        fluidRow(
          box(width = 12, title = "Tabela", status = "info", solidHeader = TRUE,
              DTOutput("tabela"))
        )
      ),

      # ------------------------ Analise ------------------------
      tabItem(
        tabName = "analise",
        fluidRow(
          box(width = 4, title = "Histograma", status = "success", solidHeader = TRUE,
              selectInput("var_num", "X (Numérica)", choices = num_vars, selected = "Balance"),
              checkboxInput("densidade", "Densidade", value = TRUE)
          ),
          box(width = 4, title = "Boxplot", status = "success", solidHeader = TRUE,
              selectInput("var_quant", "Y (Númerica)", choices = num_vars, selected = "Balance"),
              selectInput("var_qual", "X (Factor)", choices = fac_vars, selected = "Student")
          ),
          box(width = 4, title = "Gráfico de Dispersão", status = "success", solidHeader = TRUE,
              selectInput("disp_x", "X (Numérica)", choices = num_vars, selected = "Income"),
              selectInput("disp_y", "Y (Numérica)", choices = num_vars, selected = "Balance"),
              selectInput("disp_corr", "Correlação (Factor)", choices = c("None", fac_vars), selected = "Student"),
              checkboxInput("disp_lm", "Reta de Regressão", value = TRUE)
          )
        ),
        fluidRow(
          box(width = 6, title = "Histograma", status = "primary", solidHeader = TRUE,
              plotOutput("plot_hist", height = 260)),
          box(width = 6, title = "Boxplot", status = "primary", solidHeader = TRUE,
              plotOutput("boxplot", height = 260))
        ),
        fluidRow(
          box(width = 12, title = "Gráfico de Dispersão", status = "primary", solidHeader = TRUE,
              plotOutput("plot_disp", height = 340))
        )
      ),

      # ------------------------ TESTES ------------------------
      tabItem(
        tabName = "testes",
        fluidRow(
          box(width = 4, title = "T-test", status = "info", solidHeader = TRUE,
              selectInput("tt_y", "Variável Numérica", choices = num_vars, selected = "Balance"),
              selectInput("tt_g", "Grupo (2 Níveis)", choices = c("Student","Gender","Married"), selected = "Student"),
              checkboxInput("tt_var_igual", "Assumir variâncias iguais?", value = FALSE),
              actionButton("executar_ttest", "Executar t-test", icon = icon("play"))
          ),
          box(width = 4, title = "ANOVA", status = "info", solidHeader = TRUE,
              selectInput("aov_y", "Variável (Numérica)", choices = num_vars, selected = "Balance"),
              selectInput("aov_g", "Grupo (>2 níveis)", choices = c("Ethnicity"), selected = "Ethnicity"),
              actionButton("executar_anova", "Executar ANOVA", icon = icon("play"))
          ),
          box(width = 4, title = "Qui-quadrado", status = "info", solidHeader = TRUE,
              selectInput("chi_a", "Factor A", choices = fac_vars, selected = "Student"),
              selectInput("chi_b", "Factor B", choices = fac_vars, selected = "Married"),
              actionButton("executar_chi", "Executar Chi-square", icon = icon("play"))
          )
        ),
        fluidRow(
          box(width = 4, title = "Resultados", status = "primary", solidHeader = TRUE,
              verbatimTextOutput("tests_out", placeholder = TRUE)
          ),
          box(width = 4, title = "Visual do t-test", status = "warning", solidHeader = T,
              plotOutput("plot_ttest", height = 280)),
          box(width = 4, title = "Tabela de contingência", status = "warning", solidHeader = T,
              DTOutput("tbl_chi"))
        )
      )
    )
  )
)

# ----------------------------- SERVER --------------------------------

server <- function(input, output, session) {

  df <- reactive({
    d <- Credit %>%
      filter(
        Age >= input$age[1], Age <= input$age[2],
        Income >= input$income[1], Income <= input$income[2],
        Limit >= input$limit[1], Limit <= input$limit[2]
      )
    if (input$gender  != "Todos") d <- d %>% filter(Gender == input$gender)
    if (input$student != "Sim/Não") d <- d %>% filter(Student == input$student)
    if (input$married != "Sim/Não") d <- d %>% filter(Married == input$married)
    if (input$eth     != "Todas") d <- d %>% filter(Ethnicity == input$eth)
    return(d)
  })

  # KPIs
  output$clientes <- renderValueBox({
    valueBox(nrow(df()), "Número de Clientes", icon = icon("database"), color = "teal")
  })
  output$media_divida <- renderValueBox({
    valueBox(round(mean(df()$Balance), 2), "Dívida Média", icon = icon("wallet"), color = "aqua")
  })
  output$mediana_divida <- renderValueBox({
    valueBox(median(df()$Balance), "Dívida Mediana", icon = icon("scale-balanced"), color = "purple")
  })
  output$racio_divida <- renderValueBox({
    valueBox(round(mean(df()$Balance / df()$Limit) * 100, 2), "Rácio de Dívida (Balance/Limit %)",
             icon = icon("percent"), color = "fuchsia")
  })

  output$tabela <- renderDT({
    datatable(df(), options = list(pageLength = 10, scrollX = TRUE))
  })

  # Exploração: hist
  output$plot_hist <- renderPlot({
    d <- df()
    validate(need(nrow(d) > 0, "Sem observações com os filtros atuais."))
    x <- d[[input$var_num]]
    bw <- max(diff(range(x)) / 15, 1e-8)   # largura de cada uma das 15 classes
    p <- ggplot(d, aes(x = .data[[input$var_num]])) +
      geom_histogram(binwidth = bw, boundary = min(x), fill="lightblue", colour="white")
    if (isTRUE(input$densidade)) {
      # densidade x n x largura da classe = mesma escala das barras
      p <- p + geom_density(aes(y = after_stat(count) * bw), linewidth = 1, colour="pink")
    }
    p + labs(x = input$var_num, y = "Frequência")
  })

  # Exploração: boxplot
  output$boxplot <- renderPlot({
    d <- df()
    validate(need(nrow(d) > 0, "Sem observações com os filtros atuais."))
    ggplot(d, aes(x = .data[[input$var_qual]], y = .data[[input$var_quant]])) +
      geom_boxplot(fill="lightyellow", colour="black") +
      labs(x = input$var_qual, y = input$var_quant)
  })



  # Exploração: Grafico de Dispersão
  output$plot_disp <- renderPlot({
    d <- df()
    validate(need(nrow(d) > 1, "Sem observações suficientes com os filtros atuais."))
    if (input$disp_corr == "None") {
      p <- ggplot(d, aes(x = .data[[input$disp_x]], y = .data[[input$disp_y]])) +
        geom_point(alpha = 0.7)
    } else {
      p <- ggplot(d, aes(x = .data[[input$disp_x]], y = .data[[input$disp_y]], color = .data[[input$disp_corr]])) +
        geom_point(alpha = 0.7)
    }
    if (isTRUE(input$disp_lm)) {
      p <- p + geom_smooth(method = "lm" , se = FALSE)
    }
    p + labs(x = input$disp_x, y = input$disp_y)
  })

  # ---------------- Tests ----------------

  tests_txt <- reactiveVal("Clica num botão de teste para obter resultados. ")

  output$tests_out <- renderText({ tests_txt() })

  # Mensagem a mostrar quando um teste falha (em vez de a app ir abaixo)
  erro_txt <- function(e) paste0("Não foi possível executar o teste: ", conditionMessage(e))

  # t-test
  ttest_res <- eventReactive(input$executar_ttest, {
    d <- df()
    g <- input$tt_g
    y <- input$tt_y

    validate(need(nrow(d) > 1, "Não há observações suficientes com os filtros atuais."))
    grp <- droplevels(d[[g]])

    validate(
      need(nlevels(grp) == 2, "O grupo tem de ter exatamente 2 níveis com dados após filtros."),
      need(all(table(grp) > 1), "Cada nível do grupo tem de ter pelo menos 2 observações após filtros.")
    )

    d[[g]] <- grp
    f <- as.formula(paste0(y, " ~ ", g))
    t.test(f, data = d, var.equal = isTRUE(input$tt_var_igual))
  })

  observeEvent(input$executar_ttest, {
    res <- tryCatch(ttest_res(), error = function(e) e)
    if (inherits(res, "error")) return(tests_txt(erro_txt(res)))
    tests_txt(
      paste0(
        "t-test: ", input$tt_y, " por ", input$tt_g, "\n",
        capture.output(res) |> paste(collapse = "\n")
      )
    )
  }, ignoreInit = TRUE)

  output$plot_ttest <- renderPlot({
    req(input$tt_g, input$tt_y)
    d <- df()
    validate(need(nrow(d) > 0, "Sem observações com os filtros atuais."))
    g <- input$tt_g
    y <- input$tt_y
    ggplot(d, aes(x = .data[[g]], y = .data[[y]])) +
      geom_boxplot() +
      labs(x = g, y = y)
  })

  # ANOVA
  anova_res <- eventReactive(input$executar_anova, {
    d <- df()
    g <- input$aov_g
    y <- input$aov_y
    if (nlevels(droplevels(d[[g]])) < 3) {
      stop("O grupo da ANOVA precisa de >= 3 níveis após filtros.")
    }
    fit <- aov(as.formula(paste0(y, " ~ ", g)), data = d)
    list(fit = fit, tidy = broom::tidy(fit))
  })

  observeEvent(input$executar_anova, {
    res <- tryCatch(anova_res(), error = function(e) e)
    if (inherits(res, "error")) return(tests_txt(erro_txt(res)))
    tests_txt(
      paste0(
        "ANOVA: ", input$aov_y, " por ", input$aov_g, "\n\n",
        paste(capture.output(summary(res$fit)), collapse = "\n")
      )
    )
  }, ignoreInit = TRUE)

  # Chi-square
  chi_tbl <- eventReactive(input$executar_chi, {
    d <- df()
    a <- input$chi_a
    b <- input$chi_b
    validate(need(nrow(d) > 0, "Não há observações com os filtros atuais."))
    tab <- table(d[[a]], d[[b]])
    test <- chisq.test(tab)
    list(tab = tab, test = test)
  })

  observeEvent(input$executar_chi, {
    res <- tryCatch(chi_tbl(), error = function(e) e)
    if (inherits(res, "error")) return(tests_txt(erro_txt(res)))
    tests_txt(
      paste0(
        "Chi-square: associação entre ", input$chi_a, " e ", input$chi_b, "\n",
        paste(capture.output(res$test), collapse = "\n")
      )
    )
  }, ignoreInit = TRUE)

  output$tbl_chi <- renderDT({
    req(chi_tbl())
    tab <- chi_tbl()$tab
    datatable(as.data.frame.matrix(tab), options = list(pageLength = 10))
  })
}


shinyApp(ui, server)
