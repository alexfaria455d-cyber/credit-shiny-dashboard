# Credit — Shiny dashboard

Shiny dashboard for exploratory analysis and hypothesis testing on the `Credit` dataset from the [ISLR](https://cran.r-project.org/package=ISLR) package (400 credit card holders: balance, credit limit, income and socio-demographic attributes).

Built as coursework for the MSc in Statistics for Data Science at the University of Minho. The interface is in Portuguese.

## Features

- **Overview**: number of clients, mean and median balance, balance-to-limit ratio and a searchable data table.
- **Analysis**: histogram with density curve, boxplot by factor, and scatter plot with optional regression line by group.
- **Hypothesis tests**: two-sample t-test (Welch or pooled variance), one-way ANOVA and chi-square test of independence, with the contingency table.

Every tab follows the global filters in the sidebar (gender, student status, marital status, ethnicity, age, income and credit limit). When the filters leave a test without enough data, the app explains why instead of failing.

## Run

```r
install.packages(c("shiny", "shinydashboard", "ISLR", "dplyr", "ggplot2", "DT", "broom"))
shiny::runApp()
```

## License

MIT
