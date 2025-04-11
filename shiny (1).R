
library(shiny)
library(ggplot2)
library(readr)
library(dplyr)

# Carrega os dados
df <- read.csv("df_diamond_data_merged_with_other_variables.csv", stringsAsFactors = FALSE)

# Seleciona as colunas desejadas, incluindo a data
df <- df[, c("date", "diamond.price", "inflation.rate", "interest.rate", "gold.price")]
colnames(df) <- c("Date", "DiamondPrice", "InflationRate", "InterestRate", "GoldPrice")
df$Date <- as.Date(df$Date)

# Interface do usuário
ui <- fluidPage(
  titlePanel("Dashboard Interativo - Diamantes e Indicadores Econômicos"),
  sidebarLayout(
    sidebarPanel(
      selectInput("variavel", "Escolha a variável:",
                  choices = names(df)[-1]),
      selectInput("cor", "Escolha a cor da linha:",
                  choices = c("Azul" = "blue", "Vermelho" = "red",
                              "Verde" = "green", "Preto" = "black")),
      sliderInput("xlim", "Limite do eixo X (data):",
                  min = min(df$Date, na.rm = TRUE),
                  max = max(df$Date, na.rm = TRUE),
                  value = c(min(df$Date, na.rm = TRUE), max(df$Date, na.rm = TRUE)),
                  timeFormat = "%Y-%m-%d"),
      uiOutput("ylimit_ui")
    ),
    mainPanel(
      plotOutput("grafico")
    )
  )
)

# Servidor
server <- function(input, output, session) {
  
  output$ylimit_ui <- renderUI({
    req(input$variavel)
    min_y <- floor(min(df[[input$variavel]], na.rm = TRUE))
    max_y <- ceiling(max(df[[input$variavel]], na.rm = TRUE))
    sliderInput("ylim", "Limite do eixo Y:",
                min = min_y, max = max_y,
                value = c(min_y, max_y))
  })
  
  output$grafico <- renderPlot({
    req(input$variavel)
    plot_df <- df %>% filter(Date >= input$xlim[1] & Date <= input$xlim[2])
    ggplot(plot_df, aes(x = Date, y = .data[[input$variavel]])) +
        geom_line(color = input$cor, size = 1) +
        scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
        scale_y_continuous(limits = input$ylim) +
        labs(title = paste("Gráfico de", input$variavel),
            x = "Data", y = input$variavel) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
}

# Executar o app
shinyApp(ui = ui, server = server)