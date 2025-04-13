library(shiny)
library(ggplot2)
library(tidyr)
library(dplyr)
library(colourpicker)

# Lê os dados
dados <- read.csv2("anuario1.csv", stringsAsFactors = FALSE)
variaveis_numericas <- names(dados)[sapply(dados, is.numeric)]

# INTERFACE
ui <- fluidPage(
  titlePanel("📊 Comparação de Indicadores entre Estados"),
  
  tabsetPanel(
    
    # ABA DE COMPARAÇÃO ENTRE ESTADOS
    tabPanel("🔸 Comparar 2 Estados",
             sidebarLayout(
               sidebarPanel(
                 selectInput("estado1", "Estado 1:", choices = unique(dados$Estados)),
                 colourInput("cor1", "Cor Estado 1:", value = "#1f77b4"),
                 selectInput("estado2", "Estado 2:", choices = unique(dados$Estados)),
                 colourInput("cor2", "Cor Estado 2:", value = "#ff7f0e"),
                 
                 downloadButton("baixar_comparacao", "📥 Baixar gráfico comparação"),
                 downloadButton("baixar_percentual", "📥 Baixar gráfico comparativo")
               ),
               mainPanel(
                 h4("📈 Gráfico Comparativo"),
                 plotOutput("grafico_duplo"),
                 h4("📉 Comparativo lado a lado por indicador"),
                 plotOutput("grafico_percentual")
               )
             )
    ),
    
    # ABA DE CURVA DE PARETO
    tabPanel("📌 Visualizar por Indicador",
             sidebarLayout(
               sidebarPanel(
                 selectInput("indicador_unico", "Selecione um indicador:",
                             choices = variaveis_numericas),
                 downloadButton("baixar_todos_estado", "📥 Baixar gráfico por indicador")
               ),
               mainPanel(
                 h4("📊 Curva de Pareto - Todos os Estados"),
                 plotOutput("grafico_todos_estados"),
                 br(),
                 h5("Estados que compõem os 80% mais relevantes:"),
                 tableOutput("tabela_80_porcento")
               )
             )
    )
  )
)

# SERVIDOR
server <- function(input, output, session) {
  
  # DADOS COMPARAÇÃO
  dados_comparacao <- reactive({
    dados %>%
      filter(Estados %in% c(input$estado1, input$estado2)) %>%
      pivot_longer(cols = all_of(variaveis_numericas),
                   names_to = "Indicador", values_to = "Valor")
  })
  
  # GRÁFICO LINHA
  output$grafico_duplo <- renderPlot({
    ggplot(dados_comparacao(), aes(x = Indicador, y = Valor, group = Estados, color = Estados)) +
      geom_line(size = 1.3) +
      geom_point(size = 3) +
      geom_text(aes(label = round(Valor, 1)), vjust = -0.8, size = 3.5) +
      scale_color_manual(values = setNames(c(input$cor1, input$cor2),
                                           c(input$estado1, input$estado2))) +
      labs(
        title = paste("Comparação:", input$estado1, "vs", input$estado2),
        x = "Indicadores", y = "Valor"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$baixar_comparacao <- downloadHandler(
    filename = function() {
      paste0("comparacao_", input$estado1, "_vs_", input$estado2, ".png")
    },
    content = function(file) {
      ggsave(file, plot = output$grafico_duplo(), width = 10, height = 6)
    }
  )
  
  # DADOS FORMATADOS PARA COMPARAÇÃO LADO A LADO
  dados_percentual <- reactive({
    dados %>%
      filter(Estados %in% c(input$estado1, input$estado2)) %>%
      pivot_longer(cols = all_of(variaveis_numericas),
                   names_to = "Indicador", values_to = "Valor") %>%
      mutate(Categoria = case_when(
        Estados == input$estado1 ~ paste0(input$estado1, " (referência)"),
        Estados == input$estado2 ~ paste0(input$estado2, " (comparado)")
      ))
  })
  
  # GRÁFICO HORIZONTAL AGRUPADO
  output$grafico_percentual <- renderPlot({
    df <- dados_percentual()
    
    ggplot(df, aes(x = Indicador, y = Valor, fill = Categoria)) +
      geom_col(position = position_dodge(width = 0.6), width = 0.5) +
      geom_text(aes(label = round(Valor, 1)),
                position = position_dodge(width = 0.6),
                vjust = -0.4, size = 3.5) +
      scale_fill_manual(values = setNames(c(input$cor1, input$cor2),
                                          unique(df$Categoria))) +
      labs(
        title = "Comparação lado a lado – barras horizontais",
        x = "Indicador", y = "Valor"
      ) +
      coord_flip() +
      theme_minimal()
  })
  
  output$baixar_percentual <- downloadHandler(
    filename = function() {
      paste0("valores_lado_a_lado_", input$estado1, "_vs_", input$estado2, ".png")
    },
    content = function(file) {
      ggsave(file, plot = output$grafico_percentual(), width = 10, height = 6)
    }
  )
  
  # CURVA DE PARETO
  grafico_todos_estados <- reactive({
    req(input$indicador_unico)
    
    indicador <- input$indicador_unico
    df <- dados %>%
      select(Estados, all_of(indicador)) %>%
      rename(Valor = all_of(indicador)) %>%
      arrange(desc(Valor)) %>%
      mutate(
        Acumulado = cumsum(Valor),
        Percentual = round(100 * Acumulado / sum(Valor), 1),
        Estados_ordenado = reorder(Estados, -Valor)
      )
    
    corte_index <- which(df$Percentual >= 80)[1]
    
    ggplot(df, aes(x = Estados_ordenado)) +
      geom_col(aes(y = Valor), fill = "#0072B2") +
      geom_line(aes(y = Percentual * max(Valor)/100), color = "red", size = 1.2, group = 1) +
      geom_point(aes(y = Percentual * max(Valor)/100), color = "red", size = 2) +
      geom_text(
        aes(y = Percentual * max(Valor)/100, label = paste0(Percentual, "%")),
        vjust = -0.6, size = 3.5, color = "red"
      ) +
      geom_vline(xintercept = corte_index, linetype = "dashed", color = "gray30") +
      annotate("text", x = corte_index + 0.3, y = max(df$Valor) * 1.06,
               label = "Corte 80%", size = 3.5, color = "black", hjust = 0) +
      scale_y_continuous(
        name = "Valor",
        sec.axis = sec_axis(~ . * 100 / max(df$Valor), name = "Percentual Acumulado (%)")
      ) +
      labs(
        title = paste("Curva de Pareto -", indicador),
        x = "Estados"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$grafico_todos_estados <- renderPlot({
    grafico_todos_estados()
  })
  
  output$baixar_todos_estado <- downloadHandler(
    filename = function() {
      paste0("curva_pareto_", input$indicador_unico, ".png")
    },
    content = function(file) {
      ggsave(file, plot = grafico_todos_estados(), width = 10, height = 6)
    }
  )
  
  # TABELA DOS ESTADOS ATÉ 80%
  output$tabela_80_porcento <- renderTable({
    req(input$indicador_unico)
    
    indicador <- input$indicador_unico
    dados %>%
      select(Estados, all_of(indicador)) %>%
      rename(Valor = all_of(indicador)) %>%
      arrange(desc(Valor)) %>%
      mutate(
        Acumulado = cumsum(Valor),
        Percentual = round(100 * Acumulado / sum(Valor), 1)
      ) %>%
      filter(Percentual <= 80) %>%
      select(Estados, Valor, Percentual)
  })
}

# EXECUTA O APP
shinyApp(ui = ui, server = server)