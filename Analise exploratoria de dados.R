library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(GGally)
library(summarytools)
library(mice)


# ─────────────────────────────
# Leitura e preparação inicial
# ─────────────────────────────
dados <- read_delim("anuario1.csv", delim = ";")
colnames(dados) <- gsub(" ", "_", colnames(dados))

if (!"Estados" %in% colnames(dados)) {
  stop("❌ A coluna 'Estados' não foi localizada. Verifique o cabeçalho do CSV.")
}

variaveis_numericas <- c(
  "Homicidio_Doloso",
  "Latrocinio",
  "Mortes_Violentas_Intencionais",
  "Taxa",
  "Pop_urbana"
)

dir.create("histogramas", showWarnings = FALSE)
dir.create("qqplots", showWarnings = FALSE)

# ─────────────────────────────
# 1. Resumo estatístico
# ─────────────────────────────
st_options(plain.ascii = FALSE, style = "rmarkdown")
resumo <- descr(
  dados[, variaveis_numericas],
  stats = c("mean", "sd", "min", "q1", "med", "q3", "max", "iqr", "cv"),
  transpose = TRUE
)
print(resumo)

# ─────────────────────────────
# 2. Matriz de dispersão
# ─────────────────────────────
scatter_data <- dados %>%
  select(all_of(variaveis_numericas)) %>%
  drop_na()

scatter_matrix <- ggpairs(
  data = scatter_data,
  lower = list(continuous = wrap("points", size = 2, alpha = 0.8)),
  diag = list(continuous = wrap("densityDiag", alpha = 0.4)),
  upper = list(continuous = wrap("cor", size = 4)),
  title = "Matriz de Dispersão - Variáveis Numéricas"
) + theme_bw()

print(scatter_matrix)

# ─────────────────────────────
# 3. Histogramas
# ─────────────────────────────
bins <- ceiling(log2(nrow(dados)) + 1)
cat("📊 Número de bins definidos:", bins, "\n\n")

for (coluna in variaveis_numericas) {
  histograma <- ggplot(dados, aes_string(x = coluna)) +
    geom_histogram(bins = bins, fill = "steelblue", color = "white", alpha = 0.9) +
    geom_text(stat = "bin", bins = bins,
              aes(label = ..count.., y = ..count..), vjust = -0.5, size = 3) +
    labs(title = paste("Histograma -", coluna), x = coluna, y = "Frequência") +
    theme_minimal()
  
  print(histograma)
  
  ggsave(
    filename = file.path("histogramas", paste0("histograma_", coluna, ".png")),
    plot = histograma,
    width = 8,
    height = 6
  )
  
  cat("✅ Histograma salvo: histogramas/histograma_", coluna, ".png\n", sep = "")
}

# ─────────────────────────────
# 4. Análise de completude (ANTES da imputação)
# ─────────────────────────────
calcular_completude <- function(df, limite = 90) {
  total <- nrow(df)
  
  completude_df <- data.frame(
    Variável = colnames(df),
    Preenchidos = colSums(!is.na(df)),
    Total = total
  ) %>%
    mutate(
      Completude_percent = round((Preenchidos / Total) * 100, 2),
      Abaixo_90 = ifelse(Completude_percent < limite, "⚠️", "")
    ) %>%
    arrange(Completude_percent)
  
  abaixo <- completude_df %>% filter(Completude_percent < limite)
  if (nrow(abaixo) > 0) {
    cat("🔎 Atenção: Variáveis com completude abaixo de", limite, "%:\n\n")
    print(abaixo)
    cat("\n")
  } else {
    cat("✅ Todas as variáveis estão com completude ≥", limite, "%.\n\n")
  }
  
  return(completude_df)
}

plot_completude <- function(df_completude, limite = 90) {
  ggplot(df_completude, aes(x = reorder(Variável, Completude_percent),
                            y = Completude_percent,
                            fill = Completude_percent < limite)) +
    geom_bar(stat = "identity", width = 0.6) +
    coord_flip() +
    scale_fill_manual(values = c("FALSE" = "steelblue", "TRUE" = "tomato")) +
    labs(title = "Completude por Variável",
         x = "Variável",
         y = "Completude (%)") +
    geom_text(aes(label = paste0(Completude_percent, "%")),
              hjust = -0.1, size = 3.5) +
    theme_minimal() +
    ylim(0, 105) +
    guides(fill = FALSE)
}

# Executa análise antes da imputação
completude_pre <- calcular_completude(dados[, variaveis_numericas])
print(completude_pre)
print(plot_completude(completude_pre))

# ─────────────────────────────
# 5. Testes de normalidade (Shapiro-Wilk)
# ─────────────────────────────
shapiro_resultados <- data.frame(
  Variavel = character(),
  W = numeric(),
  P_Value = numeric(),
  Normal = character(),
  stringsAsFactors = FALSE
)

cat("\n📋 Teste de normalidade (Shapiro-Wilk):\n\n")

for (coluna in variaveis_numericas) {
  vetor <- dados[[coluna]]
  
  if (length(unique(vetor)) < 3 || length(vetor) < 3 || length(vetor) > 5000) {
    cat(paste0("⚠️  ", coluna, ": dados insuficientes para o teste.\n\n"))
    shapiro_resultados <- rbind(shapiro_resultados, data.frame(
      Variavel = coluna,
      W = NA,
      P_Value = NA,
      Normal = "Indeterminado"
    ))
    next
  }
  
  teste <- shapiro.test(vetor)
  w <- round(teste$statistic, 4)
  p <- round(teste$p.value, 4)
  normal <- ifelse(p > 0.05, "Sim", "Não")
  
  shapiro_resultados <- rbind(shapiro_resultados, data.frame(
    Variavel = coluna,
    W = w,
    P_Value = p,
    Normal = normal
  ))
  
  cat(paste0("✅ ", coluna, " → W = ", w, " | p = ", p, " | Normal: ", normal, "\n\n"))
}

write.csv(shapiro_resultados, "shapiro_resultados.csv", row.names = FALSE)
cat("📄 Resultado salvo em: shapiro_resultados.csv\n\n")

# ─────────────────────────────
# 6. Q-Q Plots
# ─────────────────────────────
for (coluna in variaveis_numericas) {
  cat("▶️ Q-Q plot para:", coluna, "\n")
  
  tryCatch({
    vetor <- dados[[coluna]]
    
    if (length(unique(vetor)) < 3) {
      warning(paste("📛 Valores constantes:", coluna))
      next
    }
    
    teste <- shapiro.test(vetor)
    w <- round(teste$statistic, 4)
    p <- round(teste$p.value, 4)
    
    teorico <- qqnorm(vetor, plot.it = FALSE)$x
    df_qq <- data.frame(
      Estado = dados$Estados,
      Observado = vetor,
      Teorico = teorico
    )
    
    titulo_qq <- paste0("Q-Q Plot - ", coluna, " | W = ", w, " | p = ", p)
    
    qq_plot <- ggplot(df_qq, aes(x = Teorico, y = Observado)) +
      geom_point(color = "tomato") +
      geom_text(aes(label = Estado), hjust = -0.1, size = 3, check_overlap = TRUE) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
      labs(title = titulo_qq, x = "Quantis Teóricos", y = "Quantis Observados") +
      theme_minimal()
    
    print(qq_plot)
    
    ggsave(
      filename = file.path("qqplots", paste0("qqplot_", coluna, ".png")),
      plot = qq_plot,
      width = 8,
      height = 6
    )
    
    cat("✅ Q-Q plot salvo: qqplots/qqplot_", coluna, ".png\n\n")
  }, error = function(e) {
    cat("❌ Erro ao processar:", coluna, "→", e$message, "\n\n")
  })
}

# ─────────────────────────────
# 7. Imputação de valores ausentes
# ─────────────────────────────

# Subseleção e visualização do padrão de faltantes
dados_filtrados <- dados %>%
  select(all_of(variaveis_numericas)) %>%
  as.data.frame()
cat("📊 Padrão de dados ausentes:\n")
md.pattern(dados_filtrados)

# Imputação com PMM
imputacao <- mice(dados_filtrados, method = "pmm", m = 1, seed = 123)
dados_imputados <- complete(imputacao)

# Substitui no dataset original
dados[variaveis_numericas] <- dados_imputados

cat("\n✅ Dados imputados com sucesso. Falhas preenchidas com PMM.\n")

