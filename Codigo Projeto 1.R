# ==============================================================================
# INSTRUÇÕES DE EXECUÇÃO - CHUNK 1
# 1. Use o MOUSE PARA SELECIONAR todo o código abaixo (até a linha '#Fim Chunk 1').
# 2. Com o texto selecionado, pressione CTRL + ENTER (Windows) ou CMD + RETURN (Mac).
# O QUE FAZ: Instala bibliotecas, processa os dados e salva a tabela em PNG.
# ==============================================================================

### Chunk 1

# 1. Bibliotecas
if(!require(gridExtra)) install.packages("gridExtra")
library(gridExtra)
library(grid)

# 2. Dados
dados <- data.frame(
  Ensaio = 1:9,
  Gasolina_x1 = c(0.333, 0.500, 1.000, 0.333, 0.500, 0.000, 0.000, 0.333, 0.000),
  Etanol_x2   = c(0.333, 0.500, 0.000, 0.333, 0.000, 1.000, 0.500, 0.333, 0.000),
  Metanol_x3  = c(0.333, 0.000, 0.000, 0.333, 0.500, 0.000, 0.500, 0.333, 1.000),
  Octanagem_y = c(89.9, 89.5, 87.0, 90.0, 88.0, 92.0, 91.2, 90.1, 90.0)
)
colnames(dados) <- c("Ensaio", "Gasolina (x1)", "Etanol (x2)", "Metanol (x3)", "Octanagem (y)")

# 3. Tema Profissional
meu_tema <- ttheme_default(
  base_size = 12,
  core = list(bg_params = list(fill = c("white", "#f2f2f2"), col = "white")),
  colhead = list(fg_params = list(col = "white", fontface = "bold"),
                 bg_params = list(fill = "#2c3e50"))
)

# 4. CRIAR E SALVAR A IMAGEM (O SEGREDO DO CORTE)
# Criamos o objeto da tabela
tabela_obj <- tableGrob(dados, theme = meu_tema, rows = NULL)

# Salvando em PNG com dimensões ajustadas ao tamanho da tabela
png("tabela_projeto.png",
    width = 600, height = 350, # Ajuste conforme necessário
    res = 120, # Resolução alta para não pixelar
    bg = "transparent") # Fundo transparente ajuda muito no Docs

grid.draw(tabela_obj)
dev.off() # Fecha o arquivo e salva

print("Imagem 'tabela_projeto.png' gerada na sua pasta de trabalho!")

# Roda isso para ver na tela do RStudio:
grid.newpage()
grid.draw(tabela_obj)

###FIm do Chunk 1

# ==============================================================================
# INSTRUÇÕES DE EXECUÇÃO - CHUNK 2
# 1. Use o MOUSE PARA SELECIONAR todo o código abaixo (até o final do arquivo).
# 2. Com o texto selecionado, pressione CTRL + ENTER (Windows) ou CMD + RETURN (Mac).
# O QUE FAZ: Converte a geometria para ternária e gera o gráfico estilo Montgomery.
# ==============================================================================

###Chunk 2
# --- 1. DADOS DO SEU EXERCÍCIO (Exemplo 11.5) ---
# Coordenadas do Simplex-Lattice {3,2} com as réplicas no centro
pontos_115 <- data.frame(
  x1 = c(1, 0, 0, 0.5, 0.5, 0, 1/3, 1/3, 1/3),
  x2 = c(0, 1, 0, 0.5, 0, 0.5, 1/3, 1/3, 1/3),
  x3 = c(0, 0, 1, 0, 0.5, 0.5, 1/3, 1/3, 1/3)
)

# --- 2. FUNÇÃO DE GEOMETRIA (CONVERSÃO TERNÁRIA) ---
tri_to_cart <- function(p) {
  x <- p[2] + p[3]/2
  y <- p[3] * sqrt(3)/2
  return(c(x, y))
}
coords <- t(apply(pontos_115, 1, tri_to_cart))

# --- 3. CONSTRUÇÃO DO GRÁFICO ESTILO MONTGOMERY ---
# Ajustando margens para caber os labels
par(mar=c(2,2,2,2))

# Criar fundo branco e limpo
plot(NULL, xlim=c(-0.1, 1.1), ylim=c(-0.1, 1), asp=1, 
     xlab="", ylab="", axes=FALSE)

# Desenhar o triângulo principal (Linha grossa e preta)
lines(c(0, 1, 0.5, 0), c(0, 0, sqrt(3)/2, 0), lwd=2.5, col="black")

# Adicionar Linhas de Grade (Pontilhadas e leves) - Igual ao livro
for(i in seq(0.2, 0.8, 0.2)) {
  # Linhas horizontais
  lines(c(i/2, 1-i/2), c(i*sqrt(3)/2, i*sqrt(3)/2), lty=3, col="gray80")
  # Linhas inclinadas
  lines(c(i, i/2), c(0, i*sqrt(3)/2), lty=3, col="gray80")
  lines(c(1-i, 1-i/2), c(0, i*sqrt(3)/2), lty=3, col="gray80")
}

# --- 4. PONTOS E TEXTOS ---
# Plotar os pontos experimentais (Pretos e nítidos)
points(coords[,1], coords[,2], pch=19, cex=1.8, col="black")

# Adicionar os Nomes dos Componentes (Vértices)
text(0.5, sqrt(3)/2 + 0.08, "x1", cex=1.2, font=2)
text(-0.05, -0.05, "x2", cex=1.2, font=2)
text(1.05, -0.05, "x3", cex=1.2, font=2)

# Colocar o número '3' no centro para indicar as 3 réplicas
text(coords[7,1], coords[7,2], "3", pos=3, cex=1, font=2, col="black")

# Adicionar escalas (0.2, 0.4, 0.6, 0.8) nas bordas
for(i in seq(0.2, 0.8, 0.2)) {
  text(i, -0.04, i, cex=0.8) # Base
  text(i/2 - 0.04, i*sqrt(3)/2, i, cex=0.8) # Esquerda
  text(1-i/2 + 0.04, i*sqrt(3)/2, 1-i, cex=0.8) # Direita
}


###Fim do Chunk 2