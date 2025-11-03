library(plotly)
n_observaciones <- 100
set.seed(123)
ejex = rnorm(n_observaciones, mean = 10, sd = 2)
ejey = ejex*2.4 + rnorm(length(ejex),mean = 0,sd=1)
ejez = ejex*(-2)*rnorm(n_observaciones, mean = 20, sd = 3) - 0.5 * rnorm(n_observaciones)

ejexx = rnorm(n_observaciones, mean = 10, sd = 2)
ejeyy = ejex*(-4) + rnorm(length(ejex),mean = 0,sd=1)
ejezz = ejex*(4)*rnorm(n_observaciones, mean = 20, sd = 3) - 0.5 * rnorm(n_observaciones)


datos_3d <- data.frame(
  Variable_X = c(ejex,ejexx),
  Variable_Y = c(ejey,ejeyy),
  Variable_Z = c(ejez,ejezz)
)
head(datos_3d)
cor(datos_3d)
fig <- plot_ly() %>%
  add_markers(
    x = datos_3d[, "Variable_X"],
    y = datos_3d[, "Variable_Y"],
    z = datos_3d[, "Variable_Z"],
    name = "Datos Originales (Escalados)",
    marker = list(size = 4, color = "blue", opacity = 0.8)
  )
fig


#aplicamos PCA:
pca_resultado <- prcomp(datos_3d, scale = TRUE)
datos_escalados <- scale(datos_3d)
rotaciones <- pca_resultado$rotation



# Longitud del vector para visualización
escala_vector <- 3

# Preparar un data frame con los puntos de inicio (0,0,0) y fin de los 3 vectores PCA
vectores_pca <- data.frame(
  x_inicio = rep(0, 3),
  y_inicio = rep(0, 3),
  z_inicio = rep(0, 3),
  x_fin = rotaciones[1, 1:3] * escala_vector,
  y_fin = rotaciones[2, 1:3] * escala_vector,
  z_fin = rotaciones[3, 1:3] * escala_vector,
  PC = paste0("PC", 1:3)
)

#repetimos la figura con los datos escalados:
fig <- plot_ly() %>%
  add_markers(
    x = datos_escalados[, "Variable_X"],
    y = datos_escalados[, "Variable_Y"],
    z = datos_escalados[, "Variable_Z"],
    name = "Datos Originales (Escalados)",
    marker = list(size = 4, color = "blue", opacity = 0.8)
  )
fig

#añadir direccion componentes principales:
colores_pca <- c("PC1" = "red", "PC2" = "green", "PC3" = "orange")
for (i in 1:3) {
  pc_name <- vectores_pca$PC[i]
  pc_color <- colores_pca[pc_name]
  
  # Añadir la línea del vector (traza)
  fig <- fig %>%
    add_trace(
      x = c(vectores_pca$x_inicio[i], vectores_pca$x_fin[i]),
      y = c(vectores_pca$y_inicio[i], vectores_pca$y_fin[i]),
      z = c(vectores_pca$z_inicio[i], vectores_pca$z_fin[i]),
      type = 'scatter3d',
      mode = 'lines',
      line = list(color = pc_color, width = 5),
      name = pc_name,
      showlegend = TRUE
    ) %>%
    # Añadir la etiqueta al final de la línea (para simular la punta de la flecha)
    add_trace(
      x = vectores_pca$x_fin[i] * 1.1,
      y = vectores_pca$y_fin[i] * 1.1,
      z = vectores_pca$z_fin[i] * 1.1,
      type = 'scatter3d',
      mode = 'text',
      text = pc_name,
      textfont = list(color = pc_color, size = 12),
      name = pc_name,
      showlegend = FALSE
    )
}
fig


plot(pca_resultado$x[,1:2],pch=16,col="blue")



#comparacion con funcion svd
pca_resultado <- prcomp(datos_3d, scale = TRUE)
pca_resultado_2 <- prcomp(datos_escalados, scale = FALSE)
svd_datos = svd(datos_escalados)

plot(svd_datos$u[,1:2]) #son equivalentes
plot(pca_resultado$x[,1:2]) #son equivalente
#


# ejemplo 1 #########
# Generate random values for x and y
alpha=3
x <- seq(from = 1, to = 5, by = 0.1)
y <- alpha*x
n <- length (x)
x1 <- x + runif(n,-0.25,0.25)
y1<- y + runif(n,-0.25,0.25)
A <- matrix (c(x1,y1), nrow=2, ncol=n, byrow=TRUE)
m <- 2
dim(A) #2 x 41
# Calculate princial components with function prcomp
PCs <- prcomp(t(A), center = T, retx = T)
# Eigenvectors
PCs$rotation
#Eigenvalues
PCs$sdev
# Rotated data (projections)
PCs$x

# Plot original data
library(ggplot2)
data1 <- data.frame(x1, y1)
Fig1 <- ggplot(data1) + geom_point(aes(x=x1, y=y1), color= 'red') +
  xlab('x') + ylab ('y') + theme_classic()
Fig1

# Plot rotated data
data2 <- data.frame(PC1 = PCs$x[,1], PC2 = PCs$x[,2])
Fig2 <- ggplot(data2) + geom_point(aes(x=PC1, y=PC2), color= 'red') +
  xlab('PC1') + ylab ('PC2') + scale_y_continuous(limits = c(-1,1))+
  theme_classic()
Fig2





