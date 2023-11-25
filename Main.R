# Práctica 3.1 Minería de Datos: Árboles de Decisión
# Brenda Itzel Guzmán Bonilla
# Brenda García Briones
# María José Merino Pérez


#   EVALUACIÓN ÁRBOL DE DECISIÓN


library(caret)
set.seed(1989)
source("DecisionTree.R")

# Clasificación con DecisionTree  ####

data <- read.csv("iris.csv", header = TRUE)

# Crear índices para la partición (80% entrenamiento, 20% prueba)
indices_particion <- createDataPartition(data$species, p = 0.8, list = FALSE)

# Separar los datos en conjuntos de entrenamiento y prueba
datos_entrenamiento <- data[indices_particion, ]
datos_prueba <- data[-indices_particion, ]
colnames(datos_entrenamiento)[colnames(datos_entrenamiento) == "species"] <- "label"

# Entrenamiento del árbol de decisión
tree_entrenamiento <- DecisionTree(datos_entrenamiento,  max_depth = 3, min_samples = 3)

# Predicciones en datos_prueba
predictions <- predict_decision_tree(tree_entrenamiento, datos_prueba)
datos_prueba$species <- factor(datos_prueba$species, levels = c("setosa", "versicolor", "virginica"))
predicciones_prueba_factor <- factor(predictions, levels = levels(datos_prueba$species))
predictions

# Matriz de Confusión y Accuracy
conf_matrix <- confusionMatrix(predicciones_prueba_factor, datos_prueba$species)
conf_matrix_table <- as.table(conf_matrix)
accuracy <- conf_matrix$overall["Accuracy"]

# Imprimir el árbol
print_decision_tree(tree_entrenamiento)

# Imprimir la matriz de confusión y el accuracy
print(conf_matrix_table)
cat("Accuracy:", accuracy, "\n")


# Clasificación con rpart  ####

library(rpart)
library(rpart.plot)

# Crear el árbol de decisión con los datos de entrenamiento
arbol_decision <- rpart(label ~ ., data = datos_entrenamiento, method = "class")

# Realizar predicciones en los datos de prueba
predicciones_prueba <- predict(arbol_decision, newdata = datos_prueba, type = "class")
predicciones_prueba

# Matriz de Confusión y Accuracy
matriz_confusion <- table(predicciones_prueba, datos_prueba$species)
accuracy <- sum(diag(matriz_confusion)) / sum(matriz_confusion)

# Imprimir el árbol
print(arbol_decision)

# Graficar el árbol
rpart.plot(arbol_decision, type = 4, extra = 101, fallen.leaves = TRUE, nn = TRUE)

# Imprimir la matriz de confusión y el accuracy
print(matriz_confusion)
cat("Accuracy:", accuracy, "\n")
