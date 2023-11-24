# Práctica 3.1 Minería de Datos: Árboles de Decisión
# Brenda Itzel Guzmán Bonilla
# Brenda García Briones
# María José Merino Pérez


#   ÁRBOL DE DECISIÓN (FUNCIÓN)


# Implementación del árbol de decisión
DecisionTree <- function(data, max_depth = Inf, min_samples = 3) {
  
  # Función para dividir el conjunto de datos
  split_data <- function(data, feature, threshold) {
    left <- data[data[, feature] <= threshold, ]
    right <- data[data[, feature] > threshold, ]
    return(list(left = left, right = right))
  }
  
  # Función para calcular la impureza de Gini
  calculate_gini <- function(labels) {
    if (length(labels) == 0) {
      return(0)
    }
    proportions <- table(labels) / length(labels)
    return(1 - sum(proportions^2))
  }
  
  # Función para encontrar la mejor división
  find_best_split <- function(data) {
    features <- colnames(data[, -ncol(data)])
    best_gini <- Inf
    best_split <- NULL
    
    for (feature in features) {
      thresholds <- unique(data[, feature])
      for (threshold in thresholds) {
        splits <- split_data(data, feature, threshold)
        gini_left <- calculate_gini(splits$left$label)
        gini_right <- calculate_gini(splits$right$label)
        gini <- (nrow(splits$left) * gini_left + nrow(splits$right) * gini_right) / nrow(data)
        
        if (gini < best_gini) {
          best_gini <- gini
          best_split <- list(feature = feature, threshold = threshold, 
                             gini = gini, left = splits$left, right = splits$right)
        }
      }
    }
    
    return(best_split)
  }
  
  # Función para construir el árbol recursivamente
  build_tree <- function(data, depth, min_instances = min_samples) {
    if (depth == 0 || length(unique(data$label)) == 1 || nrow(data) <= min_instances) {
      # Crear un nodo de hoja
      return(list(node_type = "leaf", class_distribution = table(data$label)))
    }
    
    # Encontrar la mejor división
    best_split <- find_best_split(data)
    
    if (is.null(best_split)) {
      # No se puede dividir más, crear un nodo de hoja
      return(list(node_type = "leaf", class_distribution = table(data$label)))
    }
    
    # Construir nodos izquierdo y derecho de manera recursiva
    left <- build_tree(best_split$left, depth - 1)
    right <- build_tree(best_split$right, depth - 1)
    
    # Crear un nodo de decisión
    return(list(node_type = "decision", feature = best_split$feature,
                threshold = best_split$threshold, left = left, right = right))
  }
  
  # Inicializar el conjunto de datos con las etiquetas
  data$label <- factor(data$label)
  
  # Llamar a la función interna para construir el árbol
  tree <- build_tree(data, depth = max_depth, min_instances = min_samples)
  
  
  return(tree)
}

# Función para imprimir la información del árbol de decisión en una tabla
print_decision_tree <- function(tree, level = 0, direction = NA) {
  # Inicializar un dataframe para almacenar la información del árbol
  tree_data <- data.frame(Level = integer(), Type = character(), Feature = character(),
                          Threshold = numeric(), Direction = character(), Class_Distribution = character(),
                          stringsAsFactors = FALSE)
  
  # Función interna para recorrer el árbol y agregar los nodos al dataframe
  traverse_tree <- function(tree, level, direction) {
    if (tree$node_type == "leaf") {
      leaf_data <- data.frame(Level = level, Type = "Leaf", Feature = NA, Threshold = NA,
                              Direction = direction, Class_Distribution = toString(tree$class_distribution))
      tree_data <<- rbind(tree_data, leaf_data)
    } else {
      type <- ifelse(level == 0, "Root", "Decision")
      decision_data <- data.frame(Level = level, Type = type, Feature = tree$feature,
                                  Threshold = tree$threshold, 
                                  Direction = direction, Class_Distribution = NA)
      tree_data <<- rbind(tree_data, decision_data)
      traverse_tree(tree$left, level + 1, "Left")
      traverse_tree(tree$right, level + 1, "Right")
    }
  }
  
  # Llamar a la función para construir el dataframe
  traverse_tree(tree, level, direction)
  
  # Ordenar el dataframe por nivel
  tree_data <- tree_data[order(tree_data$Level), ]
  
  # Imprimir el árbol (dataframe)
  print(tree_data, row.names = FALSE)
}


# Función para realizar predicciones con el árbol de decisión
predict_decision_tree <- function(tree, data) {
  predictions <- character()
  
  # Función para realizar la predicción en una instancia
  predict_instance <- function(tree, instance) {
    if (tree$node_type == "leaf") {
      # En un nodo de hoja, devuelve la clase dominante
      return(names(tree$class_distribution)[which.max(tree$class_distribution)])
    } else {
      # En un nodo de decisión, decide hacia qué rama ir
      if (instance[tree$feature] <= tree$threshold) {
        return(predict_instance(tree$left, instance))
      } else {
        return(predict_instance(tree$right, instance))
      }
    }
  }
  
  # Aplicar la predicción a cada instancia en el conjunto de datos
  for (i in 1:nrow(data)) {
    instance <- data[i, ]
    prediction <- predict_instance(tree, instance)
    predictions <- c(predictions, prediction)
  }
  
  return(predictions)
}
