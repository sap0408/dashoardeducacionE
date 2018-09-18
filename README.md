# dashoardeducacionE
dashboard con datos de educacion estatal en Costa Rica por carreras, tipo de títulos y otras variables. 2.011-2.016

shinyApp(
  ui = fluidPage(
    tabsetPanel(type = "tabs",
                tabPanel("Analisis temporal", 
                         
                         selectizeInput("state", "Escoja las variables (maximo 4):",
                                        choices = c(" "),#Declarar que por defecto no se escoja ninguna variable desde el inicio
                                        options = list(maxItems = 4), #Limita el número máximo de variables a escoger a 4
                                        multiple = TRUE #Permitir escoger más de una variable
                                        
                         ), 
                         
                         plotOutput("tempAnalysis") #Graficar análisis temporal
                         
                ),
                
                tabPanel("Analisis cruzado",
                         
                         selectizeInput("state2", "Escoja las dos variables a analizar:",
                                        choices = c(" "), #Declarar que por defecto no se escoja ninguna variable desde el inicio
                                        options = list(maxItems = 2), #Limitar el número máximo de variables a escoger a 2
                                        multiple = TRUE #Permitir escoger más de una variable
                                        
                         ),
                         
                         plotOutput("crossAnalysis") #Graficar análisis cruzado
                      
                )
    )
  ),
  
  server = function(input, output, session) {
    
    library(ggplot2)
    library(dplyr)
    library(tidyr)
    library(ggrepel)
    
    #Cargar y procesar de manera reactiva el archivo de datos
    #(es decir, que si se cambia el archivo, todo lo demás se actualice en cascada)
    
    initTable <- reactive({
      initTable = read.csv(dir()[grepl("Costa", dir())]) %>% #Leer el archivo
        gather(key, value, -X) %>% #Poner los nombres de las columnas como valores de la variable "key"
        arrange(key, X) #Ordenar por el nombre de la variable y luego por el año
      
      initTable$value = gsub(",", "", initTable$value) %>% #Quitar las comas de algunos valores
        as.numeric() #Colocar como de tipo numérico los valores
      
      names(initTable) = c("Año", "Variable", "Valor") #Cambiar los nombres de las columnas
      
      initTable #Desplegar la tabla ya procesada
      
    })
    
    
    #Graficar el análisis temporal de manera reactiva
    
    output$tempAnalysis = renderPlot({
      
      initTable = initTable() #Carga la tabla ya procesada de manera reactiva
      
      variables = input$state #Extraer de la lista desplegable las variables a tener en cuenta
      
      ggplot(initTable[initTable$Variable %in% variables, ]) + aes(x = Año, y = Valor) + #Parámetros iniciales de la gráfica
        geom_line(aes(color = Variable)) + #Estipular gráfico de línea
        labs(x = "Año", y = "Valor", legend = "", title = "Evolución de variables.") + #Etiquetas
        theme(legend.position="bottom", legend.box = "vertical", legend.title = element_blank()) #Estética general
      
    })
    
    
    #Graficar el análisis crzado de manera reactiva
    output$crossAnalysis = renderPlot({
      
      initTable = initTable() #Carga la tabla ya procesada de manera reactiva
      
      varScatter = input$state2 #Extraer de la lista desplegable las variables a tener en cuenta
      
      #Prevenir que arroje un error debido a que sólo hay una variable para el análisis
      if(length(varScatter) == 1){
        
        varScatter = NULL
        
      } 
      
      #Juntar los valores de las dos variables seleccionadas
      scatterData = cbind(initTable[which(initTable$Variable == varScatter[1]), ], 
                          initTable[which(initTable$Variable == varScatter[2]), "Valor"])
      
      #Ajustar los nombres de los campos según lo seleccionado
      names(scatterData) = c("Año", "key", varScatter[1], varScatter[2])
      
      
      ggplot(scatterData, aes_string(varScatter[1], varScatter[2], label = "Año")) + #Parámetros iniciales de la gráfica
        geom_point() + #Estipular gráfico de dispersión
        geom_text_repel(size = 2.5) + labs(title = "Análisis cruzado de variables.") #Estética general
      
      
    })
    
    #Cargar los valores posibles a escoger de las listas desplegables de manera reactiva
    observe({
      
      #Leer los datos de la tabla inicial
      newList = initTable() 
      
      #Actualizar los valores posibles a escoger
      
      updateSelectizeInput(session, "state",
                           choices = unique(newList$Variable)
      )
      
      updateSelectizeInput(session, "state2",
                           choices = unique(newList$Variable)
      )
      
    })
    
    
  }
)


