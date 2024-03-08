library(shiny)
library(DT)
library(tidyverse)
options(shiny.maxRequestSize=Inf)
# Definir a interface do usuário
ui <- fluidPage(
  titlePanel("Recomendação do número de peças"),
  sidebarLayout(
    sidebarPanel(
      numericInput(label = "Estimativa de perda caso falte alguma peça:",
                   inputId = "loss_", value = 5000),
      numericInput(label = "Número de vins fabricados por dia:",
                   inputId = "n_vins_dias", value = 80),
      actionButton("goButton", "Calcular"),
      fileInput(
        "file1", "Upload da planilha 'repbomcosto':",
        accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))
    ),
    mainPanel(
      DT::DTOutput("dataTable"), # Local para o DataTable
      htmlOutput("resultado")
    )
  )
)

# Definir a lógica do servidor
server <- function(input, output) {
  # Inicializar um reactiveVal para armazenar os dados do CSV
  csvData <- reactiveVal(NULL)
  
  observeEvent(input$goButton, {
    output$resultado <- renderText({
      paste0(
        'Número de vins fabricados por dia: ', input$n_vins_dias, '<br>',
        'Perda atribuída ao não se entregar um dos vins: R$', 
        prettyNum(round(input$loss_, 2), big.mark = '.', decimal.mark = ','),
        '<br>', 'Valor médio economizado em PN por dia: R$', 
        prettyNum(round(input$n_vins_dias*weighted.mean(
          x = filteredData$Price*(filteredData$Qty_max - filteredData$Qty),
          w = filteredData$phantom_n), 2),
          big.mark = '.', decimal.mark = ','),
        '<br>', 'Valor médio economizado em PN por mês: R$', 
        prettyNum(round(30*5/7*input$n_vins_dias*weighted.mean(
          x = filteredData$Price*(filteredData$Qty_max - filteredData$Qty),
          w = filteredData$phantom_n), 2),
          big.mark = '.', decimal.mark = ','),
        # 30 dias no mês, 5 dias uteis dentro dos 7 da semana
        '<br>', 'Valor médio economizado em PN por ano: R$', 
        prettyNum(round(365*5/7*input$n_vins_dias*weighted.mean(
          x = filteredData$Price*(filteredData$Qty_max - filteredData$Qty),
          w = filteredData$phantom_n), 2),
          big.mark = '.', decimal.mark = ','))
      # 365 dias no ano, 5 dias uteis dentro dos 7 da semana
    })
    
    # Verificar se um arquivo foi enviado
    req(input$file1)
    
    # Ler os dados do arquivo CSV
    tempData <- readr::read_delim(input$file1$datapath, 
                           delim = "|", escape_double = FALSE, trim_ws = TRUE)
    
    # Manipulação de dados: filtrar tempData com base em algum critério
    # Exemplo: selecionar linhas onde uma coluna 'Value' é maior que 10
    # Suponha que 'Value' é o nome de uma das colunas do seu CSV
    # Altere 'Value' e '10' conforme necessário para sua aplicação específica

    tempData$Phantom <- do.call(rbind, str_split(tempData$Phantom, '-'))[, 2]
    filteredData <- tempData %>% 
      filter(Alternate %in% c('S01', '01')) %>% 
      #filter(str_starts(`Component PN`, '4-')) %>% 
      select(Phantom,
             PN = `Component PN`,
             Qty = `Component Quantity` ,
             Price = `Price with Markup`) %>% 
      filter(Qty>=0) %>% # reduzir quantidades negativas
      arrange(Phantom, PN, Qty) %>% # preparar para calculo de quantil
      group_by(PN, Phantom) %>%
      mutate(n = 1) %>% # utilizado para formar o quantil
      mutate(Phantom = factor(Phantom),
             Qty_mean = mean(Qty),
             Qty_max = max(Qty),
             Quantil = cumsum(n)/max(n())) %>%
      mutate(phantom_n = n()) %>% # utilizado na media do custo ponderada (ver ui)
      rowwise() %>%
      mutate(# n * P(X>x) * (Qty ) ####
             Pedra_sobra = input$n_vins_dias * Quantil * (Qty - Qty_mean),
             # P(X<=x) * valor das pecas que sobraram ####
             Perda_falta =  (1-Quantil)*input$loss_, # P(X>x) * multa
             Perda = Pedra_sobra + Perda_falta) %>%
      group_by(Phantom, PN) %>% 
      mutate(Recomendado = min(Perda)) %>%
      filter(Recomendado==Perda) %>%
      arrange(Phantom, PN) %>%
      ungroup %>%
      mutate_if(is.numeric, round, digits = 2) %>%
      select(Phantom, PN, Qty, Qty_max, Price, Quantil, phantom_n) %>%
      na.omit()# %>% filter(Qty!=Qty_max)
    
    # Armazenar os dados filtrados para exibição
    csvData(filteredData)
  })
  
  # Renderizar o DataTable com os dados filtrados
  output$dataTable <- DT::renderDT({
    req(csvData()) # Assegura que os dados estejam disponíveis
    DT::datatable(csvData() %>% select(-phantom_n), filter = 'top', editable = TRUE)
    
  })
}

# Executar o aplicativo
shinyApp(ui = ui, server = server)
