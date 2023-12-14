# Pacotes ----
if (!require(shiny)) install.packages("shiny")
if (!require(shinydashboard)) install.packages("shinydashboard")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(dplyr)) install.packages("dplyr")
if (!require(reshape2)) install.packages("reshape2")
if (!require(lubridate)) install.packages("lubridate")
if (!require(plotly)) install.packages("plotly")

library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(reshape2)
library(lubridate)
library(plotly)

# Carga de dados ----
df3 <- read.csv("https://raw.githubusercontent.com/Leandrogomesf/phd_thesis/main/df3.csv", sep = ",")
df3$data <- as.Date(df3$data)

# UI ----
ui <- dashboardPage(
  dashboardHeader(
    title = "ANÁLISE DE COMMODITY - PONTA",
    titleWidth = 500
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Medidas Resumo", tabName = "medidas_resumo"),
      menuItem("Gráfico Valor Atual e Real", tabName = "df3_plot_rxa"),
      # menuItem("Gráfico IPCA", tabName = "df3_plot_ipca"),
      menuItem("Summary Table", tabName = "summary_table")
    ),
    downloadButton("download_plot", "Download Selected Graph", class = "btn-primary")
    
    
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "medidas_resumo",
        plotlyOutput("medidas_plot")
      ),
      tabItem(
        tabName = "df3_plot_rxa",
        plotlyOutput("df3_plot_rxa")
      ),
      #tabItem(
      #  tabName = "df3_plot_ipca",
      #  plotlyOutput("df3_plot_ipca")
      #),
      tabItem(
        tabName = "summary_table",
        DTOutput("summary_table_output")
      )
    )
  )
)

# Server ----
server <- function(input, output) {
  
  # Medidas Resumo plot ----
  output$medidas_plot <- renderPlotly({
    df3_melt <- df3[c("data", "valor", "real")]
    df3_melt <- melt(df3_melt, id.vars = "data")
    df3_melt$ano <- format(df3_melt$data, format = "%Y")
    df3_melt$variable <- gsub("valor", "atual", df3_melt$variable)
    
    medidas_plot <- ggplot(df3_melt, aes(x = ano, y = value, fill = variable)) +
      geom_boxplot(position = position_dodge(1),
                   outlier.colour = "red", outlier.shape = 8,
                   outlier.size = 4) +
      geom_dotplot(binaxis = 'y', stackdir = 'center',
                   position = position_dodge(1),
                   dotsize = 0.7) +
      theme_minimal() +
      scale_fill_manual(values = c("#56B4E9", "#E69F00")) +
      labs(title = "Medidas Resumo",
           x = NULL, y = "Value") +
      theme(legend.title = element_blank())
    
    ggplotly(medidas_plot)
  })
  
  # Gráfico Valor Atual e Real plot ----
  output$df3_plot_rxa <- renderPlotly({
    df3_melt <- df3[c("data", "valor", "real")]
    df3_melt <- melt(df3_melt, id.vars = "data")
    df3_melt$ano <- format(df3_melt$data, format = "%Y")
    df3_melt$variable <- gsub("valor", "atual", df3_melt$variable)
    
    df3_plot_rxa <- ggplot(data=df3_melt, aes(data, value, fill=variable)) +
      geom_bar(stat="identity", position=position_dodge())+
      theme_minimal() +
      theme(legend.position="bottom",
            legend.title = element_blank()) +
      theme(axis.text.x = element_text(angle = 45, hjust=1))+
      scale_fill_manual(values=c("#56B4E9", "#E69F00")) +
      labs(title = "Valor atual e valor real da commoditie", x=NULL, y="Reais")+
      scale_x_continuous(label = date_format("%m/%y"), breaks = unique(df3_melt$data))+
      scale_y_continuous(breaks = seq(0, 350, 50))
    
    ggplotly(df3_plot_rxa)
  })
  
  # Gráfico IPCA plot ----
  output$df3_plot_ipca <- renderPlotly({
    df3_melt2 <- melt(df3[c("data", "IPCA", "IPCA acumulado")], id.vars = "data")
    
    df3_plot_ipca <- ggplot(data = df3_melt2, aes(x = data, y = value, group = variable, color = variable)) +
      geom_line(size = 1) +
      geom_point() +
      theme_minimal() +
      scale_x_date(labels = date_format("%m/%y"), breaks = unique(df3$data)) +
      theme(axis.text.x = element_text(angle = 45),
            legend.title = element_blank(),
            legend.position = "bottom") +
      labs(title = "Valor IPCA ao longo dos anos", x = NULL, y = NULL) +
      scale_color_manual(values = c("#999999", "#E69F00")) +
      scale_y_continuous(breaks = seq(-1, 2, 0.2))
    
    ggplotly(df3_plot_ipca)
  })
  
  
  # Summary table ----
  output$summary_table_output <- renderDT({
    summary_df <- as.data.frame(do.call(cbind, lapply(df3, summary)))
    datatable(summary_df, options = list(lengthMenu = c(5, 10, 20), pageLength = 10))
  })
}

# Run the app ----
shinyApp(ui, server)
