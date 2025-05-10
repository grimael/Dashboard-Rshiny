library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(ggplot2)
library(plotly)
library(highcharter)
library(dplyr)
library(DT)

# Load data with corrections
df <- read.csv("Base_Work.csv", sep=";", fileEncoding="UTF-8-BOM", stringsAsFactors=FALSE)

# Verify and convert column types
df$Sales <- as.numeric(df$Sales)
df$Profit <- as.numeric(df$Profit)
df$Quantity <- as.integer(df$Quantity)
df$Order.Date <- as.Date(df$Order.Date, format="%d/%m/%Y")

# Remove rows with NA in dates
df <- df[!is.na(df$Order.Date), ]

# UI 
ui <- dashboardPage(
  skin="yellow",
  dashboardHeader(title = "Tableau de Bord "),
  dashboardSidebar(
    
    
    sidebarMenu(
      menuItem("Performance des ventes", tabName = "sales", icon = icon("dollar-sign")),
      menuItem("Clients et segmentation", tabName = "clients", icon = icon("users")),
      menuItem("Logistiques et produits", tabName = "logistics", icon = icon("truck"))
    ),
    menuItem("Filtres Globaux", icon = icon("filter"),
             dateRangeInput("date_filter", "Période", 
                            start = min(df$Order.Date),
                            end = max(df$Order.Date)),
             selectizeInput("category_filter", "Catégories", 
                            choices = unique(df$Category),
                            multiple = TRUE),
             selectizeInput("region_filter", "Régions", 
                            choices = unique(df$Region),
                            multiple = TRUE),
             actionButton("reset_filters", "Réinitialiser", icon = icon("undo"))
    )
    
    
  ),
  dashboardBody(
    tabItems(
      # Sales Tab
      tabItem(tabName = "sales",
              fluidRow(
                valueBoxOutput("total_sales", width = 4),
                valueBoxOutput("total_orders", width = 4),
                valueBoxOutput("total_quantity", width = 4)
              ),
              fluidRow(
                valueBoxOutput("avg_margin", width = 4),
                valueBoxOutput("avg_discount", width = 4),
                valueBoxOutput("top_order_value", width = 4)
              ),
              fluidRow(
                box(plotlyOutput("top_products"), width = 12, title = "Top 5 Produits"),
                box(plotlyOutput("top_profitable_products"), width = 12, title = "Top 5 Produits les Plus Rentables")
              ),
              fluidRow(
                box(plotlyOutput("sales_trend"), width = 6, title = "Tendance des Ventes"),
                box(plotlyOutput("profit_trend"), width = 6, title = "Tendance des Profits")
              ),
              fluidRow(
                box(highchartOutput("category_sales"), width = 6, title = "Ventes par Catégorie"),
                box(highchartOutput("category_profit"), width = 6, title = "Profits par Catégorie")
              )
      ),
      
      # Logistics Tab
      tabItem(tabName = "logistics",
              fluidRow(
                #valueBoxOutput("avg_shipping_time", width = 6),
                # box(title = "Mode de Livraison le Plus Utilisé", 
                # highchartOutput("most_used_ship_mode"), width = 4),
                #box(title = "Mode de Livraison - Délai Moyen le Plus Court", 
                #plotlyOutput("fastest_ship_mode"), width = 4)
              ),
              fluidRow(
                box(title = "Mode de Livraison - Coût le Plus Élevé (Marge Perdue)", 
                    plotlyOutput("costliest_ship_mode"), width = 6),
                box(title = "Répartition des Ventes par Catégorie", 
                    plotlyOutput("sales_by_category"), width = 6)
              ),
              fluidRow(
                box(title = "Sous-Catégories les Plus Populaires (Volume)", 
                    plotlyOutput("top_subcat_volume"), width = 6),
                box(title = "Sous-Catégories les Plus Rentables (Profit)", 
                    plotlyOutput("top_subcat_profit"), width = 6)
              ),
              fluidRow(
                box(title = "Villes avec le Plus Grand Volume de Ventes", 
                    plotlyOutput("top_cities_sales"), width = 6),
                box(title = "États/Régions avec le Plus Grand Volume de Ventes", 
                    highchartOutput("sales_by_state_map"), width = 6)
              ),
              fluidRow(
                box(title = "Produits avec le Plus Haut Taux de Remise Moyen", 
                    plotlyOutput("top_discount_products"), width = 12)
              )
      ),
      
      # Clients Tab
      tabItem(tabName = "clients",
              fluidRow(
                # KPI Cards
                valueBoxOutput("total_customers", width = 3),
                valueBoxOutput("avg_orders_per_customer", width = 3),
                valueBoxOutput("single_order_customers", width = 3),
                valueBoxOutput("repeat_customers", width = 3)
              ),
              fluidRow(
                # Additional KPI Cards
                valueBoxOutput("avg_time_between_orders", width = 6),
                valueBoxOutput("client_retention_rate", width = 6),
                #box(title = "Distribution des Commandes", 
                # plotlyOutput("orders_distribution"), width = 4)
              ),
              fluidRow(
                # Sales Distribution by Segment
                box(title = "Répartition des Ventes par Segment", 
                    highchartOutput("sales_by_segment"), width = 6),
                
                # Top 5 Customers by Spending
                box(title = "Top 5 Clients - Dépenses", 
                    plotlyOutput("top_spending_customers"), width = 6)
              ),
              fluidRow(
                # Top 5 Customers by Profit
                box(title = "Top 5 Clients - Rentabilité", 
                    plotlyOutput("top_profitable_customers"), width = 6),
                
                # Customers by Region
                box(title = "Répartition des Clients par Région", 
                    highchartOutput("customers_by_region"), width = 6)
              )
      )
    )
  )
)

# Server Function
server <- function(input, output,session) {
  # Réactive pour les données filtrées
  filtered_data <- reactive({
    # Commencez avec le dataframe original
    filtered_df <- df
    
    # Filtrage par date
    if (!is.null(input$date_filter)) {
      filtered_df <- filtered_df %>%
        filter(Order.Date >= input$date_filter[1] & 
                 Order.Date <= input$date_filter[2])
    }
    
    # Filtrage par catégorie
    if (length(input$category_filter) > 0) {
      filtered_df <- filtered_df %>%
        filter(Category %in% input$category_filter)
    }
    
    # Filtrage par région
    if (length(input$region_filter) > 0) {
      filtered_df <- filtered_df %>%
        filter(Region %in% input$region_filter)
    }
    
    return(filtered_df)
  })
  
  # Bouton de réinitialisation des filtres
  observeEvent(input$reset_filters, {
    updateDateRangeInput(session, "date_filter", 
                         start = min(df$Order.Date),
                         end = max(df$Order.Date))
    updateSelectizeInput(session, "category_filter", selected = NULL)
    updateSelectizeInput(session, "region_filter", selected = NULL)
  })
  
  
  
  # Compute customer-related metrics
  
  # Logistics Tab Outputs
  
  # Average Shipping Time (KPI Card)
  output$avg_shipping_time <- renderValueBox({
    avg_time <- df %>%
      mutate(shipping_time = as.numeric(Ship.Date - Order.Date)) %>%
      summarise(avg_days = mean(shipping_time, na.rm = TRUE)) %>%
      pull(avg_days)
    
    valueBox(
      paste(round(avg_time, 1), "jours"), 
      "Délai Moyen d'Expédition", 
      icon = icon("clock"), 
      color = "teal"
    )
  })
  
  # Most Used Shipping Mode (Pie Chart)
  output$most_used_ship_mode <- renderHighchart({
    ship_mode_dist <- df %>%
      group_by(Ship.Mode) %>%
      summarise(Count = n()) %>%
      arrange(desc(Count))
    
    hchart(ship_mode_dist, "pie", hcaes(x = Ship.Mode, y = Count)) %>%
      hc_title(text = "Distribution des Modes de Livraison") %>%
      hc_tooltip(pointFormat = '<b>{point.name}</b>: {point.y} commandes')
  })
  
  # Fastest Shipping Mode (Horizontal Bar Chart)
  output$fastest_ship_mode <- renderPlotly({
    ship_mode_speed <- df %>%
      mutate(shipping_time = as.numeric(Ship.Date - Order.Date)) %>%
      group_by(Ship.Mode) %>%
      summarise(Avg_Shipping_Time = mean(shipping_time, na.rm = TRUE)) %>%
      arrange(Avg_Shipping_Time)
    
    plot_ly(ship_mode_speed, 
            x = ~Avg_Shipping_Time, 
            y = ~Ship.Mode, 
            type = 'bar', 
            orientation = 'h',
            marker = list(color = 'purple')) %>%
      layout(title = "Délai Moyen par Mode de Livraison",
             xaxis = list(title = "Délai Moyen (jours)"),
             yaxis = list(title = "Mode de Livraison"))
  })
  
  # Costliest Shipping Mode by Lost Margin (Horizontal Bar Chart)
  output$costliest_ship_mode <- renderPlotly({
    ship_mode_cost <- df %>%
      group_by(Ship.Mode) %>%
      summarise(Total_Profit = sum(Profit, na.rm = TRUE)) %>%
      mutate(Lost_Margin = -Total_Profit) %>%  # Assuming negative profit indicates lost margin
      arrange(desc(Lost_Margin))
    
    plot_ly(ship_mode_cost, 
            x = ~Lost_Margin, 
            y = ~Ship.Mode, 
            type = 'bar', 
            orientation = 'h',
            marker = list(color = 'red')) %>%
      layout(title = "Coût en Marge Perdue par Mode de Livraison",
             xaxis = list(title = "Marge Perdue"),
             yaxis = list(title = "Mode de Livraison"))
  })
  
  # Sales by Category (Vertical Bar Chart)
  output$sales_by_category <- renderPlotly({
    category_sales <- df %>%
      group_by(Category) %>%
      summarise(Total_Sales = sum(Sales, na.rm = TRUE))
    
    plot_ly(category_sales, 
            x = ~Category, 
            y = ~Total_Sales, 
            type = 'bar',
            marker = list(color = 'blue')) %>%
      layout(title = "Ventes par Catégorie",
             xaxis = list(title = "Catégorie"),
             yaxis = list(title = "Ventes Totales"))
  })
  
  # Top Sub-Categories by Volume (Horizontal Bar Chart)
  output$top_subcat_volume <- renderPlotly({
    subcat_volume <- df %>%
      group_by(Sub.Category) %>%
      summarise(Total_Quantity = sum(Quantity, na.rm = TRUE)) %>%
      arrange(desc(Total_Quantity)) %>%
      head(5)
    
    plot_ly(subcat_volume, 
            x = ~Total_Quantity, 
            y = ~Sub.Category, 
            type = 'bar', 
            orientation = 'h',
            marker = list(color = 'orange')) %>%
      layout(title = "Top 5 Sous-Catégories par Volume",
             xaxis = list(title = "Quantité Totale"),
             yaxis = list(title = "Sous-Catégorie"))
  })
  
  # Top Sub-Categories by Profit (Horizontal Bar Chart)
  output$top_subcat_profit <- renderPlotly({
    subcat_profit <- df %>%
      group_by(Sub.Category) %>%
      summarise(Total_Profit = sum(Profit, na.rm = TRUE)) %>%
      arrange(desc(Total_Profit)) %>%
      head(5)
    
    plot_ly(subcat_profit, 
            x = ~Total_Profit, 
            y = ~Sub.Category, 
            type = 'bar', 
            orientation = 'h',
            marker = list(color = 'green')) %>%
      layout(title = "Top 5 Sous-Catégories par Profit",
             xaxis = list(title = "Profit Total"),
             yaxis = list(title = "Sous-Catégorie"))
  })
  
  # Top Cities by Sales Volume (Horizontal Bar Chart)
  output$top_cities_sales <- renderPlotly({
    city_sales <- df %>%
      group_by(City) %>%
      summarise(Total_Sales = sum(Sales, na.rm = TRUE)) %>%
      arrange(desc(Total_Sales)) %>%
      head(5)
    
    plot_ly(city_sales, 
            x = ~Total_Sales, 
            y = ~City, 
            type = 'bar', 
            orientation = 'h',
            marker = list(color = 'teal')) %>%
      layout(title = "Top 5 Villes par Volume de Ventes",
             xaxis = list(title = "Ventes Totales"),
             yaxis = list(title = "Ville"))
  })
  
  # Sales by State/Region (Choropleth Map)
  output$sales_by_state_map <- renderHighchart({
    state_sales <- df %>%
      group_by(State) %>%
      summarise(Total_Sales = sum(Sales, na.rm = TRUE))
    
    # Note: For a choropleth map, you need state codes (e.g., "CA" for California) and Highcharts map data
    # Assuming 'State' contains full names, you'll need to map them to 2-letter codes for a real map
    hcmap("countries/us/us-all", data = state_sales, value = "Total_Sales",
          joinBy = c("name", "State"), name = "Ventes par État",
          dataLabels = list(enabled = TRUE, format = "{point.name}")) %>%
      hc_title(text = "Ventes par État") %>%
      hc_colorAxis(minColor = "#f7f7f7", maxColor = "#2b8cbe") %>%
      hc_tooltip(pointFormat = '<b>{point.name}</b>: {point.value:,.0f} €')
  })
  
  # Top Products by Average Discount Rate (Horizontal Bar Chart)
  output$top_discount_products <- renderPlotly({
    discount_products <- df %>%
      group_by(Product.Name) %>%
      summarise(Avg_Discount = mean(Discount, na.rm = TRUE)) %>%
      arrange(desc(Avg_Discount)) %>%
      head(5)
    
    plot_ly(discount_products, 
            x = ~Avg_Discount, 
            y = ~Product.Name, 
            type = 'bar', 
            orientation = 'h',
            marker = list(color = 'purple')) %>%
      layout(title = "Top 5 Produits par Taux de Remise Moyen",
             xaxis = list(title = "Taux de Remise Moyen"),
             yaxis = list(title = "Nom du Produit"))
  })
  
  customer_metrics <- reactive({
    # Total unique customers
    total_customers <- n_distinct(df$Customer.Name)
    
    # Customer order frequency
    customer_order_counts <- df %>%
      group_by(Customer.Name) %>%
      summarise(order_count = n_distinct(Order.ID))
    
    # Single order customers
    single_order_customers <- sum(customer_order_counts$order_count == 1)
    
    # Repeat customers (more than 10 orders)
    repeat_customers <- sum(customer_order_counts$order_count > 10)
    
    # Average orders per customer
    avg_orders_per_customer <- mean(customer_order_counts$order_count)
    
    # Time between orders for repeat customers
    repeat_customer_orders <- df %>%
      filter(Customer.Name %in% (customer_order_counts %>% 
                                   filter(order_count > 1) %>% 
                                   pull(Customer.Name)))
    
    avg_time_between_orders <- repeat_customer_orders %>%
      group_by(Customer.Name) %>%
      arrange(Order.Date) %>%
      mutate(days_between = as.numeric(Order.Date - lag(Order.Date))) %>%
      summarise(avg_days = mean(days_between, na.rm = TRUE)) %>%
      pull(avg_days) %>%
      mean(na.rm = TRUE)
    
    list(
      total_customers = total_customers,
      single_order_customers = single_order_customers,
      repeat_customers = repeat_customers,
      avg_orders_per_customer = round(avg_orders_per_customer, 1),
      avg_time_between_orders = round(avg_time_between_orders, 2)
    )
  })
  
  # Sales Performance Outputs (from previous implementation)
  output$total_sales <- renderValueBox({
    valueBox(format(sum(df$Sales, na.rm=TRUE), big.mark=","), "Chiffre d'Affaires Total", icon = icon("dollar-sign"), color = "green")
  })
  
  output$total_orders <- renderValueBox({
    valueBox(length(unique(df$Order.ID)), "Nombre Total de Commandes", icon = icon("shopping-cart"), color = "blue")
  })
  
  output$total_quantity <- renderValueBox({
    valueBox(sum(df$Quantity, na.rm=TRUE), "Nombre Total de Produits Vendus", icon = icon("boxes"), color = "purple")
  })
  
  output$avg_margin <- renderValueBox({
    valueBox(round(mean(df$Profit, na.rm=TRUE), 2), "Marge Bénéficiaire Moyenne", icon = icon("chart-pie"), color = "yellow")
  })
  
  output$avg_discount <- renderValueBox({
    valueBox(round(mean(df$Discount, na.rm=TRUE) * 100, 2), "Taux de Remise Moyen", icon = icon("percent"), color = "red")
  })
  
  output$top_order_value <- renderValueBox({
    valueBox(format(max(df$Sales, na.rm=TRUE), big.mark=","), "Commande la Plus Élevée", icon = icon("shopping-basket"), color = "orange")
  })
  
  # Clients Tab Value Boxes
  output$total_customers <- renderValueBox({
    valueBox(
      customer_metrics()$total_customers, 
      "Nombre Total de Clients", 
      icon = icon("users"), 
      color = "blue"
    )
  })
  
  output$avg_orders_per_customer <- renderValueBox({
    valueBox(
      customer_metrics()$avg_orders_per_customer, 
      "Commandes Moyennes par Client", 
      icon = icon("shopping-cart"), 
      color = "green"
    )
  })
  
  output$single_order_customers <- renderValueBox({
    valueBox(
      customer_metrics()$single_order_customers, 
      "Clients à Commande Unique", 
      icon = icon("user-times"), 
      color = "red"
    )
  })
  
  output$repeat_customers <- renderValueBox({
    valueBox(
      customer_metrics()$repeat_customers, 
      "Clients Récurrents", 
      icon = icon("user-check"), 
      color = "purple"
    )
  })
  
  output$avg_time_between_orders <- renderValueBox({
    valueBox(
      paste(customer_metrics()$avg_time_between_orders, "jours"), 
      "Délai Moyen Entre Commandes", 
      icon = icon("clock"), 
      color = "yellow"
    )
  })
  
  # Sales Distribution by Segment Pie Chart
  output$sales_by_segment <- renderHighchart({
    segment_sales <- df %>%
      group_by(Segment) %>%
      summarise(Total_Sales = sum(Sales, na.rm = TRUE))
    
    hchart(segment_sales, "pie", hcaes(x = Segment, y = Total_Sales)) %>%
      hc_title(text = "Répartition des Ventes par Segment") %>%
      hc_tooltip(pointFormat = '<b>{point.name}</b>: {point.y:,.0f} €')
  })
  
  # Top 5 Spending Customers Horizontal Bar Chart
  output$top_spending_customers <- renderPlotly({
    top_spending_customers <- df %>%
      group_by(Customer.Name) %>%
      summarise(Total_Sales = sum(Sales, na.rm = TRUE)) %>%
      arrange(desc(Total_Sales)) %>%
      head(5)
    
    plot_ly(top_spending_customers, 
            x = ~Total_Sales, 
            y = ~Customer.Name, 
            type = 'bar', 
            orientation = 'h',
            marker = list(color = 'blue')) %>%
      layout(title = "Top 5 Clients - Dépenses",
             xaxis = list(title = "Ventes Totales"),
             yaxis = list(title = "Nom du Client"))
  })
  
  # Top 5 Profitable Customers Horizontal Bar Chart
  output$top_profitable_customers <- renderPlotly({
    top_profitable_customers <- df %>%
      group_by(Customer.Name) %>%
      summarise(Total_Profit = sum(Profit, na.rm = TRUE)) %>%
      arrange(desc(Total_Profit)) %>%
      head(5)
    
    plot_ly(top_profitable_customers, 
            x = ~Total_Profit, 
            y = ~Customer.Name, 
            type = 'bar', 
            orientation = 'h',
            marker = list(color = 'green')) %>%
      layout(title = "Top 5 Clients - Rentabilité",
             xaxis = list(title = "Profit Total"),
             yaxis = list(title = "Nom du Client"))
  })
  
  # Customers by Region
  output$customers_by_region <- renderHighchart({
    region_sales <- df %>%
      group_by(Region) %>%
      summarise(
        Total_Sales = sum(Sales, na.rm = TRUE),
        Customer_Count = n_distinct(Customer.Name)
      )
    
    hchart(region_sales, "column", hcaes(x = Region, y = Customer_Count)) %>%
      hc_title(text = "Répartition des Clients par Région") %>%
      hc_tooltip(pointFormat = '<b>{point.name}</b>: {point.y} clients')
  })
  
  # Client Retention Rate (Placeholder)
  output$client_retention_rate <- renderValueBox({
    valueBox(
      "60%", 
      "Taux de Rétention des Clients", 
      icon = icon("chart-line"), 
      color = "orange"
    )
  })
  
  # Orders Distribution
  output$orders_distribution <- renderPlotly({
    order_counts <- df %>%
      group_by(Customer.Name) %>%
      summarise(order_count = n_distinct(Order.ID))
    
    plot_ly(order_counts, x = ~order_count) %>%
      add_histogram(nbinsx = 10) %>%
      layout(title = "Distribution du Nombre de Commandes par Client",
             xaxis = list(title = "Nombre de Commandes"),
             yaxis = list(title = "Nombre de Clients"))
  })
  
  # Previous sales performance plots (from first implementation)
  output$top_products <- renderPlotly({
    top_products <- df %>% 
      group_by(Product.Name) %>% 
      summarise(Total_Sales = sum(Sales, na.rm=TRUE)) %>% 
      arrange(desc(Total_Sales)) %>% 
      head(5)
    
    plot_ly(top_products, x = ~Total_Sales, y = ~Product.Name, type = 'bar', orientation = 'h',
            marker = list(color = 'blue')) %>%
      layout(title = "Top 5 Produits par Ventes",
             xaxis = list(title = "Ventes Totales"),
             yaxis = list(title = "Nom du Produit"))
  })
  
  output$top_profitable_products <- renderPlotly({
    top_profit <- df %>% 
      group_by(Product.Name) %>% 
      summarise(Total_Profit = sum(Profit, na.rm=TRUE)) %>% 
      arrange(desc(Total_Profit)) %>% 
      head(5)
    
    plot_ly(top_profit, x = ~Total_Profit, y = ~Product.Name, type = 'bar', orientation = 'h',
            marker = list(color = 'green')) %>%
      layout(title = "Top 5 Produits par Rentabilité",
             xaxis = list(title = "Profit Total"),
             yaxis = list(title = "Nom du Produit"))
  })
  
  output$sales_trend <- renderPlotly({
    sales_trend <- df %>% 
      group_by(Order.Date) %>% 
      summarise(Total_Sales = sum(Sales, na.rm=TRUE))
    
    plot_ly(sales_trend, x = ~Order.Date, y = ~Total_Sales, type = 'scatter', mode = 'lines',
            line = list(color = 'blue')) %>%
      layout(title = "Évolution des Ventes",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Ventes Totales"))
  })
  
  output$profit_trend <- renderPlotly({
    profit_trend <- df %>% 
      group_by(Order.Date) %>% 
      summarise(Total_Profit = sum(Profit, na.rm=TRUE))
    
    plot_ly(profit_trend, x = ~Order.Date, y = ~Total_Profit, type = 'scatter', mode = 'lines',
            line = list(color = 'green')) %>%
      layout(title = "Évolution du Profit",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Profit Total"))
  })
  
  output$category_sales <- renderHighchart({
    category_sales <- df %>% 
      group_by(Category) %>% 
      summarise(Total_Sales = sum(Sales, na.rm=TRUE))
    
    hchart(category_sales, "pie", hcaes(x = Category, y = Total_Sales)) %>%
      hc_title(text = "Ventes par Catégorie") %>%
      hc_tooltip(pointFormat = '<b>{point.name}</b>: {point.y:,.0f} €')
  })
  
  output$category_profit <- renderHighchart({
    category_profit <- df %>% 
      group_by(Category) %>% 
      summarise(Total_Profit = sum(Profit, na.rm=TRUE))
    
    hchart(category_profit, "pie", hcaes(x = Category, y = Total_Profit)) %>%
      hc_title(text = "Profits par Catégorie") %>%
      hc_tooltip(pointFormat = '<b>{point.name}</b>: {point.y:,.0f} €')
  })
}

# Run the application
shinyApp(ui, server)