# ===== Loading the packages =====
library(shiny)
library(quantmod)
library(tidyverse)
library(tidyquant)
library(plotly)
library(lubridate)
library(reshape2)


# ===== Black-Scholes Call Option Formula =====
bs_call <- function(S, K, T, r, sigma) {
  d1 <- (log(S / K) + (r + 0.5 * sigma^2) * T) / (sigma * sqrt(T))
  d2 <- d1 - sigma * sqrt(T)
  S * pnorm(d1) - K * exp(-r * T) * pnorm(d2)
}

# ===== Black-Schole Put Option Formula =====
bs_put <- function(S, K, T, r, sigma) {
  d1 <- (log(S / K) + (r + 0.5 * sigma^2) * T) / (sigma * sqrt(T))
  d2 <- d1 - sigma * sqrt(T)
  K * exp(-r * T) * pnorm(-d2) - S * pnorm(-d1)
}

# ===== Implied Volatility Formula =====
implied_vol <- function(S, K, T, r, market_price, q = 0){
  obj <- function(sigma) {
    bs_call_iv <- bs_call(S, K, T, r, sigma)
    return((bs_call_iv-market_price)^2)
  }
  tryCatch(
    opt(obj, c(0.0001, 3), tol = 1e-6)$minimum,
    error = function(e) NA
  )
}


# ===== UI =====
ui <- fluidPage(
  titlePanel("Black-Scholes Option Pricing Application"),
  
  # Stock Search Tab
  tabsetPanel(
    tabPanel("Candlestick Plot",
             sidebarLayout(
               # Side Panel
               sidebarPanel(
                 # Input Fields
                 textInput("ticker_input", "Enter Stock Ticker:", value = "AAPL"),
                 dateInput("expiry_input", "Select Expiry Date:", value = Sys.Date() + 30),
                 uiOutput("range_slider_ui"),
                 br(),
                 # Actions Buttons (Fetch & Refresh buttons)
                 actionButton("add", "Add to Compare ➕"),
                 actionButton("fetch", "Fetch Data 📥"),
                 br(),
                 br(),
                 actionButton("clear_all", "Clear All Tickers ❌"),
                 actionButton("delete_last", "Delete Last Stock 🔙")
               ),
               # Main Panel
               mainPanel(
                 # Detail Table
                 fluidRow(
                   column(12,
                          h2("Stock Details for Selected Tickers"),
                          tableOutput("market_table_tab1")
                   )
                 ),
                 fluidRow(
                   # Candlestick Chart
                   column(12,
                          h4("Candlestick Plot of Currently Selected"),
                          plotlyOutput("candlestick_plot", height = "420px")
                   )
                 )
               )
             )
    ),
    tabPanel("Comparing Different Stocks",
             sidebarLayout(
               # Side Panel
               sidebarPanel(
                 # Input Fields
                 textInput("ticker_input", "Enter Stock Ticker:", value = "AAPL"),
                 dateInput("expiry_input", "Select Expiry Date:", value = Sys.Date() + 30),
                 uiOutput("linked_slider"),
                 br(),
                 # Actions Buttons (Fetch & Refresh buttons)
                 actionButton("add", "Add to Compare ➕"),
                 br(),
                 br(),
                 actionButton("clear_all", "Clear All Tickers ❌"),
                 actionButton("delete_last", "Delete Last Stock 🔙")
               ),
               # Main Panel
               mainPanel(
                 # Detail Table
                 fluidRow(
                   column(12,
                          h2("Stock Details for Selected Tickers"),
                          tableOutput("market_table_tab2")
                   )
                 ),
                 fluidRow(
                   # Cummalative Line Chart
                   column(12,
                          h4("Line Plot of All Selected Tickers"),
                          plotlyOutput("line_plot", height = "420px")
                   )
                 )
               )
             )),
    
    # Black-Scholes Model & Heatmap Tab
    tabPanel("Black-Schole Model",
             sidebarLayout(
               # Side Panel
               sidebarPanel(
                 numericInput("current_asset_price_input", "Current Asset Price", value = 100, min = 0, step = 0.01),
                 numericInput("strike_price_input", "Strike Price", value = 100, min = 0, step = 0.01),
                 numericInput("time_to_maturity_input", "Time to Maturity (Years)", value = 1),
                 numericInput("volatility_input", "Volatility (σ)", value = 0.2, min = 0, max = 1, step = 0.01),
                 numericInput("r_input", "Risk-Free Interest Rate", value = 0.05, min = 0, max = 1, step = 0.01),
                 br(), tags$hr(style = "border-top: 1px solid #999; margin: 10px 0;"), br(),
                 numericInput("min_spot_price_input", "Min Spot Price", value = 80, min = 0, step = 0.01),
                 numericInput("max_spot_price_input", "Max Spot Price", value = 120, min = 0, step = 0.01),
                 sliderInput("min_volatility_slider", "Min Volatility for Heatmap", min = 0, max = 1, value = 0.1),
                 sliderInput("max_volatility_slider", "Max Volatility for Heatmap", min = 0, max = 1, value = 0.3)
               ),
               # Main Panel
               mainPanel(
                 # Detail Table
                 fluidRow(
                   column(12,
                          h2("Stock Details for Selected Tickers"),
                          tableOutput("market_table_tab3")
                   )
                 ),
                 fluidRow(
                   # Call Heatmap
                   column(6,
                          h2("Call Price", style = "text-align: center; transform: translateX(-10%);"),
                          div(style = "width: 100%; aspect-ratio: 1 / 1;",
                              plotlyOutput("heatmap_call", width = "100%", height = "80%"))
                          
                   ),
                   # Put Heatmap
                   column(6,
                          h2("Put Price", style = "text-align: center; transform: translateX(-10%);"),
                          div(style = "width: 100%; aspect-ratio: 1 / 1;",
                              plotlyOutput("heatmap_put", width = "100%", height = "80%"))
                   ),
                   column(6, 
                          div(style = "width: 100%; aspect-ratio: 1 / 1; margin-top: -100px;",
                              plotlyOutput("surface_call", width = "100%", height = "100%"))),
                   column(6, 
                          div(style = "width: 100%; aspect-ratio: 1 / 1; margin-top: -100px;",
                              plotlyOutput("surface_put", width = "100%", height = "100%")))
                 )
               )
             )
    ),
    tabPanel("Implied Volatility Surface",
             sidebarLayout(
               sidebarPanel(
                 h4(strong("Model Parameters")),
                 numericInput("r_input_2", "Risk-Free Interest Rate", value = 0.015, min = 0, max = 1, step = 0.01),
                 numericInput("devidend_yield_input", "Dividend Yield", value = 0.013, min = 0, max = 1, step = 0.01),
                 br(),
                 h4(strong("Visualization Parameters")),
                 selectInput("y_type_input", "Select Y-asix:",
                             choices = c("Moneyness (Strike/Spot)", "Strike Price ($)"),
                             selected = "Moneyness (Strike/Spot)"),
                 br(),
                 h4(strong("Ticker Symbol")),
                 textInput("ticker_input_2", "Enter Stock Ticker:", value = "AAPL"),
                 br(),
                 h4(strong("Strike Price Filter Parameters")),
                 numericInput("min_strike_price_input_2", "Minimum Strike Price (% of Spot Price)", value = 80, min = 0, step = 0.01),
                 numericInput("max_strike_price_input_2", "Maximum Strike Price (% of Spot Price)", value = 120, min = 0, step = 0.01)
               ),
               mainPanel(plotlyOutput("iv_surface"))
             ))
  )
)

# ===== Server =====
server <- function(input, output, session) {
  market_data <- reactiveValues(all = list(), latest = NULL)
  iv_data <- reactiveVal()
  # Shared slider state for syncing both tabs
  slider_state <- reactiveVal(list(
    min = Sys.Date() - years(1),
    max = Sys.Date(),
    value = c(Sys.Date() - years(1), Sys.Date())
  ))
  
  # Delete Last Stock Button
  observeEvent(input$delete_last, {
    if (length(market_data$all) > 0) {
      last_ticker <- tail(names(market_data$all), 1)
      market_data$all[[last_ticker]] <- NULL
      showNotification(paste("Removed", last_ticker, "from comparison"), type = "message")
    } else {
      showNotification("No tickers to delete.", type = "warning")
    }
  })
  
  # Fetch Button (Only Fetches and Shows Latest Ticker for Candlestick)
  observeEvent(input$fetch, {
    ticker <- toupper(input$ticker_input)
    tryCatch({
      getSymbols(ticker, src = "yahoo", auto.assign = TRUE)
      ticker_data <- get(ticker)
      market_data$latest <- list(ticker = ticker, data = ticker_data)
    }, error = function(e) {
      showNotification(paste("Fetching failed for", ticker), type = "error")
    })
  })
  
  # Add Button (Adds Ticker to Compare Table and Line Plot)
  observeEvent(input$add, {
    ticker <- toupper(input$ticker_input)
    expiry <- input$expiry_input
    
    if (ticker %in% names(market_data$all)) {
      showNotification(paste(ticker, "already added."), type = "warning")
      return()
    }
    
    tryCatch({
      getSymbols(ticker, src = "yahoo", auto.assign = TRUE)
      ticker_data <- get(ticker)
      date_index <- as.Date(index(ticker_data))
      
      one_year_ago <- Sys.Date() - years(1)
      min_limit <- max(min(date_index), one_year_ago)
      max_limit <- max(date_index)
      default_range <- c(min_limit, max_limit)
      
      if (input$range_input[1] < min_limit || input$range_input[2] > max_limit) {
        slider_state(list(
          min = as.Date(min_limit),
          max = as.Date(max_limit),
          value = as.Date(default_range)
        ))
      }
      
      current_price <- as.numeric(Cl(ticker_data)[nrow(ticker_data)])
      T <- as.numeric(difftime(expiry, Sys.Date(), units = "days")) / 365
      returns <- dailyReturn(Cl(ticker_data))
      sigma <- sd(returns, na.rm = TRUE) * sqrt(252)
      getSymbols("DGS1", src = "FRED", auto.assign = TRUE)
      r <- as.numeric(last(na.omit(DGS1))) / 100
      
      market_data$all[[ticker]] <- list(
        price = current_price,
        T = T,
        sigma = sigma,
        r = r,
        ticker_data = ticker_data,
        range = default_range
      )
    }, error = function(e) {
      showNotification(paste("Adding failed for", ticker), type = "error")
    })
  })
  
  # First Tab Slider (Main UI)
  output$range_slider_ui <- renderUI({
    s <- slider_state()
    sliderInput("range_input", "Select Date Range:",
                min = s$min,
                max = s$max,
                value = s$value,
                timeFormat = "%m-%d-%Y")
  })
  
  # Second Tab Slider (Clone UI)
  output$linked_slider <- renderUI({
    s <- slider_state()
    sliderInput("range_input_clone", "Select Date Range:",
                min = s$min,
                max = s$max,
                value = s$value,
                timeFormat = "%m-%d-%Y")
  })
  
  # Sync slider value updates
  observeEvent(input$range_input, {
    slider_state(modifyList(slider_state(), list(value = input$range_input)))
  })
  
  observeEvent(input$range_input_clone, {
    slider_state(modifyList(slider_state(), list(value = input$range_input_clone)))
  })
  
  # Clear-All Button
  observeEvent(input$clear_all, {
    market_data$all <- list()
  })
  
  # Candlestick Plot
  output$candlestick_plot <- renderPlotly({
    req(market_data$latest)
    ticker_data <- market_data$latest$data
    range <- slider_state()$value
    date_index <- as.Date(index(ticker_data))
    date_filter <- date_index >= range[1] & date_index <= range[2]
    filtered_data <- ticker_data[date_filter]
    
    df <- data.frame(
      Date = index(filtered_data),
      Open = as.numeric(Op(filtered_data)),
      High = as.numeric(Hi(filtered_data)),
      Low  = as.numeric(Lo(filtered_data)),
      Close = as.numeric(Cl(filtered_data))
    )
    
    plot_ly(data = df, x = ~Date, type = "candlestick",
            open = ~Open, close = ~Close,
            high = ~High, low = ~Low,
            name = market_data$latest$ticker) %>%
      layout(
        xaxis = list(type = "category", title = "", tickformat = "%b %Y", tickmode = "auto", nticks = 10, rangeslider = list(visible = FALSE)),
        yaxis = list(title = "Price ($)"),
        plot_bgcolor = "white",
        paper_bgcolor = "white"
      )
  })
  
  # Line Plot
  output$line_plot <- renderPlotly({
    req(length(market_data$all) > 0)
    active_tickers <- names(market_data$all)
    plt <- plot_ly(type = 'scatter', mode = 'lines')
    range <- slider_state()$value
    
    for (ticker in active_tickers) {
      stock <- market_data$all[[ticker]]
      data <- stock$ticker_data
      date_index <- as.Date(index(data))
      date_filter <- date_index >= range[1] & date_index <= range[2]
      data_filtered <- data[date_filter]
      
      if (nrow(data_filtered) > 0) {
        df <- data.frame(
          Date = index(data_filtered),
          Close = as.numeric(Cl(data_filtered))
        )
        
        plt <- add_trace(
          plt,
          data = df,
          x = ~Date,
          y = ~Close,
          name = ticker,
          mode = 'lines',
          type = 'scatter'
        )
      }
    }
    
    plt %>%
      layout(
        xaxis = list(type = "category", title = "", tickformat = "%b %Y", tickmode = "auto", nticks = 10),
        yaxis = list(title = "Close Price ($)"),
        legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.1),
        plot_bgcolor = "white",
        paper_bgcolor = "white"
      )
  })
  
  # Call Heatmap
  output$heatmap_call <- renderPlotly({
    
    # Making sure the inputs are valid (Reactive Safeguard)
    req(input$min_spot_price_input, input$max_spot_price_input,
        input$min_volatility_slider, input$max_volatility_slider)
    
    # Creating a matrix based on the variation of the spot price and volatility
    spot_prices <- seq(input$min_spot_price_input, input$max_spot_price_input, length.out = 10)
    volatilities <- seq(input$min_volatility_slider, input$max_volatility_slider, length.out = 10)
    grid <- expand.grid(S = spot_prices, sigma = volatilities)
    
    # Calculating the call price using Black-Scholes Call Function for the matrix
    grid$call_price <- mapply(function(S, sigma) {
      bs_call(
        S = S,
        K = input$strike_price_input,
        T = input$time_to_maturity_input,
        r = input$r_input,
        sigma = sigma
      )
    }, grid$S, grid$sigma)
    
    # Rounding displayed result & changing text color to dark for brighter background cells
    grid$label <- sprintf("%.2f", grid$call_price)
    grid$text_color <- ifelse(grid$call_price > quantile(grid$call_price, 0.8), "black", "white")
    
    # Creating breaks for colorbar (rounding to nearest # divisible by 5)
    min_val <- floor(min(grid$call_price) / 5) * 5
    max_val <- ceiling(max(grid$call_price) / 5) * 5
    breaks_seq <- as.integer(seq(min_val, max_val, length.out = 5))
    
    # Creating the heatmap for Call Options
    hm_call <- ggplot(grid, aes(x = S, y = sigma, fill = call_price)) +
      geom_tile() +
      geom_text(aes(label = label, color = text_color), size = 2) +
      scale_fill_viridis_c(limit = range(min_val, max_val), 
                           breaks = breaks_seq) +
      theme_minimal() +
      theme(panel.grid.major = element_blank(),
            axis.text.x = element_text(size = 7),
            axis.text.y = element_text(size = 6, angle = 90)) +
      scale_x_continuous(breaks = round(unique(grid$S), 2)) +
      scale_y_continuous(breaks = round(unique(grid$sigma), 3)) +
      scale_color_manual(values = c("black" = "black", "white" = "white")) +
      labs(x = "Spot Price ($)", y = "Volatility (σ)", fill = "Call Price ($)") +
      guides(color = "none") 
    
    # Convert to Plotly
    ggplotly(hm_call) 
  })
  
  # 3D Call Surface Plot
  output$surface_call <- renderPlotly({
    
    # Making sure the inputs are valid (Reactive Safeguard)
    req(input$min_spot_price_input, input$max_spot_price_input, 
        input$min_volatility_slider, input$max_volatility_slider)
    
    # Creating a matrix based on the variation of the spot price and volatility
    spot_prices <- seq(input$min_spot_price_input, input$max_spot_price_input, length.out = 10)
    volatilities <- seq(input$min_volatility_slider, input$max_volatility_slider, length.out = 10)
    grid <- expand.grid(S = spot_prices, sigma = volatilities)
    
    # Calculating the call price using Black-Scholes Put Function for the matrix
    grid$call_price <- mapply(function(S, sigma){
      bs_call(
        S = S,
        K = input$strike_price_input, 
        T = input$time_to_maturity_input, 
        r = input$r_input,
        sigma = sigma
      )
    }, grid$S, grid$sigma)
    
    # Rounding displayed result & changing text color to dark for brighter background cells
    grid$label <- sprintf("%.2f", grid$call_price)
    
    #3D Surface Map
    z_matrix <- acast(grid, sigma ~ S, value.var = "call_price")
    
    plot_ly(x = sort(unique(grid$S)),
            y = sort(unique(grid$sigma)),
            z = ~z_matrix) %>%
      add_surface()
  })
  
  # Stock Detail Table Rendering
  render_table_fn <- function() {
    req(length(market_data$all) > 0)
    active_tickers <- names(market_data$all)
    do.call(rbind, lapply(active_tickers, function(ticker) {
      stock <- market_data$all[[ticker]]
      latest_date <- index(stock$ticker_data)[nrow(stock$ticker_data)]
      data.frame(
        Ticker = ticker,
        Date = format(latest_date, "%m-%d-%Y"),
        `Current_Asset_Price` = sprintf("$%.2f", stock$price),
        `Time_to_Maturity_Years` = sprintf("%.4f", stock$T),
        `Volatility_σ` = sprintf("%.4f", stock$sigma),
        `Risk_Free_Interest_Rate` = sprintf("%.4f", stock$r),
        stringsAsFactors = FALSE
      )
    }))
  }
  
  # Fetching detail table for all tabs
  output$market_table_tab1 <- renderTable(render_table_fn(), rownames = FALSE)
  output$market_table_tab2 <- renderTable(render_table_fn(), rownames = FALSE)
  output$market_table_tab3 <- renderTable(render_table_fn(), rownames = FALSE)
  
  # Put Heatmap
  output$heatmap_put <- renderPlotly({
    
    # Making sure the inputs are valid (Reactive Safeguard)
    req(input$min_spot_price_input, input$max_spot_price_input, 
        input$min_volatility_slider, input$max_volatility_slider)
    
    # Creating a matrix based on the variation of the spot price and volatility
    spot_prices <- seq(input$min_spot_price_input, input$max_spot_price_input, length.out = 10)
    volatilities <- seq(input$min_volatility_slider, input$max_volatility_slider, length.out = 10)
    grid <- expand.grid(S = spot_prices, sigma = volatilities)
    
    # Calculating the put price using Black-Scholes Put Function for the matrix
    grid$put_price <- mapply(function(S, sigma){
      bs_put(
        S = S,
        K = input$strike_price_input, 
        T = input$time_to_maturity_input, 
        r = input$r_input,
        sigma = sigma
      )
    }, grid$S, grid$sigma)
    
    # Rounding displayed result & changing text color to dark for brighter background cells
    grid$label <- sprintf("%.2f", grid$put_price)
    grid$text_color <- ifelse(grid$put_price > quantile(grid$put_price, 0.8), "black", "white")
    
    # Creating breaks for colorbar (rounding to nearest # divisible by 5)
    min_val <- floor(min(grid$put_price) / 5) * 5
    max_val <- ceiling(max(grid$put_price) / 5) * 5
    breaks_seq <- as.integer(seq(min_val, max_val, length.out = 5))
    
    # Creating the heatmap for Put Options
    hm_put <- ggplot(grid, aes(x = S, y = sigma, fill = put_price)) +
      geom_tile() +
      geom_text(aes(label = label, color = text_color), size = 2) +
      scale_fill_viridis_c(limit = range(min_val, max_val), 
                           breaks = breaks_seq) +
      theme_minimal() +
      theme(panel.grid.major = element_blank(),
            axis.text.x = element_text(size = 7),
            axis.text.y = element_text(size = 6, angle = 90)) +
      scale_x_continuous(breaks = round(unique(grid$S), 2)) +
      scale_y_continuous(breaks = round(unique(grid$sigma), 3)) +
      scale_color_manual(values = c("black" = "black", "white" = "white")) +
      labs(x = "Spot Price ($)", y = "Volatility (σ)", fill = "Put Price ($)") +
      guides(color = "none") 
    
    # Convert to Plotly
    ggplotly(hm_put) 
  })
  
  #3D Put Surface Plot
  output$surface_put <- renderPlotly({
    
    # Making sure the inputs are valid (Reactive Safeguard)
    req(input$min_spot_price_input, input$max_spot_price_input, 
        input$min_volatility_slider, input$max_volatility_slider)
    
    # Creating a matrix based on the variation of the spot price and volatility
    spot_prices <- seq(input$min_spot_price_input, input$max_spot_price_input, length.out = 10)
    volatilities <- seq(input$min_volatility_slider, input$max_volatility_slider, length.out = 10)
    grid <- expand.grid(S = spot_prices, sigma = volatilities)
    
    # Calculating the put price using Black-Scholes Put Function for the matrix
    grid$put_price <- mapply(function(S, sigma){
      bs_put(
        S = S,
        K = input$strike_price_input, 
        T = input$time_to_maturity_input, 
        r = input$r_input,
        sigma = sigma
      )
    }, grid$S, grid$sigma)
    
    # Rounding displayed result & changing text color to dark for brighter background cells
    grid$label <- sprintf("%.2f", grid$put_price)
    
    #3D Surface Map
    z_matrix <- acast(grid, sigma ~ S, value.var = "put_price")
    
    plot_ly(x = sort(unique(grid$S)),
            y = sort(unique(grid$sigma)),
            z = ~z_matrix) %>%
      add_surface()
  })
  
}

# ===== Run App =====
shinyApp(ui = ui, server = server)


