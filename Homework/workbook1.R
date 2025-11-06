LovelyFacetNNARModel <- function(event, bks, euler, scaleV) {
  tikrs <- c("AMZN", "NVDA", "COST", "WMT", "JNJ", "UNH", "ZSPC", "INLF")
  
  if (!(event %in% names(Timeframes))) stop("Invalid event name")
  
  range <- Timeframes[[event]]
  
  # Build a list of patchwork plots
  plots <- map(tikrs, function(tikr) {
    
    # Check if ticker has enough historical data
    tikr_data <- stocks %>% 
      filter(Ticker == tikr) %>%
      arrange(Date)
    
    if (sum(!is.na(tikr_data$Close) & tikr_data$Date < as.Date(range[1])) < 2) {
      # Not enough data to fit model, skip this stock
      message(paste("Skipping", tikr, "- not enough training data"))
      return(NULL)
    }
    
    LovelyNNARModel(
      tikr = tikr,
      tx = paste(tikr, "during", event),
      sbtx = NULL,
      bks = bks,
      range1 = range[1],
      range2 = range[2],
      euler = euler,
      scaleV = scaleV
    )
  }) %>% compact()  # Remove NULLs
  
  # Combine all plots
  wrap_plots(plots, ncol = 2)
}


LovelyFacetNNARModel(
  tikr = tikrs,
  event = "Covid",
  bks = "6 months",
  euler = 1e-6,
  scaleV = "10"
)



stocks %>% filter(Ticker == "ZSPC") %>% arrange(Date) %>% na.omit()



LovelyMultiNNARModel <- function(event, bks, euler, scaleV) {
  tikrs <- c("AMZN", "NVDA", "COST", "WMT", "JNJ", "UNH", "ZSPC", "INLF")
  range <- as.Date(Timeframes[[event]])
  
  if (!(event %in% names(Timeframes))) stop("Invalid event name")
  
  # Build a list of patchwork plots
  plots <- map(tikrs, function(tikr) {
    
    # 1. Pull data
    tikr_data <- stocks %>% 
      filter(Ticker == tikr) %>%
      arrange(Date) %>%
      drop_na(Close)   # 🛑 DROP NAs EARLY!
    
    # 2. Filter for valid pre-event training data
    valid_training_data <- tikr_data %>%
      filter(Date < as.Date(range[1]))
    
    # 3. Check usable training data
    if (nrow(valid_training_data) < 2) {
      message(paste("Skipping", tikr, "- not enough valid pre-event Close prices"))
      return(NULL)
    }
    
    # 4. Check usable event data
    event_data <- tikr_data %>%
      filter(Date >= range[1], Date <= range[2])
    
    if (nrow(event_data) < 10) {
      message(paste0("Skipping ", tikr, ": Not enough usable event data points for ", event))
      return(NULL)
    }
    
    # 5. Model
    LovelyNNARModel(
      tikr = tikr,
      tx = tikr,
      sbtx = NULL,
      bks = bks,
      range1 = range[1],
      range2 = range[2],
      euler = euler,
      scaleV = scaleV
    )
  }) %>% compact()  # Remove NULLs
  
  # Combine all plots
  wrap_plots(plots, ncol = 2) & 
    theme(legend.position = "bottom")
}

