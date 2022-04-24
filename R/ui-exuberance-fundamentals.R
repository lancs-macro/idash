ui_exuber_fundamentals <- function() {
  tabItem(
    tabName = "exuberance2",
    
    box2(
      title = "House Price to Rent",
      popover = TRUE,
      popover_title = "Note:",
      popover_content = note_shade,
      width = 12,
      plotOutput("plot_exuber_fundamentals", height = 1200)
    ),
    
    includeHTML("www/footer.html")
  )
  
}