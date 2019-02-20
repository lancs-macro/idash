# devtools::install_github("nik01010/dashboardthemes")

# Colours
red_lanc <- "rgb(185, 80, 74)"
white <- "rgb(255,255,255)"
bgrey <- "rgb(45, 59, 66)"
grey2 <- "rgb(118,118,118)"
black <- "rgb(0,0,0)"

blue_fed <- "rgb(49, 70, 88)"
grey <- "rgb(189, 201, 213)"
light_grey <- "rgb(245, 245, 245)"
letters_blue <- "rgb(23, 87, 151)"

yellow <- "rgb(255, 208, 37)"

default <- blue_fed
contrast <- grey


# think about yellow

# Title and Beta version sign

# logo_boe_website <- shinyDashboardLogoDIY(
#   boldText = "Dallas Fed"
#   ,mainText = "International House Price Database"
#   ,textSize = 20
#   ,badgeText = "BETA"
#   ,badgeTextColor = "white"
#   ,badgeTextSize = 2
#   ,badgeBackColor = contrast # "rgb(207,57,92)"
#   ,badgeBorderRadius = 3
#   
# )

# Theme options

theme_boe_website <- shinyDashboardThemeDIY(
  
  ### general
  appFontFamily = "Arial"
  ,appFontColor = black
  ,bodyBackColor = "rgb(255,255,254)"
  
  ### header
  ,logoBackColor = default
  
  ,headerButtonBackColor = default # "rgb(45,59,66)"
  ,headerButtonIconColor = white
  ,headerButtonBackColorHover = contrast
  ,headerButtonIconColorHover = contrast # "rgb(207,57,92)"
  
  ,headerBackColor = default # "rgb(45,59,66)"
  ,headerBoxShadowColor = ""
  ,headerBoxShadowSize = "0px 0px 0px"
  
  ### sidebar
  ,sidebarBackColor = contrast #default # "rgb(207,57,92)"
  ,sidebarPadding = 0
  
  ,sidebarMenuBackColor = "transparent"
  ,sidebarMenuPadding = 0
  ,sidebarMenuBorderRadius = 0
  
  ,sidebarShadowRadius = ""
  ,sidebarShadowColor = "0px 0px 0px"
  
  ,sidebarUserTextColor = white
  
  ,sidebarSearchBackColor = white
  ,sidebarSearchIconColor = default # "rgb(207,57,92)"
  ,sidebarSearchBorderColor = white
  
  ,sidebarTabTextColor = white
  ,sidebarTabTextSize = 14
  ,sidebarTabBorderStyle = "none"
  ,sidebarTabBorderColor = "none"
  ,sidebarTabBorderWidth = 0
  
  ,sidebarTabBackColorSelected = contrast # "rgb(45,59,66)"
  ,sidebarTabTextColorSelected = white
  ,sidebarTabRadiusSelected = "0px"
  
  ,sidebarTabBackColorHover = default#"rgb(186,51,83)"
  ,sidebarTabTextColorHover = white
  ,sidebarTabBorderStyleHover = "none"
  ,sidebarTabBorderColorHover = "none"
  ,sidebarTabBorderWidthHover = 0
  ,sidebarTabRadiusHover = "0px"
  
  ### boxes
  ,boxBackColor = white # "rgb(248,248,248)"
  ,boxBorderRadius = 0
  ,boxShadowSize = "0px 0px 0px"
  ,boxShadowColor = ""
  ,boxTitleSize = 18
  ,boxDefaultColor = "rgb(248,248,248)"
  ,boxPrimaryColor = "rgb(15,124,191)"
  ,boxSuccessColor = "rgb(59,133,95)"
  ,boxWarningColor = "rgb(178,83,149)"
  ,boxDangerColor = default # "rgb(207,57,92)"
  
  ,tabBoxTabColor = "rgb(248,248,248)"
  ,tabBoxTabTextSize = 14
  ,tabBoxTabTextColor = "rgb(42,102,98)"
  ,tabBoxTabTextColorSelected = contrast # "rgb(207,57,92)"
  ,tabBoxBackColor = "rgb(248,248,248)"
  ,tabBoxHighlightColor = default # "rgb(207,57,92)"
  ,tabBoxBorderRadius = 0
  
  ### inputs
  ,buttonBackColor = default # "rgb(207,57,92)"
  ,buttonTextColor = "rgb(255,255,255)"
  ,buttonBorderColor = contrast # "rgb(207,57,92)"
  ,buttonBorderRadius = 0
  
  ,buttonBackColorHover = contrast
  ,buttonTextColorHover = white
  ,buttonBorderColorHover = contrast
  
  ,textboxBackColor = white
  ,textboxBorderColor = "rgb(118,118,118)"
  ,textboxBorderRadius = 0
  ,textboxBackColorSelect = white
  ,textboxBorderColorSelect = "rgb(118,118,118)"
  
  ### tables
  ,tableBackColor = "rgb(248,248,248)"
  ,tableBorderColor = "rgb(235,235,235)"
  ,tableBorderTopSize = 1
  ,tableBorderRowSize = 1
  
)