if(!require(pacman)){install.packages("pacman"); library(pacman)}
p_load(shiny, shinythemes, dplyr, tibble, sf, leaflet, htmlwidgets, DT)

# -----------------------------
# 1. PM WEIGHTS DATA (Page 1)
# -----------------------------
pm_weights <- tribble(
  ~header,                             ~performance_measure,                  ~pm_weight, ~type, ~mode,
  "Table 9 : Economic Development",     "E1. Sensitive Features",               "100.0%",   "Old", "Highway",
  "Table 9 : Economic Development",     "ED1. Job Growth",                      "60%",      "New", "Highway",
  "Table 9 : Economic Development",     "ED2. Access to Freight Jobs",          "20%",      "New", "Highway",
  "Table 9 : Economic Development",     "ED3. Proximity to Activity Centers",   "20%",      "New", "Highway",
  
  "Table 13 : Mobility and Congestion", "S.1. Crash Frequency",                "50%",      "Old", "Active Transp.",
  "Table 13 : Mobility and Congestion", "S.2. Crash Rate",                     "50%",      "Old", "Active Transp.",
  "Table 13 : Mobility and Congestion", "M1. Walk Score and Bike Score",       "50%",      "New", "Active Transp.",
  "Table 13 : Mobility and Congestion", "M2. Congestion",                      "50%",      "New", "Active Transp.",
  
  "Table 18: Economic Development",     "ED1. Job Growth (2017-2050)",          "60%",      "Old", "Active Transp.",
  "Table 18: Economic Development",     "ED2. Freight Jobs",                   "20%",      "Old", "Active Transp.",
  "Table 18: Economic Development",     "ED3. Activity Centers",               "20%",      "Old", "Active Transp.",
  "Table 18: Economic Development",     "ED1. Job Growth",                     "60%",      "New", "Active Transp.",
  "Table 18: Economic Development",     "ED2. Access to Freight Jobs",         "20%",      "New", "Active Transp.",
  "Table 18: Economic Development",     "ED3. Proximity to Activity Centers",  "20%",      "New", "Active Transp."
)

pm_weights <- pm_weights %>%
  rename(
    Tables = header,
    `Performance Measures` = performance_measure,
    Weights = pm_weight,
    Report = type,
    Mode = mode
  ) %>%
  select(Tables, Mode, Report, `Performance Measures`, Weights)

# -----------------------------
# 2. PROJECTS DATA (Pages 2 & 3)
# -----------------------------
che_1 <- st_read('che-1.geojson', quiet = TRUE)
che_2 <- st_read('che-2.geojson', quiet = TRUE)
che_3 <- st_read('che-3.geojson', quiet = TRUE)
colh  <- st_read('colh.geojson', quiet = TRUE)
din   <- st_read('din.geojson', quiet = TRUE)
hw    <- st_read('hw.geojson', quiet = TRUE)

projects_tbl <- tibble(
  Projects = c("che_1", "che_2", "che_3", "colh", "din", "hw"),
  Locality = c("Chesterfield", "Chesterfield", "Chesterfield", "Colonial Heights", "Dinwiddie", "Hopewell"),
  Project_Description = c(
    "Route 1 (Harrowgate Rd - Jackson St) Sidewalk",
    "Woodpecker Road (Southlawn Ave - J Mitchell Jones Dr) Sidewalk",
    "Woodpecker Road / Bradley Bridge Road Roundabout",
    "Conduit Road Sidewalks Phase I",
    "Merge/Turn Lane Extension to Albemarle St. Northbound Route 1 & I-85 Exit 63B",
    "Cedar Level Rd. Southern Segment"
  ),
  Project_Type = c("Bike/Ped","Bike/Ped","Roundabout","Bike/Ped","Roadway","Bike/Ped"),
  Funding_Category = c("Both","Both","Both","Both","STBG","CMAQ"),
  Cost = c(1530000,2381547,10056184,1847951,4892088,17000000),
  Request = c(1530000,2381547,4000000,1847951,4892088,NA),
  Notes_Comments = c(
    NA,
    NA,
    "$1M from County and seeking $4M from MPO as SMART SCALE leverage",
    NA,
    NA,
    "Want to use CMAQ funds as SMART SCALE leverage; amount not stated"
  ),
  Intent = c(
    "Construct a 5-foot sidewalk with buffer to connect with improvements planned in UPC 124340.",
    "Construct a sidewalk and pedestrian crossing, creating a safer access to Ettrick Park.",
    "Construct single-lane roundabout and crosswalk. Funds are requested for leveraging in Smart Scale.",
    "Install a sidewalk and curb and gutter.",
    "Extend existing right turn lane by 800 feet.",
    "Installing dedicated bike lanes and adding a new storm drain network."
  )
)

che_1$Projects <- "che_1"; che_2$Projects <- "che_2"; che_3$Projects <- "che_3"
colh$Projects  <- "colh";  din$Projects <- "din"; hw$Projects <- "hw"

che_1 <- che_1 %>% left_join(projects_tbl, by = "Projects")
che_2 <- che_2 %>% left_join(projects_tbl, by = "Projects")
che_3 <- che_3 %>% left_join(projects_tbl, by = "Projects")
colh  <- colh  %>% left_join(projects_tbl, by = "Projects")
din   <- din   %>% left_join(projects_tbl, by = "Projects")
hw    <- hw    %>% left_join(projects_tbl, by = "Projects")

ch_lines  <- bind_rows(che_1, che_2)
ch_points <- che_3
ch_lines_centroid <- st_centroid(ch_lines)
din_centroid      <- st_centroid(din)
colh_centroid     <- st_centroid(colh)
hw_centroid       <- st_centroid(hw)

din_bbox  <- as.list(st_bbox(din))
ch_bbox   <- as.list(st_bbox(bind_rows(ch_lines, ch_points)))
colh_bbox <- as.list(st_bbox(colh))
hw_bbox   <- as.list(st_bbox(hw))

popup_text <- ~paste0(
  "<b>Project:</b> ", Project_Description, "<br/>",
  "<b>Locality:</b> ", Locality, "<br/>",
  "<b>Project Type:</b> ", Project_Type, "<br/>",
  "<b>Funding Type:</b> ", Funding_Category, "<br/>",
  "<b>Cost:</b> $", formatC(Cost, format = "d", big.mark = ","), "<br/>",
  "<b>Request:</b> ", ifelse(is.na(Request), "TBD", paste0("$", formatC(Request, format = "d", big.mark = ","))), "<br/>",
  "<b>Intent:</b> ", Intent, "<br/>",
  "<b>Notes:</b> ", Notes_Comments
)

label_box_opts <- labelOptions(
  noHide = TRUE, direction = "top", textOnly = TRUE,
  style = list("background-color" = "white", "border" = "1px solid black",
               "padding" = "2px 4px", "border-radius" = "4px")
)

# -----------------------------
# 3. UI (United theme)
# -----------------------------
ui <- navbarPage(
  title = "STBG/CMAQ Dash",
  theme = shinytheme("united"),
  
  tabPanel("STBG/CMAQ P&P Corrections",
           titlePanel("STBG/CMAQ P&P Corrections"),
           selectInput(
             inputId = "table_select",
             label = "Select Table:",
             choices = unique(pm_weights$Tables),
             selected = unique(pm_weights$Tables)[1]
           ),
           tableOutput("pm_table")
  ),
  
  tabPanel("Applications Summary",
           h2("Applications Summary"),
           DTOutput("summaryTable"),
           br(),
           tags$ul(tags$li("Scoring"))
  ),
  
  tabPanel("Project Map",
           h4("Project Location"),
           leafletOutput("projectMap", width = "100%", height = "75vh")
  )
)

# -----------------------------
# 4. SERVER
# -----------------------------
server <- function(input, output, session) {
  
  # Page 1: PM Table
  filtered_data <- reactive({
    pm_weights %>% filter(Tables == input$table_select)
  })
  
  output$pm_table <- renderTable({
    filtered_data()
  }, rownames = FALSE)
  
  # Page 2: Applications Summary
  output$summaryTable <- renderDT({
    datatable(
      projects_tbl %>% 
        mutate(
          No. = row_number(),
          Cost = paste0("$", formatC(Cost, format = "d", big.mark = ",")),
          Request = ifelse(is.na(Request), "?", paste0("$", formatC(Request, format = "d", big.mark = ",")))
        ) %>%
        select(No., Locality, Project_Description, Project_Type, Type = Funding_Category, Cost, Request, Intent),
      options = list(pageLength = 10, dom = 't'),
      rownames = FALSE
    )
  })
  
  # Page 3: Project Map
  output$projectMap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      fitBounds(din_bbox$xmin, din_bbox$ymin, din_bbox$xmax, din_bbox$ymax) %>%
      
      addPolylines(data = din, weight = 3, color = "blue", group = "Dinwiddie (DIN)", popup = popup_text) %>%
      addLabelOnlyMarkers(data = din_centroid, label = ~Project_Type, group = "Dinwiddie (DIN)", labelOptions = label_box_opts) %>%
      
      addPolylines(data = ch_lines, weight = 3, color = "red", group = "Chesterfield (CH)", popup = popup_text) %>%
      addLabelOnlyMarkers(data = ch_lines_centroid, label = ~Project_Type, group = "Chesterfield (CH)", labelOptions = label_box_opts) %>%
      addCircleMarkers(data = ch_points, radius = 6, fillColor = "red", fillOpacity = 0.9, group = "Chesterfield (CH)", popup = popup_text) %>%
      addLabelOnlyMarkers(data = ch_points, label = ~Project_Type, group = "Chesterfield (CH)", labelOptions = label_box_opts) %>%
      
      addPolylines(data = colh, weight = 3, color = "green", group = "Colonial Heights (COLH)", popup = popup_text) %>%
      addLabelOnlyMarkers(data = colh_centroid, label = ~Project_Type, group = "Colonial Heights (COLH)", labelOptions = label_box_opts) %>%
      
      addPolylines(data = hw, weight = 3, color = "purple", group = "Hopewell (HW)", popup = popup_text) %>%
      addLabelOnlyMarkers(data = hw_centroid, label = ~Project_Type, group = "Hopewell (HW)", labelOptions = label_box_opts) %>%
      
      addLayersControl(
        baseGroups = c("Dinwiddie (DIN)", "Chesterfield (CH)", "Colonial Heights (COLH)", "Hopewell (HW)"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
}

# -----------------------------
# 5. RUN APP
# -----------------------------
shinyApp(ui, server)
