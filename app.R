if(!require(pacman)){install.packages("pacman"); library(pacman)}
p_load(shiny, sf, leaflet, htmlwidgets, dplyr, DT, shinythemes, tibble)

# -----------------------------
# 1. PM WEIGHTS DATA
# -----------------------------
pm_weights <- tribble(
  ~header, ~performance_measure, ~pm_weight, ~type, ~mode,
  "Table 9 : Economic Development", "E1. Sensitive Features", "100.0%", "Old", "Highway",
  "Table 9 : Economic Development", "ED1. Job Growth", "60%", "New", "Highway",
  "Table 9 : Economic Development", "ED2. Access to Freight Jobs", "20%", "New", "Highway",
  "Table 9 : Economic Development", "ED3. Proximity to Activity Centers", "20%", "New", "Highway",
  "Table 13 : Mobility and Congestion", "S.1. Crash Frequency", "50%", "Old", "Active Transp.",
  "Table 13 : Mobility and Congestion", "S.2. Crash Rate", "50%", "Old", "Active Transp.",
  "Table 13 : Mobility and Congestion", "M1. Walk Score and Bike Score", "50%", "New", "Active Transp.",
  "Table 13 : Mobility and Congestion", "M2. Congestion", "50%", "New", "Active Transp.",
  "Table 18: Economic Development", "ED1. Job Growth (2017-2050)", "60%", "Old", "Active Transp.",
  "Table 18: Economic Development", "ED2. Freight Jobs", "20%", "Old", "Active Transp.",
  "Table 18: Economic Development", "ED3. Activity Centers", "20%", "Old", "Active Transp.",
  "Table 18: Economic Development", "ED1. Job Growth", "60%", "New", "Active Transp.",
  "Table 18: Economic Development", "ED2. Access to Freight Jobs", "20%", "New", "Active Transp.",
  "Table 18: Economic Development", "ED3. Proximity to Activity Centers", "20%", "New", "Active Transp."
) %>%
  rename(
    Tables = header,
    `Performance Measures` = performance_measure,
    Weights = pm_weight,
    Report = type,
    Mode = mode
  ) %>%
  select(Tables, Mode, Report, `Performance Measures`, Weights)

# -----------------------------
# 2. DATA LOADING
# -----------------------------
che_1 <- st_read("che-1.geojson", quiet = TRUE)
che_2 <- st_read("che-2.geojson", quiet = TRUE)
che_3 <- st_read("che-3.geojson", quiet = TRUE)
colh  <- st_read("colh.geojson", quiet = TRUE)
din   <- st_read("din.geojson", quiet = TRUE)
hw    <- st_read("hw.geojson", quiet = TRUE)

# -----------------------------
# 3. PROJECT METADATA (UPDATED COST VALUES)
# -----------------------------
projects_tbl <- tibble(
  Projects = c("che_1","che_2","che_3","colh","din","hw"),
  Locality = c("Chesterfield","Chesterfield","Chesterfield",
               "Colonial Heights","Dinwiddie","Hopewell"),
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
  Cost = c(3061714, 2964967, 11881982, 3446861, 4704599, 21094632),
  Request = c(3061714, 2964967, 4000000, 3446861, 4704599, NA),
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
colh$Projects <- "colh"; din$Projects <- "din"; hw$Projects <- "hw"

che_1 <- left_join(che_1, projects_tbl, by="Projects")
che_2 <- left_join(che_2, projects_tbl, by="Projects")
che_3 <- left_join(che_3, projects_tbl, by="Projects")
colh  <- left_join(colh,  projects_tbl, by="Projects")
din   <- left_join(din,   projects_tbl, by="Projects")
hw    <- left_join(hw,    projects_tbl, by="Projects")

# -----------------------------
# 4. GEOMETRY + BOUNDS
# -----------------------------
ch_lines  <- bind_rows(che_1, che_2)
ch_points <- che_3

din_bbox  <- as.list(st_bbox(din))
ch_bbox   <- as.list(st_bbox(bind_rows(ch_lines, ch_points)))
colh_bbox <- as.list(st_bbox(colh))
hw_bbox   <- as.list(st_bbox(hw))

popup_text <- ~paste0(
  "<b>Project:</b> ", Project_Description, "<br/>",
  "<b>Locality:</b> ", Locality, "<br/>",
  "<b>Project Type:</b> ", Project_Type, "<br/>",
  "<b>Cost:</b> $", formatC(Cost, format="d", big.mark=","), "<br/>",
  "<b>Intent:</b> ", Intent
)

# -----------------------------
# 5. UI
# -----------------------------
ui <- navbarPage(
  "STBG/CMAQ Applications Dashboard",
  theme = shinytheme("united"),
  
  tabPanel(
    "STBG/CMAQ P&P Corrections",
    selectInput("table_select","Select Table:",choices=unique(pm_weights$Tables)),
    DTOutput("pm_table")
  ),
  
  tabPanel(
    "Applications Summary",
    DTOutput("summaryTable")
  ),
  
  tabPanel(
    "Project Map",
    fluidRow(
      column(
        3,
        radioButtons(
          "locality_radio","Select Locality:",
          choices = c(
            "All"="all",
            "Dinwiddie"="din",
            "Chesterfield"="ch",
            "Colonial Heights"="colh",
            "Hopewell"="hw"
          ),
          selected="all"
        )
      ),
      column(9, leafletOutput("projectMap", height="75vh"))
    )
  )
)

# -----------------------------
# 6. SERVER
# -----------------------------
server <- function(input, output, session){
  
  output$pm_table <- renderDT({
    datatable(
      pm_weights %>% filter(Tables == input$table_select),
      options=list(dom="t"),
      rownames=FALSE
    ) %>%
      formatStyle(
        "Report",
        target="row",
        color=styleEqual(c("Old","New"),c("red","#4da6ff"))
      )
  })
  
  output$summaryTable <- renderDT({
    # Prepare the main data
    display_data <- projects_tbl %>%
      mutate(
        No. = as.character(row_number()),
        Cost_num = Cost,  # Keep numeric for calculations
        Request_num = Request,  # Keep numeric for calculations
        Cost = paste0("$", formatC(Cost, format="d", big.mark=","), " "),
        Request = ifelse(
          is.na(Request), "?",
          paste0("$", formatC(Request, format="d", big.mark=","), " ")
        )
      ) %>%
      select(No., Locality, Project_Description, Project_Type,
             Type = Funding_Category, Cost, Request, Cost_num, Request_num)
    
    # Calculate totals
    total_cost <- sum(display_data$Cost_num, na.rm = TRUE)
    total_request <- sum(display_data$Request_num, na.rm = TRUE)
    
    # Create totals row
    totals_row <- data.frame(
      No. = "Total",
      Locality = "",
      Project_Description = "",
      Project_Type = "",
      Type = "",
      Cost = paste0("$", formatC(total_cost, format="d", big.mark=","), " "),
      Request = paste0("$", formatC(total_request, format="d", big.mark=","), " "),
      Cost_num = total_cost,
      Request_num = total_request
    )
    
    # Combine data with totals row
    final_data <- bind_rows(display_data, totals_row) %>%
      select(-Cost_num, -Request_num)
    
    # Create the datatable
    datatable(
      final_data,
      options = list(
        dom = "t",
        pageLength = 7,  # Show all rows including total
        columnDefs = list(
          list(className = "dt-center", targets = 0:6)
        )
      ),
      rownames = FALSE
    ) %>%
      # Color the specific $4,000,000 request cell yellow
      formatStyle(
        "Request",
        target = "cell",
        backgroundColor = styleEqual(
          c("$4,000,000 ", "?"),
          c("yellow", "yellow")
        )
      ) %>%
      # Style the total row
      formatStyle(
        "No.",
        target = "row",
        fontWeight = styleEqual("Total", "bold"),
        backgroundColor = styleEqual("Total", "#f5f5f5")
      )
  })
  
  output$projectMap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      fitBounds(ch_bbox$xmin, ch_bbox$ymin, hw_bbox$xmax, din_bbox$ymax)
  })
  
  observe({
    proxy <- leafletProxy("projectMap") %>%
      clearShapes()
    
    if(input$locality_radio %in% c("all","din")){
      proxy %>%
        addPolylines(data=din, color="blue", popup=popup_text) %>%
        fitBounds(din_bbox$xmin, din_bbox$ymin, din_bbox$xmax, din_bbox$ymax)
    }
    
    if(input$locality_radio %in% c("all","ch")){
      proxy %>%
        addPolylines(data=ch_lines, color="red", popup=popup_text) %>%
        addCircleMarkers(data=ch_points, color="red", popup=popup_text) %>%
        fitBounds(ch_bbox$xmin, ch_bbox$ymin, ch_bbox$xmax, ch_bbox$ymax)
    }
    
    if(input$locality_radio %in% c("all","colh")){
      proxy %>%
        addPolylines(data=colh, color="green", popup=popup_text) %>%
        fitBounds(colh_bbox$xmin, colh_bbox$ymin, colh_bbox$xmax, colh_bbox$ymax)
    }
    
    if(input$locality_radio %in% c("all","hw")){
      proxy %>%
        addPolylines(data=hw, color="purple", popup=popup_text) %>%
        fitBounds(hw_bbox$xmin, hw_bbox$ymin, hw_bbox$xmax, hw_bbox$ymax)
    }
    
    if(input$locality_radio == "all"){
      proxy %>%
        fitBounds(ch_bbox$xmin, ch_bbox$ymin, hw_bbox$xmax, din_bbox$ymax)
    }
  })
}

shinyApp(ui, server)
