if(!require(pacman)){install.packages("pacman");
  library(pacman)}
p_load(shiny, sf, leaflet, htmlwidgets, dplyr, DT, rsconnect)

# -----------------------------
# 1. DATA LOADING (GeoJSON)
# -----------------------------
che_1 <- st_read('che-1.geojson', quiet = TRUE)
che_2 <- st_read('che-2.geojson', quiet = TRUE)
che_3 <- st_read('che-3.geojson', quiet = TRUE)
colh  <- st_read('colh.geojson', quiet = TRUE)
din   <- st_read('din.geojson', quiet = TRUE)
hw    <- st_read('hw.geojson', quiet = TRUE)

# -----------------------------
# 2. METADATA & JOINING
# -----------------------------
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
  Est_Cost_VDOT = c(NA,NA,NA,NA,NA,NA),
  Notes_Comments = c(
    NA,
    NA,
    "$1M from County and seeking $4M from MPO as SMART SCALE leverage",
    NA,
    NA,
    "Want to use CMAQ funds as SMART SCALE leverage; amount not stated"
  )
)

che_1$Projects <- "che_1"; che_2$Projects <- "che_2"; che_3$Projects <- "che_3"
colh$Projects  <- "colh";  din$Projects   <- "din";   hw$Projects    <- "hw"

che_1 <- che_1 %>% left_join(projects_tbl, by = "Projects")
che_2 <- che_2 %>% left_join(projects_tbl, by = "Projects")
che_3 <- che_3 %>% left_join(projects_tbl, by = "Projects")
colh  <- colh  %>% left_join(projects_tbl, by = "Projects")
din   <- din   %>% left_join(projects_tbl, by = "Projects")
hw    <- hw    %>% left_join(projects_tbl, by = "Projects")

# -----------------------------
# 3. GEOMETRY PREP
# -----------------------------
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
  "<b>Request:</b> ", ifelse(is.na(Request), "TBD",
                             paste0("$", formatC(Request, format = "d", big.mark = ","))), "<br/>",
  "<b>VDOT Est. Cost:</b> ", ifelse(is.na(Est_Cost_VDOT), "N/A", Est_Cost_VDOT), "<br/>",
  "<b>Notes:</b> ", Notes_Comments
)

label_box_opts <- labelOptions(
  noHide = TRUE, direction = "top", textOnly = TRUE,
  style = list("background-color" = "white", "border" = "1px solid black", 
               "padding" = "2px 4px", "border-radius" = "4px")
)

# -----------------------------
# 4. SHINY UI
# -----------------------------
ui <- navbarPage(
  title = "Project Applications Summary", # Clean title
  
  tabPanel("Applications Summary",
           fluidRow(
             column(12,
                    h2("Applications Summary"),
                    DTOutput("summaryTable"),
                    br(),
                    tags$ul(
                      tags$li("Scoring"),
                      tags$li("Cost update from VDOT")
                    )
             )
           )
  ),
  
  tabPanel("Project Map",
           fluidRow(
             column(12,
                    h4("Project Location"), # Removed STBG/CMAQ text
                    leafletOutput("projectMap", width = "100%", height = "75vh")
             )
           )
  )
)

# -----------------------------
# 5. SHINY SERVER
# -----------------------------
server <- function(input, output, session) {
  
  output$summaryTable <- renderDT({
    datatable(
      projects_tbl %>% 
        mutate(
          No. = row_number(),
          Cost = paste0("$", formatC(Cost, format = "d", big.mark = ",")),
          Request = ifelse(is.na(Request), "?", paste0("$", formatC(Request, format = "d", big.mark = ",")))
        ) %>%
        select(No., Locality, Project_Description, Project_Type, Type = Funding_Category, Cost, Request),
      options = list(pageLength = 10, dom = 't'),
      rownames = FALSE
    )
  })
  
  output$projectMap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      
      # Set initial zoom to Dinwiddie project extent
      fitBounds(din_bbox$xmin, din_bbox$ymin, din_bbox$xmax, din_bbox$ymax) %>%
      
      # Dinwiddie Layers
      addPolylines(data = din, weight = 3, color = "blue", group = "Dinwiddie (DIN)", popup = popup_text) %>%
      addLabelOnlyMarkers(data = din_centroid, label = ~Project_Type, group = "Dinwiddie (DIN)", labelOptions = label_box_opts) %>%
      
      # Chesterfield Layers
      addPolylines(data = ch_lines, weight = 3, color = "red", group = "Chesterfield (CH)", popup = popup_text) %>%
      addLabelOnlyMarkers(data = ch_lines_centroid, label = ~Project_Type, group = "Chesterfield (CH)", labelOptions = label_box_opts) %>%
      addCircleMarkers(data = ch_points, radius = 6, fillColor = "red", fillOpacity = 0.9, group = "Chesterfield (CH)", popup = popup_text) %>%
      addLabelOnlyMarkers(data = ch_points, label = ~Project_Type, group = "Chesterfield (CH)", labelOptions = label_box_opts) %>%
      
      # Colonial Heights Layers
      addPolylines(data = colh, weight = 3, color = "green", group = "Colonial Heights (COLH)", popup = popup_text) %>%
      addLabelOnlyMarkers(data = colh_centroid, label = ~Project_Type, group = "Colonial Heights (COLH)", labelOptions = label_box_opts) %>%
      
      # Hopewell Layers
      addPolylines(data = hw, weight = 3, color = "purple", group = "Hopewell (HW)", popup = popup_text) %>%
      addLabelOnlyMarkers(data = hw_centroid, label = ~Project_Type, group = "Hopewell (HW)", labelOptions = label_box_opts) %>%
      
      addLayersControl(
        baseGroups = c("Dinwiddie (DIN)", "Chesterfield (CH)", "Colonial Heights (COLH)", "Hopewell (HW)"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      onRender(
        sprintf("
        function(el, x) {
          var map = this;
          var boundsDIN = L.latLngBounds([%f, %f], [%f, %f]);
          var boundsCH = L.latLngBounds([%f, %f], [%f, %f]);
          var boundsCOLH = L.latLngBounds([%f, %f], [%f, %f]);
          var boundsHW = L.latLngBounds([%f, %f], [%f, %f]);

          map.on('baselayerchange', function(e) {
            if (e.name === 'Dinwiddie (DIN)') { map.fitBounds(boundsDIN, {padding: [30, 30]}); }
            if (e.name === 'Chesterfield (CH)') { map.fitBounds(boundsCH, {maxZoom: 14}); }
            if (e.name === 'Colonial Heights (COLH)') { map.fitBounds(boundsCOLH, {padding: [30, 30]}); }
            if (e.name === 'Hopewell (HW)') { map.fitBounds(boundsHW, {padding: [30, 30]}); }
          });
        }
      ",
                din_bbox$ymin, din_bbox$xmin, din_bbox$ymax, din_bbox$xmax,
                ch_bbox$ymin, ch_bbox$xmin, ch_bbox$ymax, ch_bbox$xmax,
                st_bbox(colh)$ymin, st_bbox(colh)$xmin, st_bbox(colh)$ymax, st_bbox(colh)$xmax,
                st_bbox(hw)$ymin, st_bbox(hw)$xmin, st_bbox(hw)$ymax, st_bbox(hw)$xmax
        )
      )
  })
}

shinyApp(ui = ui, server = server)
