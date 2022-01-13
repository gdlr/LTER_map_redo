## Global.r file for the LTER map

#########
# Data pre-processing:
#########

# Read in map data as sf
map_data <- read_csv("map_data.csv") %>% 
  mutate(Established = Established.x)

# Change the McMurdo longitude to display across the meridian:
map_data$longitude[24] <- map_data$longitude[24] - 360

# Turn into sf object
map_coords <- map_data %>% 
  drop_na(latitude) %>% 
  st_as_sf(coords = c("longitude", "latitude")) %>% 
  mutate(text = paste("<h3>", Name, "</h3>",
                      "<h4>", "Ecosystem Type:", Type, "</h4>",
                      "<h4>", "Current Grant:", "<a href=\"", grant_link,"\"", " target=\"_blank\"", "\">", `current-grant`,"</a>", "</h4><h4>",
                      "<a href=\"", Website,"\"", " target=\"_blank\"", "\">", "Website Link</a>", "</h4><h4>",
                      "<a href=\"", Data,"\"", " target=\"_blank\"",  "\">", "Link to Data</a>", "</h4><h4>",
                      "<a href=\"", Publications,"\""," target=\"_blank\"", "\">", "Link to Publications</a>", "</h4><h4>")) %>%
  mutate(Code = fct_reorder(Code, desc(Established)))

current_sites <- map_data %>% 
  filter(`Active?` == "Active") %>%
  mutate(Code = fct_reorder(Code, desc(Established)))

current <- map_coords %>% 
  filter(`Active?` == "Active") %>% 
  filter(!Code %in% c("LNO", "HBR", "PIE", "CDR", "JRN", "CCE", "VCR", "NES"))

current_extra <- map_coords %>% 
  filter(`Active?` == "Active") %>% 
  filter(Code %in% c("LNO", "HBR", "PIE", "CDR", "JRN", "CCE", "VCR", "NES"))  # Order = LNO, CCE, CDR, HBR, JRN, PIE, VCR

inactive_sites <- map_coords %>% 
  filter(`Active?` == "Inactive")

# Get current year
current_year <- as.numeric((format(Sys.Date(), "%Y")))

##------
# Set up palettes for both leaflet and plotly
palette = colorFactor(c("Admin" = "grey50", 
                        "Forest" = "forestgreen",
                        "Tundra" = "tan",
                        "Urban" = "grey20",
                        "Marine" = "steelblue4",
                        "Mixed Landscape" = "darkgoldenrod3",
                        "Data" = "darkred",
                        "Coastal" = "steelblue2",
                        "Grassland" = "yellowgreen",
                        "Freshwater" = "aquamarine2"),
                      levels = c("Admin", "Forest", "Tundra", "Urban", "Marine", "Mixed Landscape", "Data", "Coastal", "Grassland", "Freshwater"))

colors <- c("Admin" = "grey50", "Forest" = "forestgreen", "Tundra" = "tan", "Urban" = "grey20", "Marine" = "steelblue4", "Mixed Landscape" = "darkgoldenrod3", "Data" = "darkred", "Coastal" = "aquamarine3", "Grassland" = "yellowgreen", "Freshwater" = "aquamarine2")

### Icons

mixed_landscape <- makeIcon(
  iconUrl = "darkgoldenrod3.png",
  iconWidth = 30, iconHeight = 30,
  iconAnchorX = 15, iconAnchorY = 25
)
mixed_landscape_neg <- makeIcon(
  iconUrl = "darkgoldenrod3180.png",
  iconWidth = 30, iconHeight = 30,
  iconAnchorX = 15, iconAnchorY = 0
)
Forest <- makeIcon(
  iconUrl = "forestgreen.png",
  iconWidth = 30, iconHeight = 30,
  iconAnchorX = 15, iconAnchorY = 28
)
Admin <- makeIcon(
  iconUrl = "grey50.png",
  iconWidth = 30, iconHeight = 30,
  iconAnchorX = 15, iconAnchorY = 30
)
Coastal <- makeIcon(
  iconUrl = "coastal45.png",
  iconWidth = 22, iconHeight = 22,
  iconAnchorX = -1, iconAnchorY = 25
)
Coastalneg <- makeIcon(
  iconUrl = "coastalneg45.png",
  iconWidth = 22, iconHeight = 22,
  iconAnchorX = -1, iconAnchorY = -1
)
Marine <- makeIcon(
  iconUrl = "marineneg45.png",
  iconWidth = 22, iconHeight = 22,
  iconAnchorX = -1, iconAnchorY = 0
)
MarineNeg <- makeIcon(
  iconUrl = "marine180.png",
  iconWidth = 22, iconHeight = 22,
  iconAnchorX = 22, iconAnchorY = 0
)