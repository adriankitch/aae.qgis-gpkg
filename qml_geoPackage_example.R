library(sf)
library(RSQLite)
source("qml_functions.R")
# library(cartography)
library(mapsf)


######### VARIABLES AND SOURCE DATA ###############################################################################

source_gpkg = "gpkg/wetlands2.gpkg"

template_gpkg = "gpkg/qgis_empty.gpkg"
new_gpkg = "gpkg/qgis_wetlands_r_v2.gpkg"
layer_name = "WETLAND_CURRENT"
map_attribute = "HECTARES"
label_attribute = "WETLAND_NO"

# Load source layer into R
wetlands <- st_read(source_gpkg, layer = layer_name)

# Copy gpkg template
file.copy(from = template_gpkg, new_gpkg)
conn <- dbConnect(SQLite(), new_gpkg)

# write source layer to new gpkg
sf::st_write(wetlands, layer = layer_name, new_gpkg, append=FALSE)

######################################################################################################################
########################## DEFINE QML STYLE INPUTS  ##################################################################

wBreaks <-  as.data.frame(getBreaks(v = wetlands[[map_attribute]], nclass = 6, method = "kmeans"))
colnames(wBreaks) <- "class"
# min_val = min(wetlands$HECTARES)
# max_val = max(wetlands$HECTARES)

palette <- colorRampPalette(colors=c("#30123b", "#4662d8", "#35abf8", "#1be5b5", "#74fe5d", "#c9ef34", "#fbb938", "#f56918", "#c92903", "#7a0403"))

cols <- as.data.frame(palette(nrow(wBreaks)-1))
colnames(cols) <- "colour"

layer_param <- data.frame(limit_lower=double(),
                          limit_upper=double(),
                          label=character(),
                          HTMLcolour=character(),
                          stringsAsFactors=FALSE
)

for(i in 1:(nrow(wBreaks)-1)){
  layer_param[nrow(layer_param) + 1, ] <- c(as.double(wBreaks$class[i]), as.double(wBreaks$class[i + 1]), paste0(wBreaks$class[i], " - ", wBreaks$class[i+1]), cols$colour[i]) 

}

######################################################################################################################
############################### BUILD QML AND SAVE TO CML DOC ########################################################

xml_vector_doc = create_vector_range_classified_qml(map_attribute, layer_param, label_attribute)

# SAVE QML TO FILE
saveXML(xml_vector_doc, file=paste0("QML/", layer_name, "_vector_range_classesv2.qml"), indent=TRUE)

######################################################################################################################
############################### SAVE QML STYLE TO GEOPACKAGE #########################################################

out_xml = saveXML(xml_vector_doc, file=NULL, indent=TRUE)

dbExecute(conn, build_geopackage_style_sql(out_xml, layer_name))

dbDisconnect(conn)


