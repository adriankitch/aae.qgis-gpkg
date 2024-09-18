
source("qml_functions.R")

#VECTOR poly range

layer_name = "WETLAND_CURRENT"

layer_param <- data.frame(limit_lower=double(),
                          limit_upper=double(),
                          label=character(),
                          HTMLcolour=character(),
                          stringsAsFactors=FALSE
)

layer_param[nrow(layer_param) + 1, ] <- c(0.0, 32.210, "0 - 32", "#440154")
layer_param[nrow(layer_param) + 1, ] <- c(32.210, 135.449, "32 - 135", "#443a82")
layer_param[nrow(layer_param) + 1, ] <- c(135.449, 386.639, "135 - 387", "#31688e")
layer_param[nrow(layer_param) + 1, ] <- c(386.639, 918.059, "387 - 918",  "#21908d")
layer_param[nrow(layer_param) + 1, ] <- c(918.059, 2431.929, "918 - 2432", "#35b779")
layer_param[nrow(layer_param) + 1, ] <- c(2431.929, 6642.130, "2432 - 6642", "#8fd744")
layer_param[nrow(layer_param) + 1, ] <- c(6642.130, 37809.730, "6642 - 37810", "#fde725")

xml_vector_doc = create_vector_range_classified_qml("HECTARES", layer_param, outline_width=0.1, outline_color = "blue")

# SAVE XML TO FILE
saveXML(xml_vector_doc, file=paste0("QML/", layer_name, "_vector_range_classes.qml"), indent=TRUE)

################################################################################################################

#VECTOR poly unique

layer_name = "WETLAND_CURRENT"

layer_param <- data.frame(category=character(),
                          label=character(),
                          HTMLcolour=character(),
                          stringsAsFactors=FALSE
)

layer_param[nrow(layer_param) + 1, ] <- c("Fresh", "Fresh", "#440154")
layer_param[nrow(layer_param) + 1, ] <- c("Hypersaline", "Hypersaline", "#443a82")
layer_param[nrow(layer_param) + 1, ] <- c("Hyposaline", "Hyposaline", "#31688e")
layer_param[nrow(layer_param) + 1, ] <- c("Mesosaline", "Mesosaline",  "#21908d")
layer_param[nrow(layer_param) + 1, ] <- c("Partially Saline", "Partially Saline", "#35b779")
layer_param[nrow(layer_param) + 1, ] <- c("Saline", "Saline", "#8fd744")
layer_param[nrow(layer_param) + 1, ] <- c("Unknown", "Unknown", "#fde725")
layer_param[nrow(layer_param) + 1, ] <- c("", "", "red")

xml_vector_doc = create_vector_unique_classified_qml("SAL_REGIME", layer_param, label_attribute = "WETLAND_NO")

# SAVE XML TO FILE
saveXML(xml_vector_doc, file=paste0("QML/", layer_name, "_vector_unique_classes.qml"), indent=TRUE)

################################################################################################################

#RASTER continuous

layer_name = "perm_wet"

min = 1
max = 10

palette <- colorRampPalette(colors=c("#440154", "#443a82", "#31688e", "#21908d", "#35b779", "#8fd744", "#fde725"))

xml_raster_doc = create_raster_continuous_qml(min, max, colourRamp = palette)

saveXML(xml_raster_doc, file=paste0("QML/", layer_name, "_raster_continuous.qml"), indent=TRUE)

################################################################################################################

#RASTER classified_colourRamp

layer_name = "perm_wet"

layer_param <- data.frame(class_value=double(),
                          label=character(),
                          stringsAsFactors=FALSE
)

layer_param[nrow(layer_param) + 1, ] <- c(1, "type 1")
layer_param[nrow(layer_param) + 1, ] <- c(2, "type 2")
layer_param[nrow(layer_param) + 1, ] <- c(3, "type 3")
layer_param[nrow(layer_param) + 1, ] <- c(4, "type 4")
layer_param[nrow(layer_param) + 1, ] <- c(5, "type 5")
layer_param[nrow(layer_param) + 1, ] <- c(6, "type 6")
layer_param[nrow(layer_param) + 1, ] <- c(7, "type 7")
layer_param[nrow(layer_param) + 1, ] <- c(8, "type 8")
layer_param[nrow(layer_param) + 1, ] <- c(9, "type 9")
layer_param[nrow(layer_param) + 1, ] <- c(10, "type 10")

palette <- colorRampPalette(colors=c("#440154", "#443a82", "#31688e", "#21908d", "#35b779", "#8fd744", "#fde725"))

xml_raster_doc = create_raster_unique_classified_colourRamp_qml(layer_param, colourRamp = palette) # default = "continuous"

saveXML(xml_raster_doc, file=paste0("QML/", layer_name, "_raster_unique_classified_colourRamp.qml"), indent=TRUE)


################################################################################################################

#RASTER classified_colourDefined

layer_name = "perm_wet"

layer_param <- data.frame(class_value=double(),
                          label=character(),
                          HTMLcolour=character(),
                          stringsAsFactors=FALSE
)

layer_param[nrow(layer_param) + 1, ] <- c(1, "type 1", "#30123b")
layer_param[nrow(layer_param) + 1, ] <- c(2, "type 2", "#4662d8")
layer_param[nrow(layer_param) + 1, ] <- c(3, "type 3", "#35abf8")
layer_param[nrow(layer_param) + 1, ] <- c(4, "type 4", "#1be5b5")
layer_param[nrow(layer_param) + 1, ] <- c(5, "type 5", "#74fe5d")
layer_param[nrow(layer_param) + 1, ] <- c(6, "type 6", "#c9ef34")
layer_param[nrow(layer_param) + 1, ] <- c(7, "type 7", "#fbb938")
layer_param[nrow(layer_param) + 1, ] <- c(8, "type 8", "#f56918")
layer_param[nrow(layer_param) + 1, ] <- c(9, "type 9", "#c92903")
layer_param[nrow(layer_param) + 1, ] <- c(10, "type 10", "#7a0403")

xml_raster_doc = create_raster_unique_classified_colourDefined_qml(layer_param) # default = "continuous"

saveXML(xml_raster_doc, file=paste0("QML/", layer_name, "_raster_unique_classified_colourDefined.qml"), indent=TRUE)



