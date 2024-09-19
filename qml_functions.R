library(XML)
library(uuid)

param_global_default <- list(joinstyle = "bevel",
                              outline_width_unit = "MM",
                              outline_width = 0.06,
                              outline_style = "solid",
                              outline_color = "35,35,35,255",
                              style = "solid",
                              scaleMax = 100000,
                              scaleMin = 0,
                              bufferColor="white",
                              bufferSize=1                            
                      )

update_param_default_to_ellipsis <- function(param_list){
  
  param_default_base = param_global_default
  
  for(i in 1:length(param_list)){
    for(j in 1:length(param_default_base)){
      if(names(param_default_base[j]) == names(param_list[i])){
        
        param_default_base[[j]] = param_list[[i]]

      }
    }
  }
  
  sort_ellipsis <- param_default_base
  
}

############# QML BUILD FUNCTIONS #################################################################################

create_vector_range_classified_qml <- function(attribute_name, vector_param, label_attribute = NULL, ...) {
  
  # INPUT DF FORMAT
  # vector_param <- data.frame(limit_lower=double(),
  #                           limit_upper=double(),
  #                           label=character(),
  #                           HTMLcolour=character(),
  #                           stringsAsFactors=FALSE
  # )  
  
  
  param <- list(...)  
  param <- update_param_default_to_ellipsis(param)

  # XML STRING 
  prefix.xml <- "<qgis styleCategories='Symbology|Labeling' version='3.34.8-Prizren'>
                    <renderer-v2>
                      <ranges>
                        <range/>
                      </ranges>
                      <symbols>
                        <symbol type='fill'>
                          <layer>
                             <Option type='Map'>
                              <Option type='QString'/>
                            </Option>                       
                          </layer>
                        </symbol>
                      </symbols>
                    </renderer-v2>
                    <labeling>
                      <settings>
                        <text-style>
                          <text-buffer/>
                        </text-style>
                        <rendering/>
                      </settings>
                    </labeling>
                 </qgis>"
  
  # file uuid
  guuid = UUIDgenerate() #"9016d819-cfd4-4bd9-89e4-c868788b9e5d" #
  
  # BUILD XML TREE
  doc = xmlTreeParse(prefix.xml, useInternalNodes = T)     # PARSE STRING
  root = xmlRoot(doc)                                      # FIND ROOT
  
  renderer = newXMLNode("renderer-v2", parent=root)           # ADD TO ROOT
  xmlAttrs(renderer) = c(graduatedMethod = "GraduatedColor",
                         type = "graduatedSymbol",
                         forceraster ="0",
                         symbollevels = "0",
                         symbollevels = "0",
                         referencescale = "-1",
                         attr = attribute_name,
                         enableorderby = "0")              # ADD ATTRIBUTE
  
  ranges = newXMLNode("ranges", parent=renderer)       # ADD TO REPORT
  
  for (i in 1:nrow(vector_param)){
    range <- newXMLNode("range", parent=ranges)  # ADD COL/ROW VALUE
    xmlAttrs(range) = c(uuid=paste0("{",UUIDgenerate(),"}"),
                        upper=vector_param$limit_upper[i],
                        symbol=as.character((i-1)),
                        label=vector_param$label[i],
                        render="true",
                        lower=vector_param$limit_lower[i]) 
  }
  

  symbols = newXMLNode("symbols", parent=renderer)
  for (i in 1:nrow(vector_param)){
    symbol = newXMLNode("symbol", parent=symbols) 
    xmlAttrs(symbol) = c(type="fill",
                         is_animated="0",
                         frame_rate="10",
                         clip_to_extent="1",
                         alpha="1",
                         name=as.character((i - 1)),
                         force_rhr="0")
    layer = newXMLNode("layer", parent=symbol)
    xmlAttrs(layer) = c(pass="0",
                        id=paste0("{", guuid, "}"),
                        class="SimpleFill",
                        enabled="1",
                        locked="0") 
    Option_map = newXMLNode("Option", parent=layer)
    xmlAttrs(Option_map) = c(type = "Map")
    
    Option_QString = newXMLNode("Option", parent=Option_map)
    xmlAttrs(Option_QString) = c(type="QString", value=vector_param$HTMLcolour[i], name="color")
    Option_QString = newXMLNode("Option", parent=Option_map)
    xmlAttrs(Option_QString) = c(type="QString", value=as.character(param[["outline_width"]]), name="outline_width")
    Option_QString = newXMLNode("Option", parent=Option_map)    
    xmlAttrs(Option_QString) = c(type="QString", value=as.character(param[["outline_width_unit"]]), name="outline_width_unit")
    Option_QString = newXMLNode("Option", parent=Option_map)    
    xmlAttrs(Option_QString) = c(type="QString", value=as.character(param[["outline_style"]]), name="outline_style")   
    Option_QString = newXMLNode("Option", parent=Option_map)    
    xmlAttrs(Option_QString) = c(type="QString", value=as.character(param[["outline_color"]]), name="outline_color")      
    Option_QString = newXMLNode("Option", parent=Option_map)
    xmlAttrs(Option_QString) = c(type="QString", value=as.character(param[["joinstyle"]]), name="joinstyle")
    Option_QString = newXMLNode("Option", parent=Option_map)
    xmlAttrs(Option_QString) = c(type="QString", value=as.character(param[["style"]]), name="style")
    
  }
  
  if(!is.null(label_attribute)){
    labeling = newXMLNode("labeling", parent=root)           # ADD TO ROOT
    xmlAttrs(labeling) = c(type="simple")
      settings = newXMLNode("settings", parent=labeling)
      xmlAttrs(settings) = c(calloutType="simple")
      text.style = newXMLNode("text-style", parent=settings)
      xmlAttrs(text.style) = c(fontFamily="Open Sans",
                             fontKerning="1",
                             namedStyle="Regular",
                             textColor="0,0,0,255",
                             fontSizeUnit="Point",
                             fontSize="10",
                             fieldName=label_attribute
                             )
      text.buffer = newXMLNode("text-buffer", parent=text.style)
      xmlAttrs(text.buffer) = c(bufferOpacity="1",
                                bufferColor=as.character(param[["bufferColor"]]),
                                bufferSize=as.character(param[["bufferSize"]]),
                                bufferJoinStyle="128",
                                bufferDraw="1",
                                bufferNoFill="1")
      rendering = newXMLNode("rendering", parent=settings)
      xmlAttrs(rendering) = c(scaleVisibility="1",
                                scaleMax=as.character(param[["scaleMax"]]),
                                scaleMin=as.character(param[["scaleMin"]]))
    
  }
  
  
  removeNodes(list(root[[1]],root[[2]]))
  
  create_vector_range_classified_qml = doc
  
}

create_vector_unique_classified_qml <- function(attribute_name, vector_param, label_attribute = NULL) {

  # INPUT DF FORMAT  
  # layer_param <- data.frame(category=character(),
  #                           label=character(),
  #                           HTMLcolour=character(),
  #                           stringsAsFactors=FALSE
  # )
  
  # XML STRING 
  prefix.xml <- "<qgis styleCategories='Symbology|Labeling' version='3.34.8-Prizren'>
                    <renderer-v2>
                      <categories>
                        <category/>
                      </categories>
                      <symbols>
                        <symbol type='fill'>
                          <layer>
                             <Option type='Map'>
                              <Option type='QString'/>
                            </Option>                       
                          </layer>
                        </symbol>
                      </symbols>
                    </renderer-v2>
                    <labeling>
                      <settings>
                        <text-style>
                          <text-buffer/>
                        </text-style>
                        <rendering/>
                      </settings>
                    </labeling>
                 </qgis>"
  
  # prefix.xml <- "<qgis styleCategories='Symbology' version='3.34.8-Prizren'>
  #               </qgis>"
  
  # file uuid
  guuid = UUIDgenerate() #"9016d819-cfd4-4bd9-89e4-c868788b9e5d" #
  
  # BUILD XML TREE
  doc = xmlTreeParse(prefix.xml, useInternalNodes = T)     # PARSE STRING
  root = xmlRoot(doc)                                      # FIND ROOT
  
  renderer = newXMLNode("renderer-v2", parent=root)           # ADD TO ROOT
  xmlAttrs(renderer) = c(type = "categorizedSymbol",
                         forceraster ="0",
                         symbollevels = "0",
                         symbollevels = "0",
                         referencescale = "-1",
                         attr = attribute_name,
                         enableorderby = "0")              # ADD ATTRIBUTE
  
  categories = newXMLNode("categories", parent=renderer)       # ADD TO REPORT
  
  for (i in 1:nrow(vector_param)){
    category <- newXMLNode("category", parent=categories)  # ADD COL/ROW VALUE
    xmlAttrs(category) = c(uuid=paste0("{",UUIDgenerate(),"}"),
                        symbol=as.character((i-1)),
                        label=vector_param$label[i],
                        value=vector_param$category[i],
                        type="string",
                        render="true"
                        ) 
  }
  
  # source.symbol = newXMLNode("source-symbol", parent=renderer)       
  # symbol = newXMLNode("symbol", parent=source.symbol)       
  # xmlAttrs(symbol) = c(type="fill",
  #                      is_animated="0",
  #                      frame_rate="10",
  #                      clip_to_extent="1",
  #                      alpha="1",
  #                      name="0",
  #                      force_rhr="0")           # ADD ATTRIBUTE
  # 
  # layer = newXMLNode("layer", parent=symbol)
  # xmlAttrs(layer) = c(pass="0",
  #                     id=paste0("{", guuid, "}"),
  #                     class="SimpleFill",
  #                     enabled="1",
  #                     locked="0") 
  # 
  # Option_map = newXMLNode("Option", parent=layer)
  # xmlAttrs(Option_map) = c(type = "Map")
  # 
  # Option_QString = newXMLNode("Option", parent=Option_map)
  # xmlAttrs(Option_QString) = c(type="QString", value="3x:0,0,0,0,0,0", name="border_width_map_unit_scale")
  # Option_QString = newXMLNode("Option", parent=Option_map)
  # xmlAttrs(Option_QString) = c(type="QString", value="0,0,0,255", name="color")
  # Option_QString = newXMLNode("Option", parent=Option_map)
  # xmlAttrs(Option_QString) = c(type="QString", value="bevel", name="joinstyle")
  # Option_QString = newXMLNode("Option", parent=Option_map)
  # xmlAttrs(Option_QString) = c(type="QString", value="0,0", name="offset")
  # Option_QString = newXMLNode("Option", parent=Option_map)
  # xmlAttrs(Option_QString) = c(type="QString", value="3x:0,0,0,0,0,0", name="offset_map_unit_scale")
  # Option_QString = newXMLNode("Option", parent=Option_map)
  # xmlAttrs(Option_QString) = c(type="QString", value="MM", name="offset_unit")
  # Option_QString = newXMLNode("Option", parent=Option_map)
  # xmlAttrs(Option_QString) = c(type="QString", value="35,35,35,255", name="outline_color")
  # Option_QString = newXMLNode("Option", parent=Option_map)
  # xmlAttrs(Option_QString) = c(type="QString", value="solid", name="outline_style")
  # Option_QString = newXMLNode("Option", parent=Option_map)
  # xmlAttrs(Option_QString) = c(type="QString", value="0.06", name="outline_width")
  # Option_QString = newXMLNode("Option", parent=Option_map)
  # xmlAttrs(Option_QString) = c(type="QString", value="MM", name="outline_width_unit")
  # Option_QString = newXMLNode("Option", parent=Option_map)
  # xmlAttrs(Option_QString) = c(type="QString", value="solid", name="style")
  
  symbols = newXMLNode("symbols", parent=renderer)
  for (i in 1:nrow(vector_param)){
    symbol = newXMLNode("symbol", parent=symbols) 
    xmlAttrs(symbol) = c(type="fill",
                         is_animated="0",
                         frame_rate="10",
                         clip_to_extent="1",
                         alpha="1",
                         name=as.character((i - 1)),
                         force_rhr="0")
    layer = newXMLNode("layer", parent=symbol)
    xmlAttrs(layer) = c(pass="0",
                        id=paste0("{", guuid, "}"),
                        class="SimpleFill",
                        enabled="1",
                        locked="0") 
    Option_map = newXMLNode("Option", parent=layer)
    xmlAttrs(Option_map) = c(type = "Map")
    
    Option_QString = newXMLNode("Option", parent=Option_map)
    xmlAttrs(Option_QString) = c(type="QString", value=vector_param$HTMLcolour[i], name="color")
    Option_QString = newXMLNode("Option", parent=Option_map)
    xmlAttrs(Option_QString) = c(type="QString", value="0.06", name="outline_width")  
    
  }
  
  if(!is.null(label_attribute)){
    labeling = newXMLNode("labeling", parent=root)           # ADD TO ROOT
    xmlAttrs(labeling) = c(type="simple")
    settings = newXMLNode("settings", parent=labeling)
    xmlAttrs(settings) = c(calloutType="simple")
    text.style = newXMLNode("text-style", parent=settings)
    xmlAttrs(text.style) = c(fontFamily="Open Sans",
                             fontKerning="1",
                             namedStyle="Regular",
                             textColor="0,0,0,255",
                             fontSizeUnit="Point",
                             fontSize="10",
                             fieldName=label_attribute
    )
    text.buffer = newXMLNode("text-buffer", parent=text.style)
    xmlAttrs(text.buffer) = c(bufferOpacity="1",
                              bufferColor="250,250,250,255",
                              bufferSize="1",
                              bufferJoinStyle="128",
                              bufferDraw="1",
                              bufferNoFill="1")
    rendering = newXMLNode("rendering", parent=settings)
    xmlAttrs(rendering) = c(scaleVisibility="1",
                              scaleMax="100000",
                              scaleMin="0")
    
  }
  
  
  # VIEW XML
  # print(doc)
  
  removeNodes(list(root[[1]],root[[2]]))
  
  create_vector_unique_classified_qml = doc
  
}

create_raster_continuous_qml <- function(min, max, colourRamp){
  
  class_values <- seq(from = min, to = max, length.out = 255)

  cols <- colourRamp(255)
  
  df <- cbind(class_values, cols)
  df <- as.data.frame(df)

  prefix.xml <- "<qgis maxScale='0' styleCategories='AllStyleCategories' minScale='1e+08' version='3.34.8-Prizren' hasScaleBasedVisibilityFlag='0'>
                    <pipe>
                      <rasterrenderer>
                        <rastershader>
                          <colorrampshader>
                              <item/>
                          </colorrampshader>
                        </rastershader>
                      </rasterrenderer>
                    </pipe>
                 </qgis>" 
  
  doc = xmlTreeParse(prefix.xml, useInternalNodes = T)     # PARSE STRING
  root = xmlRoot(doc)                                      # FIND ROOT
  
  pipe = newXMLNode("pipe", parent=root)           # ADD TO ROOT
  rasterrenderer = newXMLNode("rasterrenderer", parent=pipe)
  xmlAttrs(rasterrenderer) = c(nodataColor="",
                               classificationMax="10",
                               opacity="1",
                               band="1",
                               classificationMin="1",
                               type="singlebandpseudocolor",
                               alphaBand="2")
  rastershader = newXMLNode("rastershader", parent=rasterrenderer)
  colorrampshader = newXMLNode("colorrampshader", parent=rastershader)
  xmlAttrs(colorrampshader) = c(colorRampType="INTERPOLATED",
                                maximumValue=as.character(max),
                                labelPrecision="0",
                                classificationMode="1",
                                minimumValue=as.character(min),
                                clip="0")
  
  for (i in 1:nrow(df)){ 
    item = newXMLNode("item", parent=colorrampshader)    
    
    label_val = as.character(round(as.double(df$class_values[i]), digits = 0))

    xmlAttrs(item) = c(alpha="255",
                       value=as.character(df$class_values[i]),
                       color=df$cols[i],
                       label=label_val)    
  }
  
  removeNodes(list(root[[1]]))  
  
  create_raster_continuous_qml = doc
  
}

create_raster_unique_classified_colourRamp_qml <- function(class_values, colourRamp){

  # INPUT DF FORMAT
  # layer_param <- data.frame(class_value=double(),
  #                           label=character(),
  #                           stringsAsFactors=FALSE
  # )  
  
    
  # class_values <- seq(from = min, to = max, length.out = 255)
  
  if(nrow(class_values) > 255){
    #class_values <- seq(from = min(class_values), to = max(class_values), length.out = 255)
    # message too many classes
  }

  cols <- colourRamp(nrow(class_values))
  
  df <- cbind(class_values, cols)
  df <- as.data.frame(df)
  # rgb(68, 1, 84, maxColorValue=255)
  
  prefix.xml <- "<qgis maxScale='0' styleCategories='AllStyleCategories' minScale='1e+08' version='3.34.8-Prizren' hasScaleBasedVisibilityFlag='0'>
                    <pipe>
                      <rasterrenderer>
                          <colorPalette>
                              <paletteEntry/>
                          </colorPalette>
                      </rasterrenderer>
                    </pipe>
                 </qgis>" 
  
  doc = xmlTreeParse(prefix.xml, useInternalNodes = T)     # PARSE STRING
  root = xmlRoot(doc)                                      # FIND ROOT
  
  pipe = newXMLNode("pipe", parent=root)           # ADD TO ROOT
  rasterrenderer = newXMLNode("rasterrenderer", parent=pipe)
  xmlAttrs(rasterrenderer) = c(nodataColor="",
                               type="paletted",
                               labelPrecision="0",
                               alphaBand="2",
                               band="1",
                               opacity="1")
  colorpalette = newXMLNode("colorPalette", parent=rasterrenderer)

  for (i in 1:nrow(df)){ 
    item = newXMLNode("paletteEntry", parent=colorpalette)
    xmlAttrs(item) = c(value=as.character(df$class_value[i]),
                       color=df$cols[i],
                       alpha="255",
                       label=df$label[i])
  }
  
  removeNodes(list(root[[1]]))  
  
  create_raster_unique_classified_colourRamp_qml = doc
  
}

create_raster_unique_classified_colourDefined_qml <- function(class_values){
  
  # INPUT DF FORMAT
  # layer_param <- data.frame(class_value=double(),
  #                           label=character(),
  #                           HTMLcolour=character(),
  #                           stringsAsFactors=FALSE
  # )  
  
  # class_values <- seq(from = min, to = max, length.out = 255)
  
  if(nrow(class_values) > 255){
    # message too many classes
  }
  
  df <- class_values

  prefix.xml <- "<qgis maxScale='0' styleCategories='AllStyleCategories' minScale='1e+08' version='3.34.8-Prizren' hasScaleBasedVisibilityFlag='0'>
                    <pipe>
                      <rasterrenderer>
                          <colorPalette>
                              <paletteEntry/>
                          </colorPalette>
                      </rasterrenderer>
                    </pipe>
                 </qgis>" 
  
  doc = xmlTreeParse(prefix.xml, useInternalNodes = T)     # PARSE STRING
  root = xmlRoot(doc)                                      # FIND ROOT
  
  pipe = newXMLNode("pipe", parent=root)           # ADD TO ROOT
  rasterrenderer = newXMLNode("rasterrenderer", parent=pipe)
  xmlAttrs(rasterrenderer) = c(nodataColor="",
                               type="paletted",
                               labelPrecision="0",
                               alphaBand="2",
                               band="1",
                               opacity="1")
  colorpalette = newXMLNode("colorPalette", parent=rasterrenderer)
  
  for (i in 1:nrow(df)){ 
    item = newXMLNode("paletteEntry", parent=colorpalette)
    xmlAttrs(item) = c(value=as.character(df$class_value[i]),
                       color=df$HTMLcolour[i],
                       alpha="255",
                       label=df$label[i])
  }
  
  removeNodes(list(root[[1]]))  
  
  create_raster_unique_classified_colourRamp_qml = doc
  
}

build_geopackage_style_sql <- function(qml_doc, layer_name){
  
  create_geopackage_style_sql = paste0('INSERT INTO layer_styles (f_table_name,f_geometry_column,styleName,styleQML,useAsDefault,description,f_table_schema)
    VALUES(
    	\'', layer_name ,'\',
    	\'geom\',
    	\'', layer_name ,'_render\',
    	\'',
       qml_doc
      ,'\',
      1,
      "', layer_name ,' layer render",
      ""
    );')
  
  
}


