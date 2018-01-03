if(file.exists("SBCAPE.nc.gz")){
  gunzip("SBCAPE.nc.gz")
}

data(World)
height <- 550
width <- 550
#setwd("D:/Moje dokumenty/Praca_magisterska/MagisterkaNetCDF/shiny-master/")

messageData<-data.frame(1,1)
names(messageData)<-c("from","message")
messageData <- messageData[-1,]

messageData[nrow(messageData) + 1,] = c("Support","v2")


#netCDF

ncname <- "SBCAPE"#nazwa pliku
ncfname <- paste(ncname, ".nc", sep = "")
dname <- "SBCAPE" #parapmetr zmienny
ncin <- nc_open(ncfname)

lon <- ncvar_get(ncin, "lon")
nlon <- dim(lon)


lat <- ncvar_get(ncin, "lat", verbose = F)
nlat <- dim(lat)


t <- ncvar_get(ncin, "time")
tunits <- ncatt_get(ncin, "time", "units")
nt <- dim(t)



######################################################
tmp.array <- ncvar_get(ncin, dname)
dlname <- ncatt_get(ncin, dname, "long_name")
dunits <- ncatt_get(ncin, dname, "units")
fillvalue <- ncatt_get(ncin, dname, "_FillValue")
tustr <- strsplit(tunits$value, " ")
tdstr <- strsplit(unlist(tustr)[3], "-")
tmonth = as.integer(unlist(tdstr)[2])
tday = as.integer(unlist(tdstr)[3])
tyear = as.integer(unlist(tdstr)[1])
#chron(t, origin = c(tmonth, tday, tyear))

#tmp.array[tmp.array == fillvalue$value] <- NA
#tunits$value #  informacje o sposobie przechowywania czasu w pliku
czas <- as.POSIXct(t, origin="1970-01-01", tz='GMT')



dataInput <- reactive({
  
  
  
})



#m <- 1
shinyServer(function(input, output) {
  output$messageMenu <- renderMenu({
    # Code to generate each of the messageItems here, in a list. This assumes
    # that messageData is a data frame with two columns, 'from' and 'message'.
    
    
    
    msgs <- apply(messageData, 1, function(row) {
      messageItem(from = row[["from"]], message = row[["message"]])
    })
    
    # This is equivalent to calling:
    #   dropdownMenu(type="messages", msgs[[1]], msgs[[2]], ...)
    dropdownMenu(type = "messages", .list = msgs)
  })
  
  
  output$czas_temp <- renderUI({
    m <- input$bins
    titlePanel(paste(format(czas[m],"%Y-%m-%d %H:%M")))
  })
  
  output$czas_cape <- renderUI({
    m <- input$bins
    titlePanel(paste(format(czas[m],"%Y-%m-%d %H:%M")))
  })
  

    
    
    
  output$Plot_temp <- renderPlot({
    colory <- c("white","cyan","green","yellow","orange", "red", "#600000")
    #colfunc <- colorRampPalette(c("white","cyan","green","yellow","orange", "red", "#600000"))
    m <- input$bins 
    tmp.slice <- tmp.array[, , m]
    #grid <- expand.grid(lon = lon, lat = lat)
    
    lonlat <- expand.grid(lon, lat)
    tmp.vec <- as.vector(tmp.slice)
    #length(tmp.vec)
    
    
    tmp.df01 <- data.frame(cbind(lonlat, tmp.vec))
    names(tmp.df01) <- c("lon", "lat", paste(dname, as.character(m), sep = "_"))
    pts_temp <- tmp.df01
    colnames(pts_temp) <- c("x","y","z")
    
    
    coordinates(pts_temp)=~x+y
    proj4string(pts_temp)=CRS("+init=epsg:4326") # set it to lat-long
    pts = spTransform(pts_temp,CRS("+init=epsg:4326"))
    gridded(pts_temp) = TRUE
    r_temp = raster(pts_temp)
    projection(r_temp) = CRS("+init=epsg:4326")

    
    #writeRaster(r, filename=paste(format(czas[m],"%Y-%m-%d %H:%M"),".tif", sep=""), options=c('TFW=YES'))
    
    
  tm_shape(r_temp, n.x = 5) +
      tm_raster(n=50,palette = colory, auto.palette.mapping = FALSE, interpolate = T, breaks = seq(0,4500, 250),
                title="CAPE \n[J/Kg^-2]", legend.show = F)+ 
     tm_format_Europe(title = NA, title.position = c("left", "top"),attr.outside=T,legend.outside=TRUE,
                       legend.text.size = 1.5,legend.outside.position=c("left"),
                       attr.position = c("right", "bottom"), legend.frame = TRUE,
                       inner.margins = c(.0, .0, 0, 0))+tm_scale_bar(size = 1)+
      tm_shape(World, unit = "km") +
      tm_polygons(alpha = 0)+
      tm_xlab("latitude", size = 1, rotation = 0)+
      tm_ylab("longitude", size = 1, rotation = 90)+
   tm_grid(x = c(-20,0,20,40,60),y=c(40,50,60,70,80), labels.inside.frame=T)+

    tmap_mode("plot")
    
}, height = height, width = width)
  
  
  
  output$Plot_cape <- renderPlot({
    colory <- c("white","cyan","green","yellow","orange", "red", "#600000")
    #colfunc <- colorRampPalette(c("white","cyan","green","yellow","orange", "red", "#600000"))
    m <- input$bins 
    tmp.slice <- tmp.array[, , m]
    #grid <- expand.grid(lon = lon, lat = lat)
    
    lonlat <- expand.grid(lon, lat)
    tmp.vec <- as.vector(tmp.slice)
    #length(tmp.vec)
    
    
    tmp.df01 <- data.frame(cbind(lonlat, tmp.vec))
    names(tmp.df01) <- c("lon", "lat", paste(dname, as.character(m), sep = "_"))
    pts <- tmp.df01
    colnames(pts) <- c("x","y","z")
    
    
    coordinates(pts)=~x+y
    proj4string(pts)=CRS("+init=epsg:4326") # set it to lat-long
    pts = spTransform(pts,CRS("+init=epsg:4326"))
    gridded(pts) = TRUE
    r = raster(pts)
    projection(r) = CRS("+init=epsg:4326")
    
    #writeRaster(r, filename=paste(format(czas[m],"%Y-%m-%d %H:%M"),".tif", sep=""), options=c('TFW=YES'))
    
    
    tm_shape(r, n.x = 5) +
      tm_raster(n=50,palette = colory, auto.palette.mapping = FALSE, interpolate = T, breaks = seq(0,4500, 250),
                title="CAPE \n[J/Kg^-2]", legend.show = F)+ 
      tm_format_Europe(title = NA, title.position = c("left", "top"),attr.outside=T,legend.outside=TRUE,
                       legend.text.size = 1.5,legend.outside.position=c("left"),
                       attr.position = c("right", "bottom"), legend.frame = TRUE,
                       inner.margins = c(.0, .0, 0, 0))+tm_scale_bar(size = 1)+
      tm_shape(World, unit = "km") +
      tm_polygons(alpha = 0)+
      tm_xlab("latitude", size = 1, rotation = 0)+
      tm_ylab("longitude", size = 1, rotation = 90)+
      tm_grid(x = c(-20,0,20,40,60),y=c(40,50,60,70,80), labels.inside.frame=T)+
    tmap_mode("plot")
  }, height = height, width = width)
  
  
  #tutaj będą dane z temperaturą powietrza
  output$widget_temp <- renderPlot({
    m <- input$bins 

    tmp.slice <- tmp.array[, , m]
    grid <- expand.grid(lon = lon, lat = lat)
    
    lonlat <- expand.grid(lon, lat)
    tmp.vec <- as.vector(tmp.slice)
    length(tmp.vec)
    
    
    tmp.df01 <- data.frame(cbind(lonlat, tmp.vec))
    names(tmp.df01) <- c("lon", "lat", paste(dname, as.character(m), sep = "_"))
    pts <- tmp.df01
   plot(tmp.df01$SBCAPE, type="line")
    
  })
  
  #output$widget_cape <- renderPlot({
  #  m <- input$bins 
  #
   # tmp.slice <- tmp.array[, , m]
   # grid <- expand.grid(lon = lon, lat = lat)
    
   # lonlat <- expand.grid(lon, lat)
   # tmp.vec <- as.vector(tmp.slice)
   # length(tmp.vec)
    
    
   # tmp.df01 <- data.frame(cbind(lonlat, tmp.vec))
   # names(tmp.df01) <- c("lon", "lat", paste(dname, as.character(m), sep = "_"))
  #  pts <- tmp.df01
  #  plot(tmp.df01$SBCAPE_1, type="p")
 # })
  
})

