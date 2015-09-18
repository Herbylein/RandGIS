## Functions

# 1. Write a function to create the rasters
rasterize.XYZ <- function(xyzObject){
  
  # checking conditions
  run.function <- TRUE
  
  if (is.null(xyzObject[["fileName"]]) == TRUE){
    print ("You have not provided a input xyz file")
    run.function <- FALSE
  }
  
  if (is.null(xyzObject[["CRS"]]) == TRUE){
    print ("You have not provided the CRS (coordinate system")
    run.function <- FALSE
  }
  
  if (is.null(xyzObject[["resolution"]]) == TRUE){
    print ("You have not provided the output resolution")
    run.function <- FALSE
  }
  
  # Checking if file exist
  if (xyzObject$overwrite == FALSE){
    filename = file.path(xyzObject$outDir, paste(xyzObject$fileName,".tif", sep=""))
    if (file.exists(filename) == TRUE){
      run.function <- FALSE
    }
  }  
  
  # Running the function
  if (run.function==TRUE){
    
    # Opening the file
    data.xyz <- read.table(file.path(xyzObject$inDir,xyzObject$fileName), header=FALSE, col.names = c("X", "Y", "Z"))
    
    empty.raster  <- raster(nrows=(max(data.xyz[,2])-min(data.xyz[,2])), ncols=(max(data.xyz[,1])-min(data.xyz[,1])), 
                            xmn=min(data.xyz[,1]), xmx=max(data.xyz[,1]), ymn=min(data.xyz[,2]), ymx=max(data.xyz[,2]),
                            crs = xyzObject$CRS, resolution = xyzObject$resolution)
    
    newRaster <- rasterize(data.xyz[, 1:2], empty.raster, data.xyz [,3], fun=xyzObject$Fun)
    
    filename = file.path(xyzObject$outDir, paste(xyzObject$fileName,".tif", sep=""))
    
    writeRaster(newRaster, filename=filename, format="GTiff", overwrite=TRUE)
    
    return (filename)
  }
  
}


mosaic.multi <- function(inDir=NULL, pattern=NULL, outName=NULL, FUN=NULL, tolerance = 0.5 ){
  
  # finding files in dir
  filenames <- list.files(inDir, pattern=pattern, full.names=TRUE)
  # openning the files
  
  mosaicObject <- lapply(filenames, raster)
  
  # Add atributes to 
  
  mosaicObject$filename <- file.path(inDir, outName)
  mosaicObject$fun <- FUN
  mosaicObject$tolerance <- tolerance

  
  cat("Merging a bunch of files")
  do.call(merge, mosaicObject)
  
  return(mosaicObject$filename)
  
}
