## Functions

# 1. Write a function to create the rasters from single folders
rasterize.XYZ <- function(xyzObject){
  
  # checking conditions
  run.function <- TRUE
  
  if (is.null(xyzObject[["fileName"]]) == TRUE){
    warning ("You have not provided a input xyz file")
    run.function <- FALSE
  }
  
  if (is.null(xyzObject[["CRS"]]) == TRUE){
    warning ("You have not provided the CRS (coordinate system")
    run.function <- FALSE
  }
  
  if (is.null(xyzObject[["resolution"]]) == TRUE){
    warning ("You have not provided the output resolution")
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
    
    new.raster <- rasterize(data.xyz[, 1:2], empty.raster, data.xyz [,3], fun=xyzObject$Fun)
    
    filename = file.path(xyzObject$outDir, paste(xyzObject$fileName,".tif", sep=""))
    
    writeRaster(new.raster, filename=filename, format="GTiff", overwrite=TRUE)
    
    return (filename)
  }
  
}

##################################################################################################################

# Write a function to create the rasters from single folders, but first merge them
rasterize.XYZlowerResolution <- function(xyzObject){
  
  # checking conditions
  run.function <- TRUE

  if (is.null(xyzObject[["filenames"]]) == TRUE){
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
  
  # Check files
  fileNames <- xyzObject$filenames
  #for (j in 1:length(fileNames)){
  #  if (!file.exists(fileNames[j]) == TRUE){
  #    fileNames <- fileNames[-j]
  #  }
  #}
  info <- file.info(fileNames)
  fileNames.notempty = rownames(info[info$size != 0, ])

  # Running the function
  if (run.function==TRUE){
    
    # Opening the file
    data <- lapply(fileNames.notempty, function(x) {
      read.table(x, header = FALSE, sep = "", colClasses = rep("numeric", 3) )      
    })
    # remove large values
    data.all <- do.call("rbind",data)
    data.xyz <- data.all[ data.all[[3]] < 60, ] 
    
    if (max(data.xyz[,2])-min(data.xyz[,2]) <= 0){
      cat("Error with Y coordinates. Min Y larger or equal to max Y.")
      run.function <-FALSE
    }
    if (max(data.xyz[,1])-min(data.xyz[,1]) <= 0){
      cat("Error with X coordinates. Min X larger or equal to max X.")
      run.function <-FALSE
    }
  }
  if (run.function==TRUE){
    empty.raster  <- raster(nrows=(max(data.xyz[,2])-min(data.xyz[,2])), ncols=(max(data.xyz[,1])-min(data.xyz[,1])), 
                            xmn=min(data.xyz[,1]), xmx=max(data.xyz[,1]), ymn=min(data.xyz[,2]), ymx=max(data.xyz[,2]),
                            crs = xyzObject$CRS, resolution = xyzObject$resolution)
    
    newRaster <- rasterize(data.xyz[,1:2], empty.raster, data.xyz[,3], fun=xyzObject$Fun)
    
    writeRaster(newRaster, filename=file.path(xyzObject$outDir, xyzObject$outName), format="GTiff", overwrite=TRUE)
    
    return ( file.path(xyzObject$outDir, xyzObject$outName))
    }
  
}


##################################################################################################################

mosaic.multi <- function(inDir=NULL, pattern=NULL, outName=NULL, FUN=NULL, tolerance = 0.5 ){
  # checking conditions
  run.function <- TRUE
  
  if (is.null(inDir) == TRUE){
    warning ("You have not provided an input directory file")
    run.function <- FALSE
  }
  
  if (is.null(pattern) == TRUE){
    warning ("You have not provided a search pattern")
    run.function <- FALSE
  }
  
  if (is.null(outName) == TRUE){
    warning ("You have not provided an ouput name")
    run.function <- FALSE
  }
  
  if (is.null(outName) == TRUE){
    warning ("You have not provided an fun method")
    run.function <- FALSE
  }
  
  if (is.null(tolerance) == TRUE){
    warning ("You have not provided a tolerance value")
    run.function <- FALSE
  }
  
  if (run.function == TRUE){
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

}
