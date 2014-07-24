## This includes some helper functions for data entry or extraction from the taxonomy.


##' Helper function to generate Location and Popualtion files from templates
##' @param uid unique cholera id
##' @param restricted TRUE if data is restricted (vs. public)
##' @param target.column.name What is the ISO level we wish to make these files for(name of column in EPI file)
##' @param get.gps if TRUE it trys to get the GPS coordinates for each location from google maps
##' @return NULL
generate.location.and.population.files <- function(uid,
                                                   restricted,
                                                   target.column.name="ISO_A2_L2",
                                                   get.gps=FALSE,
                                                   zoom.level=7){

    #get the epi.file
    rest <- ifelse(restricted,"Restricted","Public")
    epi.curve <- read.csv(paste0("EpiCurves/",rest,"/CHOLERA",uid,"_EPI.csv"),as.is=T)

    ## get unique locations
    if (target.column.name %in% colnames(epi.curve)){
        locs <- unique(epi.curve[,target.column.name])
        locs <- as.character(locs[which(locs!="")])
    } else {
        stop(sprintf("Target column name (%s) is not found in the EPI file.  Please check that this name is correct",target.column.name))
    }


    # check to make sure we only have one WHO region
    who.region <- as.character(unique(epi.curve$who_region))
    who.region <- who.region[who.region != ""]
    if (length(who.region) != 1) stop("Expecting only a single WHO Region for this EPI File.")

    ## check to see if it is an A1 or A2
    a.or.l <- substr(target.column.name,nchar(target.column.name)-1,nchar(target.column.name)-1)
    if (a.or.l == "L"){
        L.level <- as.numeric(substr(target.column.name,nchar(target.column.name),nchar(target.column.name)))

        iso.levels.of.interest <- paste0("ISO_A2_L",1:L.level)
        root.file.names <- apply(epi.curve,1,function(x){
            tmp <- x[c("who_region","ISO_A1",iso.levels.of.interest)]
            tmp <- gsub("[^a-zA-Z0-9-]| ", "",tmp)

            if (any(tmp == "" | is.na(tmp)))  return("")
            paste(tmp,collapse="_")
        })

        root.file.names <- unique(root.file.names[root.file.names!= ""])

    } else if (a.or.l == "A"){
        root.file.names <- apply(epi.curve,1,function(x){
            tmp <- x[c("who_region","ISO_A1")]
          #  tmp <- gsub("[^a-zA-Z0-9-]| ", "",tmp)
            if (any(tmp == "" | is.na(tmp)))  return("")
            paste(tmp,collapse="_")
        })
    }

    ## make sure we don't already have location files
    potential.loc.files <- paste0(root.file.names,"_LOC.csv")
    current.loc.files <- list.files("Location")

    conflicts <- (potential.loc.files %in% current.loc.files)
    reduced.locs <- locs
    if (any(conflicts)) {
        warning(sprintf("Not creating the following files since they already exisit in the Location directory: %s",paste(potential.loc.files[which(conflicts)],sep=",",collapse=",")))
        potential.loc.files <- potential.loc.files[-which(conflicts)]
        reduced.locs <- locs[-which(conflicts)]
    }

    if (length(potential.loc.files) > 0){
        ## read in loc file template
        loc.template <- data.frame(read.csv("LOC-Template.csv",header=F,as.is=T),"")
        class(loc.template[,2]) <- "character"
        if (get.gps){
            add.to.map.search <- readline("Enter comma seperated administrative names to help the search. For example if your locations are neighbohoods in a Kampala, Uganda you can enter Kampala,Uganda \n Input:")
            geos <- geo.code.multi(paste(reduced.locs,add.to.map.search,sep=","))
            print(geos)
            cat("Mapping locations with zoom level = ",zoom.level)
            m <- map.geo.codes(geos,zoom=zoom.level)
            print(m)
        }

        for (i in seq_along(potential.loc.files)){
            create.file <- readline(sprintf("Do you want to create %s? \n  1 = Yes \n 0  =  No \n  -99 = do not create any further Location files \n Enter Response Here:",potential.loc.files[i]))
            if (create.file == -99) {
                cat("No more Location files will be created \n")
                break
            } else if (create.file == 1){
                tmp.loc.template <- loc.template
                cat(sprintf("writing file: %s \n",potential.loc.files[i]))
                # writing NAs to everything and then adding name into the name field
                tmp.loc.template[,2] <- NA
                tmp.loc.template[1,2] <- reduced.locs[i]
                                        #strsplit(as.character(geos[i,1]),",")[[1]][1]

                if (get.gps){
                    tmp.loc.template[2:3,2] <- as.character(unlist(geos[i,2:3]))
                    tmp.loc.template[9,2] <- "Automatically generated points"
                }
                write.table(tmp.loc.template,file=paste0("Location/",potential.loc.files[i]),sep=",",row.names=FALSE,col.names=FALSE)
            }
        }
    }

    ## now onto the population files
    potential.pop.files <- paste0(root.file.names,"_POP.csv")
    current.pop.files <- list.files("Population")

    conflicts <- (potential.pop.files %in% current.pop.files)
    if (any(conflicts)) {
        warning(sprintf("Not creating the following files since they already exisit in the Population directory: %s",paste(potential.pop.files[which(conflicts)],sep=",",collapse=",")))
        potential.pop.files <- potential.pop.files[-which(conflicts)]
    }

    if (length(potential.pop.files) > 0){
        pop.template <- read.csv("POP-Template.csv")
        for (i in seq_along(potential.pop.files)){
            create.file <- readline(sprintf("Do you want to create %s? \n  1 = Yes \n 0  =  No \n  -99 = do not create any further Population files \n Enter Response Here:",potential.pop.files[i]))
            if (create.file == -99) {
                cat("No more Population files will be created \n")
                break
            } else if (create.file == 1){
                cat(sprintf("writing file: %s \n",potential.pop.files[i]))
                write.csv(pop.template,file=paste0("Population/",potential.pop.files[i]),row.names=F)
            }
        }
    }

    if (get.gps & length(potential.loc.files) > 0) return(geos)
    else return(NULL)
}

##' constructs url for google maps for an area
##' http://stackoverflow.com/questions/3257441/geocoding-in-r-with-google-maps
##' @param address
##' @param return.call
##' @param sensor
##' @return
##' @author http://stackoverflow.com/questions/3257441/geocoding-in-r-with-google-maps
construct.geocode.url <- function(address, return.call = "json", sensor = "false") {
    root <- "http://maps.google.com/maps/api/geocode/"
    u <- paste(root, return.call, "?address=", address, "&sensor=", sensor, sep = "")
    return(URLencode(u))
}

##' takes a location and attempts to geocode using google maps
##' @param address
##' @param verbose
##' @return lat and lon for an address/location
##' @author http://stackoverflow.com/questions/3257441/geocoding-in-r-with-google-maps
geo.code <- function(address,verbose=FALSE) {
    library(RCurl)
    library(RJSONIO)
    library(plyr)

    if(verbose) cat(address,"\n")
    u <- construct.geocode.url(address)
    doc <- getURL(u)
    x <- fromJSON(doc,simplify = FALSE)
    if(x$status=="OK") {
        lat <- x$results[[1]]$geometry$location$lat
        lng <- x$results[[1]]$geometry$location$lng
        location_type <- x$results[[1]]$geometry$location_type
        formatted_address <- x$results[[1]]$formatted_address
        return(data.frame(formatted_address,lat, lng,location_type))
    } else {
        return(data.frame(NA,NA,NA, NA))
    }
}

##'
##' @title
##' @param address
##' @param verbose
##' @return
geo.code.multi <- function(address,verbose=FALSE) {
    require(RCurl)
    require(RJSONIO)
    require(plyr)
    if(verbose) cat(address,"\n")
    u <- aaply(address,1,construct.geocode.url)
    doc <- aaply(u,1,getURL)
    json <- alply(doc,1,fromJSON,simplify = FALSE)
    coord <- laply(json,function(x) {
        if(x$status=="OK") {
            lat <- x$results[[1]]$geometry$location$lat
            lng <- x$results[[1]]$geometry$location$lng
            return(c(lat, lng))
        } else {
            return(c(NA,NA))
        }
    })

    if(length(address)>1) {

        coord[,1] <- as.numeric(coord[,1])
        coord[,2] <- as.numeric(coord[,2])
        rc <- data.frame(address,coord)

    } else {
        rc <- data.frame(t(c(address,coord)))
    }


    colnames(rc)=c("location","lat","lon")
    rc[,2] <- as.numeric(as.character(rc[,2]))
    rc[,3] <- as.numeric(as.character(rc[,3]))

    return(rc)
}

##' maps output from geo.code.multi
##' @param geo.codes
##' @param zoom2
##' @return
map.geo.codes <- function(geo.codes,zoom=9){
    require(ggmap)
    med.lat <- median(geo.codes[,"lat"],na.rm=T)
    med.lon <- median(geo.codes[,"lon"],na.rm=T)

    hdf <- get_map(location = c(lon = med.lon, lat = med.lat), zoom = zoom, maptype = 'roadmap')
    g <- ggmap(hdf,extent='panel') + geom_point(data=geo.codes, aes(x=lon, y=lat,label=location),alpha=.5,col="red")
    g + geom_text(data = geo.codes, aes(x = lon, y = lat, label = location), size = 2, vjust = 1, hjust = 0.3)
}
