##' checks all entries in a given directory
##' @param home.dir - root directory for the taxonomy data structure
##' @param out.file - file to write log to
##' @param append.log - append out.file or not?
##' @return
##' @author asa
checkAllEntries <- function(home.dir="./",
                            out.file="taxonomylog-all.log",
                            append.log=FALSE){
    ## first get all the id numbers based on the descripion files

    d.files <- list.files(paste0(home.dir,"/Description/"))
    my.ids <- sapply(
        strsplit(
            sapply(
                strsplit(d.files,"CHOLERA"),
                function(x) x[2]),"_"),
        function(y) y[1])

    ## TO DO: probably want to check they are all numbers here
    ## and some

    setwd(home.dir)

    ## make sure to close sink() if anytihng fails so we have the log file intact
    for (id in seq_along(my.ids)){
        tryCatch(
            checkNewEntry(as.numeric(my.ids[id]),out.file=out.file,append.log=append.log),
            warning=function(w){
            sink()
        },
            error=function(e){
                sink()
            },
            finally=function(x){
                sink()
            }
        )

        cat("------------------------------------------- \n")
    }

    sink()
}

## This function checks for the consistency of a new entry to the cholera taxonomy
##' @param cholera.id - taxonomy id
##' @param out.file  - file to write log file (leave null if you want it printed to the screen)
##' @param append.log - append current log or writeover?
##' @return NULL
##' @author asa
checkNewEntry <- function(cholera.id,out.file=NULL,append.log=TRUE){

    ## check on the OS so we can deal with paths to files correctly
    file.delim <- ifelse(Sys.info()[['sysname']]=="Windows","\\","//")
    ## for some reason switch doesn't like "\" so decided to repeat twice
    ## then remove the repeat here:
    file.delim <- substring(file.delim,1,1)

    if (!is.null(out.file)){
        sink(out.file,append=append.log,split=T)
    }
    cat(sprintf("Running Cholera Taxonomy Check on Cholera ID %.0f \n",cholera.id))

    ## set up array to pop each warning in so we can spit it out at the end
    my.warnings <- c()
    my.minor.warnings <- c() # minor warnigns for things that are not required but may help identify problems
    ## get description file from id and check that it is there
    desc.path <- paste0("CHOLERA",cholera.id,"_DESC.csv")

    if (desc.path %in% list.files("Description")){
        desc.path <- paste0("Description",file.delim,desc.path)
    } else {
        stop("Description file for this cholera taxonomy ID not found \n")
    }

    ## read in description File
    desc <- read.csv(desc.path,header=F,as.is=T)
    tmp.names <- desc[,1]
    desc <- desc[,2]
    names(desc) <- tmp.names

    ## check that all required fields are there but no checks on content yet
    cat("Checking for required fields in description file: ")

    ## fields that are always required
    ## TO DO: need to do this same thing for the other files!
    req.always.fields <- c("uid",
                           "source",
                           "contact",
                           "contact_email",
                           "is_public",
                           "contains_pii",
                           "who_region",
                           "ISO_A1",
                           "day_start",
                           "day_end",
                           "primary_time_criteria",
                           "humanitarian_crisis_assoc",
                           "reactive_vaccination",
                           "prev_vaccination")

    reqs.in.names <- req.always.fields %in% names(desc)
    if ( any(!reqs.in.names) ) {
        missing.fields <- req.always.fields[!reqs.in.names]
        stop(sprintf("Missing the following required fields; %s",
                     paste(missing.fields,collapse=", ")))
    }

    ## now check the conditionally required fields
    if (desc["contains_pii"] == 1 & (is.na(desc["IRB_protocol"]) | desc["IRB_protocol"] == "")){
        stop("IRB_protocol field missing or blank in description file. This must be completed if contains_pii = 1.")
    }

    if (desc["is_public"] == 0 & (is.na(desc["owner"]) | desc["owner"] == "")){
        stop("owner field missing. This field is required when is_public = 0 (restricted data).")
    }

    if (desc["is_public"] == 0 & (is.na(desc["owner_email"]) | desc["owner_email"] == "")){
        stop("owner_email field missing. This field is required when is_public = 0 (restricted data).")
    }

    ## check if there are either deaths or cases filled out
    if ((is.na(desc["deaths"]) | desc["deaths"] == "") & ((is.na(desc["cases"]) | desc["cases"] == ""))){
        stop("at least one of cases or deaths must be completed in the description file.")
    }

    cat("OK \n")

    cat("Checking to make other fields have correct names: ")

    all.fields <- c(
        "uid",
        "source",
        "source_uid",
        "source_url",
        "contact",
        "contact_email",
        "is_public",
        "mou_dsa_notes",
        "contains_pii",
        "IRB_protocol",
        "owner",
        "owner_email",
        "source_file",
        "who_region",
        "suspected_case_def",
        "confirmed_case_def",
        "day_start",
        "day_end",
        "primary_time_criteria",
        "deaths",
        "cases",
        "strains",
        "tet_res",
        "sul_res",
        "cip_res",
        "az_res",
        "humanitarian_crisis_assoc",
        "reactive_vaccination",
        "prev_vaccination",
        "notes")

    not.in.all.fields <- which(!names(desc) %in% all.fields)

    ## remove valid ISOs from this list
    not.in.all.fields <- not.in.all.fields[-grep("(^ISO_A2_L\\d+$)|(^ISO_A1$)",names(desc)[not.in.all.fields],perl=T)]

    ## remove valid extra fields from this list
    extra.fields.indices <- grep(".+\\_U$",names(desc)[not.in.all.fields])
    if (length(extra.fields.indices) > 0){
        not.in.all.fields <- not.in.all.fields[-extra.fields.indices]
    }

    ## if anything else left over then there is an issue
    if (length(not.in.all.fields) > 0){
        current.warning.message <- sprintf("The following field names are not valid: %s",paste(names(desc)[not.in.all.fields],collapse=","))
        my.warnings <- c(my.warnings,current.warning.message)
        cat(paste0(current.warning.message,"\n"))
    } else {
        cat ("OK \n")
    }


    ## now we will go through each field and check on the format and any other consistency checks
    ## warnings will be compiled thoughout this section and an error will be thrown at the end (for most)
    cat("Checking for field formatting in description file: ")

    ## check uid field
    if (desc["uid"] != cholera.id) {
        stop("uid does not match cholera taxonomy ID \n")
    }

    ## checking contact_email
    if (grep(".+@.+\\..+",desc["contact_email"],perl=T) != 1){
        current.warning.message <- "contact_email is not a valid email address"
        my.warnings <- c(my.warnings,current.warning.message)
        cat(paste0(current.warning.message,"\n"))
    }

    ## checking that is_public is either 0 or 1
    if (!desc["is_public"] %in% c(0,1)){
        current.warning.message <- "is_public should be either 1 or 0."
        my.warnings <- c(my.warnings,current.warning.message)
        cat(paste0(current.warning.message,"\n"))
    }

    ## checking that contains_pii is either 0 or 1
    if (!desc["contains_pii"] %in% c(0,1)){
        current.warning.message <- "contains_pii should be either 1 or 0."
        my.warnings <- c(my.warnings,current.warning.message)
        cat(paste0(current.warning.message,"\n"))
    }

    ## checking owner_email
    if ((desc["owner_email"] != "" & !is.na(desc["owner_email"])) && grep(".+@.+\\..+",desc["owner_email"],perl=T) != 1){
        current.warning.message <- "owner_email is not a valid email address"
        my.warnings <- c(my.warnings,current.warning.message)
        cat(paste0(current.warning.message,"\n"))
    }

    ## print warning about source being blank and then make sure it leads to a file
    if (is.na(desc["source_file"]) | desc["source_file"] == ""){
        cat("No source_file included. This is not required but is highly encouraged \n")
    } else {
        # TODO: here we assume that the path is included in the source_file if we change this we will need to modify this bit
        simplified.filename <- tail(strsplit(desc["source_file"],"/")$source_file,1)
        files.list <- list.files(paste0("SourceDocuments",file.delim,
                                        ifelse(desc["is_public"]==0,"Restricted","Public")))
        source.file.found <- simplified.filename %in% files.list
        if (!source.file.found){
            current.warning.message <- "Unable to find source_file. Please check that the file name is correct and that it is in the appropriate directory."
            my.warnings <- c(my.warnings,current.warning.message)
            cat(paste0(current.warning.message,"\n"))
        }
    }

    ## check who_region is valid
    who.regions <- c("AFR","AMR","SEAR","EUR","EMR","WPR")
    if (!desc["who_region"] %in% who.regions){
            current.warning.message <- paste0("who_region is incorrect. Should be one of ",paste(who.regions,collapse=","),".")
            my.warnings <- c(my.warnings,current.warning.message)
            cat(paste0(current.warning.message,"\n"))
    }

    ## require(XML)
    ## iso.url <- "http://en.wikipedia.org/wiki/ISO_3166-1_alpha-3"
    ## tables <- readHTMLTable(iso.url,header=F,as.is=T)
    ## country.codes <- tables[[1]][-1,-3]
    ## write.csv(country.codes,file="Documentation/iso_alpha3_codes.csv",row.names=F)
    country.codes <- read.csv(paste0("Documentation",file.delim,"iso_alpha3_codes.csv"),as.is=T)[,1]
    if (!desc["ISO_A1"] %in% country.codes){
        current.warning.message <- paste0("ISO_A1 is not a valid ISO-3611 alpha 3 code. Visit http://en.wikipedia.org/wiki/ISO_3166-1_alpha-3 for valid codes.")
        my.warnings <- c(my.warnings,current.warning.message)
        cat(paste0(current.warning.message,"\n"))
    }

    ## not going to check A2 codes since they can be messy

    ## check that day_start and day_end are in correct format and start is before end
    if (!is.valid.date(desc["day_start"])){
        current.warning.message <- paste0("day_start is not formatted correctly. this field must be yyyy-mm-dd")
        my.warnings <- c(my.warnings,current.warning.message)
        cat(paste0(current.warning.message,"\n"))
    }

    if (!is.valid.date(desc["day_end"])){
        current.warning.message <- paste0("day_end is not formatted correctly. this field must be yyyy-mm-dd")
        my.warnings <- c(my.warnings,current.warning.message)
        cat(paste0(current.warning.message,"\n"))
    }

    ## check that primary_time_criteria is valid
    valid.time.criteria <- c("ONSET", "CLINIC", "DEATH", "OTHER")
    if (!desc["primary_time_criteria"] %in% valid.time.criteria){
        current.warning.message <- paste0("primary_time_criteria not valid. check code book for valid options.")
        my.warnings <- c(my.warnings,current.warning.message)
        cat(paste0(current.warning.message,"\n"))
    }

    ## check that cases and or deaths are numeric only (with no spaces)
    if (!is.na(desc["cases"]) & desc["cases"] != ""){
        if (grep("^\\d+$",desc["cases"],perl=T) != 1){
            current.warning.message <- paste0("cases contains illegal characters.")
            my.warnings <- c(my.warnings,current.warning.message)
            cat(paste0(current.warning.message,"\n"))
        }
    }

    if (!is.na(desc["deaths"]) & desc["deaths"] != ""){
        if (grep("^\\d+$",desc["deaths"],perl=T) != 1){
            current.warning.message <- paste0("deaths contains illegal characters.")
            my.warnings <- c(my.warnings,current.warning.message)
            cat(paste0(current.warning.message,"\n"))
        }
    }

    ## check that strains are in list of valid strain types
    valid.strain.types <- c("O1-Unk","O1-ElTor-Unk","O1-ElTor-In","O1-ElTor-Og","O1-Class-Unk","O1-Class-In","O1-Class-Og","O1-ElTorVar-Unk","O1-ElTorVar-In","O1-ElTorVar-Og","O1-Unk-In","O1-Unk-Og","O139")
    if (grepl(",",desc["strains"],perl=T)==T){
      str.list<-strsplit(desc["strains"],",")
      str.list<-gsub("\\s","",str.list$strains)
      if(any(!str.list %in% valid.strain.types)){
        current.warning.message <- paste0("strains not recognized as a valid strain. see codebook for valid strain type codes.")
        my.warnings <- c(my.warnings,current.warning.message)
        cat(paste0(current.warning.message,"\n"))
      }
    }else{
      if (!is.na(desc["strains"]) & desc["strains"] != "" & !desc["strains"] %in% valid.strain.types){
        current.warning.message <- paste0("strains not recognized as a valid strain. see codebook for valid strain type codes.")
        my.warnings <- c(my.warnings,current.warning.message)
        cat(paste0(current.warning.message,"\n"))
    }}

    ## check that the *_res columns are either 0,1,or blank
    resistance.indices <- grep("\\_res",names(desc))
    if (length(resistance.indices) > 0){
        if (any(!desc[resistance.indices] %in% c(0,1))){
            current.warning.message <- paste0("at least one of the _res fields does not have a valid code. check the codebook for more details.")
            my.warnings <- c(my.warnings,current.warning.message)
            cat(paste0(current.warning.message,"\n"))
        }
    }

    ## humanitarian_crisis_assoc in c(0,1)
    if (!desc["humanitarian_crisis_assoc"] %in% c(0,1)){
        current.warning.message <- paste0("humanitarian_crisis_assoc not valid. This should be either 0 or 1.")
        my.warnings <- c(my.warnings,current.warning.message)
        cat(paste0(current.warning.message,"\n"))
    }

    ## reactive_vaccination in c(0,1)
    if (!desc["reactive_vaccination"] %in% c(0,1,9)){
        current.warning.message <- paste0("reactive_vaccination not valid. This should be either 0 or 1.")
        my.warnings <- c(my.warnings,current.warning.message)
        cat(paste0(current.warning.message,"\n"))
    }

    ## prev_vaccination in c(0,1)
    if (!desc["prev_vaccination"] %in% c(0,1,9)){
        current.warning.message <- paste0("prev_vaccination not valid. This should be either 0 or 1.")
        my.warnings <- c(my.warnings,current.warning.message)
        cat(paste0(current.warning.message,"\n"))
    }

    if (length(my.warnings)==0){
        cat("OK \n")
    } else {
        stop(sprintf("\n%.0f warnings issued for DESC file. Please fix these and run this check again. \n",length(my.warnings)))
        }


    ## now start looking for other files
    ## is there an EPICURVE associated with this and is it public or private?
    cat(sprintf("Looking for EpiCurve file: "))

    epi.path <- paste0("CHOLERA",cholera.id,"_EPI.csv")

    epi.files.path <- paste0("EpiCurves",file.delim,
                             ifelse(desc["is_public"]==0,"Restricted","Public"))

    if (!epi.path %in% list.files(epi.files.path)){
        current.warning.message <- paste0("EPI file not found. Please check that this file is named correctly and is in the approriate directory.")
        my.warnings <- c(my.warnings,current.warning.message)
        cat(paste0(current.warning.message,"\n"))
    } else {
        cat("OK \n")
    }

    cat(sprintf("Checking EpiCurve file: "))

    ## read in EpiCurve File
    epi <- read.csv(paste0(epi.files.path,file.delim,epi.path),header=T,as.is=T)

    ## Drop all columns that are completely blank or NA
    drop.inds <- apply(epi,2,function(x) all(is.na(x)))
    epi <- epi[,!drop.inds]

    ## basic check for all required fields
    if (any(!c("TL","TR","who_region","ISO_A1") %in% colnames(epi))){
        stop("Missing at least one of following fields TL, TR, who_region, or ISO_A1 from EPI file.")
    }

    ## check that any fields that start with TL or TR are in correct date format
    time.indices <- grep("^T[LR]",colnames(epi))

    ## returns true for each column where all dates are well formatted
    date.format.checks <- sapply(time.indices,function(x) {
        rc <- c()
        for (i in 1:nrow(epi)) rc[i] <- is.valid.date(epi[i,x])
       return(rc)
    })

    if (any(!date.format.checks)){
        current.warning.message <- sprintf("Column(s) %s in the EPI file contains at least one date that is not well formatted.",paste(colnames(epi)[which(apply(date.format.checks,2,function(x) any(x == FALSE)))],collapse=","))
        my.warnings <- c(my.warnings,current.warning.message)
        cat(paste0(current.warning.message,"\n"))
    }

    ## TO DO: may want to check that all TLs are less than or equal to TRs

    ## check that who_region is the same as that in the description file
    if (any(desc["who_region"] != epi["who_region"])){
        current.warning.message <- "At least one who_region in EPI file does not match who_region in DESC file."
        my.warnings <- c(my.warnings,current.warning.message)
        cat(paste0(current.warning.message,"\n"))
    }

    ## check that ISO_A1 is same as that in the descroption file
    if (any(desc["ISO_A1"] != epi["ISO_A1"])){
        current.warning.message <- "At least one ISO_A1 in EPI file does not match ISO_A1 in DESC file."
        my.warnings <- c(my.warnings,current.warning.message)
        cat(paste0(current.warning.message,"\n"))
    }

    ## check that ISO_A2 is same as description file if there is one
    iso.A2.indices <- grep("ISO_A2_L\\d+",names(desc),perl=T)
    if (length(iso.A2.indices) > 0){
      ## even if the name is there make sure the entry isn't blank
        blank.A2s <- which(desc[iso.A2.indices]=="" |is.na(desc[iso.A2.indices]))
        iso.A2.indices <-iso.A2.indices[-blank.A2s]
        if(length(iso.A2.indices)>0){
         ## for each enclosing A2 level in DESC, check that it is in the EPI file
          for (i in seq_along(iso.A2.indices)){
             isoA2.name <- names(desc)[iso.A2.indices[i]]
              if (any(desc[isoA2.name] != epi[isoA2.name])){
                  current.warning.message <- sprintf("%s doesn't match in EPI and DESC files. Any ISO-A2 level included in the DESC file should be the same in all EPI entries",isoA2.name)
                  my.warnings <- c(my.warnings,current.warning.message)
                  cat(paste0(current.warning.message,"\n"))
              }
          }
        }
    }

    ## check that at least one of these fields are in the epi file
    at.least.one.in.epi <- c("deaths","sCh","cCh","deaths_L","deaths_R","sCh_L","sCh_R","cCh_L","cCh_R")
    if (!any(at.least.one.in.epi %in% colnames(epi))){
        current.warning.message <- sprintf("At least one of the following fields is required in the EPI file. Please make sure at least one is included and that it is spelled correctly. (%s)",paste(at.least.one.in.epi,collapse=","))
        my.warnings <- c(my.warnings,current.warning.message)
        cat(paste0(current.warning.message,"\n"))
    }

    ## check that deaths , deaths_L, and deaths_R are numeric
    death.indices <- grep("^deaths",colnames(epi),perl=T)
    if (length(death.indices) > 0 && !is.numeric(unlist(epi[,death.indices]))){
        current.warning.message <- sprintf("Non-numeric characters detected in at least one of the following fields: %s",colnames(epi)[death.indices])
        my.warnings <- c(my.warnings,current.warning.message)
        cat(paste0(current.warning.message,"\n"))
    }

    ## check that sCh* and cCh* are numeric
    case.indices <- grep("^[sc]Ch",colnames(epi),perl=T)
    if (length(case.indices) > 0 && !is.numeric(unlist(epi[,case.indices]))){
        current.warning.message <- sprintf("Non-numeric characters detected in at least one of the following fields of the EPI file: %s",colnames(epi)[case.indices])
        my.warnings <- c(my.warnings,current.warning.message)
        cat(paste0(current.warning.message,"\n"))
    }

    ## check that age if included is numeric
    if ("age" %in% colnames(epi)){
        if (!is.numeric(unlist(epi[,"age"]))){
            current.warning.message <- "Non-numeric characters detected in at least one of the age field of the EPI file."
            my.warnings <- c(my.warnings,current.warning.message)
            cat(paste0(current.warning.message,"\n"))
        }
    }

    ## check that sex if included is in c(0,1,NA,"")
    if ("sex" %in% colnames(epi) && any(!epi[,"sex"] %in% c(0,1,"",NA))){
        current.warning.message <- "sex field in EPI file contains values other than 0,1,or blank"
        my.warnings <- c(my.warnings,current.warning.message)
        cat(paste0(current.warning.message,"\n"))
    }

    ## chack that vac if included is in c(0,1,NA,"")
    if ("vac" %in% colnames(epi) && any(!epi[,"vac"] %in% c(0,1,"",NA))){
        current.warning.message <- "vac field in EPI file contains values other than 0,1,or blank"
        my.warnings <- c(my.warnings,current.warning.message)
        cat(paste0(current.warning.message,"\n"))
    }

    ## check that addional fields in incuded here are in DESC file and vice-versa
    extra.fields.in.desc <- grep("\\_U$",names(desc),value=T)

    if (length(extra.fields.in.desc) > 0 && any(!extra.fields.in.desc %in% colnames(epi))){
        current.warning.message <- "not all extra fields metioned in the DESC file are included in the EPI file"
        my.warnings <- c(my.warnings,current.warning.message)
        cat(paste0(current.warning.message,"\n"))
    }

        possible.fields.in.epi <-c("TL",
                                   "TR",
                                   "TL_onset",
                                   "TR_onset",
                                   "TL_clinic",
                                   "TR_clinic",
                                   "TL_death",
                                   "TR_death",
                                   "who_region",
                                   "ISO_A1",
                                   "ISO_A2_L1",
                                   "ISO_A2_L2",
                                   "ISO_A2_L3",
                                   "ISO_A2_L4",
                                   "ISO_A2_L5",
                                   "ISO_A2_L6",
                                   "ISO_A2_L7",
                                   "ISO_A2_L8",
                                   "ISO_A2_L9",
                                   "ISO_A2_L10",
                                   "ISO_A2_L11",
                                   "lat_case",
                                   "long_case",
                                   "deaths",
                                   "sCh",
                                   "cCh",
                                   "deaths_L",
                                   "deaths_R",
                                   "sCh_L",
                                   "sCh_R",
                                   "cCh_L",
                                   "cCh_R",
                                   "age",
                                   "sex",
                                   "vac")


    if (any(!colnames(epi) %in% c(possible.fields.in.epi,extra.fields.in.desc))){
        current.warning.message <- "not all extra fields metioned in the EPI file are included in the DESC file"
        my.warnings <- c(my.warnings,current.warning.message)
        cat(paste0(current.warning.message,"\n"))
    }


    ## check that there is a location file for each lowest A2 level in the EPI file
    iso.cols <- sort(grep("^ISO\\_A[12]\\_*",colnames(epi),perl=T,value=T))
    expected.loc.files <- apply(epi,1,function(x) {
        if (any(x[iso.cols] == "" | is.na(x[iso.cols]))) return("")
        tmp.fn <- paste(x[c("who_region",iso.cols)],collapse="_")
        tmp.fn <- gsub("[^a-zA-Z0-9\\_\\-]| ", "",tmp.fn)
        ##gsub("\\s|'|`","",tmp.fn)
        paste0(tmp.fn,"_LOC.csv")
    })

    expected.loc.files <- unique(expected.loc.files[expected.loc.files != ""])

    missing.loc.files <- !expected.loc.files %in% list.files("Location")
    if (any(missing.loc.files)){
        current.warning.message <- sprintf("the following LOC files should be included in the Location directory but don't seem to be there: %s",paste(expected.loc.files[missing.loc.files],collapse=","))
        my.warnings <- c(my.warnings,current.warning.message)
        cat(paste0(current.warning.message,"\n"))
    }


    if (length(my.warnings) == 0){
        cat("OK \n")
    } else {
        stop(sprintf("\n%.0f warning(s) issued for EPI file. Please fix and run check again. \n",length(my.warnings)))
    }
    ## even though they are not required we are still going to check for them and let the user know if they are there or incrrectly formatted.
    cat("Checking Population File names (not required): ")

    expected.pop.files <- gsub("_LOC.csv","_POP.csv",expected.loc.files)
    missing.pop.files <- !expected.pop.files %in% list.files("Population")

    if (any(missing.pop.files)){
        current.warning.message <- sprintf("the following POP files are not included in the Population Directory (although they are not required): %s",paste(expected.pop.files[missing.pop.files],collapse=","))
        my.minor.warnings <- c(my.minor.warnings,current.warning.message)
        cat("Failed, see minor warnings at the end \n")
#        cat(paste0(current.warning.message,"\n"))
    } else {
        cat(" OK \n")
    }




    ## read each location file
    ## TODO: need to decide whether the is ISOcategories are required
    req.loc.fields <- c("name","cent_lat","cent_long")

    for (i in seq_along(expected.loc.files)){
        tmp.warning <- FALSE

        cat(sprintf("Checking %s:",expected.loc.files[i]))
        ## read in file
        loc <- read.csv(paste0("Location",file.delim,expected.loc.files[i]),header=F,as.is=T)
        tmp.names <- loc[,1]
        loc <- loc[,2]
        names(loc) <- tmp.names
        ## check that there are name, cent_lat and cent_long fields
        if (any(!req.loc.fields %in% names(loc))){
            tmp.warning <- TRUE
            current.warning.message <- sprintf("All required fields in %s were not found. Please check codebook for required fields.",expected.loc.files[i])
            my.warnings <- c(my.warnings,current.warning.message)
            cat(paste0(current.warning.message,"\n"))
        }

        isIso.indices <- grep("isISO",names(loc))
        ## TODO: check that the number of isISOs is what is expected
        if (length(isIso.indices) > 0 && any(!loc[isIso.indices] %in% c(1,2))){
            tmp.warning <- TRUE
            current.warning.message <- "isISO fields can only have 1's or 2's in them."
            my.warnings <- c(my.warnings,current.warning.message)
            cat(paste0(current.warning.message,"\n"))
        }

        if (!tmp.warning) {
            cat("OK \n")
        }

    }

    if (length(my.warnings) == 0){
        cat("Files pass test! \n")
        if (length(my.minor.warnings) > 0){
            cat("\n The following minor warnings occured. Minor warnings are checks on non-required fields so may not actually mean anything for your application. \n")
            print(my.minor.warnings)
        }
    } else {
        stop(sprintf("\n%.0f warning(s) issued for LOC files. Please fix and run check again. \n",length(my.warnings)))
    }

    if (!is.null(out.file)){
        sink()
    }

}


##' checks if a date is valid and in teh correct format
##' @param possible.date
##' @return TRUE if valid date
##' @author Andrew Azman
is.valid.date <- function(possible.date){

    days.in.month <- c(31,28,31,30,31,30,31,31,30,31,30,31)

    tmp <- regexpr("([1|2][0-9]{3})-([0-1][0-9])-([0-3][0-9])",possible.date,perl=T)

    if (attr(tmp,"match.length") != 10) return(FALSE)

    ## spilts into yr, month, date compoents
    year <- as.numeric(substr(possible.date,attr(tmp,"capture.start")[1],attr(tmp,"capture.start")[1]+attr(tmp,"capture.length")[1]-1))
    month <- as.numeric(substr(possible.date,attr(tmp,"capture.start")[2],attr(tmp,"capture.start")[2]+attr(tmp,"capture.length")[2]-1))
    day <- as.numeric(substr(possible.date,attr(tmp,"capture.start")[3],attr(tmp,"capture.start")[3]+attr(tmp,"capture.length")[3]-1))

    ## deal with leap years
    if (month == 2 && year %% 4 == 0 && (year %% 100 != 0 || year %% 400 == 0)){
        if (day < 1 | day > 29) return(FALSE)
    } else if (month >= 1 && month <= 12) {
        if (day < 1 | day > days.in.month[month]) return(FALSE)
    }

    return(TRUE)
}
