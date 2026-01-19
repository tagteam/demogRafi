### demografi_funktioner.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds and Johan Sebastian Ohlendorff
## Created: Jan 22 2024 (10:49) 
## Version: 
## Last-Updated: jan 19 2026 (14:17) 
##           By: Thomas Alexander Gerds
##     Update #: 204
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
# load libraries
# ---------------------------------------------------------------------
## source("https://raw.githubusercontent.com/tagteam/demogRafi/main/R-scripts/demofunk.R")
options("lifecycle_verbosity"="warning")

for (a in c("tidyverse","stringi","ggplot2","ggthemes","svglite")){
    if (inherits(try(do.call("library",list(a,quietly = TRUE))),"try-error")){
        stop(paste0("Package ",a," is not installed.\nInstall it with command\n install.packages('",a,"')\n
If that does not work, maybe another package is missing which is required by ",a,"?\n
Try to read and google the error message that occurs when you run\n",
"install.packages('",a,"')",
"\nand post the error message when you ask on Absalon for help."))
        do.call("library",list(a))
    }
    else{
        message("Successfully loaded package ", a)
    }
}

## Kopi af github Valeri Voevs pakke danstat fil zzz.R
SUBJECTS_ENDPOINT <- "https://api.statbank.dk/v1/subjects"
TABLES_ENDPOINT <- "https://api.statbank.dk/v1/tables"
METADATA_ENDPOINT <- "https://api.statbank.dk/v1/tableinfo"
DATA_ENDPOINT <- "https://api.statbank.dk/v1/data"

check_http_type <- function(response, expected_type = "text/json") {
	if (httr::http_type(response) != expected_type) {
		stop(paste("API did not return", expected_type), call. = FALSE)
	}
}


## Kopi af github Valeri Voevs pakke danstat funktion get_table_metadata
statistikbanken_metadata <- function(table_id, variables_only = FALSE){
	# evaluate language choices
	call_body <- list(lang = "da",table = table_id)
	result <- httr::POST(METADATA_ENDPOINT, body = call_body, encode = "json")
	check_http_type(result)
	full_result <- jsonlite::fromJSON(httr::content(result))
	if (variables_only) return(full_result$variables)
	return(full_result)
}

## Kopi af github Valeri Voevs pakke danstat funktion get_data
statistikbanken_data <- function(table_id, variables){
    # rertieve variables for this table
    valid_vars <- statistikbanken_metadata(table_id = table_id, variables_only = TRUE)
    # check variable names (ids)
    efterspurgt_vars <- sapply(variables, function(x) x[[1]])
    if (!all(tolower(efterspurgt_vars) %in% tolower(valid_vars$id))) {
        bad_input_index <- which( !(tolower(efterspurgt_vars) %in% tolower(valid_vars$id) ))
        stop(paste0("variable code '", efterspurgt_vars[bad_input_index], "' is not a variable in table '", table_id,
                    "'. Run statistikbanken_metadata('", table_id, "', variables_only = TRUE) for valid variable codes."),
             call. = FALSE)
    }
    variable_requests <- list()
    ind <- 1
    for (variable_pair in variables) {
        # check column names of variables
        if (any(!(names(variable_pair) == c("code", "values")))) stop("variables code-values pairs need to be a named lists with names 'code' and 'values'", call. = FALSE)
        variable_id <- variable_pair$code
        variable_values <- variable_pair$values
        if (any(is.na(variable_values))) variable_values <- "*"
        vars <- statistikbanken_metadata(table_id = table_id, variables_only = TRUE)
        valid_values <- c(vars[["values"]][[which(tolower(vars$id) == tolower(variable_id))]]$id,"*")
        if (all((variable_values %in% valid_values))) {
            variable_requests[[ind]] <- list(code = variable_id, values = I(variable_values))
        } else {
            warning(paste0("Values for ", variable_id, " are not valid... skipping ", variable_id), call. = FALSE)
            next
        }
        ind <- ind + 1
    }
    call_body <- list(table = table_id,lang = "da",format = "CSV",variables = variable_requests)
    result <- httr::POST(DATA_ENDPOINT, body = call_body, encode = "json")
    check_http_type(result, expected_type = "text/csv")
    return(readr::read_csv2(httr::content(result,type = "text",encoding = "UTF-8"),locale = readr::locale(decimal_mark = ",",tz = "CET")))
}


register_info <- function(register){
    register = toupper(register)
    try <- tryCatch(defaults <- statistikbanken_metadata(register)$variables,
                    error = function(e) stop(paste0("Are you offline? If not, then perhaps the registry ", register, " does not exist? Check at statistikbanken.dk. ")))
    try_da <- tryCatch(defaults_da <- statistikbanken_metadata(register)$variables,
                    error = function(e) stop(paste0("Are you offline? If not, then perhaps the registry ", register, " does not exist? Check at statistikbanken.dk. ")))
    ## combine the two lists
    varnames <- defaults$id
    values <- defaults$values
    names(values) <- varnames
    values_da <- defaults_da$values
    names(values_da) <- varnames
    for (va in names(values)){
        values[[va]]$tekst <- values_da[[va]]$text
    }
    values
}

# hent data fra statistikbanken.dk
# ---------------------------------------------------------------------
##' Hent data fra statistikbankens register 
##'
##' Mange tak til Valeri Voevs for pakken danstat 
##' @title Hent data fra statistikbankens register
##' @param register
##' @param ... Beskriv hvilke data skal hentes. Se eksemplerne. 
##' @return tibble med data fra register 
##' @examples
##' hent_data("befolk1",tid=2023)
##' @export 
##' @author Thomas A. Gerds <tag@@biostat.ku.dk>
hent_data <- function(register,...){
    register = toupper(register)
    try <- tryCatch(defaults <- statistikbanken_metadata(register)$variables,
                    error = function(e) stop(paste0("Are you offline? If not, then perhaps the registry ", register, " does not exist? Check at statistikbanken.dk. ")))
    varnames <- tolower(defaults$id)
    values <- defaults$values
    names(values) <- varnames
    user_args <- list(...)
    if (length(user_args) == 0){
        stop(paste0("Need to specify values for at least one of the variables:\n",
                    paste0(sapply(names(values),function(v){paste0(v,": ", paste0(values[[v]]$id,collapse = ", "))}),collapse = "\n")))
    }
    names(user_args) = tolower(names(user_args))
    requested_args <- names(user_args)
    # check names
    if (length(not_args <- setdiff(requested_args,varnames))>0)
        stop(paste0("The variables of register ",register, ",are:\n",
                    paste0(varnames,collapse = ","),
                    "\nThe requested variables:\n",
                    paste0(not_args,collapse = ","),
                    " do not match."))
    used_names <- intersect(names(user_args),varnames)
    # check values
    vars <- lapply(1:length(user_args),function(u){
        ua = requested_args[[u]]
        if (length(user_args[[u]]) == 1&&tolower(user_args[[u]]) == "all"){
            list(code = requested_args[[u]],
                 values = values[[ua]]$id)
        } else if (length(user_args[[u]]) == 1&&tolower(user_args[[u]]) == "all_no_total"){
            list(code = requested_args[[u]],
                 values = values[[ua]]$id[-1])
        } else{
            ## handle 99- problem and e.g., the problem with men
            if (length(not_value <- setdiff(tolower(user_args[[ua]]),tolower(values[[ua]]$id)))>0){
                match_text_with_id <- match(tolower(not_value), tolower(values[[ua]]$text))
                match_not_value_user_args <- match(tolower(not_value), tolower(user_args[[ua]]))
                if (all(!is.na(match_text_with_id))){
                    ## find which indeces of user_args[[ua]] are in a, ignoring case
                    match_not_value_user_args <- match(tolower(not_value), tolower(user_args[[ua]]))
                    user_args[[ua]][match_not_value_user_args] <- values[[ua]]$id[na.omit(match_text_with_id)]
                    list(code = requested_args[[u]],values = user_args[[ua]])
                } 
                else if (length(not_value) == 1 && not_value == "99" && ("99-" %in% values[[ua]]$id)){
                    user_args[[ua]] <- sub("99","99-",user_args[[ua]])
                    list(code = requested_args[[u]],values = user_args[[ua]])
                } else{
                    # fix 99- problem of register dod
                    list(code = "error",
                         variable = requested_args[[u]],
                         problems = paste0(not_value,collapse = ","),
                         allowed_text = paste0(values[[ua]]$text,collapse = ","),
                         allowed_id = paste0(values[[ua]]$id,collapse = ","))
                }
            } else {
                # repair case mistakes
                want = sapply(tolower(user_args[[ua]]),function(x)grep(paste0("^",x,"$"),tolower(values[[ua]]$id),value = TRUE))
                has = values[[ua]]$id[tolower(values[[ua]]$id)%in%want]
                list(code = requested_args[[u]],values = has)                
            } 
        }})
    has_problems = any(sapply(vars,"[[","code") == "error")
    if (has_problems){
        cat(paste0("\nSome of the requested values are not in the register:\n"))
        for (v in vars){
            if (v[[1]] == "error")
                cat(paste0("\nProblem with values of variable ",
                           v[["variable"]],"\n",
                           "Requested: ", v[["problems"]],"\n",
                           "Available text: ", paste("all","all_no_total",v[["allowed_text"]],sep = ","),"\n",
                           "Available id: ", v[["allowed_id"]]),"\n")
        }
        stop()
    }else{
        # fjern variable som har ingen values
        null_value <- sapply(vars,function(x){length(x$values)})
        vars <- vars[null_value>0]
        d <- statistikbanken_data(register,variables=vars)
        # formatere ALDER til numerisk 
        if ("ALDER" %in% names(d)){
            suppressWarnings(num_alder <- as.numeric(gsub(" year[s]?| years and over| år| år og derover","",d$ALDER)))
            if (mean(!is.na(num_alder))>0.5) d <- mutate(d,alder = num_alder)
            else d = mutate(d,alder = gsub(" year[s]?| years and over| år| år og derover","",d$ALDER))
            d = relocate(d,"alder")
        }
        if ("MODERSALDER" %in% names(d)){
            suppressWarnings(num_alder <- as.numeric(gsub(" year[s]?| years and over| år| år og derover","",d$MODERSALDER)))
            if (mean(!is.na(num_alder))>0.5) d <- mutate(d,alder = num_alder)
            else d = mutate(d,alder = gsub(" year[s]?| years and over| år| år og derover","",d$MODERSALDER))
            d = relocate(d,"alder")
        }
        d
        if ("TID"%in% names(d)) d = relocate(d,"TID")
        d
    }
}

mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
    condition <- eval(substitute(condition), .data, envir)
    .data[condition, ] <- .data[condition, ] %>% mutate(...)
    .data
}

samle_alder <- function(data,variable,value,by){
    if (value == "99plus"){
        if (by == "køn"){
            kvinder99plus <- data %>% filter(alder>98 & KØN == "Women") %>%
                summarise(sum(risikotid)) %>% pull()
            maend99plus <- data %>% filter(alder>98 & KØN == "Men") %>%
                summarise(sum(risikotid)) %>% pull()
            # erstat værdi i rækkerne hvor alder er lige med 99
            data <- data %>% mutate(risikotid=replace(risikotid, alder==99 & KØN=="Women",kvinder99plus))
            data <- data %>% mutate(risikotid=replace(risikotid, alder==99 & KØN=="Men",maend99plus))
        }
        # slet rækker hvor alder er over 99
        data <- data %>% filter(alder<100)
    }
    data
}

intervAlder <- function(data,
                        alder="alder",
                        breaks,
                        vars,
                        by = NULL,
                        right=TRUE,
                        label_one = NULL,
                        label_last = NULL){
    stopifnot(all(vars %in% names(data)))
    data[["alder"]] = data[[alder]]
    data <- mutate(data,
                  aldersinterval=cut(alder,
                                     breaks=breaks,
                                     right=right,
                                     include.lowest=TRUE))
    ll <- levels(data$aldersinterval)
    if (right==TRUE){
        ll <- paste0(breaks[-length(breaks)]+1,"-",breaks[-1])
        if (is.infinite(breaks[length(breaks)]))
            ll[length(ll)] <- paste0(">",breaks[length(breaks)-1])
        if (is.infinite(breaks[1]) || breaks[1]<0)
            ll[1] <- paste0("<=",breaks[2])
        else
            ll[1] <- paste0(breaks[1],"-",breaks[2])
    }else{
        # right == FALSE
        ll <- paste0(breaks[-length(breaks)],"-",breaks[-1]-1)
        if (is.infinite(breaks[length(breaks)]))
            ll[length(ll)] <- paste0(breaks[length(breaks)-1],"+")
        if (breaks[[1]]==0){
            ll[1] <- "0"
        } else{
            if (is.infinite(breaks[[1]]) || breaks[1]<0){
                ll[1] <- paste0("<",breaks[2])
            } else{
                # Fixme: this is wrong when alder is discrete
                #        and right = FALSE because then breaks[2] is
                #        not included
                ll[1] <- paste0(breaks[1],"-",breaks[2])
            }
        }
    }
    # user label first element 
    if (length(label_one) == 1){
        ll[1] <- label_one
    }
    # user label last element
    if (length(label_last) == 1){
        ll[length(ll)] = label_last
    }
    levels(data$aldersinterval) <- ll
    by <- intersect(names(data),by)
    if (length(by)>0)
        data <- do.call("arrange",c(list(data,"aldersinterval"),by))
    else
        data <- arrange(data,"aldersinterval")
    out <- NULL
    for (v in vars){
        if (length(by)>0){
            out.v <- data %>% group_by_at(c("aldersinterval",by)) %>% summarise(tHiSvAr = sum(!!sym(v)), .groups = "drop")
        } else{
            out.v <- data %>% group_by(aldersinterval) %>% summarise(tHiSvAr = sum(!!sym(v)))
        }
        names(out.v)[names(out.v) == "tHiSvAr"] = v
        if (!is.null(out)){
            out <- left_join(out,out.v,by = c(by,"aldersinterval"))
        } else {
            out <- out.v
        }
    }
    ungroup(out)
}

hent_aldersfordeling <- function(breaks,
                                 køn = c("Kvinder","Mænd"),
                                 tid = "2023",
                                 område = "Hele landet",
                                 ...){
    # risikotid metode 1
    af <- hent_data("FOLK1a",
                    "alder"=0:125,
                    køn = køn,
                    tid=paste0(tid,"K3"),
                    Område = område)
    af = mutate(af,TID = as.numeric(sub("K3","",TID)))
    # Fordeling af risikotid i aldersintervaller
    af <- rename(af,R = INDHOLD)
    by <- c("KØN","OMRÅDE","TID")
    af <- intervAlder(af,breaks=breaks, by=by,vars="R",...)
    af <- af %>% group_by_at(by) %>% mutate(V = R/sum(R))
    ungroup(af)
}

hent_mortalitetsrate_data <- function(breaks,
                                      køn = c("Kvinder","Mænd"),
                                      tid = "2023",
                                      område = "Hele landet",
                                      alder = NULL,
                                      ...){
    if (length(alder) == 0) {
        alder = 0:125
        alder_fod <- "all_no_total"
    }else{
        alder_fod <- alder
    }
    # risikotid metode 1
    af <- hent_data("FOLK1a",
                    "alder"=alder,
                    køn = køn,
                    tid=paste0(tid,"K3"),
                    Område = område)
    af = mutate(af,TID = as.numeric(sub("K3","",TID)))
    # Fordeling af risikotid i aldersintervaller
    af <- rename(af,R = INDHOLD)
    by = c("KØN","OMRÅDE","TID")
    af <- intervAlder(af,breaks=breaks, by=by,vars="R",...)
    # Antal døde i aldersintervaller
    if (tolower(køn)[1] == "i alt"){
        dd <- hent_data("FOD207",
                        "alder"=alder_fod,
                        tid=tid,
                        Område = område)
        dd = mutate(dd,KØN = "I alt")
    } else{
        dd <- hent_data("FOD207",
                        "alder"=alder_fod,
                        tid=tid,
                        køn=køn,
                        Område = område)
    }
    dd <- rename(dd,Dod = INDHOLD)
    dd <- intervAlder(dd,
                      breaks=breaks,
                      by=c("KØN","OMRÅDE","TID"),
                      vars="Dod",...)
    # join
    dat <- left_join(af,dd, by = c("aldersinterval",by))
    if (tolower(køn)[1] == "i alt") dat$KØN = NULL
    if (tolower(område)[1] == "hele landet") dat$OMRÅDE = NULL
    dat
}

hent_dodsaarsag_data <- function(køn = c("Kvinder","Mænd"),
                                 tid,
                                 årsag,...){
    breaks = c(0,1,seq(5,85,5),Inf)
    if (missing(årsag)){
        stop("Skal angive dødsårsag")
    }
    # risikotid metode 1
    af <- hent_data("FOLK1a",
                    "alder"=0:125,
                    køn = køn,
                    tid=paste0(tid,"K3"))
    af = mutate(af,TID = as.numeric(sub("K3","",TID)))
    # Fordeling af risikotid i aldersintervaller
    af <- rename(af,R = INDHOLD)
    by = c("KØN","TID")
    af <- intervAlder(af,breaks=breaks, by=by,vars="R",label_last = "85",right = FALSE)
    # Antal døde i aldersintervaller
    if (tolower(køn)[1] == "i alt"){
        dd <- hent_data("dod",
                        "alder"="all_no_total",
                        tid=tid)
        dd = mutate(dd,KØN = "I alt")
    } else{
        dd <- hent_data("dod",
                        køn = køn,
                        "alder"="all_no_total",
                        tid=tid)
    }
    dd <- rename(dd,Dod = INDHOLD)
    dd <- intervAlder(dd,
                      breaks=breaks,
                      by=c("KØN","TID"),
                      vars="Dod",right = FALSE,label_last = "85")
    # Antal døde i aldersintervaller
    if (tolower(køn)[1] == "i alt"){
        daars <- hent_data("doda1",
                           årsag = årsag,
                           "alder"="all_no_total",
                           tid=tid)
        daars = mutate(daars,KØN = "I alt")
    } else{
        daars <- hent_data("doda1",
                           årsag = årsag,
                           køn = køn,
                           "alder"="all_no_total",
                           tid=tid)
    }
    daars <- rename(daars,QDod = INDHOLD,
                    aldersinterval = alder)
    # join
    dat <- left_join(af,dd, by = c("aldersinterval",by))
    daars = mutate(daars,aldersinterval = factor(aldersinterval,levels = levels(dat$aldersinterval)))
    dat <- left_join(dat,daars, by = c("aldersinterval",by))
    if (tolower(køn)[1] == "i alt") dat$KØN = NULL
    dat$ALDER = NULL
    dat = relocate(dat,"ÅRSAG")
    dat
}

beregn_middellevetid <- function(tid,område,køn,breaks = c(0:99,Inf)){
    M <- hent_mortalitetsrate_data(tid = tid,
                                   breaks = breaks,
                                   køn = køn,
                                   område = område,
                                   right = FALSE,
                                   alder = "all_no_total")
    if (any(M$R == 0)){
      cat(paste0("Advarsel: Risikotiden er 0 for følgende rækker:\n"))
      print(M %>% 
              filter(R == 0) %>% 
              select(KØN, TID, OMRÅDE, aldersinterval) %>%
              unique())
      cat("Middellevetiden kan derfor ikke udregnes for disse områder/køn/tid\n")
    }
    last_alder_level <- levels(M$aldersinterval)[length(levels(M$aldersinterval))]
    temp <- M %>% filter(aldersinterval == last_alder_level, R != 0, Dod == 0)
    if (temp %>% nrow()){
      cat("Advarsel: Mortalitetsraterne er 0 i det sidste aldersinterval for disse områder/køn/tid \n")
      print(temp %>% select(KØN,TID,OMRÅDE,aldersinterval))
      cat("Middellevetiden kan derfor ikke udregnes for disse områder/køn/tid\n")
    }
    M %>% 
      mutate(M = Dod/R) %>% 
      group_by(KØN, TID, OMRÅDE) %>% 
      mutate(a = c(0.1, rep(0.5,length(breaks)-2)), k = rep(1,length(breaks)-1)) %>%
      group_modify(~overlevelsestavle(.)) %>%
      filter(Alder == "0") %>% 
      select(e)
}

hent_fertilitetsrate_data <- function(tid = "2023",
                                      område = "Hele landet",
                                      barnkon = NULL,
                                      ...){
    breaks = c(-Inf,seq(15,50,5),Inf)
    if (min(as.numeric(tid))<2008){
        # FIXME:
        # risikotid metode 1 men bruger 1 januar burde tilbyde metode 2
        if (område != "Hele landet") stop("Har ikke adgang til folketal i andre områder end 'Hele landet' for årstal før 2007.")
        af <- hent_data("BEFOLK1","alder"="all_no_total",køn = c("kvinder"),tid=tid)
        by = c("TID")
    } else{
        # risikotid metode 1
        af <- hent_data("FOLK1a","alder"=0:125,køn = c("kvinder"),tid=paste0(tid,"K3"),Område = område)
        by = c("OMRÅDE","TID")
    }
    if (length(barnkon)>1) by = c(by,"BARNKON")
    af = mutate(af,TID = as.numeric(sub("K3","",TID)))
    # Fordeling af risikotid i aldersintervaller
    af <- rename(af,R = INDHOLD)
    af <- intervAlder(af,breaks=breaks, right = FALSE,by=by,vars="R",...)
    # Antal fødsler i aldersintervaller
    if (min(as.numeric(tid))<2007){
        fodie <- hent_data("FOD","modersalder"="all_no_total",tid=tid,barnkon = barnkon)
    }else{
        fodie <- hent_data("FODIE","modersalder"="all_no_total",tid=tid,Område = område,barnkon = barnkon)
    }
    fodie <- rename(fodie,Fødsler = INDHOLD)
    fodie <- intervAlder(fodie,breaks=breaks,right = FALSE,by=by,vars="Fødsler",...)
    # join
    if (length(barnkon)>1) {
        afb <- rbind(mutate(af,BARNKON = "Piger"),mutate(af,BARNKON = "Drenge"))
        dat <- left_join(fodie,afb, by = c("aldersinterval",by))
    }else{
        dat <- left_join(af,fodie, by = c("aldersinterval",by))
    }
    # fjern data hvor moren var yngre end 15 eller ældre end 50
    dat <- dat %>% filter(!(aldersinterval %in% c("<15","50+")))
    if ("OMRÅDE"%in%names(dat) && tolower(område)[1] == "hele landet") dat$OMRÅDE = NULL
    dat
}

## std_mortalitetsrate <- function(område,tid,breaks,by){
## }

format_dato <- function(data,variable = "TID"){
    # transform årstal + kvartal til dato
    patt <- c("Q1","Q2","Q3","Q4","K1","K2","K3","K4")
    repl <- c("-01-01","-01-04","-01-07","-01-10")
    data[[variable]] = as.Date(stri_replace_all_regex(data[[variable]],
                                              pattern=patt,
                                              replace=repl,
                                              vectorize_all=FALSE))
    data
}

hent_IDB_screenshot_data <- function(){
    af <- read_csv("https://raw.githubusercontent.com/tagteam/demogRafi/main/data/IDB_02-28-2024.csv")
    af <- filter(af,GROUP != "TOTAL")
    af <- rename(af,Land = "Country/Area Name")
    af <- rename(af,År = "Year")
    af <- rename(af,Risikotid = "Population")
    af <- rename(af,V = "% of Population")
    af <- mutate(af,aldersinterval = gsub(" ","",GROUP))
    af <- select(af,År,Land,aldersinterval,Risikotid,V)
    af
}

hent_IDB_data_USA_NIGERIA_INDONESIA_2022 <- function(){
    idb <- read_csv("https://raw.githubusercontent.com/tagteam/demogRafi/main/data/IDB_USA_DK_INDONESIA_NIGERIA_2022.csv")
    idb <- filter(idb,GROUP != "TOTAL")
    idb <- rename(idb,Land = "Country/Area Name")
    idb <- rename(idb,År = "Year")
    idb <- rename(idb,Risikotid = "Population")
    idb <- mutate(idb,aldersinterval = gsub(" ","",GROUP))
    idb <- idb%>%mutate(aldersinterval = replace(aldersinterval,aldersinterval == "100+","99"))
    idb <- idb%>%group_by(Land, aldersinterval) %>% mutate(Risikotid = sum(Risikotid))
    idb <- filter(idb,!duplicated(interaction(Land,aldersinterval)))
    idb <- rename(idb,V = "% of Population")
    idb <- idb%>%group_by(Land) %>% mutate(totalR = sum(Risikotid))
    idb <- idb%>%group_by(Land) %>% mutate(v = 100*Risikotid/totalR)
    idb <- select(idb,År,Land,aldersinterval,Risikotid,v)
    idb
}


overlevelsestavle <- function(data,
                              mortalitet = "M",
                              alder = "aldersinterval",
                              radix=100000){
    if (!(mortalitet %in% names(data))){
        stop("Du skal angive kolonnenavn for mortalitet")
    }
    else{
        M = data[[mortalitet]]
    }
    if (!(alder %in% names(data))){
        stop("Du skal angive kolonnenavn for alder")
    }
    else{
        alder = data[[alder]]
    }
    xmax <- length(M)
    if (M[xmax] == 0) stop("Mortalitet M i sidste interval er 0. Du skal sætte startalder af det sidste aldersinterval sådan at mortalitet er større end 0.")
    if (any(is.na(M))) stop("Mortalitet M har manglende værdier.")
    if (!("a" %in% names(data))){
        a <- rep(0.5,xmax)
        a[1] <- 0.1
    }else{
        a = data[["a"]]
    }
    a[xmax] <- 1/M[xmax] 
    if  (!("k" %in% names(data)))
        k <- rep(1,xmax)
    else
        k = data[["k"]]
    # init
    q=k*M/(1+(k-a)*M)
    q[xmax] <- 1
    l0 <- radix
    l <- d <- L <- numeric(xmax)
    l[1] <- l0
    for (x in 1:xmax){
        if (x<xmax){
            l[x+1] <- l[x]*(1-q[x])
            d[x] <- l[x]-l[x+1]
        }else{
            d[x] <- l[x]
        }
        L[x] <- k[x]*l[x]-d[x]*(k[x]-a[x])
    }
    T <- rev(cumsum(rev(L)))
    e <- T/l
    dt <- tibble(Alder=alder,
                 l=round(l),
                 d=round(d),
                 p=1-q,
                 q,
                 o = l/l[1],
                 L=round(L),
                 T,
                 e)
    dt
}

tavle_sandsynlighed_barn1 <- function(data,
                                frate = "F1",
                                alder = "aldersinterval",
                                radix=100000){
    M = data[[frate]]
    if (!(alder %in% names(data))){
        stop("Du skal angive kolonnenavn for alder")
    }
    else{
        alder = data[[alder]]
    }
    xmax <- length(M)
    if (any(is.na(M))) stop("Mortalitet M har manglende værdier.")
    if (!("a" %in% names(data))){
        a <- rep(0.5,xmax)
        a[1] <- 0.1
    }else{
        a = data[["a"]]
    }
    ## a[xmax] <- 1/M[xmax] 
    if  (!("k" %in% names(data)))
        k <- rep(1,xmax)
    else
        k = data[["k"]]
    # init
    q=k*M/(1+(k-a)*M)
    q[xmax] <- 1
    l0 <- radix
    l <- d <- L <- numeric(xmax)
    l[1] <- l0
    for (x in 1:xmax){
        if (x<xmax){
            l[x+1] <- l[x]*(1-q[x])
            d[x] <- l[x]-l[x+1]
        }else{
            d[x] <- l[x]
        }
        L[x] <- k[x]*l[x]-d[x]*(k[x]-a[x])
    }
    ## T <- rev(cumsum(rev(L)))
    ## e <- T/l
    dt <- tibble(Alder=alder,
                 l=round(l),
                 d=round(d),
                 p=1-q,
                 q,
                 s1 = 1-(l/l[1]),
                 L)
    dt
}

dodsaarsagtavle <- function(data,
                            mortalitet = "M",
                            alder = "aldersinterval",
                            hQ = "hQ",
                            radix=100000){
    if (!(mortalitet %in% names(data))){
        stop("Du skal angive kolonnenavn for mortalitet")
    }
    else{
        M = data[[mortalitet]]
    }
    if (!(hQ %in% names(data))){
        stop("Du skal angive kolonnenavn for hQ = andel af døde med årsag Q")
    }
    else{
        hQ = data[[hQ]]
    }
    if (!(alder %in% names(data))){
        stop("Du skal angive kolonnenavn for alder")
    }
    else{
        alder = data[[alder]]
    }
    xmax <- length(M)
    if (M[xmax] == 0) stop("Mortalitet M i sidste interval er 0. Du skal sætte startalder af det sidste aldersinterval sådan at mortalitet er større end 0.")
    if (any(is.na(M))) stop("Mortalitet M har manglende værdier.")
    if (!("a" %in% names(data))){
        a <- rep(0.5,xmax)
        a[1] <- 0.1
    }else{
        a = data[["a"]]
    }
    a[xmax] <- 1/M[xmax]
    if  (!("k" %in% names(data)))
        k <- rep(1,xmax)
    else
        k = data[["k"]]
    # init
    q=k*M/(1+(k-a)*M)
    qQ=q*hQ
    qbarQ=q*(1-hQ)
    l0 <- radix
    l <- d  <- dQ <- dbarQ <- L <- numeric(xmax)
    l[1] <- l0
    for (x in 1:xmax){
        if (x<xmax){
            l[x+1] <- l[x]*(1-q[x])
            d[x] <- l[x]-l[x+1]
            dQ[x] <- d[x]*hQ[x]
            dbarQ[x] <- d[x]*(1-hQ[x])
        }else{
            d[x] <- l[x]
            dQ[x] <- d[x]*hQ[x]
            dbarQ[x] <- d[x]*(1-hQ[x])
        }
        L[x] <- k[x]*l[x]-d[x]*(k[x]-a[x])
    }
    LTR_Q <- rev(cumsum(rev(dQ)))/l
    risiko_Q <- cumsum(dQ)/l[1]
    ## OverLev <- (l[1]-cumsum(d))/l[1]
    OverLev <- l/l[1]
    T <- rev(cumsum(rev(L)))
    e <- T/l
    dt <- tibble(Alder=alder,
                 l=round(l),
                 d=round(d),
                 dQ=round(dQ),
                 hQ=hQ,
                 p=1-q,
                 q = q,
                 qQ = qQ,
                 L=round(L),
                 T = T,
                 e = e,
                 o = OverLev,
                 LTR_Q = LTR_Q,
                 risiko_Q = risiko_Q)
    dt
}

fertilitets_tavle <- function(tid,område = "Hele landet"){
    if (length(område)>1) stop("Kan kun 1 område af gangen.")
    if (område != "Hele landet" && min(as.numeric(tid)) <2007)
        stop("Område skal være 'Hele landet' for tid før 2007")
    ## skal brug 50 som "sidste" aldersinterval. det slettes bagefter
    x <- hent_mortalitetsrate_data(tid = tid,
                                   område = område,
                                   køn="Kvinder",
                                   breaks=c(0:50,Inf),
                                   right = FALSE,
                                   alder = 0:50,
                                   label_last = "50")
    if (length(tid)>1){
        x = x%>%
            group_by(KØN,OMRÅDE,aldersinterval)%>%
            summarize(TID = paste0(min(tid)," - ",max(tid)),
                      R = sum(R),
                      Dod = sum(Dod),.groups = "drop")
    }
    x = x%>%mutate(M = Dod/R)
    otavle <- overlevelsestavle(x,
                                mortalitet = "M",
                                alder = "aldersinterval")
    ftavle <- filter(otavle,Alder%in%Alder[-c(1:15,length(Alder))]) %>% select(Alder,L)
    ftavle = mutate(ftavle,alder = as.numeric(substr(Alder,0,2)))
    ftavle5 <- intervAlder(ftavle,breaks=seq(15,50,5),right=FALSE,var="L",label_one = "15-19",alder="alder")
    FF <- hent_fertilitetsrate_data(tid = tid,område = område)
    if (length(tid)>1){
        FF = FF%>%
            group_by(OMRÅDE,aldersinterval)%>%
            summarize(TID = paste0(min(tid)," - ",max(tid)),
                      R = sum(R),
                      Fødsler = sum(Fødsler),.groups = "drop")
    }
    FF = mutate(FF,frate = Fødsler/R)
    ## F_piger <- hent_fertilitetsrate_data(tid = tid,barnkon = "Piger")
    ## F_piger = mutate(F_piger,frate_piger = Fødsler/R)
    ftavle5 <- left_join(ftavle5,FF,by="aldersinterval")
    ## ftavle5 <- left_join(ftavle5,select(F_piger,aldersinterval,frate_piger),by="aldersinterval")
    if (område != "Hele landet"){
        PP <- hent_data("fodie",område = område,modersalder = 15:49,tid = tid,barnkon = c("D","P"))
    } else{
        PP <- hent_data("fod",modersalder = 15:49,tid = tid,barnkon = c("D","P"))
    }
    PP <- intervAlder(PP,breaks=seq(15,50,5),right=FALSE,var="INDHOLD",alder="alder",by="BARNKON",label_one = "15-19")
    PP <- pivot_wider(PP,names_from = BARNKON,values_from = INDHOLD)
    PP <- mutate(PP,Andel_piger=Piger/(Drenge+Piger))
    ftavle5 <- left_join(ftavle5,select(PP,aldersinterval,Andel_piger),by="aldersinterval")
    ftavle5 <- mutate(ftavle5,frate_piger=frate*Andel_piger)
    ## ftavle5 <- mutate(ftavle5,bidrag_NRT=frate_piger*L/100000)
    ftavle5 <- relocate(ftavle5,TID)
    ftavle5[]
}

######################################################################
### demografi_funktioner.R ends here
