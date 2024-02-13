### demografi_funktioner.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Jan 22 2024 (10:49) 
## Version: 
## Last-Updated: Feb 13 2024 (11:00) 
##           By: Thomas Alexander Gerds
##     Update #: 69
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

for (a in c("danstat","tidyverse","stringi","ggplot2","ggthemes")){
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

register_info <- function(register){
    register = toupper(register)
    try <- tryCatch(defaults <- danstat::get_table_metadata(register, language = "en")$variables,
                    error = function(e) stop(paste0("Are you offline? If not, then perhaps the registry ", register, " does not exist? Check at statistikbanken.dk. ")))
    try_da <- tryCatch(defaults_da <- danstat::get_table_metadata(register, language = "da")$variables,
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
##' Wrapper for funktionen danstat::get_data
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
    try <- tryCatch(defaults <- danstat::get_table_metadata(register,language="en")$variables,
                    error = function(e) stop(paste0("Are you offline? If not, then perhaps the registry ", register, " does not exist? Check at statistikbanken.dk. ")))
    try_da <- tryCatch(defaults_da <- danstat::get_table_metadata(register, language = "da")$variables,
                       error = function(e) stop(paste0("Are you offline? If not, then perhaps the registry ", register, " does not exist? Check at statistikbanken.dk. ")))
    for (i in 1:length(defaults$values)){
      defaults$values[[i]]$tekst <- defaults_da$values[[i]]$text
    }
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
            if (length(not_value <- setdiff(user_args[[ua]],values[[ua]]$id))){
                match_text_with_id <- match(tolower(not_value), tolower(values[[ua]]$text))
                match_text_with_id_da <- match(tolower(not_value), tolower(values[[ua]]$tekst))
                match_text_with_id <- dplyr::coalesce(match_text_with_id, match_text_with_id_da)
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
                list(code = requested_args[[u]],values = user_args[[ua]])                
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
                       "Available text: ", v[["allowed_text"]],"\n",
                       "Available id: ", v[["allowed_id"]]),"\n")
        }
        stop()
    }else{
        # fjern variable som har ingen values
        null_value <- sapply(vars,function(x){length(x$values)})
        vars <- vars[null_value>0]
        d <- danstat::get_data(register,variables=vars)
        # formatere ALDER til numerisk 
        if ("ALDER" %in% names(d)){
            suppressWarnings(num_alder <- as.numeric(gsub(" year[s]?| years and over| år| år og derover","",d$ALDER)))
            if (mean(!is.na(num_alder))>0.5) d <- mutate(d,alder = num_alder)
            else d = mutate(d,alder = gsub(" year[s]?| years and over| år| år og derover","",d$ALDER))
        }
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

intervAlder <- function(data,alder="alder",breaks,vars,by="køn",right=TRUE,label_one = NULL,label_last = NULL){
    data = mutate(data,aldersinterval=cut(alder,breaks=breaks,right=right,include.lowest=TRUE))
    ll <- levels(data$aldersinterval)
    if (right==TRUE){
        ll <- paste0(breaks[-length(breaks)]+1,"-",breaks[-1])
        if (is.infinite(breaks[length(breaks)]))
            ll[length(ll)] <- paste0(">",breaks[length(breaks)-1])
        if (is.infinite(breaks[1]) || breaks[1]<0)
            ll[1] <- paste0("<=",breaks[2])
        else
            ll[1] <- paste0(breaks[1],"-",breaks[2])
    } else{
        ll <- paste0(breaks[-length(breaks)],"-",breaks[-1]-1)
        # label last element
        if (length(label_last) == 1){
            ll[length(ll)] = label_last
        } else{
            if (is.infinite(breaks[length(breaks)]))
                ll[length(ll)] <- paste0(breaks[length(breaks)-1],"+")
        }
        # label first element 
        if (length(label_one) == 1){
            ll[1] <- label_one
        }else{
            if (breaks[[1]]==0){
                ll[1] <- "0"
            } else{
                if (is.infinite(breaks[[1]]) || breaks[1]<0){
                    ll[1] <- paste0("<",breaks[2])
                } else{
                    ll[1] <- paste0(breaks[1],"-",breaks[2])
                }
            }
        }
    }
    levels(data$aldersinterval) <- ll
    by <- names(data)[match(by,names(data),nomatch=0)]
    if (length(by)>0)
        data = do.call("arrange",c(list(data,"aldersinterval"),by))
    else
        data = arrange(data,"aldersinterval")
    out <- NULL
    for (v in vars){
        data = rename(data,"tHiSvAr" = v)
        if (length(by)>0){
            suppressMessages(out.v <- group_by(data,.dots = c("aldersinterval",by)) %>% summarise(tHiSvAr = sum(tHiSvAr)))
        } else{
            suppressMessages(out.v <- group_by(data,"aldersinterval") %>% summarise(tHiSvAr = sum(tHiSvAr)))
        }
        names(out.v)[names(out.v) == "tHiSvAr"] = v
        if (!is.null(out))
            out <- left_join(out,out.v,by = by)
        else out <- out.v
    }
    out[]
}

format_dato <- function(data,variable = "TID"){
    # transform årstal + kvartal til dato
    patt <- c("Q1","Q2","Q3","Q4")
    repl <- c("-01-01","-01-04","-01-07","-01-10")
    data[[variable]] = as.Date(stri_replace_all_regex(data[[variable]],
                                              pattern=patt,
                                              replace=repl,
                                              vectorize_all=FALSE))
    data
}


######################################################################
### demografi_funktioner.R ends here
