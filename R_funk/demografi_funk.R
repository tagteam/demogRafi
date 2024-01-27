### demografi_funktioner.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Jan 22 2024 (10:49) 
## Version: 
## Last-Updated: Jan 27 2024 (14:50) 
##           By: Thomas Alexander Gerds
##     Update #: 38
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
library(danstat)
library(tidyverse)
library(stringi)
library(ggplot2)
library(ggthemes)
options(readr.show_col_types = FALSE)

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
    defaults <- danstat::get_table_metadata(register)$variables
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
        }else{
            if (length(not_value <- setdiff(user_args[[ua]],values[[ua]]$id))){
                if (not_value == "99" && ("99-" %in% values[[ua]]$id)){
                    list(code = requested_args[[u]],
                         values = sub("99","99-",user_args[[ua]]))
                } else{
                    # fix 99- problem of register dod
                    list(code = "error",
                         variable = requested_args[[u]],
                         problems = paste0(not_value,collapse = ","),
                         allowed = paste0(values[[ua]]$id,collapse = ","))
                }
            } else{
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
                           "Available: ", v[["allowed"]]),"\n")
        }
        stop()
    }else{
        d <- danstat::get_data(register,variables=vars)
        # formatere ALDER til numerisk 
        if ("ALDER" %in% names(d))
            d <- mutate(d,ALDER=as.numeric(gsub(" year[s]?| years and over","",ALDER)))
        d
    }
}

samle_alder <- function(data,variable,value,by){
    if (value == "99plus"){
        if (by == "køn"){
            kvinder99plus <- data %>% filter(ALDER>98 & KØN == "Women") %>%
                summarise(sum(risikotid)) %>% pull()
            maend99plus <- data %>% filter(ALDER>98 & KØN == "Men") %>%
                summarise(sum(risikotid)) %>% pull()
            # erstat værdi i rækkerne hvor alder er lige med 99
            data <- data %>% mutate(risikotid=replace(risikotid, ALDER==99 & KØN=="Women",kvinder99plus))
            data <- data %>% mutate(risikotid=replace(risikotid, ALDER==99 & KØN=="Men",maend99plus))
        }
        # slet rækker hvor alder er over 99
        data <- data %>% filter(ALDER<100)
    }
    data
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
