#!/usr/bin/Rscript
##
# Produce a summary views of the random walk data
#
# $Id$
##

# TODO: Compute weighted drift delta (UI - API)

cat("----INITIALIZING------------------------------------------------\n")

args <- commandArgs(trailingOnly=F)
scriptPath <- dirname(sub("--file=","",args[grep("--file",args)]))

library(grid)
source(file=file.path(scriptPath, "load.R"))

program <- basename(commandArgs(FALSE)[1])
options(digits=3)
options(width=1000)

# Useful constants
days.per.year <- 365.25
days.per.month <- days.per.year / 12

# Common plot/chart parameters
pdf.parms.w90 <- list(width.in=3.5, height.in=(3.5*1.1), points=8, dpi=100)
png.parms.w90 <- list(width.in=6.7, height.in=(6.7*1.1), points=16, dpi=100)
pdf.parms.n90 <- list(width.in=1.75, height.in=(3.5*1.1), points=8, dpi=100)
png.parms.n90 <- list(width.in=4.0, height.in=(6.7*1.1), points=16, dpi=100)
pdf.parms.h90 <- list(width.in=3.5, height.in=(2.1*1.1), points=8, dpi=100)
png.parms.h90 <- list(width.in=6.7, height.in=(4.0*1.1), points=16, dpi=100)
pdf.parms.p90 <- list(width.in=3, height.in=(3/.9), points=8, dpi=100)
png.parms.p90 <- list(width.in=3, height.in=(3/.9), points=8, dpi=100)
colors <- list(walk="goldenrod3",
               #walk="dodgerblue4",
               api="chartreuse4",
               api.faded=rgb(t(col2rgb("chartreuse4")), alpha=192, maxColorValue=255),
               ui="dodgerblue4",
               ui.faded=rgb(t(col2rgb("dodgerblue4")), alpha=192, maxColorValue=255),
               timemap="chartreuse4",
               memento="dodgerblue4",
               heat0="goldenrod3", heat1="firebrick3", heat2="black",
               invisible=rgb(0,0,0,0))
#print(colors)

#=======================================================================
create.subsets <- function(data) {
#=======================================================================

    cat("----CREATING SUBSETS--------------------------------------------\n")

    # A data frame to capture the subset stats
    stats <- data.frame(subset=character(),
                        dmoz=numeric(), seng=numeric(),
                        deli=numeric(), btly=numeric(),
                        all =numeric() )
    add.stats <- function(subset, all, dmoz=NULL, seng=NULL, deli=NULL, btly=NULL) {
        rbind(stats, data.frame(
                subset=subset,
                dmoz=if (is.null(dmoz)) NA else
                     if (is.list(dmoz)) NROW(dmoz)
                        else dmoz,
                seng=if (is.null(seng)) NA else
                     if (is.list(seng)) NROW(seng)
                        else seng,
                deli=if (is.null(deli)) NA else
                     if (is.list(deli)) NROW(deli)
                        else deli,
                btly=if (is.null(btly)) NA else
                     if (is.list(btly)) NROW(btly)
                        else btly,
                all =if (is.null(all)) NA else
                     if (is.list(all)) NROW(all)
                        else all ))
    }

    fmt1 <- "  %-18s                                          %9s\n"
    fmt5 <- "  %-18s  %8s  %8s  %8s  %8s  %9s\n"
    cat(sprintf("  ------subset------  --dmoz--  --seng--  --deli--  --btly--  --total--\n"))
    cat(sprintf(fmt1, "data$all", prettyNum(NROW(data$all), big.mark=",")))
    #stats <- add.stats("all", data$all)
    data$all.dmoz <- data$all[data$all$sample == "dmoz",]
    data$all.seng <- data$all[data$all$sample == "seng",]
    data$all.deli <- data$all[data$all$sample == "deli",]
    data$all.btly <- data$all[data$all$sample == "btly",]
    cat(sprintf(fmt5, "data$all.*",
                      prettyNum(NROW(data$all.dmoz), big.mark=","),
                      prettyNum(NROW(data$all.seng), big.mark=","),
                      prettyNum(NROW(data$all.deli), big.mark=","),
                      prettyNum(NROW(data$all.btly), big.mark=","),
                      prettyNum(NROW(data$all), big.mark=",") ))
    stats <- add.stats("all.*", data$all,
            data$all.dmoz, data$all.seng,
            data$all.deli, data$all.btly)

    # Subset containing just the last step. Used to lookup walk attributes
    data$all.L <- data$all[data$all$step == data$all$step_count,]
    data$dmoz.L <- data$all.L[data$all.L$sample == "dmoz",]
    data$seng.L <- data$all.L[data$all.L$sample == "seng",]
    data$deli.L <- data$all.L[data$all.L$sample == "deli",]
    data$btly.L <- data$all.L[data$all.L$sample == "btly",]
    cat(sprintf(fmt5, "data$*.L",
                      prettyNum(NROW(data$dmoz.L), big.mark=","),
                      prettyNum(NROW(data$seng.L), big.mark=","),
                      prettyNum(NROW(data$deli.L), big.mark=","),
                      prettyNum(NROW(data$btly.L), big.mark=","),
                      prettyNum(NROW(data$all.L), big.mark=",") ))
    stats <- add.stats("*.L", data$all.L,
            data$dmoz.L, data$seng.L,
            data$deli.L, data$btly.L)

    # All successful steps
    data$okay.all  <- data$all[data$all$status == "OKAY",]
    data$okay.dmoz <- data$okay.all[data$okay.all$sample == "dmoz",]
    data$okay.seng <- data$okay.all[data$okay.all$sample == "seng",]
    data$okay.deli <- data$okay.all[data$okay.all$sample == "deli",]
    data$okay.btly <- data$okay.all[data$okay.all$sample == "btly",]
    cat(sprintf(fmt5, "data$okay.*",
                      prettyNum(NROW(data$okay.dmoz), big.mark=","),
                      prettyNum(NROW(data$okay.seng), big.mark=","),
                      prettyNum(NROW(data$okay.deli), big.mark=","),
                      prettyNum(NROW(data$okay.btly), big.mark=","),
                      prettyNum(NROW(data$okay.all), big.mark=",") ))
    stats <- add.stats("okay.*", data$okay.all,
            data$okay.dmoz, data$okay.seng,
            data$okay.deli, data$okay.btly)

    # Just the first step of successful walks
    data$okay.1.all  <- data$okay.all[data$okay.all$step == 1,]
    data$okay.1.dmoz <- data$okay.1.all[data$okay.1.all$sample == "dmoz",]
    data$okay.1.seng <- data$okay.1.all[data$okay.1.all$sample == "seng",]
    data$okay.1.deli <- data$okay.1.all[data$okay.1.all$sample == "deli",]
    data$okay.1.btly <- data$okay.1.all[data$okay.1.all$sample == "btly",]
    cat(sprintf(fmt5, "data$okay.1.*",
                      prettyNum(NROW(data$okay.1.dmoz), big.mark=","),
                      prettyNum(NROW(data$okay.1.seng), big.mark=","),
                      prettyNum(NROW(data$okay.1.deli), big.mark=","),
                      prettyNum(NROW(data$okay.1.btly), big.mark=","),
                      prettyNum(NROW(data$okay.1.all), big.mark=",") ))
    stats <- add.stats("okay.1.*", data$okay.1.all,
            data$okay.1.dmoz, data$okay.1.seng,
            data$okay.1.deli, data$okay.1.btly)

    # Just the last step of successful walks
    data$okay.L.all  <- data$okay.all[data$okay.all$step == data$okay.all$step_count-1,]
    data$okay.L.dmoz <- data$okay.L.all[data$okay.L.all$sample == "dmoz",]
    data$okay.L.seng <- data$okay.L.all[data$okay.L.all$sample == "seng",]
    data$okay.L.deli <- data$okay.L.all[data$okay.L.all$sample == "deli",]
    data$okay.L.btly <- data$okay.L.all[data$okay.L.all$sample == "btly",]
    cat(sprintf(fmt5, "data$okay.L.*",
                      prettyNum(NROW(data$okay.L.dmoz), big.mark=","),
                      prettyNum(NROW(data$okay.L.seng), big.mark=","),
                      prettyNum(NROW(data$okay.L.deli), big.mark=","),
                      prettyNum(NROW(data$okay.L.btly), big.mark=","),
                      prettyNum(NROW(data$okay.L.all), big.mark=",") ))
    stats <- add.stats("okay.L.*", data$okay.L.all,
            data$okay.L.dmoz, data$okay.L.seng,
            data$okay.L.deli, data$okay.L.btly)

    # Subset containing just the steps with drift > 1yr
    data$okay.1yr.all.u  <- data$okay.all[data$okay.all$u_drift > 365.25,]
    data$okay.1yr.dmoz.u <- data$okay.dmoz[data$okay.dmoz$u_drift > 365.25,]
    data$okay.1yr.seng.u <- data$okay.seng[data$okay.seng$u_drift > 365.25,]
    data$okay.1yr.deli.u <- data$okay.deli[data$okay.deli$u_drift > 365.25,]
    data$okay.1yr.btly.u <- data$okay.btly[data$okay.btly$u_drift > 365.25,]
    cat(sprintf(fmt5, "data$okay.*.1yr.u",
                      prettyNum(NROW(data$okay.1yr.dmoz.u), big.mark=","),
                      prettyNum(NROW(data$okay.1yr.seng.u), big.mark=","),
                      prettyNum(NROW(data$okay.1yr.deli.u), big.mark=","),
                      prettyNum(NROW(data$okay.1yr.btly.u), big.mark=","),
                      prettyNum(NROW(data$okay.1yr.all.u), big.mark=",") ))
    stats <- add.stats("okay.*.1yr.u", data$okay.1yr.all.u,
            data$okay.1yr.dmoz.u, data$okay.1yr.seng.u,
            data$okay.1yr.deli.u, data$okay.1yr.btly.u)

    # Subset containing just the steps with drift > 1yr
    data$okay.5yr.all.u  <- data$okay.all[data$okay.all$u_drift > (365.25*5),]
    data$okay.5yr.dmoz.u <- data$okay.dmoz[data$okay.dmoz$u_drift > (365.25*5),]
    data$okay.5yr.seng.u <- data$okay.seng[data$okay.seng$u_drift > (365.25*5),]
    data$okay.5yr.deli.u <- data$okay.deli[data$okay.deli$u_drift > (365.25*5),]
    data$okay.5yr.btly.u <- data$okay.btly[data$okay.btly$u_drift > (365.25*5),]
    cat(sprintf(fmt5, "data$okay.*.5yr.u",
                      prettyNum(NROW(data$okay.5yr.dmoz.u), big.mark=","),
                      prettyNum(NROW(data$okay.5yr.seng.u), big.mark=","),
                      prettyNum(NROW(data$okay.5yr.deli.u), big.mark=","),
                      prettyNum(NROW(data$okay.5yr.btly.u), big.mark=","),
                      prettyNum(NROW(data$okay.5yr.all.u), big.mark=",") ))
    stats <- add.stats("okay.*.5yr.u", data$okay.5yr.all.u,
            data$okay.5yr.dmoz.u, data$okay.5yr.seng.u,
            data$okay.5yr.deli.u, data$okay.5yr.btly.u)

    # Choice data
    data$choice.all <- data$okay.all[!is.na(data$okay.all$choice),]
    cat(sprintf(fmt1, "data$choice.all", prettyNum(NROW(data$choice.all), big.mark=",")))
    stats <- add.stats("choice.*", data$choice.all)
    data$choice.L.all <- data$choice.all[data$choice.all$step == data$choice.all$step_count-1,]
    cat(sprintf(fmt1, "data$choice.L.all", prettyNum(NROW(data$choice.L.all), big.mark=",")))
    stats <- add.stats("choice.L.*", data$choice.L.all)

    # All unsuccessful steps (these are the last steps)
    data$ntok.all <- data$all[data$all$status != "OKAY" &
                              data$all$status != "SPAM" &
                              data$all$status != "JUNK" &
                              data$all$status != "DUP", ]
    cat(sprintf(fmt1, "data$ntok.all", prettyNum(NROW(data$ntok.all), big.mark=",")))
    stats <- add.stats("ntok.*", data$ntok.all)

    # Get the very first step
    data$ntok.1.all  <- data$ntok.all[data$ntok.all$step == 1,]
    data$ntok.1.dmoz <- data$ntok.1.all[data$ntok.1.all$sample == "dmoz",]
    data$ntok.1.seng <- data$ntok.1.all[data$ntok.1.all$sample == "seng",]
    data$ntok.1.deli <- data$ntok.1.all[data$ntok.1.all$sample == "deli",]
    data$ntok.1.btly <- data$ntok.1.all[data$ntok.1.all$sample == "btly",]
    cat(sprintf(fmt5, "data$ntok.1.*",
                      prettyNum(NROW(data$ntok.1.dmoz), big.mark=","),
                      prettyNum(NROW(data$ntok.1.seng), big.mark=","),
                      prettyNum(NROW(data$ntok.1.deli), big.mark=","),
                      prettyNum(NROW(data$ntok.1.btly), big.mark=","),
                      prettyNum(NROW(data$ntok.1.all), big.mark=",") ))
    stats <- add.stats("ntok.1.*", data$ntok.1.all,
            data$ntok.1.dmoz, data$ntok.1.seng,
            data$ntok.1.deli, data$ntok.1.btly)

    # Get the very last step, the one that has the stop reason
    data$ntok.L.all <- data$ntok.all[data$ntok.all$step == data$ntok.all$step_count,]
    data$ntok.L.dmoz <- data$ntok.L.all[data$ntok.L.all$sample == "dmoz",]
    data$ntok.L.seng <- data$ntok.L.all[data$ntok.L.all$sample == "seng",]
    data$ntok.L.deli <- data$ntok.L.all[data$ntok.L.all$sample == "deli",]
    data$ntok.L.btly <- data$ntok.L.all[data$ntok.L.all$sample == "btly",]
    cat(sprintf(fmt5, "data$ntok.L.*",
                      prettyNum(NROW(data$ntok.L.dmoz), big.mark=","),
                      prettyNum(NROW(data$ntok.L.seng), big.mark=","),
                      prettyNum(NROW(data$ntok.L.deli), big.mark=","),
                      prettyNum(NROW(data$ntok.L.btly), big.mark=","),
                      prettyNum(NROW(data$ntok.L.all), big.mark=",") ))
    stats <- add.stats("ntok.L.*", data$ntok.L.all,
            data$ntok.L.dmoz, data$ntok.L.seng,
            data$ntok.L.deli, data$ntok.L.btly)

    # Add a few ratios to the data statistics
    stats <- add.stats("steps/walk",
            NROW(data$okay.all)  / NROW(data$okay.L.all),
            NROW(data$okay.dmoz) / NROW(data$okay.L.dmoz),
            NROW(data$okay.seng) / NROW(data$okay.L.seng),
            NROW(data$okay.deli) / NROW(data$okay.L.deli),
            NROW(data$okay.btly) / NROW(data$okay.L.btly) )
    print(stats)
    filename <- "data-statistics";
    write.data.frame(stats, tabledir, filename)
    data
}

#===============================================================================
# Load an RData file into the caller's environment, if the file is up to date;
# otherwise, delete it to force recreation.
#===============================================================================

load.rdata <- function(data_filename, rdata_filename) {
    if (file.access(rdata_filename, 4) == 0) {
        info <- file.info(data_filename, rdata_filename)
        if (info[data_filename,]$mtime >= info[rdata_filename,]$mtime) {
            cat("Need to rebuild RData\n")
            file.remove(rdata_filename)
            NULL
        } else {
            cat("Loading saved RData...")
            load(rdata_filename)
            cat("\n")
            object
        }
    }
}

#=======================================================================
save.rdata <- function(object, rdata_filename) {
#=======================================================================
    cat("Saving to ", rdata_filename, "...", sep="")
    save(object, file=rdata_filename, compress="bzip2")
    cat("\n")
}

#===============================================================================
# Load the data
#===============================================================================

load.drift.data <- function(data_filename, rdata_filename) {

    cat("----LOADING DATA------------------------------------------------\n")

    #=======================================================================
    load.raw.data <- function(data_filename) {
    #=======================================================================
        data <- list()
        cat("Loading data from ", data_filename, "...", sep="")
        data$all <- load.data(data_filename)
        cat(NROW(data$all), "rows\n")
        cat("\n")
        data
    }

    #=======================================================================
    calculate.aggregates <- function(data) {
    #=======================================================================
        cat("Calculating aggregates: ")
        cat("status...")
        data$all$aggr.status <- factor(sapply(data$all$status, get.aggr.status),
                                       levels=aggr.statuses, ordered=T)
        cat("tm_status...")
        data$all$aggr.tm_status <- factor(sapply(data$all$tm_status, get.aggr.status),
                                          levels=aggr.statuses, ordered=T)
        cat("m_status...")
        data$all$aggr.m_status <- factor(sapply(data$all$m_status, get.aggr.status),
                                         levels=aggr.statuses, ordered=T)
        cat("u_status...")
        data$all$aggr.u_status <- factor(sapply(data$all$m_status, get.aggr.status),
                                         levels=aggr.statuses, ordered=T)
        cat("\n")
        data
    }

    ############################################################
    # If the RData file exists and is up to date, load it.
    # Otherwise, delete it so that is recreated.
    ############################################################

    data <- load.rdata(data_filename, rdata_filename)

    ############################################################
    # If the RData file does not exist, import the data.
    ############################################################

    if (file.access(rdata_filename, 4) == -1) {
        data <- load.raw.data(data_filename)
        data <- calculate.aggregates(data)
        save.rdata(data, rdata_filename)
    }

    data
}

#===============================================================================
calculate.occurrences <- function(step_counts) {
#===============================================================================
    # Create the barplot table
    counts <- table(step_counts)
    steps <- 1:50
    counts <- sapply(steps, function(i) counts[as.character(i)])
    names(counts) <- as.character(steps)
    counts
}

#===============================================================================
calculate.choice.stats <- function(data, data_filename, rdata_filename) {
#===============================================================================

    cat("----CALCULATING CHOICE STATISTICS-------------------------------\n")

    #=======================================================================
    calculate <- function(data) {
    #=======================================================================
        cat("Calculating choice statistics...")
        choice.stats <- lapply(data, FUN=function(ss) {
           ss.50 <- ss[ss$step <= 50,]
           aggregate(ss.50[c("m_drift", "u_drift")],
                     by=list(choice=ss.50$choice),
                     FUN=function(x) { c(
                         mean=mean(x, na.rm=T),
                         sd=sd(x, na.rm=T),
                         median=median(x, na.rm=T),
                         min=min(x, na.rm=T),
                         max=max(x, na.rm=T)) } )
        })
        cat("\n")
        choice.stats
    }

    ############################################################
    # If the RData file exists and is up to date, load it.
    # Otherwise, delete it so that is recreated.
    ############################################################

    choice.stats <- load.rdata(data_filename, rdata_filename)

    ############################################################
    # If the RData file does not exist or is older than
    # the data file, load the data from the file.
    ############################################################

    if (file.access(rdata_filename, 4) == -1) {
        choice.stats <- calculate(data)
        cat("Saving choice.stats RData...")
        save.rdata(choice.stats, rdata_filename)
        cat("\n")
    }

    choice.stats
}

#===============================================================================
calculate.domains.stats <- function(data, data_filename, rdata_filename) {
#===============================================================================

    cat("----CALCULATING DOMAINS STATISTICS------------------------------\n")

    #=======================================================================
    calculate <- function(data) {
    #=======================================================================
        cat("Calculating domains statistics...")
        domains.stats <- lapply(data, FUN=function(ss) {
           ss.50 <- ss[ss$step <= 50,]
           aggregate(ss.50[c("m_drift", "u_drift")],
                     by=list(domains=ss.50$m_domain_count),
                     FUN=function(x) { c(
                         mean=mean(x, na.rm=T),
                         sd=sd(x, na.rm=T),
                         median=median(x, na.rm=T),
                         min=min(x, na.rm=T),
                         max=max(x, na.rm=T)) } )
        })
        cat("\n")
        domains.stats
    }

    ############################################################
    # If the RData file exists and is up to date, load it.
    # Otherwise, delete it so that is recreated.
    ############################################################

    domains.stats <- load.rdata(data_filename, rdata_filename)

    ############################################################
    # If the RData file does not exist or is older than
    # the data file, load the data from the file.
    ############################################################

    if (file.access(rdata_filename, 4) == -1) {
        domains.stats <- calculate(data)
        cat("Saving domains.stats RData...")
        save.rdata(domains.stats, rdata_filename)
        cat("\n")
    }

    domains.stats
}

#===============================================================================
calculate.step.stats <- function(data, data_filename, rdata_filename) {
#===============================================================================

    cat("----CALCULATING STEP STATISTICS---------------------------------\n")

    #=======================================================================
    calculate <- function(data) {
    #=======================================================================
        cat("Calculating step statistics...")
        step.stats <- lapply(data, FUN=function(ss) {
           ss.50 <- ss[ss$step <= 50,]
           aggregate(ss.50[c("m_drift", "u_drift", "choice", "m_domain_count")],
                     by=list(step=ss.50$step),
                     FUN=function(x) { c(
                         mean=mean(x, na.rm=T),
                         sd=sd(x, na.rm=T),
                         median=median(x, na.rm=T),
                         min=min(x, na.rm=T),
                         max=max(x, na.rm=T)) } )
        })
        cat("\n")
        step.stats
    }

    ############################################################
    # If the RData file exists and is up to date, load it.
    # Otherwise, delete it so that is recreated.
    ############################################################

    step.stats <- load.rdata(data_filename, rdata_filename)

    ############################################################
    # If the RData file does not exist or is older than
    # the data file, load the data from the file.
    ############################################################

    if (file.access(rdata_filename, 4) == -1) {
        step.stats <- calculate(data)
        cat("Saving step.stats RData...")
        save.rdata(step.stats, rdata_filename)
        cat("\n")
    }

    step.stats
}

#===============================================================================
calculate.walk.stats <- function(data, data_filename, rdata_filename) {
#===============================================================================

    cat("----CALCULATING WALK STATISTICS---------------------------------\n")

    #=======================================================================
    calculate.stats <- function(data, walk.data) {
    #=======================================================================
        cat("Calculating walk statistics...\n")
        data.50 <- data[data$step <= 50,]
        stats <- as.data.frame(
            aggregate(choice ~ walk, data=data.50,
                FUN=function(x) { c(
                    mean=mean(x, na.rm=T),
                    sd=sd(x, na.rm=T),
                    median=median(x, na.rm=T),
                    min=min(x, na.rm=T),
                    max=max(x, na.rm=T)) } ) )
        row.names(stats) <- stats$walk
        stats$status <- unlist(lapply(stats$walk,
                               function(w) walk.data$status[walk.data$walk == w]))
        stats$step_count <- unlist(lapply(stats$walk,
                                   function(w) walk.data$step_count[walk.data$walk == w]))
        stats$sample <- unlist(lapply(stats$walk,
                               function(w) walk.data$sample[walk.data$walk == w]))
        stats
    }

    #=======================================================================
    create.walk.stats.subsets <- function(walk.stats) {
    #=======================================================================
        cat("Creating walk statistics subsets...\n")
        walk.stats$okay.all <- walk.stats$all[walk.stats$all$step_count > 1,]
        walk.stats
    }

    ############################################################
    # If the RData file exists and is up to date, load it.
    # Otherwise, delete it so that is recreated.
    ############################################################

    walk.stats <- load.rdata(data_filename, rdata_filename)

    ############################################################
    # If the RData file does not exist or is older than
    # the data file, load the data from the file.
    ############################################################

    if (file.access(rdata_filename, 4) == -1) {
        walk.stats <- list()
        walk.stats$all <- calculate.stats(data$all, data$all.L)
        cat("Saving walk.stats RData...")
        save.rdata(walk.stats, rdata_filename)
        cat("\n")
    }

    create.walk.stats.subsets(walk.stats)
}

#===============================================================================
create.tables.and.charts <- function(data, step.stats, walk.stats,
                                     chartdir, tabledir) {
#===============================================================================

    ############################################################
    # Basic counts
    ############################################################

    cat("----BASIC COUNTS -----------------------------------------------\n")

    ############################################################
    # Choice by walk length
    ############################################################

    cat("----CHARTING CHOICE---------------------------------------------\n")

    choice.summary <- summary(data$choice.all$choice, na.rm=T)
    cat("choice.summary:\n")
    print(choice.summary)

    filename <- "choice-bystep-scatter-all"
    plot.formats(filename, png.parms.w90, pdf.parms.w90, function() {
        data <- data$choice.all[data$choice.all$step <= 50,]
        make.step.heatplot(data$step, data$choice, ymin=0,
                      "Choice by Step", "Step Number", "Choice",
                      y.scale=1, y.unit="", y.ticks=1000, col=colors$api)
    })
    cat(filename, "correlation", cor(data$choice.all$step, data$choice.all$m_drift), "\n")

    filename <- "choice-bystep-mean-all"
    plot.formats(filename, png.parms.w90, pdf.parms.w90, function() {
        data <- step.stats$choice.all
        make.scatter.plot(
            data$step, cbind(data$choice[,"mean"]),
            "Mean Choice by Step", "Step Number", "Mean Choice",
            ymin=0, y.scale=1, y.unit="", y.ticks=10, col=colors$api)
    })

    filename <- "choice-bystep-median-all"
    plot.formats(filename, png.parms.w90, pdf.parms.w90, function() {
        data <- step.stats$choice.all
        make.scatter.plot(
            data$step, cbind(data$choice[,"median"]),
            "Median Choice by Step", "Step Number", "Median Choice",
            ymin=0, y.scale=1, y.unit="", y.ticks=10, col=colors$api)
    })

    ############################################################
    # Domains by step number
    ############################################################

    cat("----CHARTING DOMAINS--------------------------------------------\n")

#    filename <- "domains-bystep-scatter-all"
#    plot.formats(filename, png.parms.w90, pdf.parms.w90, function() {
#        data <- data$okay.all[data$okay.all$step <= 50,]
#        make.step.heatplot(data$step, data$m_domain_count, ymin=0,
#                      "Domains by Step", "Step Number", "Domains",
#                      ymax=40,
#                      y.scale=1, y.unit="", y.ticks=10, col=colors$api)
#    })
#    cat(filename, "correlation", cor(data$okay.all$step, data$okay.all$m_drift), "\n")
#    filename <- "domains-bylength-scatter-all"
#    plot.formats(filename, png.parms.w90, pdf.parms.w90, function() {
#        data <- data$okay.L.all[data$okay.L.all$step <= 50,]
#        make.step.heatplot(data$step, data$m_domain_count, ymin=0,
#                      "Domains by Walk Length", "Walk Length", "Domains",
#                      ymax=40,
#                      y.scale=1, y.unit="", y.ticks=10, col=colors$api)
#    })
#    cat(filename, "correlation", cor(data$okay.L.all$step, data$okay.L.all$m_drift), "\n")

    filename <- "domains-bystep-mean-all"
    plot.formats(filename, png.parms.w90, pdf.parms.w90, function() {
        data <- step.stats$okay.all
        make.scatter.plot(
            data$step, cbind(data$m_domain_count[,"mean"]),
            "Mean Domains by Step",
            "Step Number", "Mean Domains",
            ymin=0, y.scale=1, y.unit="",
            col=colors$api)
    })
    filename <- "domains-bylength-mean-all"
    plot.formats(filename, png.parms.w90, pdf.parms.w90, function() {
        data <- step.stats$okay.L.all
        make.scatter.plot(
            data$step, cbind(data$m_domain_count[,"mean"]),
            "Mean Domains by Walk Length",
            "Walk Length", "Mean Domains",
            ymin=0, y.scale=1, y.unit="",
            col=colors$api)
    })

    filename <- "domains-bystep-median-all"
    plot.formats(filename, png.parms.w90, pdf.parms.w90, function() {
        data <- step.stats$okay.all
        make.scatter.plot(
            data$step, cbind(data$m_domain_count[,"median"]),
            "Median Domains by Step",
            "Step Number", "Median Domains",
            ymin=0, y.scale=1, y.unit="",
            col=colors$api)
    })
    filename <- "domains-bylength-median-all"
    plot.formats(filename, png.parms.w90, pdf.parms.w90, function() {
        data <- step.stats$okay.L.all
        make.scatter.plot(
            data$step, cbind(data$m_domain_count[,"median"]),
            "Median Domains by Walk Length",
            "Walk Length", "Median Domains",
            ymin=0, y.scale=1, y.unit="",
            col=colors$api)
    })

    ############################################################
    # Stop causes
    ############################################################

    cat("----CALCULATING & CHARTING STOP CAUSES--------------------------\n")

    summarize.stop.causes <- function(data) {
        #cat("Summarizing stop causes...")
        causes <- data.frame(status=aggr.statuses)
        causes$count <- table(data$aggr.status)
        causes$ratio <- prop.table(causes$count)
        #causes$percent <- causes$ratio * 100.0
        causes$percent_ch <- sapply(causes$ratio, as.character.percent)
        causes$tm_count <- table(data$aggr.tm_status)
        causes$tm_ratio <- causes$ratio * (causes$tm_count / causes$count)
        #causes$tm_percent <-causes$tm_ratio * 100.0
        causes$tm_percent_ch <- sapply(causes$tm_ratio, as.character.percent)
        causes$m_count <- causes$count - causes$tm_count
        causes$m_ratio <- causes$ratio * (causes$m_count / causes$count)
        #causes$m_percent <- causes$m_ratio * 100.0
        causes$m_percent_ch <- sapply(causes$m_ratio, as.character.percent)
        #cat("\n")
        causes
    }

    calc.and.chart <- function(data, filename, title) {
        stop.causes <- summarize.stop.causes(data)
        write.data.frame(stop.causes, tabledir, filename)
        counts <- rbind(stop.causes$tm_count, stop.causes$count - stop.causes$tm_count)
        rownames(counts) <- c("Timemap", "Memento")
        percentages <- rbind(stop.causes$tm_ratio * 100.0, stop.causes$m_ratio * 100.0)
        rownames(percentages) <- c("Timemap", "Memento")
        #print(percentages)
        plot.formats(filename, png.parms.w90, pdf.parms.w90, function() {
            make.stacked.barplot.percentages(
                percentages,
                title, "Stop Cause",
                col=c(colors$timemap, colors$memento), las=3)
        })
        stop.causes
    }

    calc.and.chart(data$ntok.L.all, "stopcauses-all-stepn",
                   "Successful Walk Stop Causes")
    calc.and.chart(data$ntok.L.dmoz, "stopcauses-dmoz-stepn",
                   "Successful Walk Stop Causes (DMOZ)")
    calc.and.chart(data$ntok.L.seng, "stopcauses-seng-stepn",
                   "Successful Walk Stop Causes (Search Eng.)")
    calc.and.chart(data$ntok.L.deli, "stopcauses-deli-stepn",
                   "Successful Walk Stop Causes (Delicious)")
    calc.and.chart(data$ntok.L.btly, "stopcauses-btly-stepn",
                   "Successful Walk Stop Causes (Bitly)")
    calc.and.chart(data$ntok.1.all, "stopcauses-all-step1",
                   "Failed Walk Stop Causes")
    calc.and.chart(data$ntok.1.dmoz, "stopcauses-dmoz-step1",
                   "Failed Walk Stop Causes (DMOZ)")
    calc.and.chart(data$ntok.1.seng, "stopcauses-seng-step1",
                   "Failed Walk Stop Causes (Search Eng.)")
    calc.and.chart(data$ntok.1.deli, "stopcauses-deli-step1",
                   "Failed Walk Stop Causes (Delicious)")
    calc.and.chart(data$ntok.1.btly, "stopcauses-btly-step1",
                   "Failed Walk Stop Causes (Bitly)")

    ############################################################
    # Drift by choice
    ############################################################

    cat("----CHARTING DRIFT BY CHOICE------------------------------------\n")

    common_ymax <- max(data$okay_all$m_drift, data$okay.all$u_drift, na.rm=T)

    filename <- "drift-bychoice-scatter-all-api"
    plot.formats(filename, png.parms.w90, pdf.parms.n90, function() {
        data <- data$okay.all[data$okay.all$step <= 50 & ! is.na(data$okay.all$choice),]
        make.choice.heatplot(data$choice, data$m_drift,
                      "Drift by Choice (API)", "Choice", "Drift (Months)",
                      days.per.year, "y",
                      col=colors$api,
                      ymin=0, ymax=common_ymax)
    })
    cat(filename, "correlation", cor(data$okay.all$choice, data$okay.all$m_drift), "\n")

    filename <- "drift-bychoice-scatter-all-ui"
    plot.formats(filename, png.parms.w90, pdf.parms.n90, function() {
        data <- data$okay.all[data$okay.all$step <= 50 & ! is.na(data$okay.all$choice),]
        make.choice.heatplot(data$choice, data$u_drift,
                      "Drift by Step (UI)", "Choice", "Drift (Months)",
                      y.scale=days.per.year, y.unit="y",
                      col=colors$ui,
                      ymin=0, ymax=common_ymax)
    })
    cat(filename, "correlation", cor(data$okay.all$choice, data$okay.all$u_drift), "\n")

    filename <- "drift-bychoice-mean-all"
    plot.formats(filename, png.parms.w90, pdf.parms.w90, function() {
        data <- choice.stats$okay.all[choice.stats$okay.all$choice > 0,]
        make.scatter.plot.choice.2(
            data$choice,
            cbind(data$u_drift[,"mean"], data$m_drift[,"mean"]),
            "Mean Drift by Choice", "Choice", "Mean Drift (Months)",
            y.ticks = 6 * days.per.month,
            col=c(colors$ui, colors$api))
    })

    filename <- "drift-bychoice-sd-1-all"
    plot.formats(filename, png.parms.w90, pdf.parms.w90, function() {
        data <- choice.stats$okay.all[choice.stats$okay.all$choice > 0,]
        make.scatter.plot.choice.2(
            data$choice,
            cbind(data$u_drift[,"sd"], data$m_drift[,"sd"]),
            "Mean Drift by Choice", "Choice", "Mean Drift (Months)",
            y.ticks = 6 * days.per.month,
            col=c(colors$ui, colors$api))
    })

    filename <- "drift-bychoice-median-all"
    plot.formats(filename, png.parms.w90, pdf.parms.w90, function() {
        data <- choice.stats$okay.all[choice.stats$okay.all$choice > 0,]
        make.scatter.plot.choice.2(
            data$choice,
            cbind(data$u_drift[,"median"], data$m_drift[,"median"]),
            "Drift Standard Deviation by Choice", "Choice", "Mean Drift (Months)",
            y.ticks = 6 * days.per.month,
            col=c(colors$ui, colors$api))
    })

#    filename <- "drift-bychoice-mean-all-api"
#    plot.formats(filename, png.parms.w90, pdf.parms.w90, function() {
#        data <- choice.stats$okay.all
#        make.scatter.plot.choice(
#            data$choice, cbind(data$m_drift[,"mean"]),
#            "Mean Drift by Choice (API)", "Choice", "Mean Drift (Months)",
#            ymin=0, y.scale=1, y.unit="", y.ticks=100, col=colors$api)
#    })
#
#    filename <- "drift-bychoice-mean-all-ui"
#    plot.formats(filename, png.parms.w90, pdf.parms.w90, function() {
#        data <- choice.stats$okay.all
#        make.scatter.plot.choice(
#            data$choice, cbind(data$u_drift[,"mean"]),
#            "Mean Drift by Choice (UI)", "Choice", "Mean Drift (Months)",
#            ymin=0, y.scale=1, y.unit="", y.ticks=100, col=colors$ui)
#    })
#
#    filename <- "drift-bychoice-median-all-api"
#    plot.formats(filename, png.parms.w90, pdf.parms.w90, function() {
#        data <- choice.stats$okay.all
#        make.scatter.plot.choice(
#            data$choice, cbind(data$m_drift[,"median"]),
#            "Median Drift by Choice (API)", "Choice", "Median Drift (Months)",
#            ymin=0, y.scale=1, y.unit="", y.ticks=100, col=colors$api)
#    })
#
#    filename <- "drift-bychoice-median-all-ui"
#    plot.formats(filename, png.parms.w90, pdf.parms.w90, function() {
#        data <- choice.stats$okay.all
#        make.scatter.plot.choice(
#            data$choice, cbind(data$u_drift[,"median"]),
#            "Median Drift by Choice (UI)", "Choice", "Median Drift (Months)",
#            ymin=0, y.scale=1, y.unit="", y.ticks=100, col=colors$ui)
#    })

    ############################################################
    # Drift by domains
    ############################################################

    cat("----CHARTING DRIFT BY DOMAINS-----------------------------------\n")

    common_ymax <- max(data$okay_all$m_drift, data$okay.all$u_drift, na.rm=T)

#    filename <- "drift-bydomains-scatter-all-api"
#    plot.formats(filename, png.parms.w90, pdf.parms.n90, function() {
#        data <- data$okay.all[data$okay.all$step <= 50 & ! is.na(data$okay.all$domains),]
#        make.domains.heatplot(data$domains, data$m_drift,
#                      "Drift by Domain Count (API)", "Domain Count", "Drift (Months)",
#                      days.per.year, "y",
#                      col=colors$api,
#                      ymin=0, ymax=common_ymax)
#    })
#    cat(filename, "correlation", cor(data$okay.all$domains, data$okay.all$m_drift), "\n")
#
#    filename <- "drift-bydomains-scatter-all-ui"
#    plot.formats(filename, png.parms.w90, pdf.parms.n90, function() {
#        data <- data$okay.all[data$okay.all$step <= 50 & ! is.na(data$okay.all$domains),]
#        make.domains.heatplot(data$domains, data$u_drift,
#                      "Drift by Step (UI)", "Step Number", "Drift (Months)",
#                      y.scale=days.per.year, y.unit="y",
#                      col=colors$ui,
#                      ymin=0, ymax=common_ymax)
#    })
#    cat(filename, "correlation", cor(data$okay.all$domains, data$okay.all$u_drift), "\n")

    filename <- "drift-bydomains-mean-all"
    plot.formats(filename, png.parms.w90, pdf.parms.w90, function() {
        data <- domains.stats$okay.all
        make.scatter.plot.domains.2(
            data$domains,
            cbind(data$m_drift[,"mean"], data$u_drift[,"mean"]),
            "Mean Drift by Domain Count (API)", "Domain Count", "Mean Drift (Months)",
            #ymin=0, y.scale=1, y.unit="", y.ticks=10,
            col=c(colors$api, colors$ui))
    })

    filename <- "drift-bydomains-sd-1-all"
    plot.formats(filename, png.parms.w90, pdf.parms.w90, function() {
        data <- domains.stats$okay.all
        make.scatter.plot.domains.2(
            data$domains,
            cbind(data$m_drift[,"sd"], data$u_drift[,"sd"]),
            "Mean Drift by Domain Count", "Domain Count", "Mean Drift (Months)",
            #ymin=0, y.scale=1, y.unit="", y.ticks=10,
            col=c(colors$api, colors$ui))
    })

    filename <- "drift-bydomains-median-all"
    plot.formats(filename, png.parms.w90, pdf.parms.w90, function() {
        data <- domains.stats$okay.all
        make.scatter.plot.domains.2(
            data$domains,
            cbind(data$m_drift[,"median"], data$u_drift[,"median"]),
            "Median Drift by Domain Count", "Domain Count", "Mean Drift (Months)",
            #ymin=0, y.scale=1, y.unit="", y.ticks=10,
            col=c(colors$api, colors$ui))
    })

    ############################################################
    # Drift by walk length
    ############################################################

    cat("----CHARTING DRIFT----------------------------------------------\n")

    filename <- "drift-bystep-scatter-all-api"
    plot.formats(filename, png.parms.w90, pdf.parms.w90, function() {
        data <- data$okay.all[data$okay.all$step <= 50,]
        make.step.heatplot(data$step, data$m_drift,
                      "Drift by Step (API)", "Step Number", "Drift (Years)",
                      days.per.year, "y", col=colors$api)
    })
    cat(filename, "correlation", cor(data$okay.all$step, data$okay.all$m_drift), "\n")
    #ct <- cor.test(data$okay.all$step, data$okay.all$m_drift)
    #print(ct)

    filename <- "drift-bystep-scatter-all-ui"
    plot.formats(filename, png.parms.w90, pdf.parms.w90, function() {
        data <- data$okay.all[data$okay.all$step <= 50,]
        make.step.heatplot(data$step, data$u_drift,
                      "Drift by Step (UI)", "Step Number", "Drift (Years)",
                      col=colors$ui,
                      y.scale=days.per.year, y.unit="y",
                      ymin=0)
    })
    cat(filename, "correlation", cor(data$okay.all$step, data$okay.all$u_drift), "\n")
    #ct <- cor.test(data$okay.all$step, data$okay.all$u_drift)
    #print(ct)

    filename <- "drift-bystep-scatter-dmoz-api"
    plot.formats(filename, png.parms.w90, pdf.parms.w90, function() {
        data <- data$okay.dmoz[data$okay.dmoz$step <= 50,]
        make.step.heatplot(data$step, data$m_drift,
                      "Drift by Step (DMOZ/API)", "Step Number", "Drift (Years)",
                      days.per.year, "y", col=colors$api)
    })
    cat(filename, "correlation", cor(data$okay.dmoz$step, data$okay.dmoz$m_drift), "\n")
    #ct <- cor.test(data$okay.dmoz$step, data$okay.dmoz$m_drift)
    #print(ct)

    filename <- "drift-bystep-scatter-dmoz-ui"
    plot.formats(filename, png.parms.w90, pdf.parms.w90, function() {
        data <- data$okay.dmoz[data$okay.dmoz$step <= 50,]
        make.step.heatplot(data$step, data$u_drift,
                      "Drift by Step (DMOZ/UI)", "Step Number", "Drift (Years)",
                      days.per.year, "y", col=colors$ui)
    })
    cat(filename, "correlation", cor(data$okay.dmoz$step, data$okay.dmoz$u_drift), "\n")
    #ct <- cor.test(data$okay.dmoz$step, data$okay.dmoz$u_drift)
    #print(ct)

    filename <- "drift-bystep-scatter-seng-api"
    plot.formats(filename, png.parms.w90, pdf.parms.w90, function() {
        data <- data$okay.seng[data$okay.seng$step <= 50,]
        make.step.heatplot(data$step, data$m_drift,
                      "Drift by Step (Search Eng./API)", "Step Number", "Drift (Years)",
                      days.per.year, "y", col=colors$api)
    })
    cat(filename, "correlation", cor(data$okay.seng$step, data$okay.seng$m_drift), "\n")
    #ct <- cor.test(data$okay.seng$step, data$okay.seng$m_drift)
    #print(ct)

    filename <- "drift-bystep-scatter-seng-ui"
    plot.formats(filename, png.parms.w90, pdf.parms.w90, function() {
        data <- data$okay.seng[data$okay.seng$step <= 50,]
        make.step.heatplot(data$step, data$u_drift,
                      "Drift by Step (Search Eng./UI)", "Step Number", "Drift (Years)",
                      days.per.year, "y", col=colors$ui)
    })
    cat(filename, "correlation", cor(data$okay.seng$step, data$okay.seng$u_drift), "\n")
    #ct <- cor.test(data$okay.seng$step, data$okay.seng$u_drift)
    #print(ct)

    filename <- "drift-bystep-scatter-deli-api"
    plot.formats(filename, png.parms.w90, pdf.parms.w90, function() {
        data <- data$okay.deli[data$okay.deli$step <= 50,]
        make.step.heatplot(data$step, data$m_drift,
                      "Drift by Step (Delicious/API)", "Step Number", "Drift (Years)",
                      days.per.year, "y", col=colors$api)
    })
    cat(filename, "correlation", cor(data$okay.deli$step, data$okay.deli$m_drift), "\n")
    #ct <- cor.test(data$okay.deli$step, data$okay.deli$m_drift)
    #print(ct)

    filename <- "drift-bystep-scatter-deli-ui"
    plot.formats(filename, png.parms.w90, pdf.parms.w90, function() {
        data <- data$okay.deli[data$okay.deli$step <= 50,]
        make.step.heatplot(data$step, data$u_drift,
                      "Drift by Step (Delicious/UI)", "Step Number", "Drift (Years)",
                      days.per.year, "y", col=colors$ui)
    })
    cat(filename, "correlation", cor(data$okay.deli$step, data$okay.deli$u_drift), "\n")
    #ct <- cor.test(data$okay.deli$step, data$okay.deli$u_drift)
    #print(ct)

    filename <- "drift-bystep-scatter-btly-api"
    plot.formats(filename, png.parms.w90, pdf.parms.w90, function() {
        data <- data$okay.btly[data$okay.btly$step <= 50,]
        make.step.heatplot(data$step, data$m_drift,
                      "Drift by Step (Bitly/API)", "Step Number", "Drift (Years)",
                      days.per.year, "y", col=colors$api)
    })
    cat(filename, "correlation", cor(data$okay.btly$step, data$okay.btly$m_drift), "\n")
    #ct <- cor.test(data$okay.btly$step, data$okay.btly$m_drift)
    #print(ct)

    filename <- "drift-bystep-scatter-btly-ui"
    plot.formats(filename, png.parms.w90, pdf.parms.w90, function() {
        data <- data$okay.btly[data$okay.btly$step <= 50,]
        make.step.heatplot(data$step, data$u_drift,
                      "Drift by Step (Bitly/UI)", "Step Number", "Drift (Years)",
                      days.per.year, "y", col=colors$ui)
    })
    cat(filename, "correlation", cor(data$okay.btly$step, data$okay.btly$u_drift), "\n")
    #ct <- cor.test(data$okay.btly$step, data$okay.btly$u_drift)
    #print(ct)

    ############################################################
    # Mean Drift
    ############################################################

    cat("----CALCULATING & CHARTING MEAN & MEDIAN DRIFT------------------\n")

    # Mean

    common_ymax <- max(step.stats$okay.all$m_drift[,"mean"],
                       step.stats$okay.all$u_drift[,"mean"],
                       step.stats$okay.dmoz$m_drift[,"mean"],
                       step.stats$okay.dmoz$u_drift[,"mean"],
                       step.stats$okay.seng$m_drift[,"mean"],
                       step.stats$okay.seng$u_drift[,"mean"],
                       step.stats$okay.deli$m_drift[,"mean"],
                       step.stats$okay.deli$u_drift[,"mean"],
                       step.stats$okay.btly$m_drift[,"mean"],
                       step.stats$okay.btly$u_drift[,"mean"],
#                       step.stats$okay.all$m_drift[,"sd"],
#                       step.stats$okay.all$u_drift[,"sd"],
#                       step.stats$okay.dmoz$m_drift[,"sd"],
#                       step.stats$okay.dmoz$u_drift[,"sd"],
#                       step.stats$okay.seng$m_drift[,"sd"],
#                       step.stats$okay.seng$u_drift[,"sd"],
#                       step.stats$okay.deli$m_drift[,"sd"],
#                       step.stats$okay.deli$u_drift[,"sd"],
#                       step.stats$okay.btly$m_drift[,"sd"],
#                       step.stats$okay.btly$u_drift[,"sd"],
                       na.rm=T)
    filename <- "drift-bystep-mean-all"
    write.data.frame(step.stats$okay.all, tabledir, filename)
    plot.formats(filename, png.parms.w90, pdf.parms.w90, function() {
        make.scatter.plot.2(
            step.stats$okay.all$step,
            cbind(step.stats$okay.all$m_drift[,"mean"],
                  step.stats$okay.all$u_drift[,"mean"]),
            ymax=common_ymax,
            "Mean Drift by Step",
            "Step Number", "Mean Drift (Months)",
            col=c(colors$api, colors$ui))
    })
    cat(filename, "correlation",
        cor(step.stats$okay.all$step, step.stats$okay.all$m_drift[,"mean"]),
        cor(step.stats$okay.all$step, step.stats$okay.all$m_drift[,"mean"],
            method="kendall"),
        "(API)\n")
    cat(filename, "correlation",
        cor(step.stats$okay.all$step, step.stats$okay.all$u_drift[,"mean"]),
        cor(step.stats$okay.all$step, step.stats$okay.all$u_drift[,"mean"],
            method="kendall"),
        "(UI)\n")

    filename <- "drift-bystep-mean-dmoz"
    write.data.frame(step.stats$okay.dmoz, tabledir, filename)
    plot.formats(filename, png.parms.w90, pdf.parms.w90, function() {
        make.scatter.plot.2(
            step.stats$okay.dmoz$step,
            cbind(step.stats$okay.dmoz$m_drift[,"mean"],
                  step.stats$okay.dmoz$u_drift[,"mean"]),
            ymax=common_ymax,
            "Mean Drift by Step (DMOZ)",
            "Step Number", "Mean Drift (Months)",
            col=c(colors$api, colors$ui))
    })
    cat(filename, "correlation",
        cor(step.stats$okay.dmoz$step, step.stats$okay.dmoz$m_drift[,"mean"]),
        cor(step.stats$okay.dmoz$step, step.stats$okay.dmoz$m_drift[,"mean"],
            method="kendall"),
        "(API)\n")
    cat(filename, "correlation",
        cor(step.stats$okay.dmoz$step, step.stats$okay.dmoz$u_drift[,"mean"]),
        cor(step.stats$okay.dmoz$step, step.stats$okay.dmoz$u_drift[,"mean"],
            method="kendall"),
        "(UI)\n")

    filename <- "drift-bystep-mean-seng"
    plot.formats(filename, png.parms.w90, pdf.parms.w90, function() {
        make.scatter.plot.2(
            step.stats$okay.seng$step,
            cbind(step.stats$okay.seng$m_drift[,"mean"],
                  step.stats$okay.seng$u_drift[,"mean"]),
            ymax=common_ymax,
            "Mean Drift by Step (Search Eng.)",
            "Step Number", "Mean Drift (Months)",
            col=c(colors$api, colors$ui))
    })
    cat(filename, "correlation",
        cor(step.stats$okay.seng$step, step.stats$okay.seng$m_drift[,"mean"]),
        cor(step.stats$okay.seng$step, step.stats$okay.seng$m_drift[,"mean"],
            method="kendall"),
        "(API)\n")
    cat(filename, "correlation",
        cor(step.stats$okay.seng$step, step.stats$okay.seng$u_drift[,"mean"]),
        cor(step.stats$okay.seng$step, step.stats$okay.seng$u_drift[,"mean"],
            method="kendall"),
        "(UI)\n")

    filename <- "drift-bystep-mean-deli"
    plot.formats(filename, png.parms.w90, pdf.parms.w90, function() {
        make.scatter.plot.2(
            step.stats$okay.deli$step,
            cbind(step.stats$okay.deli$m_drift[,"mean"],
                  step.stats$okay.deli$u_drift[,"mean"]),
            ymax=common_ymax,
            "Mean Drift by Step (Delicious)",
            "Step Number", "Mean Drift (Months)",
            col=c(colors$api, colors$ui))
    })
    cat(filename, "correlation",
        cor(step.stats$okay.deli$step, step.stats$okay.deli$m_drift[,"mean"]),
        cor(step.stats$okay.deli$step, step.stats$okay.deli$m_drift[,"mean"],
            method="kendall"),
        "(API)\n")
    cat(filename, "correlation",
        cor(step.stats$okay.deli$step, step.stats$okay.deli$u_drift[,"mean"]),
        cor(step.stats$okay.deli$step, step.stats$okay.deli$u_drift[,"mean"],
            method="kendall"),
        "(UI)\n")

    filename <- "drift-bystep-mean-btly"
    plot.formats(filename, png.parms.w90, pdf.parms.w90, function() {
        make.scatter.plot.2(
            step.stats$okay.btly$step,
            cbind(step.stats$okay.btly$m_drift[,"mean"],
                  step.stats$okay.btly$u_drift[,"mean"]),
            ymax=common_ymax,
            "Mean Drift by Step (Bitly)",
            "Step Number", "Mean Drift (Months)",
            col=c(colors$api, colors$ui))
    })
    cat(filename, "correlation",
        cor(step.stats$okay.btly$step, step.stats$okay.btly$m_drift[,"mean"]),
        cor(step.stats$okay.btly$step, step.stats$okay.btly$m_drift[,"mean"],
            method="kendall"),
        "(API)\n")
    cat(filename, "correlation",
        cor(step.stats$okay.btly$step, step.stats$okay.btly$u_drift[,"mean"]),
        cor(step.stats$okay.btly$step, step.stats$okay.btly$u_drift[,"mean"],
            method="kendall"),
        "(UI)\n")

    # Standard Deviation

    common_ymax <- max(step.stats$okay.all$m_drift[,"mean"],
                       step.stats$okay.all$u_drift[,"mean"],
                       step.stats$okay.all$m_drift[,"sd"],
                       step.stats$okay.all$u_drift[,"sd"],
                       na.rm=T)
    #cat("create.charts.and.tables:common_ymax =", common_ymax, "\n") #debug
    filename <- "drift-sd-1-all"
    write.data.frame(step.stats$okay.all, tabledir, filename)
    plot.formats(filename, png.parms.w90, pdf.parms.w90, function() {
        make.scatter.plot.2(
            step.stats$okay.all$step,
            cbind(step.stats$okay.all$m_drift[,"mean"],
                  step.stats$okay.all$u_drift[,"mean"]),
            ymax=common_ymax,
            "Mean Drift by Step",
            "Step Number", "Mean Drift (Months)",
            col=c(colors$api, colors$ui))
    })
    filename <- "drift-sd-2-all"
    write.data.frame(step.stats$okay.all, tabledir, filename)
    plot.formats(filename, png.parms.w90, pdf.parms.w90, function() {
        make.scatter.plot.2(
            step.stats$okay.all$step,
            cbind(step.stats$okay.all$m_drift[,"mean"],
                  step.stats$okay.all$u_drift[,"mean"],
                  step.stats$okay.all$m_drift[,"sd"],
                  step.stats$okay.all$u_drift[,"sd"]),
            ymax=common_ymax,
            "Mean Drift by Step",
            "Step Number", "Mean Drift (Months)",
            col=c(colors$api, colors$ui, colors$api.faded, colors$ui.faded),
            lcol=c(colors$api, colors$ui),
            pch=c(19,19, 21, 21),
            cex=c(1, 1, 1, 1))
    })

    # Median

    filename <- "drift-bystep-median-all"
    plot.formats(filename, png.parms.h90, pdf.parms.h90, function() {
        make.scatter.plot.2(
            step.stats$okay.all$step,
            cbind(step.stats$okay.all$m_drift[,"median"],
                  step.stats$okay.all$u_drift[,"median"]),
            ymax=common_ymax / 3,
            "Median Drift by Step",
            "Step Number", "Median Drift (Months)",
            col=c(colors$api, colors$ui))
    })
    filename <- "drift-bystep-median-dmoz"
    plot.formats(filename, png.parms.h90, pdf.parms.h90, function() {
        make.scatter.plot.2(
            step.stats$okay.dmoz$step,
            cbind(step.stats$okay.dmoz$m_drift[,"median"],
                  step.stats$okay.dmoz$u_drift[,"median"]),
            ymax=common_ymax / 3,
            "Median Drift by Step",
            "Step Number", "Median Drift (Months)",
            col=c(colors$api, colors$ui))
    })
    filename <- "drift-bystep-median-seng"
    plot.formats(filename, png.parms.h90, pdf.parms.h90, function() {
        make.scatter.plot.2(
            step.stats$okay.seng$step,
            cbind(step.stats$okay.seng$m_drift[,"median"],
                  step.stats$okay.seng$u_drift[,"median"]),
            ymax=common_ymax / 3,
            "Median Drift by Step",
            "Step Number", "Median Drift (Months)",
            col=c(colors$api, colors$ui))
    })
    filename <- "drift-bystep-median-deli"
    plot.formats(filename, png.parms.h90, pdf.parms.h90, function() {
        make.scatter.plot.2(
            step.stats$okay.deli$step,
            cbind(step.stats$okay.deli$m_drift[,"median"],
                  step.stats$okay.deli$u_drift[,"median"]),
            ymax=common_ymax / 3,
            "Median Drift by Step",
            "Step Number", "Median Drift (Months)",
            col=c(colors$api, colors$ui))
    })
    filename <- "drift-bystep-median-btly"
    plot.formats(filename, png.parms.h90, pdf.parms.h90, function() {
        make.scatter.plot.2(
            step.stats$okay.btly$step,
            cbind(step.stats$okay.btly$m_drift[,"median"],
                  step.stats$okay.btly$u_drift[,"median"]),
            ymax=common_ymax / 3,
            "Median Drift by Step",
            "Step Number", "Median Drift (Months)",
            col=c(colors$api, colors$ui))
    })

    ############################################################
    # Charts: walk length occurrences & ECDF
    ############################################################

    cat("----CALCULATING & CHARTING OCCURANCES & ECDF--------------------\n")

    occurrences <- list()
    occurrences$all  <- calculate.occurrences(data$okay.L.all$step_count-1)
    occurrences$dmoz <- calculate.occurrences(data$okay.L.dmoz$step_count-1)
    occurrences$seng <- calculate.occurrences(data$okay.L.seng$step_count-1)
    occurrences$deli <- calculate.occurrences(data$okay.L.deli$step_count-1)
    occurrences$btly <- calculate.occurrences(data$okay.L.btly$step_count-1)
    filename <- "occurrences-bylength"
    write.data.frame(
        data.frame(
            step_count=names(occurrences$all),
            dmoz=occurrences$dmoz,
            seng=occurrences$seng,
            deli=occurrences$deli,
            btly=occurrences$btly,
            all =occurrences$all),
        tabledir, filename)

    filename <- "occurrences-all-bylength-linear"
    plot.formats(filename, png.parms.w90, pdf.parms.w90, function() {
        make.occurrences.barplot(
            occurrences$all,
            "Occurrences", "Walk Length", "Occurrences",
            col=colors$walk)
    })

    filename <- "occurrences-dmoz-bylength-linear"
    plot.formats(filename, png.parms.w90, pdf.parms.w90, function() {
        make.occurrences.barplot(
            occurrences$dmoz,
            "Occurrences (DMOZ)", "Walk Length", "Occurrences",
            col=colors$walk)
    })
    filename <- "occurrences-seng-bylength-linear"
    plot.formats(filename, png.parms.w90, pdf.parms.w90, function() {
        make.occurrences.barplot(
            occurrences$seng,
            "Occurrences (Search Eng.)", "Walk Length", "Occurrences",
            col=colors$walk)
    })
    filename <- "occurrences-deli-bylength-linear"
    plot.formats(filename, png.parms.w90, pdf.parms.w90, function() {
        make.occurrences.barplot(
            occurrences$deli,
            "Occurrences (Delicious)", "Walk Length", "Occurrences",
            col=colors$walk)
    })
    filename <- "occurrences-btly-bylength-linear"
    plot.formats(filename, png.parms.w90, pdf.parms.w90, function() {
        make.occurrences.barplot(
            occurrences$btly,
            "Occurrences (Bitly)", "Walk Length", "Occurrences",
            col=colors$walk)
    })

    filename <- "occurrences-all-bylength-log"
    plot.formats(filename, png.parms.w90, pdf.parms.w90, function() {
        make.occurrences.barplot(
            occurrences$all,
            "Occurrences (log scale)", "Walk Length", "Occurrences (log scale)",
            col=colors$walk, log="y")
    })

    filename <- "occurrences-dmoz-bylength-log"
    plot.formats(filename, png.parms.w90, pdf.parms.w90, function() {
        make.occurrences.barplot(
            occurrences$dmoz,
            "Occurrences (DMOZ/log scale)", "Walk Length", "Occurrences (log scale)",
            col=colors$walk, log="y")
    })
    filename <- "occurrences-seng-bylength-log"
    plot.formats(filename, png.parms.w90, pdf.parms.w90, function() {
        make.occurrences.barplot(
            occurrences$seng,
            "Occurrences (Search Eng./log scale)", "Walk Length", "Occurrences (log scale)",
            col=colors$walk, log="y")
    })
    filename <- "occurrences-deli-bylength-log"
    plot.formats(filename, png.parms.w90, pdf.parms.w90, function() {
        make.occurrences.barplot(
            occurrences$deli,
            "Occurrences (Delicious/log scale)", "Walk Length", "Occurrences (log scale)",
            col=colors$walk, log="y")
    })
    filename <- "occurrences-btly-bylength-log"
    plot.formats(filename, png.parms.w90, pdf.parms.w90, function() {
        make.occurrences.barplot(
            occurrences$btly,
            "Occurrences (Bitly/log scale)", "Walk Length", "Occurrences (log scale)",
            col=colors$walk, log="y")
    })

    filename <- "occurrences-all-bylength-ecdf"
    plot.formats(filename, png.parms.w90, pdf.parms.w90, function() {
        make.ecdfplot(data$okay.L.all$step_count,
                      "Walk Length ECDF", "Walk Length", "ECDF",
                      col=colors$walk)
    })
    filename <- "occurrences-dmoz-bylength-ecdf"
    plot.formats(filename, png.parms.w90, pdf.parms.w90, function() {
        make.ecdfplot(data$okay.L.dmoz$step_count,
                      "Walk Length ECDF (DMOZ)", "Walk Length", "ECDF",
                      col=colors$walk)
    })
    filename <- "occurrences-seng-bylength-ecdf"
    plot.formats(filename, png.parms.w90, pdf.parms.w90, function() {
        make.ecdfplot(data$okay.L.seng$step_count,
                      "Walk Length ECDF (Search Eng.)", "Walk Length", "ECDF",
                      col=colors$walk)
    })
    filename <- "occurrences-deli-bylength-ecdf"
    plot.formats(filename, png.parms.w90, pdf.parms.w90, function() {
        make.ecdfplot(data$okay.L.deli$step_count,
                      "Walk Length ECDF (Delicious)", "Walk Length", "ECDF",
                      col=colors$walk)
    })
    filename <- "occurrences-btly-bylength-ecdf"
    plot.formats(filename, png.parms.w90, pdf.parms.w90, function() {
        make.ecdfplot(data$okay.L.btly$step_count,
                      "Walk Length ECDF (Bitly)", "Walk Length", "ECDF",
                      col=colors$walk)
    })

    cat("----DONE-------------------------------------------------------\n")

}

#===============================================================================
# Utility
#===============================================================================

aggr.statuses <- ordered(c(
        #"1xx", "2xx", "3xx", #
        "403", "404",
        #"4xx", #
        "503",
        #"5xx", #
        #"HTTP xxx",
        "Download failed", #
        "Not HTML", #
        #"Unparsable HTML", #
        "No Common Links",
        "No Unused Links", #
        #"No Mementos", #
        "Other"))

get.aggr.status <- function(status) {
    if (is.na(status)) NA
    else if (status %in% c("200", "403", "404", "503")) status
    #else if (grepl("^1[0-9][0-9]$", status)) "1xx" #
    #else if (grepl("^2[0-9][0-9]$", status)) "2xx" #
    #else if (grepl("^3[0-9][0-9]$", status)) "3xx" #
    #else if (grepl("^4[0-9][0-9]$", status)) "4xx" #
    #else if (grepl("^5[0-9][0-9]$", status)) "5xx" #
    #else if (grepl("^[0-9][0-9][0-9]$", status)) "HTTP xxx" #
    else if (grepl("^ECURL[0-9]+$", status)) "Download failed" #
    else if (status == "NOT_HTML") "Not HTML" #
    #else if (status == "ESOUP") "Unparsable HTML" #
    else if (status == "NO_CLINKS") "No Common Links"
    else if (status == "NO_ULINKS") "No Unused Links" #
    #else if (status == "NO_MEMS") "No Mementos" #
    else "Other"
}

as.character.percent <- function(ratios, threashold=0.01) {
    sapply(
        ratios,
        function(ratio) {
            if (is.nan(ratio))
                NA
            else if (ratio < threashold)
                paste0("<", prettyNum(threashold*100, nsmall=1), "%")
            else
                paste0(prettyNum(round(ratio*100, 1), nsmall=1), "%")
        })
}

#===============================================================================
# barplots
#===============================================================================

make.occurrences.barplot <- function(occurrences, title, xlab, ylab, col, log="") {
    names(occurrences) <- c("1", NA, NA, NA, NA, NA, NA, NA, NA, "10",
                             NA, NA, NA, NA, NA, NA, NA, NA, NA, "20",
                             NA, NA, NA, NA, NA, NA, NA, NA, NA, "30",
                             NA, NA, NA, NA, NA, NA, NA, NA, NA, "40",
                             NA, NA, NA, NA, NA, NA, NA, NA, NA, "50")
    barplot(occurrences, log=log, main=title, xlab=xlab, ylab=ylab, col=col)
}

make.stacked.barplot.factors <- function(data, title, xlab, ylab, col, las=0) {
    m <- par()$mar
    par.save <- par(mar=c(10,m[2],m[3],m[4]))
    barplot(data,
            main=title, xlab=xlab, ylab=ylab, las=las,
            col=col, legend.text=rownames(data), args.legend=list(cex=2/3), beside=F)
    par(par.save)
}

make.stacked.barplot.percentages <- function(data, title, xlab, col, las=0) {
    m <- par()$mar
    par.save <- par(mar=c(10,m[2],m[3],m[4]))
    barplot(data, ylim=c(0.0, 100.0),
            main=title, xlab="", ylab="Percent", las=las,
            #mgp=c(3, 0.5, 0),
            col=col, legend=rownames(data), beside=F)
    mtext(xlab, side=1, line=8)
    par(par.save)
}

#===============================================================================
# ECDF
#===============================================================================

make.ecdfplot <- function(data, title, xlab, ylab, col) {
    e <- ecdf(data)
    xmin <- min(data)
    xmax <- max(data)
    steps <- xmin:xmax
    counts <- as.vector(e(steps))
    plot(steps, counts, ylim=c(0.0, 1.0), xlim=c(xmin, xmax),
         main=title, xlab=xlab, ylab=ylab,
         type="o", pch=19, cex=0.25, col=col, ann=TRUE)
}

#===============================================================================
# Scatter plots
#===============================================================================

make.step.heatplot <- function(
        x, y, title, xlab, ylab, col,
        y.scale, y.unit, y.ticks=y.scale,
        ymin=min(y), ymax=max(y), xmax=max(x),
        groups=40) {
    ymin=floor(ymin / y.scale) * y.scale
    ymax=ceiling(ymax / y.scale) * y.scale
    #cat("ymin =",ymin, "ymax =", ymax,"\n")
    heat.steps <- 5
    breaks=seq(ymin, ymax, length.out=groups+1)
    mids=sapply(1:(length(breaks)-1),
                function(i) (breaks[i] + breaks[i+1]) / 2)
    #cat("breaks ="); print(breaks)
    #cat("mids ="); print(mids)
    heat <- sapply(0:xmax,
                   function(s) hist(y[x==s], breaks=breaks, plot=F)$counts)
    heat <- mapply(function(b,s) c(mids[b], s, heat[b,s+1]),
                   b=sort(rep(1:(length(breaks)-1), xmax+1)),
                   s=0:xmax, SIMPLIFY=F)
    heat <- as.data.frame(do.call(rbind, heat))
    colnames(heat) <- c("mid", "step", "count")
    heat <- heat[heat$count > 0,]
    heat$log <- log2(heat$count)
    #logmax <- ceiling(max(heat$log))
    logmax <- 16.0
    heat$color.index <- lapply(floor(heat$log * (heat.steps / (logmax-1))),
                               function(x) min(x, heat.steps))
    heat$color.index <- floor(heat$log * (heat.steps / (logmax-1)))
    heat.palette <- colorRampPalette(
        c(col,colors$heat0,colors$heat1,colors$heat2))(heat.steps+1)
    heat$color <- heat.palette[heat$color.index+1]
    plot(heat$step, heat$mid, ylim=c(ymin, ymax),
         main=title, xlab=xlab, ylab=ylab,
         axes=FALSE, pch=15, cex=0.75, col=heat$color)
    #abline(lm(y ~ x), col=colors$heat, lwd=1.5)
    axis(1, at=c(min(x),10,20,30,40,max(x)))
    yticks=seq(ymin, ymax, by=y.ticks)
    ylabel=sapply(yticks, function(u)
                  paste0(u / y.scale, if (u != 0) y.unit else ""))
    axis(2, at=yticks, lab=ylabel)
    legend("topright", cex=2/3,
           c("At least 1 memento", "At least 8 mementos",
             "At least 64 mementos", "At least 512 mementos",
             "At least 4,096 mementos", "At least 32,768 mementos"),
           fill=c(heat.palette[1],
                  heat.palette[2],
                  heat.palette[3],
                  heat.palette[4],
                  heat.palette[5],
                  heat.palette[6]))
}

make.choice.heatplot <- function(
        x, y, title, xlab, ylab, col,
        y.scale, y.unit, y.ticks=y.scale,
        ymin=min(y), ymax=max(y), xmax=max(x),
        groups=40) {
    ymin=floor(ymin / y.scale) * y.scale
    ymax=ceiling(ymax / y.scale) * y.scale
    cat("ymin =",ymin, "ymax =", ymax,"\n")
    heat.steps <- 5
    breaks=seq(ymin, ymax, length.out=groups+1)
    mids=sapply(1:(length(breaks)-1),
                function(i) (breaks[i] + breaks[i+1]) / 2)
    #cat("breaks ="); print(breaks)
    #cat("mids ="); print(mids)
    #cat("xmax ="); print(xmax)
    xedges <- ceiling(exp(seq(log(1), log(2^16), length.out = 17)))
    #cat("xedges ="); print(xedges)
    #cat("length(xedges) ="); print(length(xedges))
    heat <- sapply(2:length(xedges),
                   function(i) hist(y[x>=xedges[i-1]&x<xedges[i]], breaks=breaks, plot=F)$counts)
    #print(heat[1:5,])
    heat <- mapply(function(b,s) c(mids[b], s, heat[b,s]),
                   b=sort(rep(1:(length(breaks)-1), length(xedges)-1)),
                   s=1:(length(xedges)-1), SIMPLIFY=F)
    heat <- as.data.frame(do.call(rbind, heat))
    colnames(heat) <- c("mid", "choice", "count")
    #print(heat[1:10,])
    #heat <- heat[heat$count > 0,] # dump the zeros
    heat$log <- log2(heat$count)
    #logmax <- ceiling(max(heat$log))
    logmax <- 16.0
    heat$color.index <- lapply(floor(heat$log * (heat.steps / (logmax-1))),
                               function(x) min(x, heat.steps))
    heat$color.index <- floor(heat$log * (heat.steps / (logmax-1)))
    heat.palette <- colorRampPalette(
        c(col,colors$heat0,colors$heat1,colors$heat2))(heat.steps+1)
    heat$color <- heat.palette[heat$color.index+1]
    m <- par()$mar
    par.save <- par(mar=c(m[1],m[2],m[3],0.5))
    plot(heat$choice, heat$mid, ylim=c(ymin, ymax),
         main=title, xlab=xlab, ylab=ylab,
         axes=FALSE, pch=15, cex=0.75, col=heat$color)
    #abline(lm(y ~ x), col=colors$heat, lwd=1.5)
    #axis(1, at=c(min(x),10,20,30,40,max(x)))
    axis(1, at=c(1, 4, 8, 12, 16),
                lab=c("1", "16", "256", "4Ki", "64Ki"))
    yticks=seq(ymin, ymax, by=y.ticks)
    ylabel=sapply(yticks, function(u)
                  paste0(u / y.scale, if (u != 0) y.unit else ""))
    axis(2, at=yticks, lab=ylabel)
    legend("topright", cex=1/2,
           c("At least 1 link", "At least 8 links",
             "At least 64 links", "At least 512 links",
             "At least 4,096 links", "At least 32,768 links"),
           fill=c(heat.palette[1],
                  heat.palette[2],
                  heat.palette[3],
                  heat.palette[4],
                  heat.palette[5],
                  heat.palette[6]))
    par(par.save)
}

make.scatter.plot <- function(
        x, y, title, xlab, ylab, cols,
        pch=19, cex=1,
        y.scale=days.per.month, y.unit="m", y.ticks=y.scale,
        ymin=min(y), ymax=max(y), log="" ) {
    ymin=floor(floor(ymin / y.scale) * y.scale)
    ymax=ceiling(ceiling(ymax / y.scale) * y.scale)
    plot.new()
    plot.window(xlim=range(x), ylim=c(ymin, ymax))
    title(title, xlab=xlab, ylab=ylab)
    axis(1, at=c(min(x),10,20,30,40,max(x)))
    yticks=seq(ymin, ymax, by=y.ticks)
    ylabel=sapply(yticks, function(days)
                  paste0(days / y.scale, if (days != 0) y.unit else ""))
    axis(2, at=yticks, lab=ylabel)
    plot.xy(xy.coords(x, y), type="p", pch=pch, cex=cex, col=cols)
    #lines(loess.smooth(x, y, family="symmetric"), col=cols)
    abline(lm(y ~ x), col=cols)
}

make.scatter.plot.choice <- function(
        x, y, title, xlab, ylab, cols,
        pch=19, cex=1,
        y.scale=days.per.month, y.unit="m", y.ticks=y.scale,
        ymin=min(y), ymax=max(y) ) {
    ymin=floor(floor(ymin / y.scale) * y.scale)
    ymax=ceiling(ceiling(ymax / y.scale) * y.scale)
    plot.new()
    plot.window(xlim=range(x), ylim=c(ymin, ymax))
    title(title, xlab=xlab, ylab=ylab)
    axis(1, at=c(min(x), 2000, 4000, 6000, 8000, 10000)) #max(x)))
    yticks=seq(ymin, ymax, by=y.ticks)
    ylabel=sapply(yticks, function(days)
                  paste0(days / y.scale, if (days != 0) y.unit else ""))
    axis(2, at=yticks, lab=ylabel)
    plot.xy(xy.coords(x, y), type="p", pch=pch, cex=cex, col=cols, log="x")
    #lines(loess.smooth(x, y, family="symmetric"), col=cols)
    abline(lm(y ~ x), col=cols)
}

make.scatter.plot.choice.2 <- function(
        x, ys, title, xlab, ylab, cols, lcols=cols,
        pch=c(19, 19), cex=c(1/2, 1/2),
        y.scale=days.per.month, y.unit="m", y.ticks=y.scale,
        ymin=min(ys, na.rm=T), ymax=max(ys, na.rm=T) ) {
    spreadx <- function(x) {
        factor <- 10
        cut <- 2000
        sapply(x, function(x) { if (x < cut) x
                                else (x - cut) / factor + cut })
    }
    #cat("make.scatter.plot.2:ymax =", ymax, "\n") #debug
    ymin <- floor(floor(ymin / y.scale) * y.scale)
    ymax <- ceiling(ceiling(ymax / y.scale) * y.scale)
    adjx <- spreadx(x)
    #adjx <- x
    print(range(x))
    print(range(adjx))
    plot.new()
    plot.window(xlim=c(1, max(adjx)), ylim=c(ymin, ymax))
    title(title, xlab=xlab, ylab=ylab)
    xticks <- c(1, 250, 500, 750, 1000, 1250, 1500, 1750, 2000, 5000, 10000)
    xlabs <- c(1, NA, 500, NA, 1000, NA, 1500, NA, "2K", "5K", "10K")
    axis(1, at=spreadx(xticks), labels=xlabs)
    #xticks <- c(1, 2000, 4000, 6000, 8000, 10000)
    #xlabs <- c(1, 2000, 4000, 6000, 8000, 10000)
    #axis(1, at=xticks, labels=xlabs)
    yticks=seq(ymin, ymax, by=y.ticks)
    ylabel=sapply(yticks, function(days)
                  paste0(days / y.scale, if (days != 0) y.unit else ""))
    axis(2, at=yticks, lab=ylabel)
    legend("topright", cex=2/3, c("API", "UI"), fill=lcols)
    plot.xy(xy.coords(adjx, ys[,1]),
            type="p", pch=pch[1], cex=cex[1], col=cols[1])
    abline(lm(ys[,1] ~ adjx), col=cols[1])
    plot.xy(xy.coords(adjx, ys[,2]),
            type="p", pch=pch[2], cex=cex[2], col=cols[2])
    abline(lm(ys[,2] ~ adjx), col=cols[2])
    if (length(ys[1,]) > 2) {
        plot.xy(xy.coords(x, ys[,3]),
                type="p", pch=pch[3], cex=cex[3], col=cols[3])
        abline(lm(ys[,3] ~ x), col=cols[3])
        plot.xy(xy.coords(x, ys[,4]),
                type="p", pch=pch[4], cex=cex[4], col=cols[4])
        abline(lm(ys[,4] ~ x), col=cols[4])
    }
}

make.scatter.plot.domains <- function(
        x, y, title, xlab, ylab, cols,
        pch=19, cex=1,
        y.scale=days.per.month, y.unit="m", y.ticks=y.scale,
        ymin=min(y), ymax=max(y) ) {
    ymin=floor(floor(ymin / y.scale) * y.scale)
    ymax=ceiling(ceiling(ymax / y.scale) * y.scale)
    plot.new()
    plot.window(xlim=range(x), ylim=c(ymin, ymax))
    title(title, xlab=xlab, ylab=ylab)
    axis(1, at=c(min(x), 5, 10, 15, max(x)))
    yticks=seq(ymin, ymax, by=y.ticks)
    ylabel=sapply(yticks, function(days)
                  paste0(days / y.scale, if (days != 0) y.unit else ""))
    axis(2, at=yticks, lab=ylabel)
    plot.xy(xy.coords(x, y), type="p", pch=pch, cex=cex, col=cols, log="x")
    #lines(loess.smooth(x, y, family="symmetric"), col=cols)
    abline(lm(y ~ x), col=cols)
}

make.scatter.plot.domains.2 <- function(
        x, ys, title, xlab, ylab, cols, lcols=cols,
        pch=c(19, 19), cex=c(1, 1),
        y.scale=days.per.month, y.unit="m", y.ticks=y.scale,
        ymin=min(ys, na.rm=T), ymax=max(ys, na.rm=T) ) {
    #cat("make.scatter.plot.2:ymax =", ymax, "\n") #debug
    ymin=floor(floor(ymin / y.scale) * y.scale)
    ymax=ceiling(ceiling(ymax / y.scale) * y.scale)
    #cat("make.scatter.plot.2:ymax =", ymax, " (adjusted)\n") #debug
    plot.new()
    plot.window(xlim=range(x), ylim=c(ymin, ymax))
    title(title, xlab=xlab, ylab=ylab)
    axis(1, at=c(min(x), 5, 10, 15, max(x)))
    yticks=seq(ymin, ymax, by=y.ticks)
    ylabel=sapply(yticks, function(days)
                  paste0(days / y.scale, if (days != 0) y.unit else ""))
    axis(2, at=yticks, lab=ylabel)
    legend("topright", cex=2/3, c("API", "UI"), fill=lcols)
    plot.xy(xy.coords(x, ys[,1]),
            type="p", pch=pch[1], cex=cex[1], col=cols[1])
    #lines(loess.smooth(x, ys[,1], family="symmetric"), col=cols[1])
    abline(lm(ys[,1] ~ x), col=cols[1])
    plot.xy(xy.coords(x, ys[,2]),
            type="p", pch=pch[2], cex=cex[2], col=cols[2])
    #lines(loess.smooth(x, ys[,2], family="symmetric"), col=cols[2])
    abline(lm(ys[,2] ~ x), col=cols[2])
    if (length(ys[1,]) > 2) {
        plot.xy(xy.coords(x, ys[,3]),
                type="p", pch=pch[3], cex=cex[3], col=cols[3])
        #lines(loess.smooth(x, ys[,3], family="symmetric"), col=cols[3])
        abline(lm(ys[,3] ~ x), col=cols[3])
        plot.xy(xy.coords(x, ys[,4]),
                type="p", pch=pch[4], cex=cex[4], col=cols[4])
        #lines(loess.smooth(x, ys[,4], family="symmetric"), col=cols[4])
        abline(lm(ys[,4] ~ x), col=cols[4])
    }
}

make.scatter.plot.2 <- function(
        x, ys, title, xlab, ylab, cols, lcols=cols,
        pch=c(19, 19), cex=c(1, 1),
        y.scale=days.per.month, y.unit="m",
        ymin=min(ys, na.rm=T), ymax=max(ys, na.rm=T) ) {
    #cat("make.scatter.plot.2:ymax =", ymax, "\n") #debug
    ymin=floor(floor(ymin / y.scale) * y.scale)
    ymax=ceiling(ceiling(ymax / y.scale) * y.scale)
    #cat("make.scatter.plot.2:ymax =", ymax, " (adjusted)\n") #debug
    plot.new()
    plot.window(xlim=range(x), ylim=c(ymin, ymax))
    title(title, xlab=xlab, ylab=ylab)
    axis(1, at=c(min(x),10,20,30,40,max(x)))
    yticks=seq(floor(ymin / y.scale) * y.scale,
               ceiling(ymax / y.scale) * y.scale,
               by=y.scale)
    ylabel=sapply(yticks, function(days)
                  paste0(days / y.scale, if (days != 0) y.unit else ""))
    axis(2, at=yticks, lab=ylabel)
    legend("topright", cex=2/3, c("API", "UI"), fill=lcols)
    plot.xy(xy.coords(x, ys[,1]),
            type="p", pch=pch[1], cex=cex[1], col=cols[1])
    #lines(loess.smooth(x, ys[,1], family="symmetric"), col=cols[1])
    abline(lm(ys[,1] ~ x), col=cols[1])
    plot.xy(xy.coords(x, ys[,2]),
            type="p", pch=pch[2], cex=cex[2], col=cols[2])
    #lines(loess.smooth(x, ys[,2], family="symmetric"), col=cols[2])
    abline(lm(ys[,2] ~ x), col=cols[2])
    if (length(ys[1,]) > 2) {
        plot.xy(xy.coords(x, ys[,3]),
                type="p", pch=pch[3], cex=cex[3], col=cols[3])
        #lines(loess.smooth(x, ys[,3], family="symmetric"), col=cols[3])
        abline(lm(ys[,3] ~ x), col=cols[3])
        plot.xy(xy.coords(x, ys[,4]),
                type="p", pch=pch[4], cex=cex[4], col=cols[4])
        #lines(loess.smooth(x, ys[,4], family="symmetric"), col=cols[4])
        abline(lm(ys[,4] ~ x), col=cols[4])
    }
}

#===============================================================================
# Graph devices
#===============================================================================

make.basepath <- function(dir, name, suffix="") {
    if (length(suffix[0]) > 0) {
        #file.path(dir, paste0("randomwalk-", name, "-", suffix[0]))
        file.path(dir, paste0(name, "-", suffix[0]))
    } else {
        #file.path(dir, paste0("randomwalk-", name))
        file.path(dir, name)
    }
}

open.png <- function(basepath, png.parms) {
    filename <- paste0(basepath, "-", as.character(png.parms$dpi), ".png")
    cat("Plotting", filename, "\n")
    #cat("          width =", png.parms$width.in, "in, ",
    #    "height =", png.parms$height.in, "in, dpi=", png.parms$dpi, "\n")
    png(filename=filename, width=png.parms$width.in,
        height=png.parms$height.in, units="in",
        res=png.parms$dpi, pointsize=png.parms$points, bg="transparent")
    filename
}

open.pdf <- function(basepath, pdf.parms) {
    filename <- paste0(basepath, ".pdf")
    cat("Plotting", filename, "\n")
    #cat("          width =", pdf.parms$width.in, "in, ",
    #     height =", pdf.parms$height.in, "\n")
    pdf(file=filename, width=pdf.parms$width.in,
        height=pdf.parms$height.in,
        pointsize=pdf.parms$points, bg="transparent")
    filename
}

#===============================================================================
# Plot both the PNG and PDF
#===============================================================================

plot.formats <- function(filename, png.parms.w90, pdf.parms.w90, plot.f) {
    #open.png(make.basepath(chartdir, filename), png.parms.w90)
    #plot.f()
    #dev.off()
    open.pdf(make.basepath(chartdir, filename), pdf.parms.w90)
    plot.f()
    dev.off()
}

#=======================================================================
print.subset.sizes <- function(data) {
#=======================================================================
    fmt <- "data$%-11s  %8s\n"
    cat(sprintf("-----subset-----  --rows--\n"))
    lapply(names(data),
           FUN=function(name) {
               d=data[[name]]
               cat(sprintf(fmt, name, prettyNum(NROW(d), big.mark=",")))
           })
    cat("----------------  --------\n")
}

#===============================================================================
# Text file output
#===============================================================================

write.data.frame <- function(data, tabledir, filename) {
    filename <- paste0(make.basepath(tabledir, filename), ".txt")
    scipen <- getOption("scipen")
    options(scipen=10)
    cat("writing ", filename, "\n")
    write.table(data, file=filename, row.names=F, col.names=T, sep="\t", quote=F)
    options(scipen=scipen)
}

#===============================================================================
# Initialization and stand-alone run
#===============================================================================

if (substr(program,1,4) != "Rgui") {

    # Get the command line arguments
    args <- commandArgs(TRUE)
    if (length(args) < 1) {
        cat("usage: randomwalk-timeline.R infile.txt\n")
        quit(status=1)
    }
    
    # Check that the input file exists
    data_filename <- normalizePath(args[1])
    cat("data_filename =", data_filename, "\n")
    if (file.access(data_filename, 4) == -1) {
        cat(data_filename, "does not exists\n")
        quit(status=1)
    }
    
    # Compute the RData file name
    data_rdata_filename <- paste0(data_filename, "-data.RData")
    walk.stats_rdata_filename <- paste0(data_filename, "-walkstats.RData")
    step.stats_rdata_filename <- paste0(data_filename, "-stepstats.RData")
    choice.stats_rdata_filename <- paste0(data_filename, "-choicestats.RData")
    domains.stats_rdata_filename <- paste0(data_filename, "-domainsstats.RData")
    cat("data_rdata_filename =", data_rdata_filename, "\n")
    cat("walk.stats_rdata_filename =", walk.stats_rdata_filename, "\n")
    cat("step.stats_rdata_filename =", step.stats_rdata_filename, "\n")
    cat("choice.stats_rdata_filename =", choice.stats_rdata_filename, "\n")
    cat("domains.stats_rdata_filename =", domains.stats_rdata_filename, "\n")

    # Construct the PNG basename
    chartdir <- file.path(dirname(data_filename), "charts")
    tabledir <- file.path(dirname(data_filename), "tables")
    cat("chartdir = ", chartdir, "\n")
    cat("tabledir = ", tabledir, "\n")
    
    # Setup the data and statistics
    data <- load.drift.data(data_filename, data_rdata_filename)
    data <- create.subsets(data)
    step.stats <- calculate.step.stats(data, data_filename, step.stats_rdata_filename)
    choice.stats <- calculate.choice.stats(data, data_filename, choice.stats_rdata_filename)
    domains.stats <- calculate.domains.stats(data, data_filename, domains.stats_rdata_filename)
    #walk.stats <- calculate.walk.stats(data, data_filename, walk.stats_rdata_filename)

    # Create the plots
    create.tables.and.charts(data, step.stats, walk.stats, chartdir, tabledir)
    quit(status=0)
    
}

#end