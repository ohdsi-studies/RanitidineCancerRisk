
# Load data from data folder:
loadFile <- function(file) {
  # file = files[13]
  tableName <- gsub("(_t[0-9]+_c[0-9]+)|(_)[^_]*\\.rds", "", file) 
  camelCaseName <- SqlRender::snakeCaseToCamelCase(tableName)
  if (!(tableName %in% splittableTables)) {
    newData <- readRDS(file.path(dataFolder, file))
    newData <- data.frame(lapply(newData, function(x){
      if(is.factor(x)) {
        as.character(x)
      } else {x}
    }), stringsAsFactors=FALSE)
    colnames(newData) <- SqlRender::snakeCaseToCamelCase(colnames(newData))
    if (exists(camelCaseName, envir = .GlobalEnv)) {
      existingData <- get(camelCaseName, envir = .GlobalEnv)
      newData <- rbind(existingData, newData)
    }
    assign(camelCaseName, newData, envir = .GlobalEnv)
  }
  invisible(NULL)
}

getBalance <- function(databaseId,
                       dataFolder,
                       targetId,
                       comparatorId,
                       analysisId,
                       outcomeId = NULL){
  pathToRds<-file.path(dataFolder,
                       sprintf("covariate_balance_t%d_c%d_%s.rds",targetId,comparatorId,databaseId))
  balance <- readRDS(pathToRds)
  
  pathToRds<-file.path(dataFolder,
                       sprintf("covariate_%s.rds",databaseId))
  covariate <- readRDS(pathToRds)
  
  colnames(balance)<-SqlRender::snakeCaseToCamelCase(colnames(balance))
  colnames(covariate)<-SqlRender::snakeCaseToCamelCase(colnames(covariate))
  
  if(is.null(outcomeId)){
    balance <- balance[balance$analysisId == analysisId, ]
  }else{
    balance <- balance[balance$analysisId == analysisId & balance$outcomeId == outcomeId, ]
  }
  
  covariate <- covariate[covariate$analysisId == analysisId,]
  balance <- merge(balance, covariate[,c("covariateId", "covariateAnalysisId", "covariateName")])
  balance <- balance[ c("covariateId",
                        "covariateName",
                        "covariateAnalysisId", 
                        "targetMeanBefore", 
                        "comparatorMeanBefore", 
                        "stdDiffBefore", 
                        "targetMeanAfter", 
                        "comparatorMeanAfter",
                        "stdDiffAfter")]
  colnames(balance) <- c("covariateId",
                         "covariateName",
                         "analysisId",
                         "beforeMatchingMeanTreated",
                         "beforeMatchingMeanComparator",
                         "beforeMatchingStdDiff",
                         "afterMatchingMeanTreated",
                         "afterMatchingMeanComparator",
                         "afterMatchingStdDiff")
  balance$absBeforeMatchingStdDiff <- abs(balance$beforeMatchingStdDiff)
  balance$absAfterMatchingStdDiff <- abs(balance$afterMatchingStdDiff)
  return(balance)
}

plotCovariateBalanceScatterPlot <- function(balance, 
                                            beforeLabel = "Before stratification", 
                                            afterLabel = "After stratification",
                                            limits = NULL) {
  if(is.null(limits)){limits <- c(min(c(balance$absBeforeMatchingStdDiff, balance$absAfterMatchingStdDiff),
                                      na.rm = TRUE),
                                  max(c(balance$absBeforeMatchingStdDiff, balance$absAfterMatchingStdDiff),
                                      na.rm = TRUE))}
  theme <- ggplot2::element_text(colour = "#000000", size = 12)
  plot <- ggplot2::ggplot(balance, ggplot2::aes(x = absBeforeMatchingStdDiff, y = absAfterMatchingStdDiff)) +
    ggplot2::geom_point(color = rgb(0, 0, 0.8, alpha = 0.3), shape = 16, size = 2) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::geom_vline(xintercept = 0) +
    ggplot2::scale_x_continuous(beforeLabel, limits = limits) +
    ggplot2::scale_y_continuous(afterLabel, limits = limits) +
    ggplot2::theme(text = theme)
  
  return(plot)
}

prepareTable1 <- function(balance,
                          beforeLabel = "Before stratification",
                          afterLabel = "After stratification",
                          targetLabel = "Target",
                          comparatorLabel = "Comparator",
                          percentDigits = 1,
                          stdDiffDigits = 2,
                          output = "latex",
                          pathToCsv = "Table1Specs.csv") {
  if (output == "latex") {
    space <- " "
  } else {
    space <- "&nbsp;"
  }
  specifications <- read.csv(pathToCsv, stringsAsFactors = FALSE)
  
  fixCase <- function(label) {
    idx <- (toupper(label) == label)
    if (any(idx)) {
      label[idx] <- paste0(substr(label[idx], 1, 1),
                           tolower(substr(label[idx], 2, nchar(label[idx]))))
    }
    return(label)
  }
  
  formatPercent <- function(x) {
    result <- format(round(100 * x, percentDigits), digits = percentDigits + 1, justify = "right")
    result <- gsub("^-", "<", result)
    result <- gsub("NA", "", result)
    result <- gsub(" ", space, result)
    return(result)
  }
  
  formatStdDiff <- function(x) {
    result <- format(round(x, stdDiffDigits), digits = stdDiffDigits + 1, justify = "right")
    result <- gsub("NA", "", result)
    result <- gsub(" ", space, result)
    return(result)
  }
  
  resultsTable <- data.frame()
  for (i in 1:nrow(specifications)) {
    if (specifications$analysisId[i] == "") {
      resultsTable <- rbind(resultsTable,
                            data.frame(Characteristic = specifications$label[i], value = ""))
    } else {
      idx <- balance$analysisId == specifications$analysisId[i]
      if (any(idx)) {
        if (specifications$covariateIds[i] != "") {
          covariateIds <- as.numeric(strsplit(specifications$covariateIds[i], ";")[[1]])
          idx <- balance$covariateId %in% covariateIds
        } else {
          covariateIds <- NULL
        }
        if (any(idx)) {
          balanceSubset <- balance[idx, ]
          if (is.null(covariateIds)) {
            balanceSubset <- balanceSubset[order(balanceSubset$covariateId), ]
          } else {
            balanceSubset <- merge(balanceSubset, data.frame(covariateId = covariateIds,
                                                             rn = 1:length(covariateIds)))
            balanceSubset <- balanceSubset[order(balanceSubset$rn, balanceSubset$covariateId), ]
          }
          balanceSubset$covariateName <- fixCase(gsub("^.*: ", "", balanceSubset$covariateName))
          if (specifications$covariateIds[i] == "" || length(covariateIds) > 1) {
            resultsTable <- rbind(resultsTable, data.frame(Characteristic = specifications$label[i],
                                                           beforeMatchingMeanTreated = NA,
                                                           beforeMatchingMeanComparator = NA,
                                                           beforeMatchingStdDiff = NA,
                                                           afterMatchingMeanTreated = NA,
                                                           afterMatchingMeanComparator = NA,
                                                           afterMatchingStdDiff = NA,
                                                           stringsAsFactors = FALSE))
            resultsTable <- rbind(resultsTable, data.frame(Characteristic = paste0(space,
                                                                                   space,
                                                                                   space,
                                                                                   space,
                                                                                   balanceSubset$covariateName),
                                                           beforeMatchingMeanTreated = balanceSubset$beforeMatchingMeanTreated,
                                                           beforeMatchingMeanComparator = balanceSubset$beforeMatchingMeanComparator,
                                                           beforeMatchingStdDiff = balanceSubset$beforeMatchingStdDiff,
                                                           afterMatchingMeanTreated = balanceSubset$afterMatchingMeanTreated,
                                                           afterMatchingMeanComparator = balanceSubset$afterMatchingMeanComparator,
                                                           afterMatchingStdDiff = balanceSubset$afterMatchingStdDiff,
                                                           stringsAsFactors = FALSE))
          } else {
            resultsTable <- rbind(resultsTable, data.frame(Characteristic = specifications$label[i],
                                                           beforeMatchingMeanTreated = balanceSubset$beforeMatchingMeanTreated,
                                                           beforeMatchingMeanComparator = balanceSubset$beforeMatchingMeanComparator,
                                                           beforeMatchingStdDiff = balanceSubset$beforeMatchingStdDiff,
                                                           afterMatchingMeanTreated = balanceSubset$afterMatchingMeanTreated,
                                                           afterMatchingMeanComparator = balanceSubset$afterMatchingMeanComparator,
                                                           afterMatchingStdDiff = balanceSubset$afterMatchingStdDiff,
                                                           stringsAsFactors = FALSE))
          }
        }
      }
    }
  }
  resultsTable$beforeMatchingMeanTreated <- formatPercent(resultsTable$beforeMatchingMeanTreated)
  resultsTable$beforeMatchingMeanComparator <- formatPercent(resultsTable$beforeMatchingMeanComparator)
  resultsTable$beforeMatchingStdDiff <- formatStdDiff(resultsTable$beforeMatchingStdDiff)
  resultsTable$afterMatchingMeanTreated <- formatPercent(resultsTable$afterMatchingMeanTreated)
  resultsTable$afterMatchingMeanComparator <- formatPercent(resultsTable$afterMatchingMeanComparator)
  resultsTable$afterMatchingStdDiff <- formatStdDiff(resultsTable$afterMatchingStdDiff)
  
  headerRow <- as.data.frame(t(rep("", ncol(resultsTable))))
  colnames(headerRow) <- colnames(resultsTable)
  headerRow$beforeMatchingMeanTreated <- targetLabel
  headerRow$beforeMatchingMeanComparator <- comparatorLabel
  headerRow$afterMatchingMeanTreated <- targetLabel
  headerRow$afterMatchingMeanComparator <- comparatorLabel
  
  subHeaderRow <- as.data.frame(t(rep("", ncol(resultsTable))))
  colnames(subHeaderRow) <- colnames(resultsTable)
  subHeaderRow$Characteristic <- "Characteristic"
  subHeaderRow$beforeMatchingMeanTreated <- "%"
  subHeaderRow$beforeMatchingMeanComparator <- "%"
  subHeaderRow$beforeMatchingStdDiff <- "Std. diff"
  subHeaderRow$afterMatchingMeanTreated <- "%"
  subHeaderRow$afterMatchingMeanComparator <- "%"
  subHeaderRow$afterMatchingStdDiff <- "Std. diff"
  
  resultsTable <- rbind(headerRow, subHeaderRow, resultsTable)
  
  colnames(resultsTable) <- rep("", ncol(resultsTable))
  colnames(resultsTable)[2] <- beforeLabel
  colnames(resultsTable)[5] <- afterLabel
  return(resultsTable)
}

plotKaplanMeier <- function(kaplanMeier, 
                            targetName, 
                            comparatorName,
                            ylims = NULL,
                            xBreaks = NULL,
                            targetColor = NULL,
                            comparatorColor = NULL,
                            targetColorFill = NULL,
                            comparatorColorFill = NULL,
                            pValue = NULL,
                            title = NULL,
                            yLabel = NULL,
                            medianIqr = NULL) {
  data <- rbind(data.frame(time = kaplanMeier$time,
                           s = kaplanMeier$targetSurvival,
                           lower = kaplanMeier$targetSurvivalLb,
                           upper = kaplanMeier$targetSurvivalUb,
                           strata = paste0(" ", targetName, "    ")),
                data.frame(time = kaplanMeier$time,
                           s = kaplanMeier$comparatorSurvival,
                           lower = kaplanMeier$comparatorSurvivalLb,
                           upper = kaplanMeier$comparatorSurvivalUb,
                           strata = paste0(" ", comparatorName)))
  
  if(is.null(xBreaks)){
    xBreaks <- kaplanMeier$time[!is.na(kaplanMeier$targetAtRisk)]
  }else{
      xBreaks <- xBreaks[xBreaks %in% kaplanMeier$time[!is.na(kaplanMeier$targetAtRisk)]]
  }
  #xlims <- c(-max(data$time)/40, max(data$time))
  xlims <- c(-max(xBreaks)/40, max(xBreaks))
  
  if(is.null(ylims)){
    ylims <- c(min(data$lower), max(data$upper))
  }
  xLabel <- "Follow-Up Duration (Days)"
  if(is.null(yLabel)) yLabel <- "Cumulative Incidence"
  
  if(is.null(targetColor)) targetColor = rgb(0.8, 0, 0, alpha = 0.8)
  if(is.null(comparatorColor)) comparatorColor = rgb(0, 0, 0.8, alpha = 0.8)
  if(is.null(targetColorFill)) targetColorFill = rgb(0.8, 0, 0, alpha = 0.3)
  if(is.null(comparatorColorFill)) comparatorColorFill = rgb(0, 0, 0.8, alpha = 0.3)
  
  plot <- ggplot2::ggplot(data, ggplot2::aes(x = time,
                                             y = s,
                                             color = strata,
                                             fill = strata,
                                             ymin = lower,
                                             ymax = upper)) +
    ggplot2::geom_ribbon(color = rgb(0, 0, 0, alpha = 0)) +
    ggplot2::geom_step(size = 1) +
    ggplot2::scale_color_manual(values = c(rgb(255/255,99/255,71/255, alpha = 0.8),
                                           rgb(30/255,144/255,255/255, alpha = 0.8))) +
    ggplot2::scale_fill_manual(values = c(rgb(255/255,99/255,71/255, alpha = 0.3),
                                          rgb(30/255,144/255,255/255, alpha = 0.3))) +
    # ggplot2::scale_color_manual(values = c(rgb(0.8, 0, 0, alpha = 0.8),
    #                                        rgb(0, 0, 0.8, alpha = 0.8))) +
    # ggplot2::scale_fill_manual(values = c(rgb(0.8, 0, 0, alpha = 0.3),
    #                                       rgb(0, 0, 0.8, alpha = 0.3))) +
    ggplot2::scale_x_continuous(xLabel, limits = xlims, breaks = xBreaks) +
    ggplot2::scale_y_continuous(yLabel, limits = ylims) +
    theme_bw()+ #remove background
    theme (panel.border = element_blank(), axis.line = element_line())+#only x and y axis, not box
    theme (panel.grid.major.x = element_blank() , #remove vertical grid line
           panel.grid.minor.x = element_blank()  #remove vertical grid line
    )+
    ggplot2::theme(legend.title = ggplot2::element_blank(),
                   legend.position = "top",
                   legend.key.size = ggplot2::unit(1, "lines"),
                   plot.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::theme(axis.title.y = ggplot2::element_text(vjust = -10)) 
  
  if(!is.null(pValue)){
    plot <- plot+ggplot2::annotate("text", label = pValue, parse =T,
                                   x=Inf,y=-Inf,hjust=2,vjust=-2, color = "black")
  }
  if(!is.null(title)){
    plot <- plot+ggplot2::ggtitle(title)
  }
  
  if(is.null(xBreaks)){
    targetAtRisk <- kaplanMeier$targetAtRisk[!is.na(kaplanMeier$targetAtRisk)]
    comparatorAtRisk <- kaplanMeier$comparatorAtRisk[!is.na(kaplanMeier$comparatorAtRisk)]
  } else {
    targetAtRisk <- kaplanMeier$targetAtRisk[(!is.na(kaplanMeier$targetAtRisk))&(kaplanMeier$time%in%xBreaks)]
    comparatorAtRisk <- kaplanMeier$comparatorAtRisk[!is.na(kaplanMeier$comparatorAtRisk)&(kaplanMeier$time%in%xBreaks)]
  }
  
  labels <- data.frame(x = c(0, xBreaks, xBreaks),
                       y = as.factor(c("Number at risk",
                                       rep(targetName, length(xBreaks)),
                                       rep(comparatorName, length(xBreaks)))),
                       label = c("",
                                 formatC(targetAtRisk, big.mark = ",", mode = "integer"),
                                 formatC(comparatorAtRisk, big.mark = ",", mode = "integer")))
  labels$y <- factor(labels$y, levels = c(comparatorName, targetName, "Number at risk"))
  dataTable <- ggplot2::ggplot(labels, 
                               ggplot2::aes(x = x, y = y, label = label)
                               ) + 
    ggplot2::geom_text(size = 3.5, vjust = 0.5) + 
    ggplot2::scale_x_continuous(xLabel,
                                limits = xlims,
                                breaks = xBreaks) + 
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   legend.position = "none",
                   panel.border = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_text(color = "white"),
                   axis.title.x = ggplot2::element_text(color = "white"),
                   axis.title.y = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_line(color = "white")
                   )
  plots <- list(plot, dataTable)
  grobs <- widths <- list()
  for (i in 1:length(plots)) {
    grobs[[i]] <- ggplot2::ggplotGrob(plots[[i]])
    widths[[i]] <- grobs[[i]]$widths[2:5]
  }
  maxwidth <- do.call(grid::unit.pmax, widths)
  for (i in 1:length(grobs)) {
    grobs[[i]]$widths[2:5] <- as.list(maxwidth)
  }
  plot <- gridExtra::grid.arrange(grobs[[1]], grobs[[2]], heights = c(400, 100))
  
  
  return(plot)
}

doMeta <- function(data,
                   targetId = 874,
                   comparatorId = 929,
                   outcomeId = 1236,
                   analysisId = 1,
                   targetName = "ticagrelor",
                   comparatorName = "clopidogrel",
                   outcomeName = "GI bleeding",
                   calibration=F){
  data <- data %>% filter(.data$targetId == !!targetId,
                          .data$comparatorId == !!comparatorId,
                          .data$outcomeId == !!outcomeId,
                          .data$analysisId == !!analysisId)
  databaseIds <- data$databaseId
  if(calibration){
    logRr = data$calibratedLogRr
    logLb95Ci = log( data$calibratedCi95Lb )
    logUb95Ci = log( data$calibratedCi95Ub )
    seLogRr = data$calibratedSeLogRr
  }else{
    logRr = data$logRr
    logLb95Ci = log( data$ci95Lb )
    logUb95Ci = log( data$ci95Ub )
    #seLogRr = (logUb95Ci-logLb95Ci) / (2 * qnorm(0.975))
    seLogRr = data$seLogRr
  }
  
  meta <- meta::metagen(TE = logRr, 
                        seTE = seLogRr, 
                        studlab = databaseIds, 
                        sm = "HR", 
                        hakn = FALSE,
                        comb.fixed = TRUE,
                        comb.random = TRUE
  )
  
  meta$n.e <- data$targetSubjects
  meta$event.e <- data$targetOutcomes
  meta$event.rate.t <- round(with(data, targetOutcomes/(targetDays/365))*1000,1)
  meta$person.year.t <- with(data, round((targetDays/365),0))
  
  meta$n.c <- data$comparatorSubjects
  meta$event.c <- data$comparatorOutcomes
  meta$event.rate.c <- round(with(data, comparatorOutcomes/(comparatorDays/365))*1000,1)
  meta$person.year.c<-with(data, round((comparatorDays/365),0))
  
  return (list(meta=meta,
               databaseIds = databaseIds,
               targetId = targetId,
               comparatorId = comparatorId,
               outcomeId = outcomeId,
               analysisId = analysisId,
               targetName = targetName,
               comparatorName = comparatorName,
               outcomeName = outcomeName
  ))
}

forestPlotGenerator<-function(metaResult,
                              space = "             ",
                              limited = F,
                              subgroup = F){
  meta=metaResult$meta
  targetName = metaResult$targetName
  comparatorName = metaResult$comparatorName
  ### Initial setting for jama
  #meta::settings.meta("jama") #meta::settings.meta("meta")
  
  ### decrease margins so the full space is used
  #par(mar=c(5,5,1,2))
  
  xLim= ceiling(max (1/exp(meta$lower.random),exp(meta$upper.random)))
  if(limited){
    leftCols = c("studlab",
                 "effect","ci")
    leftLabs= c("Source", "HR","95% CI")
  }else{
    leftCols = c("studlab",
                 "n.e","event.e" ,#"event.rate.t", 
                 "n.c","event.c",#"event.rate.c",
                 "effect","ci"#,"HR"
    )
    leftLabs= c("Source", "Total","Event","Total","Event","HR","95% CI")
  }
  if(!subgroup){
    forestPlot<-meta::forest.meta(meta,
                                  studlab=TRUE,
                                  
                                  #overall=TRUE,
                                  #pooled.totals=meta$comb.random, 
                                  pooled.total =TRUE,
                                  pooled.events=TRUE,
                                  leftcols = leftCols,
                                  rightcols=F,
                                  #col.study="black",
                                  #col.square="gray",
                                  #col.inside="white",
                                  #col.diamond="gray",
                                  #col.diamond.lines="black",
                                  leftlabs = leftLabs,
                                  lab.e = paste0(space,targetName),
                                  lab.c = paste0(space,comparatorName),
                                  lab.e.attach.to.col = c("n.e"),
                                  lab.c.attach.to.col = c("n.c"),
                                  # leftlabs = c("Event rate", "Event rate"#, "HR (95% CI)"
                                  #              ),
                                  # rightcols = c("w.random"),
                                  fontsize=12,
                                  comb.fixed = FALSE,
                                  comb.random = TRUE,
                                  text.random = "Overall",
                                  col.diamond.random = "royalblue",
                                  col.diamond.lines = "black",
                                  #xlab = "Hazard Ratio (95% CI)",
                                  
                                  digits = 2,
                                  digits.pval =3,
                                  digits.I2 = 1,
                                  just.studlab="left",
                                  #just.addcols ="right",
                                  just.addcols.left= "right",
                                  #just.addcols.right= "right",
                                  just = "center",
                                  xlim = c(round(1/xLim,2),xLim),
                                  plotwidth ="8cm",
                                  #layout="JAMA",
                                  spacing =1,
                                  addrow.overall=TRUE,
                                  print.I2 = TRUE,
                                  # overall.hetstat = T,
                                  # hetstat= F,
                                  # print.I2=F,
                                  print.pval.I2=F,
                                  print.tau2 = F,
                                  # print.Q	=F,
                                  print.pval.Q = F,
                                  # zero.pval = F,
                                  # print.Rb  = F,
                                  #smlab = "",
                                  #sortvar=TE,
                                  label.lef = sprintf("Favors\n%s",targetName),#capitalize(targetName)),
                                  label.right = sprintf("Favors\n%s",comparatorName),#capitalize(comparatorName)),
                                  scientific.pval = F,#meta::gs("scientific.pval"), 
                                  big.mark =","#meta::gs("big.mark),
                                  
    )
  }else{
    forestPlot<-meta::forest.meta(meta,
                                  studlab=TRUE,
                                  pooled.total =TRUE,
                                  pooled.events=TRUE,
                                  leftcols = leftCols,
                                  rightcols=F,
                                  leftlabs = leftLabs,
                                  lab.e = paste0(space,targetName),
                                  lab.c = paste0(space,comparatorName),
                                  lab.e.attach.to.col = c("n.e"),
                                  lab.c.attach.to.col = c("n.c"),
                                  fontsize=12,
                                  comb.fixed = FALSE,
                                  comb.random = TRUE,
                                  text.random = "Overall",
                                  col.diamond.random = "royalblue",
                                  col.diamond.lines = "black",
                                  digits = 2,
                                  digits.pval =3,
                                  digits.I2 = 1,
                                  just.studlab="left",
                                  just.addcols.left= "right",
                                  just = "center",
                                  xlim = c(round(1/xLim,2),xLim),
                                  plotwidth ="8cm",
                                  spacing =1,
                                  addrow.overall=TRUE,
                                  print.I2 = TRUE,
                                  print.pval.I2=F,
                                  print.tau2 = F,
                                  print.pval.Q = F,
                                  label.lef = sprintf("Favors\n%s",targetName),#capitalize(targetName)),
                                  label.right = sprintf("Favors\n%s",comparatorName),#capitalize(comparatorName)),
                                  scientific.pval = F,#meta::gs("scientific.pval"), 
                                  big.mark =","#meta::gs("big.mark),
    )
  }
  
  
  
  
  # forestPlot<-meta::forest.meta(meta,
  #                               studlab=TRUE,
  #                               #overall=TRUE,
  #                               #pooled.totals=meta$comb.random, 
  #                               pooled.events=TRUE,
  #                               leftcols = c("studlab",
  #                                            "n.e","event.e" ,#"event.rate.t", 
  #                                            "n.c","event.c",#"event.rate.c",
  #                                            "effect","ci"#,"HR"
  #                               ),
  #                               #col.study="black",
  #                               #col.square="gray",
  #                               #col.inside="white",
  #                               #col.diamond="gray",
  #                               #col.diamond.lines="black",
  #                               lab.e = paste0(space,targetName),
  #                               lab.c = paste0(space,comparatorName),
  #                               lab.e.attach.to.col = c("n.e"),
  #                               lab.c.attach.to.col = c("n.c"),
  #                               # leftlabs = c("Event rate", "Event rate"#, "HR (95% CI)"
  #                               #              ),
  #                               # rightcols = c("w.random"),
  #                               digits = 3,
  #                               comb.fixed = FALSE,
  #                               comb.random = TRUE,
  #                               text.random = "Overall",
  #                               xlab = "Hazard Ratio (95% CI)",
  #                               just.studlab="left",
  #                               just.addcols ="right",
  #                               xlim = c(0.5,2),
  #                               plotwidth ="8cm",
  #                               layout="JAMA",
  #                               spacing =1,
  #                               addrow.overall=TRUE,
  #                               test.overall.random =F,
  #                               overall.hetstat = F,
  #                               print.tau2 = F,
  #                               print.Q	=F,
  #                               print.pval.Q = F,
  #                               label.lef = sprintf("Favors\n%s",capitalize(targetName)),
  #                               label.right = sprintf("Favors\n%s",capitalize(comparatorName)),
  #                               scientific.pval = T,#meta::gs("scientific.pval"), 
  #                               big.mark =" "#meta::gs("big.mark)
  # )
  return(forestPlot)
}


plotPs <- function(ps, targetName, comparatorName,
                   showEquiposeLabel = TRUE, equipoiseBounds = c(0.3,0.7), 
                   fileName = NULL) {
  psOrigin <- ps
  ps <- rbind(data.frame(x = ps$preferenceScore, y = ps$targetDensity, group = targetName),
              data.frame(x = ps$preferenceScore, y = ps$comparatorDensity, group = comparatorName))
  ps$group <- factor(ps$group, levels = c(as.character(targetName), as.character(comparatorName)))
  theme <- ggplot2::element_text(colour = "#000000", size = 12, margin = ggplot2::margin(0, 0.5, 0, 0.1, "cm"))
  plot <- ggplot2::ggplot(ps,
                          ggplot2::aes(x = x, y = y, color = group, group = group, fill = group)) +
    ggplot2::geom_density(stat = "identity") +
    ggplot2::scale_fill_manual(values = c(rgb(0.8, 0, 0, alpha = 0.5),
                                          rgb(0, 0, 0.8, alpha = 0.5))) +
    ggplot2::scale_color_manual(values = c(rgb(0.8, 0, 0, alpha = 0.5),
                                           rgb(0, 0, 0.8, alpha = 0.5))) +
    ggplot2::scale_x_continuous("Preference score", limits = c(0, 1)) +
    ggplot2::scale_y_continuous("Density") +
    ggplot2::theme(legend.title = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   legend.position = "top",
                   legend.text = theme,
                   axis.text = theme,
                   axis.title = theme)
  if (showEquiposeLabel) {
    labelsLeft <- c()
    labelsRight <- c()
    if (showEquiposeLabel) {
      equiIndex <- psOrigin$preferenceScore>=equipoiseBounds[1] & psOrigin$preferenceScore<=equipoiseBounds[2]
      equipoise <- mean (sum(psOrigin$targetDensity[equiIndex]), sum(psOrigin$comparatorDensity[equiIndex]))/100
      labelsRight <- c(labelsRight, sprintf("%2.1f%% is in equipoise", 
                                            equipoise * 100))
    }
    if (length(labelsLeft) > 0) {
      dummy <- data.frame(text = paste(labelsLeft, collapse = "\n"))
      plot <- plot + ggplot2::geom_label(x = 0, #y = max(d$y) * 1.24, 
                                         hjust = "left", vjust = "top", alpha = 0.8, 
                                         ggplot2::aes(label = text), data = dummy, size = 3.5)
    }
    if (length(labelsRight) > 0) {
      dummy <- data.frame(text = paste(labelsRight, collapse = "\n"))
      plot <- plot + ggplot2::annotate("label", x = 1, y = max(ps$y) * 1, 
                                       hjust = "right", vjust = "top", 
                                       alpha = 0.8, 
                                       label = labelsRight,
                                       #ggplot2::aes(label = labelsRight),
                                       #ggplot2::aes(label = text), data = dummy, 
                                       size = 3.5)
      # plot <- plot + ggplot2::geom_label(x = 1, y = max(ps$y) * 1.24, 
      #                                    hjust = "right", vjust = "top", 
      #                                    alpha = 0.8, 
      #                                    ggplot2::aes(label = labelsRight),
      #                                    ggplot2::aes(label = text), data = dummy, 
      #                                    size = 3.5)
    }
  }
  if (!is.null(fileName)) 
    ggplot2::ggsave(fileName, plot, width = 5, height = 3.5, 
                    dpi = 400)
  return(plot)
}

# uncapitalize <- function(x) {
#   if (is.character(x)) {
#     substr(x, 1, 1) <- tolower(substr(x, 1, 1))
#   }
#   x
# }
# 
# capitalize <- function(x) {
#   substr(x, 1, 1) <- toupper(substr(x, 1, 1))
#   x
# }


plotScatter <- function(controlResults,
                        onlyUncalibrated = F) {
  size <- 2
  labelY <- 0.7
  d <- rbind(data.frame(yGroup = "Uncalibrated",
                        logRr = controlResults$logRr,
                        seLogRr = controlResults$seLogRr,
                        ci95Lb = controlResults$ci95Lb,
                        ci95Ub = controlResults$ci95Ub,
                        trueRr = controlResults$effectSize),
             data.frame(yGroup = "Calibrated",
                        logRr = controlResults$calibratedLogRr,
                        seLogRr = controlResults$calibratedSeLogRr,
                        ci95Lb = controlResults$calibratedCi95Lb,
                        ci95Ub = controlResults$calibratedCi95Ub,
                        trueRr = controlResults$effectSize))
  d <- d[!is.na(d$logRr), ]
  d <- d[!is.na(d$ci95Lb), ]
  d <- d[!is.na(d$ci95Ub), ]
  if (nrow(d) == 0) {
    return(NULL)
  }
  d$Group <- as.factor(d$trueRr)
  d$Significant <- d$ci95Lb > d$trueRr | d$ci95Ub < d$trueRr
  temp1 <- aggregate(Significant ~ Group + yGroup, data = d, length)
  temp2 <- aggregate(Significant ~ Group + yGroup, data = d, mean)
  temp1$nLabel <- paste0(formatC(temp1$Significant, big.mark = ","), " estimates")
  temp1$Significant <- NULL
  
  temp2$meanLabel <- paste0(formatC(100 * (1 - temp2$Significant), digits = 1, format = "f"),
                            "% of CIs include ",
                            temp2$Group)
  temp2$Significant <- NULL
  dd <- merge(temp1, temp2)
  dd$tes <- as.numeric(as.character(dd$Group))
  
  breaks <- c(0.1, 0.25, 0.5, 1, 2, 4, 6, 8, 10)
  theme <- ggplot2::element_text(colour = "#000000", size = 12)
  themeRA <- ggplot2::element_text(colour = "#000000", size = 12, hjust = 1)
  themeLA <- ggplot2::element_text(colour = "#000000", size = 12, hjust = 0)
  
  d$Group <- paste("True hazard ratio =", d$Group)
  dd$Group <- paste("True hazard ratio =", dd$Group)
  alpha <- 1 - min(0.95 * (nrow(d)/nrow(dd)/50000)^0.1, 0.95)
  
  if(onlyUncalibrated){
    d <- d[d$yGroup=="Uncalibrated",]
    dd <- dd[dd$yGroup=="Uncalibrated",]
  }
  #To reverse the order of graph
  d$yGroup <- factor(d$yGroup, levels = c("Uncalibrated", "Calibrated"))
  dd$yGroup <- factor(dd$yGroup, levels = c("Uncalibrated", "Calibrated"))
  
  plot <- ggplot2::ggplot(d, ggplot2::aes(x = logRr, y = seLogRr), environment = environment()) +
    ggplot2::geom_vline(xintercept = log(breaks), colour = "#AAAAAA", lty = 1, size = 0.5) +
    ggplot2::geom_abline(ggplot2::aes(intercept = (-log(tes))/qnorm(0.025), slope = 1/qnorm(0.025)),
                         colour = rgb(0.8, 0, 0),
                         linetype = "dashed",
                         size = 1,
                         alpha = 0.5,
                         data = dd) +
    ggplot2::geom_abline(ggplot2::aes(intercept = (-log(tes))/qnorm(0.975), slope = 1/qnorm(0.975)),
                         colour = rgb(0.8, 0, 0),
                         linetype = "dashed",
                         size = 1,
                         alpha = 0.5,
                         data = dd) +
    ggplot2::geom_point(size = size,
                        color = rgb(0, 0, 0, alpha = 0.05),
                        alpha = alpha,
                        shape = 16) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::geom_label(x = log(0.15),
                        y = 0.9,
                        alpha = 1,
                        hjust = "left",
                        ggplot2::aes(label = nLabel),
                        size = 5,
                        data = dd) +
    ggplot2::geom_label(x = log(0.15),
                        y = labelY,
                        alpha = 1,
                        hjust = "left",
                        ggplot2::aes(label = meanLabel),
                        size = 5,
                        data = dd) +
    ggplot2::scale_x_continuous("Hazard ratio",
                                limits = log(c(0.1, 10)),
                                breaks = log(breaks),
                                labels = breaks) +
    ggplot2::scale_y_continuous("Standard Error", limits = c(0, 1)) +
    ggplot2::facet_grid(yGroup ~ Group) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   axis.text.y = themeRA,
                   axis.text.x = theme,
                   axis.title = theme,
                   legend.key = ggplot2::element_blank(),
                   strip.text.x = theme,
                   strip.text.y = theme,
                   strip.background = ggplot2::element_blank(),
                   legend.position = "none")
  
  return(plot)
}


gridForest <- function(results, breaks = c(0.9,1,1.1,1.2), 
                       outlierMoverLower= 0.03,outlierMoverUpper= 0.1,
                       outlierMoverUse = T,
                       xLimits=c(0.85,1.3),
                       varX = "outcomeName",
                       varY = "TAR",
                       # cols = NULL,
                       xLab = "Definition of the Outcomes",
                       yLab = "Hazard Ratio (95% Confidence Interval)"){
  #hrExpression<-expression("Hazard Ratio (95% Confidence Interval) \n Favor Clopidogrel     Favor Tiacgrelor")
  hrExpression <- yLab
  shapeValue = c(17,21)#shape for closed and open center
  # if(is.null(cols)) cols = as.numeric(results$Adjustment)
  if (min(as.numeric(results$Significance))==2) shapeValue = c(21,17)
  if(outlierMoverUse){
    resultPlot<-ggplot2::ggplot(data=results,
                                aes(x = Adjustment,y = rr, ymin =  ci95Lb, ymax = ci95Ub, shape =  Significance))+
      ggplot2::geom_pointrange(aes(col = Adjustment, shape=Significance), size = 0.6
      )+
      scale_shape_manual(values=shapeValue)+ #shape for closed and open center
      geom_hline(yintercept=1, linetype="dotted")+
      #ggplot2::geom_hline(aes(fill=Adjustment),yintercept =1, linetype=2)+
      xlab(xLab)+ ylab(hrExpression)+
      ggplot2::geom_errorbar(aes(ymin=ci95Lb, ymax=ci95Ub,col=Adjustment),width=0.2,cex=1) +
      geom_segment(aes(x = Adjustment, xend = Adjustment, y = rr, yend = rr - ci95LbOut-outlierMoverLower,col=Adjustment),
                   arrow = ggplot2::arrow(angle=45,
                                          unit (0.3,"cm")),
                   size=1, show.legend=FALSE, na.rm = T)+
      geom_segment(aes(x = Adjustment, xend = Adjustment, y = rr, yend = rr - ci95UbOut+outlierMoverUpper,col=Adjustment),
                   arrow = ggplot2::arrow(angle=45,
                                          unit (0.3,"cm")),
                   size=1, show.legend=FALSE, na.rm = T)+
      ggplot2::facet_grid(as.formula(paste0(varX,"~",varY)))+
      #facet_wrap(~matching,strip.position="left",nrow=9,scales = "free_y") +
      ggplot2::theme(plot.title=element_text(size=18,face="bold"),
                     axis.text.y=element_blank(),
                     axis.ticks.y=element_blank(),
                     axis.text.x=element_text(face="bold"),
                     axis.title=element_text(size=18,face="bold"),
                     strip.text.y = element_text(hjust=0,vjust = 1,angle=180,face="bold"),
                     strip.text.x = element_blank()
      )+
      ggplot2::coord_flip(ylim = xLimits)+scale_y_continuous(trans='log10', breaks = breaks
      )+
      ggplot2::theme_bw()+
      ggplot2::theme(axis.text.y=element_blank(),
                     panel.grid.major.y = element_blank(),
                     axis.ticks.y=element_blank())+
      scale_x_discrete(limits=rev(levels(results$Adjustment))) +
      scale_fill_brewer(palette = "Greens")
  }else{
    resultPlot<-ggplot2::ggplot(data=results,
                                aes(x = Adjustment,y = rr, ymin =  ci95Lb, ymax = ci95Ub, shape =  Significance))+
      ggplot2::geom_pointrange(aes(col=Adjustment, shape=Significance), size = 0.6
      )+
      scale_shape_manual(values=shapeValue)+ #shape for closed and open center
      geom_hline(yintercept=1, linetype="dotted")+
      #ggplot2::geom_hline(aes(fill=Adjustment),yintercept =1, linetype=2)+
      xlab(xLab)+ ylab(hrExpression)+
      ggplot2::geom_errorbar(aes(ymin=ci95Lb, ymax=ci95Ub,col=Adjustment),width=0.2,cex=1) +
      
      ggplot2::facet_grid(as.formula(paste0(varX,"~",varY)))+
      #facet_wrap(~matching,strip.position="left",nrow=9,scales = "free_y") +
      ggplot2::theme(plot.title=element_text(size=18,face="bold"),
                     axis.text.y=element_blank(),
                     axis.ticks.y=element_blank(),
                     axis.text.x=element_text(face="bold"),
                     axis.title=element_text(size=18,face="bold"),
                     strip.text.y = element_text(hjust=0,vjust = 1,angle=180,face="bold"),
                     strip.text.x = element_blank()
      )+
      ggplot2::coord_flip(ylim = xLimits)+scale_y_continuous(trans='log10', breaks = breaks
      )+
      ggplot2::theme_bw()+
      ggplot2::theme(axis.text.y=element_blank(),
                     panel.grid.major.y = element_blank(),
                     axis.ticks.y=element_blank())+
      scale_x_discrete(limits=rev(levels(results$Adjustment)))
  }
  
  
  return(resultPlot)
}
####loadShinyResults####
loadShinyResults <- function(shinyFile){
  ParallelLogger::logInfo("Loading main results from ", shinyFile, " for meta-analysis")
  results<-readRDS(shinyFile)
  colnames(results) <- SqlRender::snakeCaseToCamelCase(colnames(results))
  colnames(results)[colnames(results) == "ci95lb"] <- "ci95Lb"
  colnames(results)[colnames(results) == "ci95ub"] <- "ci95Ub"
  
  ncs <- readRDS(gsub("cohort_method_result","negative_control_outcome",shinyFile))
  colnames(ncs) <- SqlRender::snakeCaseToCamelCase(colnames(ncs))
  results$trueEffectSize <- NA
  idx <- results$outcomeId %in% ncs$outcomeId
  results$trueEffectSize[idx] <- 1
  return(results)
}


###Calculation of aggregated balance (for only categorical values)
aggregateCategoricalBalance <- function(data){
  # reference: Dongsheng Yang and Jarrod E. Dalton. “A Unified Approach to Measuring the Effect Size between Two Groups Using SAS®,” SAS Global Forum 2012
  beforeMatchingNumTreated <- sum(data$beforeMatchingMeanTreated*data$beforeTargetPop, na.rm = T)
  beforeMatchingNumComparator  <- sum(data$beforeMatchingMeanComparator*data$beforeComparatorPop, na.rm = T)
  afterMatchingNumTreated <- sum(data$afterMatchingMeanTreated*data$afterTargetPop, na.rm = T)
  afterMatchingNumComparator  <- sum(data$afterMatchingMeanComparator*data$afterComparatorPop, na.rm = T)
  
  beforeMatchingMeanTreated <- sum(data$beforeMatchingMeanTreated*data$beforeTargetPop, na.rm = T)/data$beforeTargetPopAll[1]
  beforeMatchingMeanComparator  <- sum(data$beforeMatchingMeanComparator*data$beforeComparatorPop, na.rm = T)/data$beforeComparatorPopAll[1]
  afterMatchingMeanTreated <- sum(data$afterMatchingMeanTreated*data$afterTargetPop, na.rm = T)/data$afterTargetPopAll[1]
  afterMatchingMeanComparator  <- sum(data$afterMatchingMeanComparator*data$afterComparatorPop, na.rm = T)/data$afterComparatorPopAll[1]
  
  # beforeMatchingMeanTreated <- sum(data$beforeMatchingMeanTreated*data$beforeTargetPop, na.rm = T)/sum(data$beforeTargetPop)
  # beforeMatchingMeanComparator  <- sum(data$beforeMatchingMeanComparator*data$beforeComparatorPop, na.rm = T)/sum(data$beforeComparatorPop)
  # afterMatchingMeanTreated <- sum(data$afterMatchingMeanTreated*data$afterTargetPop, na.rm = T)/sum(data$afterTargetPop)
  # afterMatchingMeanComparator  <- sum(data$afterMatchingMeanComparator*data$afterComparatorPop, na.rm = T)/sum(data$afterComparatorPop)
  
  beforeMatchingStdDiff <- # aggregated sd was calculated as: (sum of sd of t and c)/2
    (beforeMatchingMeanTreated - beforeMatchingMeanComparator) / 
    sqrt((beforeMatchingMeanTreated*(1-beforeMatchingMeanTreated) + beforeMatchingMeanComparator*(1-beforeMatchingMeanComparator))/2) ##using average of squared sd to calculate SD
  afterMatchingStdDiff <- # aggregated sd was calculated as: (sum of sd of t and c)/2
    (afterMatchingMeanTreated - afterMatchingMeanComparator) / 
    sqrt((afterMatchingMeanTreated*(1-afterMatchingMeanTreated) + afterMatchingMeanComparator*(1-afterMatchingMeanComparator))/2) ##using average of squared sd to calculate SD
  
  aggregatedReseult <- data.frame(
    covariateId = unique(data$covariateId)[1],
    covariateName = unique(data$covariateName)[1],
    analysisId = unique(data$analysisId)[1],
    beforeMatchingNumTreated = beforeMatchingNumTreated,
    beforeMatchingMeanTreated = beforeMatchingMeanTreated,
    beforeMatchingNumComparator = beforeMatchingNumComparator,
    beforeMatchingMeanComparator = beforeMatchingMeanComparator,
    beforeMatchingStdDiff = beforeMatchingStdDiff,
    afterMatchingNumTreated = afterMatchingNumTreated, 
    afterMatchingMeanTreated = afterMatchingMeanTreated,
    afterMatchingNumComparator = afterMatchingNumComparator,
    afterMatchingMeanComparator = afterMatchingMeanComparator,
    afterMatchingStdDiff = afterMatchingStdDiff,
    absBeforeMatchingStdDiff = abs(beforeMatchingStdDiff),
    absAfterMatchingStdDiff = abs(afterMatchingStdDiff)
  )
  return(aggregatedReseult)
}
# beforeMatchingTotalPop <- sum(aggBals$beforeTargetPop, aggBals$beforeComparatorPop)
# beforeMatchingWithCovTarget <- sum(with(aggBals, beforeMatchingMeanTreated*beforeTargetPop))
# beforeMatchingWithCovComparator <- sum(with(aggBals, beforeMatchingMeanComparator*beforeComparatorPop))
# beforeMatchingWithCovTotal <- beforeMatchingWithCovTarget + beforeMatchingWithCovComparator
# beforeMatchingWithoutCovTarget <- sum(aggBals$beforeTargetPop) - sum(with(aggBals, beforeMatchingMeanTreated*beforeTargetPop))
# beforeMatchingWithoutCovComparator <- sum(aggBals$beforeComparatorPop)- sum(with(aggBals, beforeMatchingMeanComparator*beforeComparatorPop))
# beforeMatchingWithoutCovTotal <- beforeMatchingWithoutCovTarget + beforeMatchingWithoutCovComparator
# beforeMatchingTotalMean <- (beforeMatchingMeanTreated*sum(aggBals$beforeTargetPop)+beforeMatchingMeanComparator*sum(aggBals$beforeComparatorPop))/beforeMatchingTotalPop

# afterMatchingTotalPop <- sum(aggBals$afterTargetPop, aggBals$afterComparatorPop)
# afterMatchingWithCovTarget <- sum(with(aggBals, afterMatchingMeanTreated*afterTargetPop))
# afterMatchingWithCovComparator <- sum(with(aggBals, afterMatchingMeanComparator*afterComparatorPop))
# afterMatchingWithCovTotal <- afterMatchingWithCovTarget + afterMatchingWithCovComparator
# afterMatchingWithoutCovTarget <- sum(aggBals$afterTargetPop) - sum(with(aggBals, afterMatchingMeanTreated*afterTargetPop))
# afterMatchingWithoutCovComparator <- sum(aggBals$afterComparatorPop)- sum(with(aggBals, afterMatchingMeanComparator*afterComparatorPop))
# afterMatchingWithoutCovTotal <- afterMatchingWithoutCovTarget + afterMatchingWithoutCovComparator
# afterMatchingTotalMean <- (afterMatchingMeanTreated*sum(aggBals$afterTargetPop)+afterMatchingMeanComparator*sum(aggBals$afterComparatorPop))/afterMatchingTotalPop

# beforeMatchingStdDiff <- (beforeMatchingMeanTreated - beforeMatchingMeanComparator) / 
#   sqrt(( (beforeMatchingTotalMean^2)*beforeMatchingWithoutCovTotal+((1-beforeMatchingTotalMean)^2)*beforeMatchingWithCovTotal)/
#          (beforeMatchingTotalPop)) ##using average of squared sd to calculate SD
# afterMatchingStdDiff <- (afterMatchingMeanTreated - afterMatchingMeanComparator) / 
#   sqrt(( (afterMatchingTotalMean^2)*afterMatchingWithoutCovTotal+((1-afterMatchingTotalMean)^2)*afterMatchingWithCovTotal)/
#          (afterMatchingTotalPop)) ##using average of squared sd to calculate SD