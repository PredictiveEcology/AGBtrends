#' Timeseries interval reference
#'
#' @param timeint timeseries intervals as a named list with years corresponding to each interval
#'                (named `t1`, `t2`, `t3`, etc.).
#'
#' @param years integer vector of timeseries years
#'
#' @return data.frame with columns `tp` (time period), `yr1` (first years of each time period),
#'         and `t2` (the last years of each time period)
#'
#' @export
#'
#' @examples
#' timeint <- list(
#'   t1 = 1:5, t2 = 6:10, t3 = 11:15, t4 = 16:20, t5 = 21:25, t6 = 26:31
#' )
#' years <- unname(unlist(timeint)) + 1983L
#' tref(timeint, years)
tref <- function(timeint, years) {
  n_int <- length(timeint)

  cbind.data.frame(
    tp = paste0("t", seq_len(n_int)),
    yr1 = years[vapply(timeint, FUN = utils::head, n = 1L,
                       FUN.VALUE = integer(1), USE.NAMES = FALSE)],
    yr2 = years[vapply(timeint, FUN = utils::tail, n = 1L,
                       FUN.VALUE = integer(1), USE.NAMES = FALSE)]
  )
}

#' Plot zone summary statistics
#'
#' @param files2plot character vector of file paths to `.rds` files from which to load a
#'                   previously-built summary `data.frame` object to use for constructing plots
#'
#' @param tref timeseries interval reference `data.frame` as output by [tref()]
#'
#' @param weighted logical indicating whether to use weighted mean and standard deviation
#'
#' @param xVar character string indicating which variable to plot on the x-axis
#'
#' @param groupVar character string indicating the variable to group by
#'
#' @param catVar character string indicating the category variable
#'
#' @param ptype integer indicating plot type: 1 is TODO; 2 is TODO
#'
#' @param plotResult logical indicating whether to plot to the active graphics device
#'
#' @return `ggplot` object
#'
#' @export
#' @rdname plotZoneStats
plotZoneStats <- function(files2plot = NULL, tref = NULL, weighted = TRUE, plotResult = TRUE) {
  stopifnot(
    length(files2plot) == 1,
    !is.null(tref)
  )

  sumtab <- readRDS(files2plot) |>
    mutate(ageClass = factor(ageClass, levels = unique(ageClass), ordered = TRUE))
  catvar <- names(sumtab)[4]

  ## y-axis label corresponding to examined time period
  tp <- ifelse(str_detect(basename(files2plot), "_t"),
               paste0("(", str_c(filter(tref, tp == str_sub(files2plot, start = -6L, end = -5L)) |>
                                   select(yr1, yr2) |> unlist(), collapse = "-"), ")"),
               paste0("(", min(tref$yr1), "-", max(tref$yr2),")")
  )

  if (weighted) {
    meanVar <- "wtd.mean.slope"
    sdVar <- "wtd.sd"
  } else {
    meanVar <- "mean.slope"
    sdVar <- "sd"
  }

  gp <- ggplot(sumtab, aes(x = ageClass, y = !!as.name(meanVar), group = !!as.name(catvar),
                           color = !!as.name(catvar))) +
    xlab("stand age") +
    ylab(paste0("n-weighted mean AGB trend ", tp)) +
    scale_x_discrete(breaks = sumtab$ageClass, labels = sumtab$ageClass) +
    geom_hline(yintercept = 0, linetype = 2) +
    geom_line(linetype = 3) +
    geom_errorbar(aes(ymin = !!as.name(meanVar) - (!!as.name(sdVar) / sqrt(count)),
                      ymax = !!as.name(meanVar) + (!!as.name(sdVar) / sqrt(count))),
                  width = 0.2
    )

  if (isTRUE(plotResult)) {
    plot(gp)
  }

  return(invisible(gp))
}

#' @export
#' @rdname plotZoneStats
plotZoneStatsIntervals <- function(files2plot = NULL, tref = NULL, weighted = TRUE,
                                   xVar = "ageClass", groupVar = "tp", catVar = NULL,
                                   ptype = 1, plotResult = TRUE) {
  ## compile summary tables
  sumtab <- do.call(rbind, lapply(files2plot, function(x) {
    y <- readRDS(x) |>
      mutate(ageClass = factor(ageClass, levels = unique(ageClass), ordered = TRUE))
    y$tp <- factor(rep(str_sub(x, start = -6L, end = -5L), nrow(y)),
                   levels = paste0("t", seq_len(length(files2plot))), ordered = TRUE)
    return(y |> relocate(tp, .before = value))
  }))

  ## validate arguments
  if (weighted) {
    meanVar <- "wtd.mean.slope"
    sdVar <- "wtd.sd"
  } else {
    meanVar <- "mean.slope"
    sdVar <- "sd"
  }

  if (is.null(catVar)) catVar <- names(sumtab)[5]
  if (!is.null(groupVar)) groupVar <- match.arg(groupVar, names(sumtab))
  xVar <- match.arg(xVar, names(sumtab))
  if (xVar == "ageClass") {
    xlabel <- "stand age"
  } else if (xVar == "tp") {
    xlabel <- paste0("5-year time period (", min(tref$yr1), "-", max(tref$yr2), ")")
  }

  if (ptype == 1) {
    gp <- ggplot(sumtab, aes(x = !!as.name(xVar), y = !!as.name(meanVar),
                             group = !!as.name(groupVar), color = !!as.name(groupVar))) +
      labs(
        x = xlabel,
        y = "n-weighted mean AGB trend"
      ) +
      geom_hline(yintercept = 0, linetype = "dotted") +
      geom_line() +
      facet_grid(~ .data[[catVar]])
  } else {
    if (ptype == 2) {
      gp <- lapply(unique(sumtab[, catVar]), function(cat) {
        x <- filter(sumtab, !!as.name(catVar) == cat)

        return(ggplot(x, aes(x = !!as.name(xVar), y = !!as.name(meanVar),
                             group = !!as.name(groupVar), color = !!as.name(groupVar))) +
                 geom_line() +
                 xlab(xlabel) +
                 ylab("n-weighted mean AGB trend") +
                 geom_hline(yintercept = 0, linetype = "dotted") +
                 geom_line() +
                 ggtitle(cat))
      })
      names(gp) <- unique(sumtab[, catVar])
    }

    if (!plotResult) {
      return(gp)
    }
    if (ptype == 1) print(gp) else lapply(gp, print)
  }
}
