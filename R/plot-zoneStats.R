#' Plot summary statistics
#'
#' @param file2plot TODO
#' @param weighted TODO
#' @param out TODO
#'
#' @return TODO
#'
#' @export
plotZoneStats <- function(file2plot = NULL, weighted = TRUE, out = 1) {
  sumtab <- readRDS(file2plot) |>
    mutate(ageClass = factor(ageClass, levels = unique(ageClass), ordered = TRUE))
  catvar <- names(sumtab)[4]

  # y-axis label corresponding to examined time period
  tref <- cbind.data.frame(
    tp = paste0("t", 1:6),
    yr1 = c(1984, 1989, 1994, 1999, 2004, 2009),
    yr2 = c(1988, 1993, 1998, 2003, 2008, 2014)
  )

  tp <- ifelse(str_detect(file2plot, "_t"),
               paste0("(", str_c(filter(tref, tp == str_sub(file2plot, start = -6L, end = -5L)) |>
                                   select(yr1, yr2) |> unlist(), collapse = "-"), ")"),
               "(1984-2014)"
  )

  if (weighted) meanVar <- "wtd.mean.slope" else meanVar <- "mean.slope"
  if (weighted) sdVar <- "wtd.sd" else sdVar <- "sd"

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

  if (out == 1) plot(gp) else return(gp)
}

#' Plot zone stats intervals
#'
#' TODO DESCRIPTION NEEDED
#'
#' @param files2plot TODO
#'
#' @param weighted TODO
#'
#' @param xVar TODO
#'
#' @param groupVar TODO
#'
#' @param catVar TODO
#'
#' @param ptype integer indicating plot type: 1 is XXX; 2 i- YYY. TODO
#'
#' @param plotResult TODO
#'
#' @return TODO
#'
#' @export
#' @importFrom dplyr mutate relocate
#' @importFrom ggplot2 aes facet_grid geom_hline geom_line ggplot ggtitle labs xlab ylab
#' @importFrom stringr str_sub
plotZoneStatsIntervals <- function(files2plot = NULL, weighted = TRUE, xVar = "ageClass",
                                   groupVar = "tp", catVar = NULL, ptype = 1, plotResult = TRUE) {
  ## compile summary tables
  sumtab <- do.call(rbind, lapply(files2plot, function(x) {
    y <- readRDS(x) |>
      mutate(ageClass = factor(ageClass, levels = unique(ageClass), ordered = TRUE))
    y$tp <- factor(rep(str_sub(x, start = -6L, end = -5L), nrow(y)),
                   levels = paste0("t", 1:6), ordered = TRUE
    )
    return(y |> relocate(tp, .before = value))
  }))

  ## validate arguments
  if (weighted) meanVar <- "wtd.mean.slope" else meanVar <- "mean.slope"
  if (weighted) sdVar <- "wtd.sd" else sdVar <- "sd"

  if (is.null(catVar)) catVar <- names(sumtab)[5]
  if (!is.null(groupVar)) groupVar <- match.arg(groupVar, names(sumtab))
  xVar <- match.arg(xVar, names(sumtab))
  if (xVar == "ageClass") xlabel <- "stand age"
  if (xVar == "tp") xlabel <- "5-year time period (1984-2014)"

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
