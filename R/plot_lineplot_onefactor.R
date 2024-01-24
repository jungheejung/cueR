#' Plot a Line Plot for One Factor
#'
#' This function creates a line plot with error bars for a specified factor from the given dataset.
#' It is designed to plot mean values with associated error margins (like confidence intervals) for a single task.
#'
#' @param data A data frame containing the data to plot.
#' @param iv A string specifying the name of the independent variable column in `data`.
#' @param mean A string specifying the name of the column in `data` that contains mean values.
#' @param error A string specifying the name of the column in `data` that contains error margins.
#' @param color A vector specifying the color(s) to use for the plot lines.
#' @param xlab A string for the x-axis label.
#' @param ylab A string for the y-axis label.
#' @param ggtitle A string for the plot title.
#'
#' @return A ggplot object representing the line plot.
#'
#' @examples
#' # Example usage:
#' # plot_lineplot_onefactor(my_data, "Task 1", "Group", "MeanScore", "ErrorMargin", "blue", "X-Axis", "Y-Axis", "Plot Title")
#'
#' @import ggplot2
#' @export
plot_lineplot_onefactor <- function(df, iv,  mean, error,
                      color, xlab, ylab, ggtitle) {

    g <- ggplot(data = df, aes(
        x = factor(.data[[iv]]),
        y = .data[[mean]]),

        cex.lab = 1.5, cex.axis = 2, cex.main = 1.5, cex.sub = 1.5) +
        geom_errorbar(aes(
            ymin = (.data[[mean]] - .data[[error]]),
            ymax = (.data[[mean]] + .data[[error]])
        ), width = .1) +
        geom_line(aes(group = 1), data = df) +

        geom_point() +

        ggtitle(ggtitle) +
        xlab(xlab) +
        ylab(ylab) +

        theme_classic() +
        theme(legend.position = "none") +
        theme(aspect.ratio = .6)
    return(g)
}
