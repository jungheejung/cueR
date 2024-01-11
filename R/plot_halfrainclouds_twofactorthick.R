plot_halfrainclouds_twofactorthick <- function(subjectwise, groupwise,
                                      iv1, iv2, sub_mean, group_mean, se, subject,
                                      ggtitle, legend_title, xlab, ylab, task_name, ylim,
                                      w, h, dv_keyword, color, save_fname) {
    library(ggplot2)
    g <- ggplot(
        data = subjectwise,
        aes(
            y = .data[[sub_mean]],
            x = .data[[iv1]],
            fill = .data[[iv2]]
        )
    ) +
        # geom_flat_violin(
        #     aes(fill = .data[[iv2]]),
        #     position = position_nudge(x = .1, y = 0),
        #     adjust = 1.5, trim = FALSE, alpha = .3, colour = NA
        # ) +

        geom_half_violin(
            data = subjectwise,
        aes(color = .data[[iv2]]), #factor(.data[[iv2]])),
        side = 'r',
        position = position_nudge(x = .2, y = 0),
        # position = 'dodge',
        adjust = 1.5,
        trim = FALSE,
        alpha = .2,
        colour = NA
        ) +

        geom_line(
            data = subjectwise,
            aes(
                group = .data[[subject]],
                y = .data[[sub_mean]],
                x = as.numeric(as.factor(.data[[iv1]])) - .25,
                color = .data[[iv2]]
            ),
            linetype = 3, color = "grey", alpha = .3
        ) +
        geom_point(
            data = subjectwise,
            aes(
                x = as.numeric(as.factor(.data[[iv1]])) - .25,
                y = .data[[sub_mean]],
                color = .data[[iv2]]
            ),
            position = position_jitter(width = .05),
            size = 1, alpha = 0.7, shape = 20
        ) +


        geom_boxplot(
        data = subjectwise,
        aes(x = .data[[iv1]],
            y = .data[[sub_mean]],
            fill = .data[[iv2]]),
        outlier.shape = NA,
        alpha = 0.8,
        width = .3,
        notch = FALSE,
        notchwidth = 0,
        varwidth = FALSE,
        colour = "black",
        ) +

        # use summary stats __________________________________________________________________________________ # nolint

        geom_errorbar(
            data = groupwise,
            aes(
                x = as.numeric(as.factor(.data[[iv1]])) + .2,
                y = .data[[group_mean]],
                group = .data[[iv2]],
                colour = .data[[iv2]],
                ymin = .data[[group_mean]] - .data[[se]],
                ymax = .data[[group_mean]] + .data[[se]]
            ), width = .05
        ) +


        # legend stuff __________________________________________________________________________________ # nolint
        expand_limits(x = 3.5) +
        guides(fill = "none") +
        guides(color = "none") +
        guides(fill = guide_legend(title = legend_title)) +
        # geom_text()


        scale_fill_manual(values = color) +
        scale_color_manual(values = color) +
        ggtitle(ggtitle) +
        # coord_flip() + #vertical vs horizontal
        xlab(xlab) +
        ylab(ylab) +
        theme_classic()

    if (!is.null(ylim)) {
        g + ylim(ylim)
    } else {
        g
    }
    #ggsave(save_fname, width = w, height = h)
    return(g)
}