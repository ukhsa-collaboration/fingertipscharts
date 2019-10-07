#' fingertips theme
#' @param theme string; theme of chart, current are fingertips only
#' @param base_size base font size
#' @param base_family base font family
#' @param base_line_size base size for line elements
#' @param base_rect_size base size for rect elements
#' @import ggplot2
#' @title Fingertips theme for ggplot2
theme_phe <- function(theme = "fingertips",
                      base_size = 11, base_family = "",
                      base_line_size = base_size/22,
                      base_rect_size = base_size/22) {
        phe_key <- list(
                fingertips = list(
                        colour_title = "black",
                        colour_strip = "white",
                        colour_strip_background = "#02AE94",
                        base_colour = "#11175E",
                        line_colour = "#666666",
                        axis_line_colour = "#666666")
        )
        half_line <- base_size/2

        # Starts with theme_grey and then modify some parts
        ggplot2::theme_grey(
                base_size = base_size,
                base_family = base_family,
                base_line_size = base_line_size,
                base_rect_size = base_rect_size
        ) %+replace%
                ggplot2::theme(
                        line = element_line(colour = phe_key[[theme]]$line_colour,
                                            size = 0.5,
                                            linetype = 1,
                                            lineend = "butt"),
                        rect = element_rect(fill = "white",
                                            colour = "black",
                                            size = 0.5,
                                            linetype = 1),
                        text = element_text(family = base_family,
                                            face = "plain",
                                            colour = phe_key[[theme]]$base_colour,
                                            size = base_size,
                                            lineheight = 0.9,
                                            hjust = 0.5,
                                            vjust = 0.5,
                                            angle = 0,
                                            margin = margin(),
                                            debug = FALSE),
                        axis.line = element_line(colour = phe_key[[theme]]$axis_line_colour),
                        axis.line.x = element_line(),
                        axis.line.y = element_line(),
                        axis.text = element_text(size = rel(0.8)),
                        axis.text.x = element_text(margin = margin(t = 0.8 * half_line/2),
                                                   vjust = 1),
                        axis.text.y = element_text(margin = margin(r = 0.8 * half_line/2),
                                                   hjust = 1),
                        #axis.ticks = element_line(colour = "grey20"),
                        axis.ticks.length = unit(half_line/2, "pt"),
                        axis.title.x = element_text(margin = margin(t = 0.8 * half_line,
                                                                    b = 0.8 * half_line/2)),
                        axis.title.y = element_text(angle = 90,
                                                    margin = margin(r = 0.8 * half_line,
                                                                    l = 0.8 * half_line/2)),
                        legend.background = element_rect(colour = NA),
                        legend.margin = margin(),
                        legend.key = element_rect(fill = NA,
                                                  colour = "white"),
                        legend.key.size = unit(1.2, "lines"),
                        legend.key.height = NULL,
                        legend.key.width = NULL,
                        legend.text = element_text(size = rel(0.8)),
                        legend.text.align = NULL,
                        legend.title = element_text(hjust = 0),
                        legend.title.align = NULL,
                        legend.position = "right",
                        legend.direction = NULL,
                        legend.justification = "center",
                        legend.box = NULL,
                        panel.background = element_blank(),
                        panel.border = element_blank(),
                        panel.grid.major = element_line(),
                        panel.grid.major.y = element_line(colour = "#ECECDE"),
                        panel.grid.major.x = element_blank(),
                        panel.grid.minor = element_blank(),
                        panel.spacing = unit(half_line, "pt"),
                        panel.spacing.x = NULL,
                        panel.spacing.y = NULL,
                        panel.ontop = FALSE,
                        strip.background = element_rect(fill = phe_key[[theme]]$colour_strip_background,
                                                        colour = NA),
                        strip.text = element_text(colour = phe_key[[theme]]$colour_strip,
                                                  size = rel(1.1),
                                                  face="bold"),
                        strip.text.x = element_text(margin = margin(t = half_line,
                                                                    b = half_line),
                                                    hjust = 0.1),
                        strip.text.y = element_text(angle = -90,
                                                    margin = margin(l = half_line,
                                                                    r = half_line)),
                        strip.switch.pad.grid = unit(0.1, "cm"),
                        strip.switch.pad.wrap = unit(0.1, "cm"),
                        plot.background = element_blank(),
                        plot.title = element_text(size = rel(1.5),
                                                  margin = margin(b = half_line * 1.2),
                                                  hjust = 0,
                                                  colour = phe_key[[theme]]$colour_title,
                                                  lineheight=.8, face="bold"),
                        plot.subtitle = element_text(size = rel(1.0),
                                                     margin = margin(b = half_line * 1.2),
                                                     hjust = 0,
                                                     colour = phe_key[[theme]]$base_colour,
                                                     lineheight=.8, face="bold"),
                        plot.margin = margin(half_line, half_line, half_line, half_line),
                        complete = TRUE
                )
}


#' Fingertips scale fill
#'
#' @param theme string; current options are fingertips only for discrete
#'   scales
#' @param ... inputs to the scale_manual (for discrete values)
#' @importFrom ggplot2 scale_fill_manual
#' @title Fingertips scale fill for ggplot2
scale_fill_phe <- function(theme="fingertips", ...) {
        phe_key <- list(
                fingertips = c('Better' = '#92D050', 'Higher' = '#BED2FF',
                               'Similar' = '#FFC000',
                               'Lower'='#5555E6', 'Worse' = '#C00000',
                               'Not compared' = '#C9C9C9',
                               'None' = '#A6A6A6')
        )
        scale_fill_manual(values=phe_key[[theme]], ...)
}
