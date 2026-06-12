#'Plot sensor data coloured by flag value
#'
#'@param dat Data frame of flagged sensor string data in long or wide format.
#'  Must include at least one column name with the string "_flag_variable".
#'
#'@param vars Character vector of variables to plot. Default is \code{vars =
#'  "all"}, which will make a plot for each recognized variable in \code{dat}.
#'
#'@param labels Logical argument indicating whether to convert numeric flag
#'  values to text labels for the legend.
#'
#'@param ncol Number of columns for faceted plots.
#'
#'@param flag_title Logical argument indicating whether to include a ggtitle of
#'  the qc test and variable plotted.
#'
#'@param plotly_friendly Logical argument. If \code{TRUE}, the legend will be
#'  plotted when \code{plotly::ggplotly} is called on \code{p}. Default is
#'  \code{FALSE}, which makes the legend look better in a static figure.
#'
#'@param jitter_height Numeric value. Amount of vertical jitter in the
#'  deth_crosscheck figure. Only recommended to check for overlapping deployment
#'  points because it introduces random noise into the scatter plot that can be
#'  misleading.
#'
#'@inheritParams qc_test_all
#'
#'@return Returns a list of ggplot objects; one figure for each test in
#'  \code{qc_tests} and variable in \code{vars}. Points are coloured by the flag
#'  value and panels are faceted by depth and sensor. faceted by depth and
#'  sensor.
#'
#'@importFrom lubridate as_datetime
#'
#'@export
#'

qc_plot_flags <- function(
    dat,
    qc_tests = c("climatology",
                 "depth_crosscheck",
                 "grossrange",
                 "rolling_sd",
                 "spike"),
    vars = "all",
    labels = TRUE,
    ncol = NULL,
    flag_title = TRUE,
    plotly_friendly = FALSE,
    jitter_height = 0
) {

  p <- list(NULL)
  p_out <- list(NULL)

  if (!("variable" %in% colnames(dat))) {
    dat <- qc_pivot_longer(dat, qc_tests = qc_tests)
  }

  dat <- dat %>%
    rename(tstamp = contains("timestamp")) %>%
    mutate(
      tstamp = as_datetime(tstamp),

      sensor = paste(sensor_type, sensor_serial_number, sep = "-"),
      depth_label = paste0(sensor_depth_at_low_tide_m, " m", " (", sensor, ")"),
      depth_label = ordered(
        depth_label,
        levels = gtools::mixedsort(unique(depth_label))
      )
    )

  # depth_crosscheck plot is made with a different function from the other tests
  if("depth_crosscheck" %in% qc_tests) {

    qc_tests <- qc_tests[qc_tests != "depth_crosscheck"]

    p_out[["depth_crosscheck"]] <- ggplot_depth_crosscheck(
      dat,
      flag_title = flag_title,
      labels = labels,
      jitter_height = jitter_height,
      ncol = ncol
    )

    p_out <- Filter(Negate(is.null), p_out)
  }

  if (vars == "all") vars <- unique(dat$variable)

  if (isTRUE(labels)) dat <- dat %>% qc_assign_flag_labels()

  if (is.null(ncol)) ncol <- 1

  # plot for each variable
  for (i in seq_along(vars)) {
    var_i <- vars[i]

    dat_i <- filter(dat, variable == var_i)

    # if nrow is 0 don't make a plot
    if (nrow(dat_i) == 0) {
      stop("No data for variable << ", var_i, " >>")
    }

    # plot for each test
    for (j in seq_along(qc_tests)) {
      qc_test_j <- qc_tests[j]

      p[[qc_test_j]] <- ggplot_flags(
        dat_i,
        qc_test = qc_test_j, var = var_i, ncol = ncol,
        plotly_friendly = plotly_friendly,
        flag_title = flag_title
      )

      p <- Filter(Negate(is.null), p) # remove empty list element
    }
    p_out[[var_i]] <- p
  }
  p_out <- Filter(Negate(is.null), p_out) # not sure why first element was null

  p_out
}


#' Create ggplot for one qc_test and variable
#'
#' @param dat Data frame of flagged sensor string data in long format. Must
#'   include a column named with the string "_flag_value".
#'
#' @param qc_test qc test to plot.
#'
#' @param var variable to plot.
#'
#' @inheritParams qc_plot_flags
#'
#' @return Returns a ggplot object; a figure for \code{qc_test} and \code{var}.
#'   Points are coloured by the flag value and panels are faceted by depth and
#'   sensor.
#'
#' @importFrom ggplot2 aes element_rect element_text facet_wrap geom_point
#'   ggplot ggtitle guides guide_legend  scale_colour_manual scale_x_datetime
#'   scale_y_continuous theme_light theme
#'
#' @importFrom gtools mixedsort


ggplot_flags <- function(
    dat,
    qc_test,
    var,
    ncol = NULL,
    flag_title = TRUE,
    plotly_friendly = FALSE
) {
  # https://www.visualisingdata.com/2019/08/five-ways-to-design-for-red-green-colour-blindness/
  flag_colours <- c("chartreuse4", "grey24", "#EDA247", "#DB4325")

  flag_column <- paste0(qc_test, "_flag_value")

  p <- dat %>%
    ggplot(aes(tstamp, value, colour = !!sym(flag_column))) +
    geom_point(show.legend = TRUE) +
    scale_y_continuous(var) +
    scale_x_datetime("Date") +
    scale_colour_manual("Flag Value", values = flag_colours, drop = FALSE) +
    facet_wrap(~depth_label, ncol = ncol) +
    theme_light() +
    theme(
      strip.text = element_text(colour = "black", size = 10),
      strip.background = element_rect(fill = "white", colour = "darkgrey")
    )

  if(isFALSE(plotly_friendly)) {
    p <- p + guides(color = guide_legend(override.aes = list(size = 4)))
  }


  if (isTRUE(flag_title)) p <- p + ggtitle(paste0(qc_test, " test: ", var))

  p
}

#' Create a ggplot for the depth crosscheck test
#'
#' @param dat  Data frame of flagged sensor string data. Must include columns
#'   \code{depth_measured_m}, \code{sensor_depth_at_low_tide_m},
#'   \code{depth_crosscheck_flag_value}.
#'
#' @param jitter_height Numeric value. Amount of vertical jitter. Only
#'   recommended to check for overlapping deployment points because it
#'   introduces random noise into the scatter plot that can be misleading.
#'
#' @inheritParams qc_plot_flags
#'
#' @return Returns a ggplot object. Points are coloured by the flag value.
#'
#' @importFrom dplyr filter group_by summarise ungroup
#' @importFrom ggplot2 aes element_text element_rect geom_abline geom_hline
#'   geom_point geom_vline ggplot ggtitle guides guide_legend position_jitter
#'   scale_colour_manual scale_x_datetime scale_y_continuous theme_light theme
#' @importFrom gtools mixedsort
#' @importFrom stringr str_remove_all
#'
#' @export

ggplot_depth_crosscheck <- function(
    dat,
    flag_title = TRUE,
    labels = TRUE,
    jitter_height = 0,
    ncol = NULL) {

  flag_colours <- c("chartreuse4", "grey24", "#EDA247", "#DB4325")

  labels <- labels

  if (("variable" %in% colnames(dat))) {
    dat <- dat %>% rename(timestamp_utc = tstamp) %>% ss_pivot_wider()
  }

  if(!("sensor_depth_measured_m" %in% colnames(dat))) {
    stop("sensor_depth_measured_m must be present in dat to plot depth_crosscheck figure")
  }
  if(!("sensor_depth_at_low_tide_m" %in% colnames(dat))) {
    stop("sensor_depth_at_low_tide_m must be present in dat to plot depth_crosscheck figure")
  }

  dat <- dat %>%
    filter(
      !is.na(sensor_depth_measured_m) & !is.na(sensor_depth_at_low_tide_m)
    ) %>%
    group_by(
      county, station, deployment_range,
      sensor_serial_number, depth_label, sensor_depth_at_low_tide_m,
      depth_crosscheck_flag
    ) %>%
    summarise(
      min_sensor_depth_measured_m =
        min(sensor_depth_measured_m, na.rm = TRUE)
    ) %>%
    ungroup()

  if (isTRUE(labels)) dat <- dat %>% qc_assign_flag_labels()

  if(nrow(dat) == 0) {
    stop("No observations to plot for depth_crosscheck figure.")
  }

  p <- ggplot(dat,
    aes(min_sensor_depth_measured_m, sensor_depth_at_low_tide_m,
        col = depth_crosscheck_flag)
  ) +
    geom_hline(
      yintercept = unique(dat$sensor_depth_at_low_tide_m),
      linewidth = 1, col = "grey70"
    ) +
    geom_vline(
      xintercept = unique(dat$sensor_depth_at_low_tide_m),
      linewidth = 1, col = "grey70"
    ) +
    geom_point(
      size = 3, alpha = 1, show.legend = TRUE,
      position = position_jitter(width = 0, height = jitter_height)
    ) +
    scale_colour_manual("Flag Value",values = flag_colours, drop = FALSE) +
    facet_wrap(~depth_label, ncol = ncol) +
    theme_light() +
    theme(
      strip.text = element_text(colour = "black", size = 10),
      strip.background = element_rect(fill = "white", colour = "darkgrey")
    )

  if (isTRUE(flag_title)) p + ggtitle("depth_crosscheck test")

  p
}

