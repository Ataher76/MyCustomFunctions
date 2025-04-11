#' Plot and Model Length-Weight Relationships with Optional Log Transformation
#'
#' This function visualizes and models the relationship between length and weight 
#' (or any two continuous variables) using linear regression. It supports both 
#' standard and log-log transformations. The plot includes a fitted line, 
#' optional confidence shading, and displays the regression equation, R², and p-value.
#'
#' @param data A data frame with at least two columns: the first for length, the second for weight.
#' @param log_transform Logical. Whether to apply a log-log transformation to the variables. Default is \code{TRUE}.
#' @param pch Plotting character for data points. Default is \code{15}.
#' @param shade Logical. Whether to add a shaded confidence interval around the fitted line. Default is \code{TRUE}.
#' @param point_col Color of the points. Default is \code{"black"}.
#' @param line_col Color of the regression line. Default is \code{"red"}.
#' @param shade_col Color for the confidence interval polygon. Default is semi-transparent red.
#' @param lwd Line width of the regression line. Default is \code{2}.
#' @param legend_show Logical. Whether to show the legend. Default is \code{TRUE}.
#' @param main Title of the plot. Default is \code{"Length-Weight Relationship"}.
#' @param xlab Optional. Custom x-axis label. If \code{NULL}, a label will be generated.
#' @param ylab Optional. Custom y-axis label. If \code{NULL}, a label will be generated.
#'
#' @return A list containing:
#' \item{model}{The fitted \code{lm} object}
#' \item{intercept}{The estimated intercept}
#' \item{slope}{The estimated slope}
#' \item{r_squared}{R-squared value}
#' \item{correlation_r}{Correlation coefficient (r)}
#' \item{p_value}{P-value for slope}
#'
#' @examples
#' \dontrun{
#' LWRelation(mydata, log_transform = TRUE)
#' LWRelation(mydata, log_transform = FALSE, legend_show = FALSE)
#' }
#'
#' @export
LWRelation <- function(data,
                       log_transform = TRUE,
                       pch = 15,
                       shade = TRUE,
                       point_col = "black",
                       line_col = "red",
                       shade_col = rgb(1, 0, 0, alpha = 0.2),
                       lwd = 2,
                       legend_show = TRUE,
                       main = "Length-Weight Relationship",
                       xlab = NULL,
                       ylab = NULL) {
  
  if (ncol(data) < 2) stop("Data must have at least two columns.")
  
  x_raw <- data[[1]]
  y_raw <- data[[2]]
  
  if (log_transform) {
    x <- log(x_raw)
    y <- log(y_raw)
    xlab <- xlab %||% paste0("log(", names(data)[1], ")")
    ylab <- ylab %||% paste0("log(", names(data)[2], ")")
  } else {
    x <- x_raw
    y <- y_raw
    xlab <- xlab %||% names(data)[1]
    ylab <- ylab %||% names(data)[2]
  }
  
  model <- lm(y ~ x)
  a <- if (log_transform) round(exp(coef(model)[1]), 4) else round(coef(model)[1], 4)
  b <- round(coef(model)[2], 4)
  p_value <- signif(summary(model)$coefficients[2, 4], 4)
  r_squared <- round(summary(model)$r.squared, 4)
  correlation_r <- round(sqrt(r_squared), 4)
  
  x_seq <- seq(min(x), max(x), length.out = 100)
  newdata <- data.frame(x = x_seq)
  preds <- predict(model, newdata, interval = "confidence")
  
  plot(x, y, pch = pch, col = point_col, xlab = xlab, ylab = ylab, main = main)
  
  if (shade) {
    polygon(c(x_seq, rev(x_seq)), c(preds[, "lwr"], rev(preds[, "upr"])), 
            col = shade_col, border = NA)
  }
  
  lines(x_seq, preds[, "fit"], col = line_col, lwd = lwd)
  
  # Equation text
  eqn <- if (log_transform) {
    paste0("W = ", a, " L^", b)
  } else {
    paste0("y = ", a, " + ", b, "x")
  }
  r_text <- paste0("R² = ", r_squared)
  p_text <- paste0("p = ", p_value)
  
  # Positioning annotation in top-left corner
  usr <- par("usr")  # get plot limits
  text_x <- usr[1] + 0.05 * (usr[2] - usr[1])
  text_y <- usr[4] - 0.05 * (usr[4] - usr[3])
  line_spacing <- 0.07 * (usr[4] - usr[3])
  
  text(text_x, text_y, eqn, pos = 4, cex = 0.9)
  text(text_x, text_y - line_spacing, r_text, pos = 4, cex = 0.9)
  text(text_x, text_y - 2 * line_spacing, p_text, pos = 4, cex = 0.9)
  
  # Legend
  if (legend_show) {
    legend("bottomright", legend = c("Observed", "Fit", "95% CI"),
           col = c(point_col, line_col, shade_col),
           pch = c(pch, NA, 15), lty = c(NA, 1, NA), pt.cex = c(1, NA, 2), bty = "n")
  }
  
  return(list(
    model = model,
    intercept = a,
    slope = b,
    r_squared = r_squared,
    correlation_r = correlation_r,
    p_value = p_value
  ))
}

# Internal helper for null-coalesce
`%||%` <- function(a, b) if (!is.null(a)) a else b



check <- iris %>% select(1,4)

LWRelation(data = check, pch = 4, shade = T, main = NULL, log_transform = F, legend_show = F)


