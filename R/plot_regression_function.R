## Function to plot linear regression on graphs
geom_lm <- function(formula = y ~ x, color = alpha('gray15', 0.75), 
                    linewidth = 0.5, linetype = 'dashed', se = TRUE, ...) {
  geom_smooth(formula = formula, se = se, method = 'lm', color = color,
              linewidth = linewidth, linetype = linetype, ...)
}

## Function to plot non-linear regression lines using the nls method
geom_nls <- function(formula = y ~ a * exp(b * x), 
                     start = list(a = 0.05, b = -1),
                     se = FALSE, 
                     color = alpha('gray15', 0.75), 
                     linewidth = 0.5, 
                     linetype = 'dashed', ...) {
  stat_smooth(method = 'nls', 
              formula = formula, 
              method.args = list(start = start),
              se = se, 
              color = color, 
              linewidth = linewidth, 
              linetype = linetype, 
              ...)
}