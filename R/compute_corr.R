#' Computes a tidy correlation
#'
#' more information can be added here.
#' reference for building package:
#' https://www.pipinghotdata.com/posts/2020-10-25-your-first-r-package-in-1-hour/
#'
#' @param data input data set
#' @param var1 name of variable 1
#' @param var2 name of variable 2
#'
#'
#' @return A tibble with the Pearson correlation and p-value
#' @export
#'
#' @examples
#'
#' @importFrom rlang .data
#'
compute_corr <- function(data, var1, var2){

  # compute correlation ----
  stats::cor.test(
    x = data %>% dplyr::pull({{var1}}),
    y = data %>% dplyr::pull({{var2}})
  ) %>%
    # tidy up results ----
  broom::tidy() %>%
    # retain and rename relevant bits ----
  dplyr::select(
    correlation = .data$estimate,
    pval = .data$p.value
  )

}
