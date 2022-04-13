#' The blackstyle Code Style
#'
#' Use [styler::style_text()] to format code according to the unofficial
#' \pkg{blackstyle} style guide. Follows the
#' [tidyverse style guide](https://style.tidyverse.org) as implemented by
#' [styler::tidyverse_style()], but with additional style rules that enforce
#' consistent line breaks inside function calls and left-justification of
#' function arguments in function definitions.
#'
#' @section Using the \pkg{blackstyle} code style: You can set the \pkg{blackstyle}
#'   code style as the default code style for \pkg{styler} (and its associated
#'   RStudio addins, like "Style active file" and "Style selection") by calling
#'   `blackstyle::use_black_style()`. If you would rather set this option globally
#'   for all session, you can add the following to your `.Rprofile`:
#'
#'   ```
#'   options(styler.addins_style_transformer = "blackstyle::black_style_transformer()")
#'   ```
#'
#' @examples
#' \dontrun{
#' use_black_style()
#' # Use styler addins
#' styler:::style_selection()
#' }
#'
#' ex_code <- "mtcars %>% mutate(mpg = mpg * 2,\n\t\t     cyl = paste(cyl)) %>% head()"
#' cat(ex_code)
#'
#' black_style_text(ex_code)
#' @param ... Arguments passed to underling \pkg{styler} functions (identified
#'   by removing the `black_` prefix), except for `transformers`, which is set to
#'   the `black_style_transformer()` internally.
#' @name black_style
NULL

#' @describeIn black_style Set the \pkg{blackstyle} style as the default style for
#'   \pkg{styler} addins.
#' @export
use_black_style <- function() {
  options(styler.addins_style_transformer = "blackstyle::black_style_transformer()")
}

#' @describeIn black_style A code transformer for use with [styler::style_text()]
#' @export
black_style_transformer <- function(...) {
  original_style <- biocthis::bioc_style(indent_by = 2, ...)

  # line breaks between *all* arguments if line breaks between *any*
  original_style$line_break$set_linebreak_each_argument_if_multi_line <- function(pd) {
    if (!(any(pd$token == "','"))) {
      return(pd)
    }
    # does this expression contain expressions with linebreaks?
    has_children <- purrr::some(pd$child, purrr::negate(is.null))
    has_internal_linebreak <- FALSE
    is_function_definition <- pd$token[1] == "FUNCTION"
    if (has_children && !is_function_definition) {
      children <- pd$child

      # don't count anything inside {} as internal linebreaks
      idx_pre_open_brace <- which(pd$token_after == "'{'")
      if (length(idx_pre_open_brace)) {
        children[idx_pre_open_brace + 1] <- NULL
      }

      has_internal_linebreak <- children %>%
        purrr::discard(is.null) %>%
        purrr::map_lgl(function(x) {
          sum(x$newlines, x$lag_newlines) > 0
        }) %>%
        any()
    }

    if (!has_internal_linebreak && sum(pd$newlines, pd$lag_newlines) < 1) {
      return(pd)
    }

    idx_comma <- which(pd$token == "','")
    idx_open_paren <- grep("'[[(]'", pd$token)
    idx_close_paren <- grep("'(]|\\))'", pd$token)
    idx_comma_has_comment <- which(pd$token[idx_comma + 1] == "COMMENT")
    idx_comma[idx_comma_has_comment] <- idx_comma[idx_comma_has_comment] + 1
    pd[idx_comma + 1L, "lag_newlines"] <- 1L
    if (length(idx_open_paren)) {
      pd[idx_open_paren[1] + 1L, "lag_newlines"] <- 1L
    }
    if (length(idx_close_paren)) {
      pd[idx_close_paren[length(idx_close_paren)], "lag_newlines"] <- 1L
    }
    pd
  }

  # Function arguments on new lines, indented with 2 spaces
  original_style$indention$update_indention_ref_fun_dec <- function(pd_nested) {
    if (pd_nested$token[1] == "FUNCTION" && nrow(pd_nested) > 4) {
      seq <- seq.int(3L, nrow(pd_nested) - 2L)
      pd_nested$indention_ref_pos_id[seq] <- 0L
      pd_nested$indent[seq] <- 2L
    }
    pd_nested
  }

  original_style
}

#' @describeIn black_style Style text using the \pkg{blackstyle} code style
#' @inheritParams styler::style_text
#' @export
black_style_text <- function(text, ...) {
  styler::style_text(text, ..., transformers = black_style_transformer())
}

#' @describeIn black_style Style a file using the \pkg{blackstyle} code style
#' @inheritParams styler::style_file
#' @export
black_style_file <- function(path, ...) {
  styler::style_file(path, ..., transformers = black_style_transformer())
}

#' @describeIn black_style Style a directory using the \pkg{blackstyle} code style
#' @export
black_style_dir <- function(path, ...) {
  styler::style_dir(path, ..., transformers = black_style_transformer())
}

#' @describeIn black_style Style a package using the \pkg{blackstyle} code style
#' @inheritParams styler::style_pkg
#' @export
black_style_pkg <- function(pkg = ".", ...) {
  styler::style_pkg(pkg, ..., transformers = black_style_transformer())
}
