# in_round <- function(x)
# {
#   stringr::str_match_all(x, "(?<=\\().+?(?=\\))")[[1]][,1]
# }
# string <- "What kind of cheese isn't your cheese? (wonder) Nacho cheese! (groan) (Laugh)"
# in_parentheses(string)

# in_square <- function(x)
# {
#   stringr::str_match_all(x, "(?<=\\[).+?(?=\\])")[[1]][,1]
# }
# string <- "What kind of cheese isn't your cheese? [wonder] Nacho cheese! [groan] [Laugh]"
# in_squared_brackets(string)

# in_curly <- function(x)
# {
#   stringr::str_match_all(x, "(?<=\\{).+?(?=\\})")[[1]][,1]
# }

# string <- "What kind of cheese isn't your cheese? {wonder} Nacho cheese! {groan} {Laugh}"
# in_curly_brackets(string)


embrace <- function(.x, embrace = "({.x})"){
  out <-glue(embrace)
  out
}

get_embrace <- function(.x, .left = "\\(", .right  = "\\)", re_embrace = "({.x})"){
  x_match <- glue("(?<={.left}).+?(?={.right})")
  stringr::str_match_all(.x, x_match)[[1]][,1]
}

