
library(stringr)

names <- c("id" 
           , "foo"
           , "fu_foo"
           , "tele_foo"
           , "bar"
           , "fu_bar"
           , "baz"
           , "qux")

# input: `names`, output: c("foo", "bar")
# find which strings in `names` have "fu_" and "tele_" prefixes

names[str_detect(string = names, pattern = "fu_|tele_")]

str_replace_all(
  string = names[str_detect(string = names, pattern = "fu_|tele_")],
  pattern = "fu_|tele_",
  replacement = ""
)


unique(
  str_replace_all(
    string = names[str_detect(string = names, pattern = "fu_|tele_")],
    pattern = "fu_|tele_",
    replacement = ""
  )
)

get_ift_dups <- function(x) {
  unique(
    str_replace_all(
      string = x[str_detect(string = x, pattern = "fu_|tele_")],
      pattern = "fu_|tele_",
      replacement = ""
    )
  )  
}

get_ift_dups(names)


names_u3
str_replace_all(string = names_u3,
                pattern = "_c2", 
                replacement = "")