
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

###

# NSE

greet <- function(name) {
  "How do you do, name?"
}
greet("Hadley")

greet <- function(name) {
  paste0("How do you do, ", name, "?")
}
greet("Hadley")

greet <- function(name) {
  glue::glue("How do you do, {name}?")
}
greet("Hadley")

mutate_y <- function(df) {
  mutate(df, y = a + x)
}
df1 <- tibble(x = 1:3)
a <- 10
mutate_y(df1)

mutate_y <- function(df) {
  mutate(df, y = .data$a + .data$x)
}
mutate_y(df1)

df <- tibble(
  g1 = c(1, 1, 2, 2, 2),
  g2 = c(1, 2, 1, 2, 1),
  a = sample(5),
  b = sample(5)
)

df %>% 
  group_by(g1) %>% 
  summarize(a = mean(a))

df %>%
  group_by(g2) %>% 
  summarize(a = mean(a))

my_summarize <- function(df, group_var) {
  df %>% 
    group_by(group_var) %>% 
    summarize(a = mean(a))
}
my_summarize(df, g1)
my_summarize(df, "g2")

# these don't work because `group_by` works like `"`... it quotes the 
# variable passed to it... it doens't evaluate it

# so, when we pass `group_by` (or similar functions) an unquoted value, 
# we need to first quote it, and THEN tell `group_by` NOT to quote
# its input because we've already quoted it

# `quo` works like `"`... it quotes its input rather than evaluating it
quo(g1)
quo(a + b + c)
quo("a")

my_summarize(df, quo(g1))
# same error as above b/c we haven't told `group_by` that we've already
# taken care of the quoting... so we have to rewrite the funcion to 
# accomplish this

my_summarize <- function(df, group_var) {
  df %>% 
    group_by(!! group_var) %>% 
    summarize(a = mean(a))
}
my_summarize(df, quo(g1))
my_summarize(df, quo(g2))

# this can be improved so that user doesn't have to pass `quo(x)` as
# an argument

my_summarize <- function(df, group_var) {
  quo_group_var <- quo(group_var)
  print(quo_group_var)
  df %>% 
    group_by(!! quo_group_var) %>% 
    summarize(a = mean(a))
}
my_summarize(df, g1)

# the above doesn't work because `quo` is too literal... we can use
# `enquo` instead to grap not ^group_var but the value the user passed,
# which was ^g1

my_summarize <- function(df, group_var) {
  enquo_group_var <- enquo(group_var)
  print(enquo_group_var)
  df %>% 
    group_by(!! enquo_group_var) %>% 
    summarize(a = mean(a))
}
my_summarize(df, g1)


# different input variable

df
summarize(df, mean = mean(a), sum = sum(a), n = n())
summarize(df, mean = mean(a * b), sum = sum(a * b), n = n())

my_var <- quo(a)
my_var
summarize(df, mean = mean(!! my_var), sum = sum(!! my_var), n = n())
quo(
  summarize(df,
            mean = mean(!! my_var),
            sum = sum(!! my_var),
            n = n())
)
my_summarize2 <- function(df, expr) {
  expr <- enquo(expr)
  
  summarize(df,
            mean = mean(!! expr),
            sum = sum(!! expr),
            n = n())
}
df
my_summarize2(df, a)
my_summarize2(df, b*2)
my_summarize2(df, a*b)


# different input and output variable

mutate(df, mean_a = mean(a), sum_a = sum(a))
mutate(df, mean_b = mean(b), sum_b = sum(b))











