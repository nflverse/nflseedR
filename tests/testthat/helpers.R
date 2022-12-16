test_dir <- getwd()

load_test_games <- function(dir = test_dir){
  readRDS(file.path(dir, "games.rds"))
}

load_reference <- function(type = c("div", "conf", "draft"),
                           dir = test_dir){
  type <- match.arg(type)
  file_name <- switch (type,
    "div" = "reference_div_ranks.rds",
    "conf" = "reference_conf_seeds.rds",
    "draft" = "reference_draft_order.rds",
  )
  readRDS(file.path(dir, file_name))
}
