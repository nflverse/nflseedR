library(hexSticker)
library(showtext)
library(tidyverse)

## Loading Google fonts (http://www.google.com/fonts)
# font_add_google("Architects Daughter", "seb")
# font_add_google("Alfa Slab One", "seb")
font_add_google("Audiowide", "seb")
## Automatically use showtext to render text for future devices
showtext_auto()

p <-
  tibble(x = 1:7, y = c(6, 5, 3, 2, 4, 1, 7)) %>%
  ggplot(aes(x = x, y = y)) +
  geom_col(color = "white", width = 0.85) +
  theme_void() +
  theme_transparent()


sticker(
  p,
  package = "nflseedR",
  p_family = "seb",
  p_y = 0.6,
  p_size = 20,
  s_x = 1,
  s_y = 1.15,
  s_width = 1.5,
  s_height = 0.75,
  spotlight = TRUE,
  l_y = 1.5,
  l_alpha = 0.3,
  l_width = 5,
  h_fill = "#323232",
  h_color = "black",
  h_size = 0.8,
  filename = "man/figures/logo.png",
  url = "https://nflseedr.com",
  u_color = "white",
  u_size = 5
)

# usethis::use_logo("data-raw/logo.png")
