library(showtext)
library(tidyverse)

## Loading Google fonts (http://www.google.com/fonts)
# font_add_google("Architects Daughter", "seb")
# font_add_google("Alfa Slab One", "seb")
font_add_google("Audiowide", "seb")
## Automatically use showtext to render text for future devices
showtext_auto()

ggplot(data = NULL) +
  geom_text(
    aes(x = 0, y = 0, label = "nflseedR"),
    family = "seb", size = 10, color = "white", hjust = 0.5
  ) +
  geom_text(
    aes(x = 0.399, y = -0.33, label = "by @LeeSharpeNFL and @mrcaseb"),
    size = 1.5, color = "white", hjust = 0.5
  ) +
  coord_cartesian(xlim = c(-1, 1), ylim = c(-1, 1)) +
  theme_void() +
  theme(plot.background = element_rect(fill = "#323232", colour = "transparent"))

.width = 7
ggsave("man/figures/gh_preview.png", dpi = 600, width = .width, height = .width/2, units = "cm")
