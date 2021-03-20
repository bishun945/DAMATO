library(hexSticker)
library(showtext)
library(magrittr)
## Loading Google fonts (http://www.google.com/fonts)
# font_add_google("Gochi Hand", "gochi")
font_add_google("Montserrat", "mont")
## Automatically use showtext to render text for future devices
showtext_auto()

p <- cowplot::ggdraw() + 
  cowplot::draw_image("./inst/tomato.png", scale = 0.9)

## use the ggplot2 example
sticker(p, 
        package="MADATO", 
        p_size=25, p_x = 1, p_y = 1.4, 
        p_family = "mont", p_fontface = "bold",
        s_x=1, s_y=.85, s_width=1.3, s_height=0.8,
        h_fill = "#FDFFE1", h_color = "black", p_color = "black", h_size = 1, 
        # h_fill = "#007DFF", h_color = "black", p_color = "#FF8200",
        # h_fill = "#8AAAE5", h_color = "gray75", p_color = "#FFFFFF",
        spotlight = FALSE, l_x = 1, l_y = 0.5, l_alpha = 0.4,
        filename = "./man/figures/logo.png"
)

# sticker(p, 
#         package="FCMm", 
#         p_size=28, p_x = 1, p_y = 1.4, 
#         p_family = "mont", p_fontface = "bold",
#         s_x=1, s_y=.85, s_width=1.3, s_height=0.8,
#         h_fill = "#FDFFE1", h_color = "black", p_color = "black", h_size = 1, 
#         # h_fill = "#007DFF", h_color = "black", p_color = "#FF8200",
#         # h_fill = "#8AAAE5", h_color = "gray75", p_color = "#FFFFFF",
#         spotlight = TRUE, l_x = 1, l_y = 0.5, l_alpha = 0.4,
#         filename = "./man/figures/logo.svg"
# )


# add this if you get proxy issue when rendering `README.Rmd`
Sys.setenv(http_proxy="http://127.0.0.1:7890")


