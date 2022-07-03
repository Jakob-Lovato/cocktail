library(tidyverse)

# Download data from github
boston_cocktails <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/boston_cocktails.csv')

# See most common ingredients
boston_cocktails %>% 
  count(ingredient) %>% 
  arrange(desc(n))

# Prevalence of powdered sugar confused me (I think of "powdered sugar" as confectioners sugar)
# Investigating the Mr. Boston website where the data was pulled from, it seems the website
# means what I would consider to be "granulated sugar" when it says "powdered sugar"

# Get list of all ingredients containing the word "sugar"
sugar_index <- grepl("sugar", boston_cocktails$ingredient, ignore.case = TRUE)

# Count of any instance of "sugar"
boston_cocktails[sugar_index,] %>% 
  count(ingredient) %>%
  arrange(desc(n))

# "Powdered Sugar" occurs 90 times while "Sugar" occurs 8 times. I will combine the two into
# just "Sugar" and it won't effect the ranking anyway
cocktail <- 
  boston_cocktails %>% 
  mutate(ingredient = str_replace(ingredient, "Powdered Sugar", "Sugar"))

# Check ingredient ranking again on new data
cocktail %>% 
  count(ingredient) %>% 
  arrange(desc(n))

# Create new dataset of just the top 10 ingredients for plotting ease,
# and change values to percents. I.e. "what percentage of cocktails do
# each ingredient occur in?" 
top10 <- 
  cocktail %>% 
  count(ingredient) %>% 
  arrange(desc(n)) %>% 
  top_n(10) %>%
  mutate(n = n / 990) %>% #990 cocktails in dataset
  as.data.frame()

# Create factors so ggplot doesn't reorder x-axis
top10$ingredient <- factor(top10$ingredient, levels = top10$ingredient)

# Pre-set colors and fonts
fg_col <- rgb(230, 221, 190, maxColorValue = 255)
bg_col <- rgb(5, 3, 32, maxColorValue = 255)
line_col <- rgb(62, 62, 55, maxColorValue = 255)
title_col <- rgb(252, 252, 248, maxColorValue = 255)
body_font <- "Proxima Nova"
title_font <- "Optima"

# Create plot of 10 most common ingredients
ggplot(top10, aes(ingredient, n)) +
  geom_bar(stat = "identity", 
           fill = fg_col) +
  
  labs(x = "Ingredient", 
       y = "Percent of Cocktails Using Ingredient") + 
  
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1,
                                   vjust = 1.1,
                                   size = 12,
                                   family = body_font,
                                   color = fg_col),
        
        axis.text.y = element_text(hjust = 1,
                                   size = 12,
                                   family = body_font,
                                   color = fg_col),
        
        axis.title.x = element_text(size = 18,
                                    family = body_font, 
                                    color = title_col),
        
        axis.title.y = element_text(size = 15,
                                    family = body_font, 
                                    color = title_col,
                                    margin = margin(#l = 10,
                                                    r = 20)),
        
        panel.grid.major.y = element_line(size = 0.5,
                                          color = line_col,
                                          linetype = "dotted"),
        
        panel.grid.minor.y = element_line(size = 0.5,
                                          color = line_col,
                                          linetype = "dotted"),
        
        panel.grid.major.x = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        plot.background = element_rect(fill = bg_col),
        plot.margin = margin(25, 20, 20, 20)) +
  
  annotate(geom = "curve", 
           x = 3, 
           y = 0.17, 
           xend = 1.75, 
           yend = 0.175, 
           curvature = .3, 
           arrow = arrow(length = unit(2, "mm")), 
           color = title_col) +
  
  annotate(geom = "text", 
           x = 3.1, 
           y = 0.16, 
           label = "Gin is the most common \ningredient, being used in \nabout 18% of cocktails", 
           hjust = "left",
           family = body_font,
           #fontface = "italic",
           size = 3,
           color = title_col,) +
  
  #title
  annotate(geom = "text",
           x = 10.5,
           y = 0.165,
           label = "Ten Most Common \nCocktail Ingredients",
           hjust = "right",
           family = title_font,
           fontface = "bold.italic",
           size = 8,
           color = title_col) +
  
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) 

# save with device = cairo.pdf to embed fonts
ggsave("cocktailplot.pdf", width = 1530, height = 1128, units = "px", scale = 1.75, device = cairo_pdf)
