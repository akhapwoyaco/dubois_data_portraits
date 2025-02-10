#
#############################
#
#
library(lattice)
data_csv = read.csv('data.csv')
data_csv
#
data_csv$class_actual_average <- factor(
  x = paste(
    data_csv$Class, data_csv$Actual.Average, 
    sep = "  |  "), ordered = T)
#
data_csv[,c(-1,-2)]
#
barchart(
  class_actual_average ~ Rent+Food+Clothes+Tax+Other, data = data_csv[,c(-1,-2)],
  stack = TRUE, auto.key = list(space = 'top',column = 5), 
  xlab = NULL,
  scales = list(
    x = list(
      labels = NULL, draw = FALSE),
    y = list(
      rot = 0, cex = 1.25, tck = -1
    )
  ),
  par.settings = list(
    axis.line = list(col = NA),
    axis.text = list(justify = 'center')
  ),
  # panel = function(x, y, ...){
  #   panel.barchart(x, y, ...)
  #   for (i in 1:length(x)){
  #     print(paste("i: ", i, "  , x[i]", x[i]))
  #     
  #     cummulative_heights = cumsum(x[1:i])
  #     panel.text(
  #       x[i], # cummulative_heights,
  #       y[i], labels = as.character(x[i]))
  #   }
  # },
  main = "F"
)
#
library(ggplot2)
library(dplyr)
library(tidyr)
#
data_csv
#
data_csv |> 
  select(-Class, -Actual.Average) |>
  pivot_longer(
    cols = -class_actual_average, 
    names_to = "variables",
    values_to = "values"
  ) |> 
  group_by(class_actual_average) |>
  mutate(values_y = cumsum(values) - .1*values) |>

  ggplot(aes(y = class_actual_average, x = values, fill = variables)) +
  geom_col() +
  scale_fill_brewer(palette = "Dark2") +
  # geom_text(
  #   aes(x = values_y, label = values),
  #   position = position_stack(vjust = 0.5)
  #   ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    plot.background = element_blank(),
    legend.position = 'top',
    legend.title = element_blank()
  )
#






