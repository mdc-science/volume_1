# Packages to be loaded
pacman::p_load(pacman, reshape2, plyr, ggsci, ggforce, ggpubr, scales, ggrepel, 
               RColorBrewer, ggthemes, ggpmisc, ggh4x, tidyverse, here, knitr, 
               readxl, kableExtra, conflicted, ggbreak)

conflicts_prefer(here::here)
conflicts_prefer(ggplot2::annotate)
conflicts_prefer(dplyr::mutate)
conflicts_prefer(dplyr::summarize)
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::rename)
conflicts_prefer(dplyr::arrange)