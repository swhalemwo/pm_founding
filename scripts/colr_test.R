
## ** testing color things

nbr_colrs <- 12

colr_test_df <- as_tibble(cbind(rep(seq(1,10), nbr_colrs)), .name_repair=~c("x"))
colr_test_df$id <- unlist(lapply(seq(1,nbr_colrs), function(x) rep(paste0(rep(letters[x],5), collapse = ""), 10)))
colr_test_df$y <- unlist(lapply(seq(1,nbr_colrs), function(x) rep(x, 10)))

label_df <- colr_test_df %>%
    group_by(id) %>%
    summarise(x=sample(x,1), label = sample(id, 1))

colr_test_df <- as_tibble(merge(colr_test_df, label_df, all.x = T))

devtools::install_github("hoesler/rwantshue")
library(rwantshue)

scheme <- iwanthue() # get the iwanthue singleton
color_manual_auto <- lapply(seq(1,10), function(x) scheme$hex(12, color_space = hcl_presets$fancy_light))

color_space <- list(
    c(0,360),
    c(30,100),
    c(0,80))


color_manual_auto <- lapply(seq(1,5), function(x)
    scheme$hex(12, force_mode=T, quality=50, color_space=color_space))


ggplot(colr_test_df, aes(x=x, y=y, color=id)) +
    geom_line(size=2) +
    scale_color_manual(values = color_manual_auto[[1]])+ 
    geom_label_repel(aes(label = label))

