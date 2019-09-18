######################################
##Messing around with factors###
#####################################

plot_test <- bigram_filter %>%
  unite(bigram, word1, word2, sep = " ") %>% 
  count(branch, bigram) %>% 
  bind_tf_idf(bigram, branch, n) %>%
  arrange(desc(tf_idf)) %>%
  mutate(bigram = factor(bigram, levels = rev(unique(bigram)))) %>%
  group_by(branch) %>% 
  filter(branch %in% combat_arms) %>% 
  top_n(15, tf_idf) %>%
  ungroup %>%
  mutate(branch = factor(branch, levels = c("AR", "IN", "FA")))

ggplot(plot_test, aes(bigram, tf_idf, fill = branch)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~branch, ncol = 1, scales = "free") +
  coord_flip()


##########OTHER TEST PLOTS NOT INCLUDED IN PAPER SO FAR###########

####################TF IDF on BIGRAMS###############################
bigram_tf_idf = bigram_filter %>% 
  unite(bigram, word1, word2, sep = " ") %>% 
  count(branch, bigram) %>% 
  bind_tf_idf(bigram, branch, n)%>% 
  arrange(desc(tf_idf))

bigram_idf_ordered <- bigram_tf_idf %>% 
  mutate(bigram = factor(bigram, levels = rev(unique(bigram)))) %>% 
  group_by(branch) %>% 
  top_n(15) %>% 
  ungroup() %>% 
  arrange(branch, tf_idf) %>% 
  mutate(order = row_number())

### PLOT OF TF_IDF, Not separated by Senior Rater Label###
bigram_tf_idf %>%
  arrange(tf_idf) %>%
  filter(branch %in% combat_support) %>% 
  mutate(bigram = factor(bigram, levels = rev(unique(bigram)))) %>% 
  group_by(branch) %>% 
  top_n(15) %>% 
  ungroup() %>% 
  ggplot(aes(bigram, tf_idf, fill = branch)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~branch, ncol = 2, scales = "free") +
  theme_hc() +
  coord_flip()+
  scale_colour_hc()+
  scale_fill_hc()


######WORD CLOUD BY SENIOR LABEL USING BIGRAMs#####################

top_rates = c("Most Qualified", "Highly Qualified")
dev.new(width=30, height=30, noRStudioGD = TRUE)
par(mar=c(1,1,1,1))

log_bigram_filt <- bigram_filter %>% group_by(branch) %>% filter(branch %in% logistics)
maneuver_bigram_filt <- bigram_filter %>% group_by(branch) %>% filter(branch %in% combat_arms)

#ALL OERS
bigram_filter %>% group_by(srLabel) %>% 
  filter(srLabel %in% top_rates) %>%
  unite(bigram, word1, word2, sep = " ") %>% 
  count(bigram, srLabel, sort = TRUE) %>% 
  acast(bigram ~ srLabel, value.var = "n", fill = 0) %>% 
  comparison.cloud(
    colors = c("#00B2FF", "red"),
    title.size=1.5, max.words=200)

#LOGISTICS
log_bigram_filt %>% 
  group_by(srLabel) %>% 
  filter(srLabel %in% top_rates) %>%
  unite(bigram, word1, word2, sep = " ") %>% 
  count(bigram, srLabel, sort = TRUE) %>% 
  acast(bigram ~ srLabel, value.var = "n", fill = 0) %>% 
  comparison.cloud(
    colors = c("#00B2FF", "red"),
    title.size=1.5, max.words=200)

#Maneuver
maneuver_bigram_filt %>% 
  group_by(srLabel) %>% 
  filter(srLabel %in% top_rates) %>%
  unite(bigram, word1, word2, sep = " ") %>% 
  count(bigram, srLabel, sort = TRUE) %>% 
  acast(bigram ~ srLabel, value.var = "n", fill = 0) %>% 
  comparison.cloud(
    colors = c("#00B2FF", "red"),
    title.size=1.5, max.words=200)
