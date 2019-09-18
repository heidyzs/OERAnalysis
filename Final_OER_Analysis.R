library(tidyverse)
library(ggthemes)
library(ggrepel)
library(gridExtra)
library(tidytext)
library(tidylo)
library(tidyr)
library(scales)
library(reshape2)
library(wordcloud)

oers = readxl::read_xlsx("OER_Narratives.xlsx")
names(oers)
summary(oers)

oers$gender = as.factor(oers$gender)
oers$branch = as.factor(oers$branch)
oers$raterLabel = as.factor(oers$raterLabel)
oers$srLabel = as.factor(oers$srLabel)

summary(oers %>% filter(branch == "47"))

oers1 = oers %>% filter(srLabel %in% c("Highly Qualified", "Most Qualified", "Qualified", "Not Qualified")) %>% 
  filter(raterLabel %in% c("Capable", "Excels","Proficient","Unsatisfactory")) %>% drop_na()

oers1$srLabel = fct_relevel(oers1$srLabel,"Most Qualified","Highly Qualified","Qualified","Not Qualified")

summary(oers1)


####SUMMARY GRAPH OF DATA BY BRANCH ###################
base_branch = c("AD", "AG", "AR", "AV", "CM", "CY", "EN", "FA", "FM", "IN", "MI", "MP", "MS", "OD", "EOD", "QM", "SC", "TC")
combat_arms = c("IN", "AR", "FA")
combat_support = c("IN", "QM", "AR","TC", "FA","OD")
logistics = c("QM", "TC", "OD")

oers1_pect = oers1 %>% 
  group_by(branch, srLabel) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count))
#filter(branch %in% base_branch)

oers2 = oers1_pect %>% 
  ungroup() %>% 
  group_by(branch) %>% 
  mutate(total = sum(count)) %>% 
  ungroup() %>% 
  mutate(final = sum(count)) %>% 
  mutate(percent = total/final)

addline_format <- function(x,...){
  gsub(',','\n',x)
}

#FINAL GRAPH, SORTED BY ORDER#
ggplot(data = oers1_pect, mapping = aes(x = reorder(branch, -count), y = count, fill = srLabel))+
  geom_bar(stat = "identity")+
  labs(y = "Number of OERs", x = "Branch", fill = "Block Check Rating")+
  theme_minimal()+
  scale_fill_manual(values=c("Most Qualified"="springgreen3", "Highly Qualified"="lightgoldenrod", "Qualified"="orangered1", "Not Qualified"="firebrick"))


###A Single Bar Chart, TOTAL OER PROFILE DISTIBUTION, FINAL GRAPH##################
oers1c = oers1 %>% 
  group_by(srLabel) %>% 
  tally() %>% 
  mutate(year = "2017") %>% 
  mutate(pct=n/sum(n))



ggplot(data = oers1c,mapping = aes(x = year, y= pct,fill = srLabel))+
  geom_bar(stat = "identity")+
  geom_text_repel(aes(label = scales::percent(pct)), position = "stack",
                  size = 4.25, hjust = 1.25, vjust = 1) +
  #geom_text_repel(aes(label = paste(as.character(signif(100*pct, 3))) + "%"), position = "stack",
                  #size = 4.25, hjust = 1.25, vjust = 1)+
  geom_abline(intercept = .5, slope = 0, linetype = 2)+
  theme_hc() +
  scale_colour_hc()+
  scale_fill_hc()+
  xlab("Total OER Profile")+
  ylab("Percentage of OERs Received")+
  labs(fill = "")+
  #ggtitle("Distribution of OERS in 2017") + 
  scale_y_continuous(labels=percent) +
  scale_fill_manual(values=c("Most Qualified"="springgreen3", "Highly Qualified"="lightgoldenrod", "Qualified"="orangered1", "Not Qualified"="firebrick")) +
  coord_flip()

#################### DISTRIBUTION OF OERS FOR BRANCHES WITH GREATER THAN 100 OERS############
oers1a = oers1 %>% 
  group_by(branch, srLabel) %>% 
  tally() %>% 
  mutate(pct = n/sum(n))

oers1b = oers1a %>% group_by(branch) %>% filter(branch %in% base_branch) %>%  filter(sum(n)>100)

ggplot(data = oers1b,mapping = aes(x = branch, y= pct,fill = srLabel))+
  geom_bar(stat= "identity")+
  geom_abline(intercept = .5, slope = 0, linetype = 2)+
  theme_hc() +
  scale_colour_hc()+
  scale_fill_hc()+
  xlab("Branch")+
  ylab("Percentage of OERs Received")+
  labs(fill = "")+
  #ggtitle("Distribution of OERS for Braches with Greater than 100 OERS in 2017") +
  scale_y_continuous(labels=percent)+
  scale_fill_manual(values=c("Most Qualified"="springgreen3", "Highly Qualified"="lightgoldenrod", "Qualified"="orangered1", "Not Qualified"="firebrick"))


#####TEXT ANALYSIS###############

#SENIOR NARRATIVES
tidy_oer_senior = oers1 %>% unnest_tokens(srNarrativeWords, srNarrative) %>% 
  anti_join(stop_words, by = c("srNarrativeWords" ="word")) %>% count(branch, srNarrativeWords, sort = TRUE)
tidy_oer_senior <- tidy_oer_senior[!grepl(".*xx.*", tidy_oer_senior$srNarrativeWords),]


###TF_IDF ANALYSIS of One Word####

tidy_idf_senior = tidy_oer_senior %>% bind_tf_idf(srNarrativeWords, branch, n)
tidy_idf_senior = tidy_idf_senior %>% arrange(desc(tf_idf))


#Combat Arms and Support, SORTED#
combined_tf_ordered <-
  tidy_idf_senior %>%
  filter(branch %in% combat_support) %>%
  mutate(srNarrativeWords = factor(srNarrativeWords, levels = rev(unique(srNarrativeWords)))) %>% 
  group_by(branch) %>%
  top_n(10) %>% 
  ungroup() %>% 
  arrange(branch, tf_idf) %>% 
  mutate(order = row_number())

combined_tf_ordered$branch <-  factor(combined_tf_ordered$branch, 
                                      levels = c("IN", "AR", "FA", "QM", "TC", "OD"))

###TF-IDF FINAL GRAPH###
tf_idf_word_plot <- ggplot(combined_tf_ordered, aes(order, tf_idf, fill = branch)) +
  geom_bar(stat = 'identity', show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~branch, ncol = 3, scales = "free") +
  theme_hc() +
  coord_flip()+
  scale_colour_hc()+
  scale_fill_hc() +
  scale_x_continuous(
    breaks = combined_tf_ordered$order,
    labels = paste0(combined_tf_ordered$srNarrativeWords),
    expand = c(0,0)
  ) +
  ggtitle("Tf-Idf By Branch")


#Log Odds (By Hand, Unweighted)#

df1 <- tidy_idf_senior %>% 
  group_by(branch, srNarrativeWords) %>% 
  count(srNarrativeWords, wt = n) #gets count of each individaul word, by branch

df2 <- tidy_idf_senior %>% ungroup() %>% count(srNarrativeWords, wt = n)

dfjoined <- left_join(df1, df2, by = "srNarrativeWords")
dfjoined <- dfjoined %>% mutate(wordsnotinbranch = n.y-n.x)


freq2_df <- tidy_idf_senior %>% group_by(branch) %>% count(branch, wt = n) %>% 
  ungroup() %>% 
  mutate(total = sum(n)) %>% #gets count of total words for branch
  mutate(totalwordsnotinbranch = total-n) 

dfjoined <- left_join(dfjoined, freq2_df, by = "branch")

dfjoined <- dfjoined %>% mutate(top= (n.x+1)/(n+1),
                                bottom = (wordsnotinbranch+1)/(totalwordsnotinbranch+1),
                                logoddsratio = log(top/bottom)) %>% ungroup()

unweighted_log_odds_ordered <- dfjoined %>% 
  filter(n.x >= 20) %>% select(branch, srNarrativeWords, logoddsratio) %>% 
  filter(branch %in%combat_support) %>% 
  mutate(srNarrativeWords = factor(srNarrativeWords, levels = rev(unique(srNarrativeWords)))) %>% 
  group_by(branch) %>%
  top_n(10) %>% 
  ungroup() %>% 
  arrange(branch, logoddsratio) %>% 
  mutate(order = row_number())

unweighted_log_odds_ordered$branch <-  factor(unweighted_log_odds_ordered$branch,
                                        levels = c("IN", "AR", "FA", "QM", "TC", "OD"))

#Final Plot
unweighted_log_odds_plot <- ggplot(unweighted_log_odds_ordered, aes(order, logoddsratio, fill = branch)) +
  geom_bar(stat = 'identity', show.legend = FALSE) +
  labs(x = NULL, y = "log_odds") +
  facet_wrap(~branch, ncol = 3, scales = "free") +
  theme_hc() +
  coord_flip()+
  scale_colour_hc()+
  scale_fill_hc() +
  scale_x_continuous(
    breaks = unweighted_log_odds_ordered$order,
    labels = paste0(unweighted_log_odds_ordered$srNarrativeWords),
    expand = c(0,0)
  ) +
  ggtitle("Unweighted Log-Odds Ratio By Branch")


#Weighted Log Odds Using bind_log_odds from tidylo package
word_log_odds <- tidy_idf_senior %>% bind_log_odds(branch, srNarrativeWords, n) %>% arrange(desc(n))

word_log_odds_ordered <- 
  word_log_odds %>% 
  filter(branch %in%combat_support) %>% 
  mutate(srNarrativeWords = factor(srNarrativeWords, levels = rev(unique(srNarrativeWords)))) %>% 
  group_by(branch) %>%
  top_n(10) %>% 
  ungroup() %>% 
  arrange(branch, log_odds) %>% 
  mutate(order = row_number())

word_log_odds_ordered$branch <-  factor(word_log_odds_ordered$branch,
                                      levels = c("IN", "AR", "FA", "QM", "TC", "OD"))
  
word_log_odds_plot <- ggplot(word_log_odds_ordered, aes(order, log_odds, fill = branch)) +
  geom_bar(stat = 'identity', show.legend = FALSE) +
  labs(x = NULL, y = "log_odds") +
  facet_wrap(~branch, ncol = 3, scales = "free") +
  theme_hc() +
  coord_flip()+
  scale_colour_hc()+
  scale_fill_hc() +
  scale_x_continuous(
    breaks = word_log_odds_ordered$order,
    labels = paste0(word_log_odds_ordered$srNarrativeWords),
    expand = c(0,0)
    ) +
  ggtitle("Weighted Log-Odds By Branch")

#Log Odds vs Tf-Idf Comparison plot  
grid.arrange(tf_idf_word_plot, word_log_odds_plot, nrow = 2)

#Weighted Log Odds vs Unweighted Log Odds
grid.arrange(word_log_odds_plot, unweighted_log_odds_plot)


####################################BIGRAMS##############################################
bigram_senior = oers1 %>% unnest_tokens(bigram, srNarrative, token = "ngrams", n=2)
bigram_separated <- bigram_senior %>% separate(bigram, c("word1", "word2"), sep = " ")
bigram_separated <- bigram_separated[!grepl(".*xx.*", bigram_separated$word1),] 
bigram_separated <- bigram_separated[!grepl(".*xx.*", bigram_separated$word2),]

custom_stop_words<-c("i","in","with","an","have", "the", "is", "a", "senior", "rate",
               "for","to","and","of", "has", "he", "his", "her","she", "be", "career", "as", "by", "as", "at", "this",
                     "who", "rated", "commander", "command")

bigram_filter <- bigram_separated %>% 
  filter(!word1 %in% stop_words$word) %>% 
  filter(!word2 %in% stop_words$word) %>% 
  filter(!word1 %in% custom_stop_words) %>% 
  filter(!word2 %in% custom_stop_words)

top_qualified = c("Most Qualified", "Highly Qualified")


#Unweighted Log Odds on Bigrams
#Combat Arms
df1_bigram_combat <- bigram_filter %>% select(branch, word1, word2, srLabel) %>% 
  unite(bigram, word1, word2, sep = " ") %>% 
  filter(branch %in% combat_arms) %>% 
  filter(srLabel %in% top_qualified) %>% 
  group_by(srLabel, bigram) %>% count(bigram) %>% ungroup()
  

df2_bigram_combat <- df1_bigram_combat %>% ungroup() %>% count(bigram, wt = n)

dfjoined_bigram_combat <- left_join(df1_bigram_combat, df2_bigram_combat, by = "bigram")
dfjoined_bigram_combat <- dfjoined_bigram_combat %>% mutate(bigramsnotinlabel = n.y-n.x)


freq2_bigrams_combat <- df1_bigram_combat %>% group_by(srLabel) %>% count(srLabel, wt = n) %>% 
  ungroup() %>% 
  mutate(total = sum(n)) %>% #gets count of total words for branch
  mutate(totalbigramsnotinlabel = total-n)

dfjoined_bigram_combat <- left_join(dfjoined_bigram_combat, freq2_bigrams_combat, by = "srLabel")

dfjoined_bigram_combat <- dfjoined_bigram_combat %>% mutate(top= (n.x+1)/(n+1),
                                bottom = (bigramsnotinlabel+1)/(totalbigramsnotinlabel+1),
                                logoddsratio = log(top/bottom)) %>% ungroup()

dfjoined_bigram_combat %>% filter(n.x >= 20) %>% select(srLabel, bigram, n.x, logoddsratio)

unweighted_log_odds_bigrams_ordered_combat <- 
  dfjoined_bigram_combat %>% filter(n.x >= 20) %>% select(srLabel, bigram, logoddsratio) %>% 
  mutate(bigram = factor(bigram, levels = rev(unique(bigram)))) %>% 
  group_by(srLabel) %>%
  top_n(15) %>% 
  ungroup() %>% 
  arrange(srLabel, logoddsratio) %>% 
  mutate(order = row_number())

unweighted_log_odds_bigrams_ordered_combat$srLabel <-  factor(unweighted_log_odds_bigrams_ordered_combat$srLabel,
                                              levels = c("Most Qualified", "Highly Qualified"))
#Final Plot
unweighted_log_odds_bigrams_ordered_plot_combat <- ggplot(unweighted_log_odds_bigrams_ordered_combat, aes(order, logoddsratio, fill = srLabel)) +
  geom_bar(stat = 'identity', show.legend = FALSE) +
  labs(x = NULL, y = "log_odds") +
  facet_wrap(~srLabel, ncol = 2, scales = "free") +
  theme_hc() +
  coord_flip()+
  scale_colour_hc()+
  scale_x_continuous(
    breaks = unweighted_log_odds_bigrams_ordered_combat$order,
    labels = paste0(unweighted_log_odds_bigrams_ordered_combat$bigram),
    expand = c(0,0)
  ) +
  scale_fill_manual(values=c("Most Qualified"="springgreen3", "Highly Qualified"="lightgoldenrod")) +
  ggtitle("Unweighted Log-Odds Ratio Combat Arms for Bigrams")

#Logistics
df1_bigram_logistics <- bigram_filter %>% select(branch, word1, word2, srLabel) %>% 
  unite(bigram, word1, word2, sep = " ") %>% 
  filter(branch %in% logistics) %>% 
  filter(srLabel %in% top_qualified) %>% 
  group_by(srLabel, bigram) %>% count(bigram) %>% ungroup()


df2_bigram_logistics<- df1_bigram_logistics %>% ungroup() %>% count(bigram, wt = n)

dfjoined_bigram_logistics <- left_join(df1_bigram_logistics, df2_bigram_logistics, by = "bigram")
dfjoined_bigram_logistics <- dfjoined_bigram_logistics %>% mutate(bigramsnotinlabel = n.y-n.x)

#gets count of total words for branch
freq2_bigrams_logistics <- df1_bigram_logistics %>% group_by(srLabel) %>% count(srLabel, wt = n) %>% 
  ungroup() %>% 
  mutate(total = sum(n)) %>% 
  mutate(totalbigramsnotinlabel = total-n)

dfjoined_bigram_logistics <- left_join(dfjoined_bigram_logistics, freq2_bigrams_logistics, by = "srLabel")

dfjoined_bigram_logistics <- dfjoined_bigram_logistics %>% mutate(top= (n.x+1)/(n+1),
                                                            bottom = (bigramsnotinlabel+1)/(totalbigramsnotinlabel+1),
                                                            logoddsratio = log(top/bottom)) %>% ungroup()

dfjoined_bigram_logistics %>% filter(n.x >= 20) %>% select(srLabel, bigram, n.x, logoddsratio)

unweighted_log_odds_bigrams_ordered_logistics <- 
  dfjoined_bigram_logistics %>% filter(n.x >= 20) %>% select(srLabel, bigram, logoddsratio) %>% 
  mutate(bigram = factor(bigram, levels = rev(unique(bigram)))) %>% 
  group_by(srLabel) %>%
  top_n(15) %>% 
  ungroup() %>% 
  arrange(srLabel, logoddsratio) %>% 
  mutate(order = row_number())

unweighted_log_odds_bigrams_ordered_logistics$srLabel <-  factor(unweighted_log_odds_bigrams_ordered_logistics$srLabel,
                                                              levels = c("Most Qualified", "Highly Qualified"))
#Final Plot
unweighted_log_odds_bigrams_ordered_plot_logistics <- ggplot(unweighted_log_odds_bigrams_ordered_logistics, aes(order, logoddsratio, fill = srLabel)) +
  geom_bar(stat = 'identity', show.legend = FALSE) +
  labs(x = NULL, y = "log_odds") +
  facet_wrap(~srLabel, ncol = 2, scales = "free") +
  theme_hc() +
  coord_flip()+
  scale_colour_hc()+
  scale_x_continuous(
    breaks = unweighted_log_odds_bigrams_ordered_logistics$order,
    labels = paste0(unweighted_log_odds_bigrams_ordered_logistics$bigram),
    expand = c(0,0)
  ) +
  scale_fill_manual(values=c("Most Qualified"="springgreen3", "Highly Qualified"="lightgoldenrod")) +
  ggtitle("Unweighted Log-Odds Ratio Logistics for Bigrams")

#Comparison of Plots
grid.arrange(unweighted_log_odds_bigrams_ordered_plot_combat, unweighted_log_odds_bigrams_ordered_plot_logistics, nrow = 2)

#Weighted Log Odds on Bigrams

#Filter data for maneuver branches
bigram_log_odds_maneuver <- bigram_filter %>% 
  filter(srLabel %in% top_qualified) %>% 
  filter(branch %in% combat_arms) %>% 
  unite(bigram, word1, word2, sep = " ") %>% 
  count(srLabel, bigram, sort = TRUE) %>% 
  bind_log_odds(srLabel, bigram, n) %>% 
  arrange(-log_odds)

#MQ Plot#
log_odds_combat_mq <- bigram_log_odds_maneuver %>% filter(srLabel == "Most Qualified") %>% 
  top_n(15) %>% 
  ungroup %>% 
  mutate(bigram = reorder(bigram, log_odds)) %>% 
  ggplot(aes(bigram, log_odds, fill = srLabel)) + 
  geom_col(fill = "springgreen3", show.legend = FALSE) +
  labs(x = NULL, y = "Log Odds") +
  coord_flip () +
  theme_hc() +
  ggtitle("Most Qualified Maneuver Branches")

#HQ Plot#
log_odds_combat_hq <- bigram_log_odds_maneuver %>% filter(srLabel == "Highly Qualified") %>% 
  top_n(15) %>% 
  ungroup %>% 
  mutate(bigram = reorder(bigram, log_odds)) %>% 
  ggplot(aes(bigram, log_odds, fill = srLabel)) + 
  geom_col(fill = "lightgoldenrod", show.legend = FALSE) +
  labs(x = NULL, y = "Log Odds") +
  coord_flip () +
  theme_hc() +
  ggtitle("Highly Qualified Maneuver Branches")

#Filter data for logistics plot
bigram_log_odds_logistics <- bigram_filter %>% 
  filter(srLabel %in% top_qualified) %>% 
  filter(branch %in% logistics) %>% 
  unite(bigram, word1, word2, sep = " ") %>% 
  count(srLabel, bigram, sort = TRUE) %>% 
  bind_log_odds(srLabel, bigram, n) %>% 
  arrange(-log_odds)

#MQ Plot
log_odds_logistics_mq <- bigram_log_odds_logistics %>% filter(srLabel == "Most Qualified") %>% 
  top_n(15) %>% 
  ungroup %>% 
  mutate(bigram = reorder(bigram, log_odds)) %>% 
  ggplot(aes(bigram, log_odds, fill = srLabel)) + 
  geom_col(fill = "springgreen3", show.legend = FALSE) +
  labs(x = NULL, y = "Log Odds") +
  coord_flip () +
  theme_hc() +
  ggtitle("Most Qualified Logistics Branches")

#HQ Plot
log_odds_logistics_hq <- bigram_log_odds_logistics %>% filter(srLabel == "Highly Qualified") %>% 
  top_n(15) %>% 
  ungroup %>% 
  mutate(bigram = reorder(bigram, log_odds)) %>% 
  ggplot(aes(bigram, log_odds, fill = srLabel)) + 
  geom_col(fill = "lightgoldenrod", show.legend = FALSE) +
  labs(x = NULL, y = "Log Odds") +
  coord_flip () +
  theme_hc() +
  ggtitle("Highly Qualified Logistics Branches")


#Log Odds Plot
grid.arrange(log_odds_combat_mq, log_odds_combat_hq, log_odds_logistics_mq, log_odds_logistics_hq,
             ncol = 2)

#Log Odds vs Percentages Comparison
grid.arrange(log_odds_combat_mq, log_odds_combat_hq, log_odds_logistics_mq, log_odds_logistics_hq,
             combat_mq_bigram_plot, combat_hq_bigram_plot, logistics_mq_bigram_plot, logistics_hq_bigram_plot,
             ncol = 2)


#Bigram Plots on Percentages by Senior Rater Label

#used to get a count of total combat arms
total_combat_most_qualified <- oers1 %>% filter(srLabel == "Most Qualified") %>% 
  filter(branch %in% combat_arms) %>% 
  count(srLabel)
#8864 combat_arms

#Filter Data
most_qualified_combat_bigrams <- bigram_filter %>% 
  filter(srLabel == "Most Qualified") %>% 
  unite(bigram, word1, word2, sep = " ") %>% 
  group_by(branch) %>% 
  filter(branch %in% combat_arms) %>% 
  ungroup() %>% 
  count(bigram, sort = TRUE) %>%
  mutate(perc=n/8864) %>% 
  arrange(perc) %>% 
  top_n(15) %>% 
  ungroup() %>%
  mutate(order = row_number())

#Generate MQ Plot
combat_mq_bigram_plot <-
  ggplot(most_qualified_combat_bigrams, aes(order, perc)) +
  geom_col(fill = "springgreen3", show.legend = FALSE) +
  labs(x = NULL, y = "Percentage of Occurrences") +
  coord_flip()+
  theme_hc()+
  scale_y_continuous(labels=percent)+
  scale_x_continuous(
    breaks = most_qualified_combat_bigrams$order,
    labels = paste0(most_qualified_combat_bigrams$bigram),
    expand = c(0,0)
  )+
  ggtitle("Most Qualified Maneuver Branches")

#used to get a count
total_combat_highly_qualified <- oers1 %>% filter(srLabel == "Highly Qualified") %>% 
  filter(branch %in% combat_arms) %>% 
  count(srLabel)
#11,762 total

#Filter Data
highly_qualified_combat_bigrams <- bigram_filter %>% 
  filter(srLabel == "Highly Qualified") %>% 
  unite(bigram, word1, word2, sep = " ") %>% 
  group_by(branch) %>% 
  filter(branch %in% combat_arms) %>% 
  ungroup() %>% 
  count(bigram, sort = TRUE) %>%
  mutate(perc=n/11762) %>% 
  arrange(perc) %>% 
  top_n(15) %>% 
  ungroup() %>%
  mutate(order = row_number())

#Create HQ Plot
combat_hq_bigram_plot <-
  ggplot(highly_qualified_combat_bigrams, aes(order, perc, fill = "lightgoldenrod")) +
  geom_col(fill = "lightgoldenrod", show.legend = FALSE) +
  labs(x = NULL, y = "Percentage of Occurrences") +
  coord_flip()+  
  theme_hc() +
  scale_colour_hc()+
  scale_fill_hc() +
  scale_y_continuous(labels=percent)+
  scale_x_continuous(
    breaks = highly_qualified_combat_bigrams$order,
    labels = paste0(highly_qualified_combat_bigrams$bigram),
    expand = c(0,0)
  )+
  ggtitle("Highly Qualified Maneuver Branches")

total_log_most_qualified <- oers1 %>% filter(srLabel == "Most Qualified") %>% 
  filter(branch %in% logistics) %>% 
  count(srLabel)
#3819 total

#Filter Data
most_qualified_logistics_bigrams <- bigram_filter %>% 
  filter(srLabel == "Most Qualified") %>% 
  unite(bigram, word1, word2, sep = " ") %>% 
  group_by(branch) %>% 
  filter(branch %in% logistics) %>% 
  ungroup() %>% 
  count(bigram, sort = TRUE) %>%
  mutate(perc=n/3819) %>% 
  arrange(perc) %>% 
  top_n(15) %>% 
  ungroup() %>%
  mutate(order = row_number())

#Generate MQ Plot
logistics_mq_bigram_plot <-
  ggplot(most_qualified_logistics_bigrams, aes(order, perc)) +
  geom_col(fill = "springgreen3", show.legend = FALSE) +
  labs(x = NULL, y = "Percentage of Occurrences") +
  coord_flip()+  
  theme_hc() +
  scale_colour_hc()+
  scale_fill_hc()+
  scale_y_continuous(labels=percent)+
  scale_x_continuous(
    breaks = most_qualified_logistics_bigrams$order,
    labels = paste0(most_qualified_logistics_bigrams$bigram),
    expand = c(0,0)
  )+
  ggtitle("Most Qualified Logistics Branches")

total_logistics_highly_qualified <- oers1 %>% filter(srLabel == "Highly Qualified") %>% 
  filter(branch %in% logistics) %>% 
  count(srLabel)
#8388 total

#Filter Data
highly_qualified_logistics_bigrams <- bigram_filter %>% 
  filter(srLabel == "Highly Qualified") %>% 
  unite(bigram, word1, word2, sep = " ") %>% 
  group_by(branch) %>% 
  filter(branch %in% logistics) %>% 
  ungroup() %>% 
  count(bigram, sort = TRUE) %>%
  mutate(perc=n/8388) %>% 
  arrange(perc) %>% 
  top_n(15) %>% 
  ungroup() %>%
  mutate(order = row_number())

#Generate HQ Plot
logistics_hq_bigram_plot <-
  ggplot(highly_qualified_logistics_bigrams, aes(order, perc)) +
  geom_col(fill = "lightgoldenrod", show.legend = FALSE) +
  labs(x = NULL, y = "Percentage of Occurrences") +
  coord_flip()+  
  theme_hc() +
  scale_colour_hc()+
  scale_fill_hc()+
  scale_y_continuous(labels=percent)+
  scale_x_continuous(
    breaks = highly_qualified_logistics_bigrams$order,
    labels = paste0(highly_qualified_logistics_bigrams$bigram),
    expand = c(0,0)
  )+
  ggtitle("Highly Qualified Logistics Branches")


#Final Plot arranged
grid.arrange(combat_mq_bigram_plot, combat_hq_bigram_plot, logistics_mq_bigram_plot, logistics_hq_bigram_plot, ncol = 2)


#Network Graphs on Bigrams#
library(igraph)
library(ggraph)

bigram_graph_maneuver_mq <- bigram_counts_maneuver %>% 
  filter(srLabel == "Most Qualified") %>% 
  select(bigram, n) %>% 
  separate(bigram, c("word1", "word2"), sep = " ") %>% 
  filter(n > 200) %>% 
  graph_from_data_frame()

set.seed(300)
a <- grid::arrow(type = "closed", length = unit(.1, "inches"))

network_graph_maneuver_mq <- ggraph(bigram_graph_maneuver_mq, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "#009999", 
                 show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, "inches")) +
  geom_node_point(size = 4) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines")) +
  theme_void() +
  ggtitle("Most Qualified")


bigram_graph_maneuver_hq <- bigram_counts_maneuver %>% 
  filter(srLabel == "Highly Qualified") %>% 
  select(bigram, n) %>% 
  separate(bigram, c("word1", "word2"), sep = " ") %>% 
  filter(n > 200) %>% 
  graph_from_data_frame()

set.seed(2019)

network_graph_maneuver_hq <- ggraph(bigram_graph_hq, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "#009999", 
                 show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, "inches")) +
  #scale_edge_width(range = c(0.1, 2)) +
  geom_node_point(size = 4) +
  geom_node_text(aes(label = name), repel = TRUE,
                  point.padding = unit(0.2, "lines")) +
  theme_void() + 
  ggtitle("Highly Qualified")

grid.arrange(network_graph_maneuver_mq, network_graph_maneuver_hq, ncol = 2)


#Network Graph on Bigrams with Clusters
par(mfrow=c(1, 2))
clp_mq_maneuver <- cluster_optimal(bigram_graph_maneuver_mq)
V(bigram_graph_maneuver_mq)$community <- clp_mq_maneuver$membership
E(bigram_graph_maneuver_mq)$width <- E(bigram_graph_maneuver_mq)$n/300

mq_clusters_maneuver <- plot(bigram_graph_maneuver_mq, layout = layout_with_fr,
                    #edge.width = n/300, 
                    edge.arrow.size = 0.2,
                    vertex.color = V(bigram_graph_maneuver_mq)$community, vertex.size = 10,
                    vertex.frame.color = "gray", vertex.label.color = "black",
                    vertex.label.cex = 0.8, vertex.label.dist = 2, 
                    main = "Most Qualified Maneuver Branches")

clp_hq_maneuver <- cluster_optimal(bigram_graph_maneuver_hq)
V(bigram_graph_maneuver_hq)$community <- clp_hq_maneuver$membership

hq_clusters_maneuver <- plot(bigram_graph_maneuver_hq, layout = layout_with_fr,
                    edge.width = n/300, 
                    edge.arrow.size = 0.2,
                    vertex.color = V(bigram_graph_maneuver_hq)$community, vertex.size = 10,
                    vertex.frame.color = "gray", vertex.label.color = "black",
                    vertex.label.cex = 0.8, vertex.label.dist = 2,
                    main = "Highly Qualified Maneuver Branches")
par(mfrow=c(1,1))

#Correlations#
library(widyr)

oer_words_by_branch <- oers1 %>% unnest_tokens(srNarrativeWords, srNarrative) %>% 
  anti_join(stop_words, by = c("srNarrativeWords" ="word"))

oer_words_by_branch <- oer_words_by_branch[!grepl(".*xx.*", oer_words_by_branch$srNarrativeWords),]

oer_words_by_branch <- oer_words_by_branch %>% select(branch, srLabel, srNarrativeWords)

oer_words_by_branch$srLabel <- as.character(oer_words_by_branch$srLabel)
oer_words_by_branch$srLabel[oer_words_by_branch$srLabel == "Most Qualified"] <- "MQ"
oer_words_by_branch$srLabel[oer_words_by_branch$srLabel == "Highly Qualified"] <- "HQ"
oer_words_by_branch$srLabel <- as.factor(oer_words_by_branch$srLabel)


word_cors <- oer_words_by_branch %>% select(srLabel, branch, srNarrativeWords) %>% 
  filter(srLabel %in% c("MQ", "HQ")) %>% 
  #filter(branch %in% combat_arms) %>% 
  ungroup() %>% 
  group_by(srNarrativeWords) %>% filter(n() >=20) %>% 
  pairwise_cor(srNarrativeWords, branch, sort = TRUE, upper = FALSE) %>% 
  filter(correlation < 1) #keep getting erros,can't get it to work when filtered with branch

word_cors %>%
  filter(correlation > .6) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation, edge_width = correlation), edge_colour = "royalblue") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines")) +
  theme_void()

#Frequency Plot#
#Combat Arms#
tidy_words_combat <- oer_words_by_branch %>% filter(branch %in% combat_arms) %>% 
  filter(srLabel %in%(c("Most Qualified", "Highly Qualified"))) %>% 
  select(srLabel, srNarrativeWords) %>% 
  group_by(srLabel, srNarrativeWords) %>% 
  filter(n() >= 20) %>% 
  filter(!srNarrativeWords %in% c("jake", "pete", "ed", "alex", "andy", "nate", "phil", "cadet"))

freq_combat <- tidy_words_combat %>% 
  group_by(srLabel) %>% 
  count(srNarrativeWords, sort = TRUE) %>% 
  left_join(tidy_words_combat %>% group_by(srLabel) %>% summarise(total = n())) %>% 
  mutate(freq = n/total)
  
freq_combat <- freq_combat %>% select(srLabel, srNarrativeWords, freq) %>% 
  spread(srLabel, freq) %>% 
  drop_na()

combat_freq_plot <- ggplot(freq_combat, aes(x = freq_combat$"Most Qualified", y = freq_combat$"Highly Qualified")) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = srNarrativeWords), check_overlap = TRUE, vjust = 1, size = 3) +
  labs(x = "Most Qualified", y = "Highly Qualified") +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red") +
  ggtitle("Combat Arms Frequency Plot")

#Logistics#
tidy_words_log <- oer_words_by_branch %>% filter(branch %in% logistics) %>% 
  filter(srLabel %in%(c("Most Qualified", "Highly Qualified"))) %>% 
  select(srLabel, srNarrativeWords) %>% 
  group_by(srLabel, srNarrativeWords) %>% 
  filter(n() >= 20) %>% 
  filter(!srNarrativeWords %in% c("chris", "mike"))

freq_log <- tidy_words_log %>% 
  group_by(srLabel) %>% 
  count(srNarrativeWords, sort = TRUE) %>% 
  left_join(tidy_words_combat %>% group_by(srLabel) %>% summarise(total = n())) %>% 
  mutate(freq = n/total)

freq_log <- freq_log %>% select(srLabel, srNarrativeWords, freq) %>% 
  spread(srLabel, freq) %>% 
  drop_na()

#colnames(freq_log) <- c("srNarrativeWords", "MQ", "HQ")

#freq_log <- freq_log %>% replace_na(list(MQ = 0.00001, HQ = 0.00001))


log_freq_plot <- ggplot(freq_log, aes(x = freq_log$"Most Qualified", y = freq_log$"Highly Qualified")) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = srNarrativeWords), check_overlap = TRUE, vjust = 0.75, size = 3) +
  labs(x = "Most Qualified", y = "Highly Qualified") +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red") +
  ggtitle("Logistics Frequency Plot")

grid.arrange(combat_freq_plot, log_freq_plot, ncol = 2)



