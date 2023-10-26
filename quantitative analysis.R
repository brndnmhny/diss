#### Setup ####

setwd("C:/Users/brend/Documents/diss/")
pacman::p_load(tidyverse, igraph, ggraph, ggplot2, grid, gridExtra, lubridate, forcats, openxlsx, psych, hash, backbone, irr)

left_set = c("GeorgeFloydRevolution", "DenverProtests", "policebrutality",
             "RepublicanValues", "dsa", "leftistvexillology", "thedavidpakmanshow",
             "racism", "VaushV", "asianamerican", "Trumpvirus", "ACAB",
             "WayOfTheBern", "SocialistRA", "AntifascistsofReddit", "BlackLivesMatter",
             "Anarchy101",
             "esist", "MarchAgainstNazis", "Political_Revolution",
             "DemocraticSocialism", "AOC", "COMPLETEANARCHY", "BreadTube",
             "ShitLiberalsSay", "Fuckthealtright", "DankLeft", "ENLIGHTENEDCENTRISM",
             "punk", "communism", "FragileWhiteRedditor", "Anarchism", "lostgeneration",
             "socialism", "ToiletPaperUSA", "collapse", "SandersForPresident",
             "Bad_Cop_No_Donut", "WitchesVsPatriarchy", "ABoringDystopia",
             "LateStageCapitalism", "WhitePeopleTwitter", "BlackPeopleTwitter", "TwoXChromosomes")
right_set = c("askaconservative", "AskThe_Donald", "AskTrumpSupporters",
              "CoincidenceTheorist", "Conservative", "ConservativeMemes", "conservatives",
              "ConservativesOnly", "DrainTheSwamp", "HillaryForPrison",
              "kotakuinaction2", "LouderWithCrowder", "NEWPOLITIC", "ProtectAndServe",
              "Republican", "ShitPoliticsSays", "TimPool", "TruthLeaks", "tucker_carlson", "walkaway")
sub_set = c(left_set, right_set)

posts <- read_csv("quantitative analysis/post_stats.csv") %>% mutate(subreddit = paste("r/", subreddit, sep = ""))

top_threads <- readxl::read_xlsx("qualitative coding/subreddit_sample_top_threads.xlsx") %>% 
  filter(date > '2020-05-25') %>% mutate(subreddit = substr(subreddit, 3, nchar(subreddit)))
left_threads <- top_threads %>% filter(subreddit %in% left_set)
right_threads <- top_threads %>% filter(subreddit %in% right_set)

# coded_subs <- c("Anarchy101", "AskTrumpSupporters", "BlackLivesMatter", "BreadTube", "CoincidenceTheorist",
#                 "Conservative", "DrainTheSwamp", "FragileWhiteRedditor", "kotakuinaction2", "LateStageCapitalism")
# 
# for (sub in coded_subs){
#   obj = str_c(sub)
#   file = str_c("qualitative coding/code matrices/", sub, "_Code Matrix.xlsx")
#   data <- readxl::read_xlsx(file) 
#   if (grepl(" : ", data[,1])){
#   data <- data %>% separate(`...1`, c("nrow", "Entry"))  %>% select(-nrow)%>% 
#     arrange(as.numeric(Entry))
#   } else{
#     colnames(data)[1] <- "Entry"
#     data$Entry <- as.character(data$Entry)
#   }
#   for (i in 2:length(colnames(data))){
#     #print(i)
#     name = unlist(str_split(colnames(data)[i], " : "))[2]
#     paste(name)
#     colnames(data)[i] <- name
#   }
#   assign(obj, data)
# }

mods <- readxl::read_xlsx("methods/sampling/mod_set.xlsx")


#### Exploratory ####

sub_stats_1 <- posts %>% group_by(subreddit) %>% summarize(`posts_may&june` = n(), removal_pct = (1 - (sum(removal=="None") / n())) * 100,
                                                         mod_removal_pct = sum(removal=="moderator"|removal=="automod_filtered") / n() * 100,
                                                         reddit_removal_pct = sum(removal=="reddit") / n() * 100,
                                                         score_min = min(score), score_mean = mean(score), score_median = median(score),
                                                         score_max = max(score), score_sd = sd(score),
                                                         ratio_min = min(ratio), ratio_mean = mean(ratio), ratio_median = median(ratio),
                                                         ratio_max = max(ratio), ratio_sd = sd(ratio),
                                                         stickied_pct = sum(`stickied?`==TRUE) / n() * 100,
                                                         pinned_pct = sum(`pinned?`==TRUE) / n() * 100,
                                                         length_min = min(`post length`), length_mean = mean(`post length`), length_median = median(`post length`),
                                                         length_max = max(`post length`), length_sd = sd(`post length`),
                                                         comments_min = min(comments), comments_mean = mean(comments), comments_median = median(comments),
                                                         comments_max = max(comments), comments_sd = sd(comments),
                                                         crossposts_min = min(crossposts), crossposts_mean = mean(crossposts), crossposts_median = median(crossposts),
                                                         crossposts_max = max(crossposts), crossposts_sd = sd(crossposts),
                                                         selfpost_pct = sum(`selfpost?`) / n() * 100,
                                                         videopost_pct = sum(`videopost?`) / n() * 100,
                                                         awards_min = min(awards), awards_mean = mean(awards), awards_median = median(awards),
                                                         awards_pct = sum(awards>0) / n() * 100,
                                                         awards_max = max(awards), awards_sd = sd(awards),
                                                         flair_pct = (1 - (sum(author_flair=="None") / n())) * 100
                                                         ) %>% slice(-35)

sub_stats_2 <- rbind(readxl::read_xlsx("methods/sampling/subreddit_codeset.xlsx", sheet = "left_set", range = "A1:H45"), 
                     readxl::read_xlsx("methods/sampling/subreddit_codeset.xlsx", sheet = "right_set", range = "A1:H22"))[,-4]

all_stats <- posts %>% summarize(`posts_may&june` = n(), removal_pct = (1 - (sum(removal=="None") / n())) * 100,
                                 mod_removal_pct = sum(removal=="moderator"|removal=="automod_filtered") / n() * 100,
                                 reddit_removal_pct = sum(removal=="reddit") / n() * 100,
                                 score_min = min(score), score_mean = mean(score), score_median = median(score),
                                 score_max = max(score), score_sd = sd(score),
                                 ratio_min = min(ratio), ratio_mean = mean(ratio), ratio_median = median(ratio),
                                 ratio_max = max(ratio), ratio_sd = sd(ratio),
                                 stickied_pct = sum(`stickied?`==TRUE) / n() * 100,
                                 pinned_pct = sum(`pinned?`==TRUE) / n() * 100,
                                 length_min = min(`post length`), length_mean = mean(`post length`), length_median = median(`post length`),
                                 length_max = max(`post length`), length_sd = sd(`post length`),
                                 comments_min = min(comments), comments_mean = mean(comments), comments_median = median(comments),
                                 comments_max = max(comments), comments_sd = sd(comments),
                                 crossposts_min = min(crossposts), crossposts_mean = mean(crossposts), crossposts_median = median(crossposts),
                                 crossposts_max = max(crossposts), crossposts_sd = sd(crossposts),
                                 selfpost_pct = sum(`selfpost?`) / n() * 100,
                                 videopost_pct = sum(`videopost?`) / n() * 100,
                                 awards_min = min(awards), awards_mean = mean(awards), awards_median = median(awards),
                                 awards_pct = sum(awards>0) / n() * 100,
                                 awards_max = max(awards), awards_sd = sd(awards),
                                 flair_pct = (1 - (sum(author_flair=="None") / n())) * 100
                                 ) %>% slice(-35) %>% mutate(subreddit = "all")

sub_stats <- merge(sub_stats_2, sub_stats_1, by = "subreddit")

for (i in unique(posts$removal)){
  pct <- nrow(posts %>% filter(removal == i)) / 297505 * 100
  print(paste(i, pct, sep = ": "))
}

left_posts <- posts %>% mutate(subreddit = substring(subreddit, 3)) %>% filter(subreddit %in% left_set)
right_posts <- posts %>% mutate(subreddit = substring(subreddit, 3)) %>% filter(subreddit %in% right_set)
posts = rbind(left_posts, right_posts)

# link_bases = c()
# link_bases_left = c()
# link_bases_right = c()
# for (p in 1:nrow(posts)){
#   #print(posts[p,])
#   i = posts[p,]$link
#   sub = posts[p,]$subreddit
#   #print(i)
#   #print(sub)
#   if (i != "None"){
#     split1 = str_split_i(i, ".com/|.it/|.be/|.org/|.net/|.gov/|.edu/|.cc/|.is/|.fm/|.uk/|.va/|.tw/|.ca/|.fr/|.news/|.co/|.moe/|.in/|.is/|.fo/|.ly/|.au/|.hk/|.us/|.il/|.tv/|.nyc/", 1)
#     split2 = str_split_i(split1, "https://|http://", 2)
#     #print(split2)
#     if (isTRUE(grepl("www.", split2, fixed = TRUE))){
#       split3 = str_split_i(split2, "www.", 2)
#       #print(split3)
#       link_bases <- c(link_bases, split3)
#       if (sub %in% left_set){
#         link_bases_left <- c(link_bases_left, split3)
#       }else{
#         link_bases_right <- c(link_bases_right, split3)
#       }
#     }  else{
#       #print(split2)
#       link_bases <- c(link_bases, split2)
#       if (sub %in% left_set){
#         link_bases_left <- c(link_bases_left, split2)
#       }else{
#         link_bases_right <- c(link_bases_right, split2)
#       }
#     }
#   }
# }
# 
# link_freq <- as.data.frame(table(link_bases)) %>% arrange(-Freq)
# link_freq_left <- as.data.frame(table(link_bases_left)) %>% arrange(-Freq)
# link_freq_right <- as.data.frame(table(link_bases_right)) %>% arrange(-Freq)
# 
# wb <- createWorkbook()
# addWorksheet(wb, "links")
# addWorksheet(wb, "left links")
# addWorksheet(wb, "right links")
# writeData(wb, "links", link_freq)
# writeData(wb, "left links", link_freq_left)
# writeData(wb, "right links", link_freq_right)
# saveWorkbook(wb, file = "quantitative analysis/shared links.xlsx", overwrite = TRUE)

link_freq <- readxl::read_xlsx("quantitative analysis/shared links.xlsx", sheet = "links")
link_freq_left <- readxl::read_xlsx("quantitative analysis/shared links.xlsx", sheet = "left links")
link_freq_right <- readxl::read_xlsx("quantitative analysis/shared links.xlsx", sheet = "right links")


#### Coding Analysis ####

coded_threads <-  coded_threads_nosubs <- readxl::read_xlsx("qualitative coding/coded threads.xlsx") %>% mutate_if(is.double, as.numeric) %>% 
  mutate(Republicansasracist = Republicansasracist + Trumpasracist) %>% 
  mutate(Progovernment = `Progovernment spending`, .before = `Just a joke`) %>% 
  mutate(`post?` = ifelse(depth == 0, 1, 0), .after = depth) %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% select(-Trumpasracist, -`Progovernment spending`)

subframes <- hash("Anticapitalism" = c("Anarchism", "Anticlassism", "Communism"), "AntiBLM" = "ALM",
                  "AntiDemocrat" = c("AntiBiden", "AntiObama", "Clinton corruption", "Democratsascriminals",
                                     "Democratsasfascist", "Democratsasinsurrectionists", "Liberalsasracist"),
                  "Antigovernment" = c("Governmentascorrupt", "Governmentasexploitative", 
                                       "Governmentasinefficient"),
                  "Antipolice" = c("Policingasbrutality", "Policingascapitalist", "Policingasracist"),
                  "Propolice" = c("Policeasheroes", "Policeasvictims"),
                  "Antiprotests" = c("Protestsasinauthentic", "Protestsasrioting", "Protestsasterrorism", 
                                     "Protestsastheft", "Protestsasunorganized", "Save Our Statues"),
                  "Proprotests" = c("People over property", "Protestersasvictims", "Protestsaspatriotic",
                                    "Protestsaspeaceful", "Protestsaspractice"),
                  "AntiRepublican" = c("AntiTrump", "Republicansasfascist", "Republicansasracist"),
                  "ProRepublican" = c("Conservativesasvictims", "ProTrump")
                  #, "Solutions" = c("Defund_abolish the police", "Dual power", "Education",
                  #                "End police unions", "Fire bad cops", "Increased police accountability",
                  #                "Violence against protesters")
                  )

counterframes <- c("Capitalism" = "Anticapitalism", "Anticapitalism" = "Capitalism", "Communism" = "Anticommunism",
                   "Anticommunism" = "Communism", "Christianity" = "AntiChristianity",
                   "AntiChristianity" = "Christianity", "Feminism" = "Antifeminism",
                   "Antifeminism" = "Feminism", "Antiracism" = "White supremacy", "White supremacy" = "Antiracism",
                   "Antiabortion" = "Prochoice", "Prochoice" = "Antiabortion", "AntiBernie" = "ProBernie",
                   "ProBernie" = "AntiBernie", "AntiBLM" = "ProBLM", "ProBLM" = "AntiBLM", 
                   "Intersectionality" = "Class first", "Class first" = "Intersectionality",
                   "AntiAmerica" = "ProAmerica", "ProAmerica" = "AntiAmerica", "AntiDemocrat" = "ProDemocrat",
                   "ProDemocrat" = "AntiDemocrat", "Floydascriminal" = "Floydasmartyr",
                   "Floydasmartyr" = "Floydascriminal", "Antigovernment" = "Progovernment",
                   "Progovernment" = "Antigovernment", "Mediaasconservative" = "Mediaasliberal",
                   "Mediaasliberal" = "Mediaasconservative", "Antipolice" = "Propolice",
                   "Propolice" = "Antipolice", "Antiprotests" = "Proprotests", 
                   "Proprotests" = "Antiprotests", "AntiRepublican" = "ProRepublican", 
                   "ProRepublican" = "AntiRepublican", "AntiTrump" = "ProTrump", "ProTrump" = "AntiTrump",
                   "BLMasreformist" = "BLMasrevolutionary",
                   "BLMasrevolutionary" = "BLMasreformist", "Reformasnecessary" = "Revolutionasinevitable",
                   "Revolutionasinevitable" = "Reformasnecessary", "Antiporn" = "Proporn",
                   "Proporn" = "Antiporn")

for (x in keys(subframes)){
  subs = subframes[[x]]
  for (y in subs){
    #print(y)
    coded_threads_nosubs <- coded_threads_nosubs %>% mutate(!!sym(x) := !!sym(x) + !!sym(y)) %>% 
      select(-!!sym(y))
  }
}

coded_threads_nosubs <- coded_threads_nosubs %>% mutate(Discourses = rowSums(.[12:39]), 
                                                        Frames = rowSums(.[42:194]))

del = c()
for (i in 9:length(coded_threads)){
  if (sum(coded_threads[,i], na.rm = TRUE) <= 1){
    del <- append(del, i)
  }
}
coded_threads <- coded_threads[-del]

left_codes <- coded_threads %>% filter(subreddit %in% left_set)
left_codes_sums <- rbind(left_codes, c(NA, NA, NA, NA, NA, NA, NA, NA, colSums(left_codes[9:length(left_codes)], na.rm = TRUE)))
left_codes_nosubs <- coded_threads_nosubs %>% filter(subreddit %in% left_set)
right_codes <- coded_threads %>% filter(subreddit %in% right_set)
right_codes_sums <- rbind(right_codes, c(NA, NA, NA, NA, NA, NA, NA, NA, colSums(right_codes[9:length(right_codes)], na.rm = TRUE)))
right_codes_nosubs <- coded_threads_nosubs %>% filter(subreddit %in% right_set)
left_freq <- as.data.frame(t(left_codes_sums[nrow(left_codes_sums), 
                                             order(left_codes_sums[nrow(left_codes_sums),], decreasing = TRUE)]))
right_freq <- as.data.frame(t(right_codes_sums[nrow(right_codes_sums), 
                                               order(right_codes_sums[nrow(right_codes_sums),], decreasing = TRUE)]))
left_codes_sums <- right_codes_sums <- NULL

colnames(left_freq) <- colnames(right_freq) <- "freq"

left_content <- left_freq %>% slice(1, 2, 3, 8)
left_stats <- left_freq %>% slice(-1, -2, -3, -8) %>% filter(freq > 0)
left_stats$freq <- as.numeric(left_stats$freq)
left_stats$code <- rownames(left_stats)
left_stats <- left_stats %>% mutate(ratio = freq / sum(freq))
left_stats <- left_stats[,c(2,1,3)]
right_stats <- right_freq %>% slice(-1, -2, -91, -118) %>% filter(freq > 0) 
right_content <- right_freq %>% slice(1, 2, 91, 118)
right_stats$freq <- as.numeric(right_stats$freq)
right_stats$code <- rownames(right_stats)
right_stats <- right_stats %>% mutate(ratio = freq / sum(freq))
right_stats <- right_stats[,c(2,1,3)]
content <- merge(left_content, right_content, by = "row.names")
colnames(content) <- c("codes", "left", "right")

consensus <- function(frame, set){
  set_codes <- get(paste(set, "_codes", sep = ""))
  set_stats <- get(paste(set, "_stats", sep = ""))
  #print(set_codes)
  if (is.null(invert(subframes)[[frame]]) == FALSE){
    superframe = invert(subframes)[[frame]]
    #print(superframe)
  } else{
    superframe = "superframe"
  }
  if (frame %in% counterframes){
    counterframe <- counterframes[[frame]]
    #print(counterframe)
    nosub <- get(paste(set, "_codes_nosubs", sep = ""))
    if (counterframe %in% colnames(nosub)){
      #print("in nosub")
      s <- nosub %>% filter(!!sym(counterframe) > 0) %>% select(score)
      #print(s)
      #print(nrow(s))
      if (nrow(s) > 0){
        subset_counter <- sum(s)
        counter_ratio <- nrow(nosub %>% filter(!!sym(counterframe) > 0)) / sum(set_stats$freq)
      } else{
        subset_counter = 0
        counter_ratio = 0
      }
    } else {
      #print("not in nosub")
      s <- set_codes %>% filter(!!sym(counterframe) > 0) %>% select(score)
      #print(nrow(s))
      if (nrow(s) > 0){
        #print("summing")
        subset_counter <- sum(s)
        counter_ratio <- nrow(set_codes %>% filter(!!sym(counterframe) > 0)) / sum(set_stats$freq)
      } else{
        subset_counter = 0
        counter_ratio = 0
      }
    }
  } else if(superframe %in% counterframes){
      counterframe <- counterframes[[superframe]]
      #print(counterframe)
      nosub <- get(paste(set, "_codes_nosubs", sep = ""))
      s <- nosub %>% filter(!!sym(counterframe) > 0) %>% select(score)
      #print(s)
      if (nrow(s) > 0){
        #print("summing")
        subset_counter <- sum(s)
        counter_ratio <- nrow(nosub %>% filter(!!sym(counterframe) > 0)) / sum(set_stats$freq)
      } else{
        subset_counter = 0
        counter_ratio = 0
      }
  } else{
    subset_counter = 0
    counter_ratio = 0
  }
  #print(subset_counter)
  #print(counterframe)
  #frame <- enquo(frame)
  #print(frame)
  subset <- set_codes %>% filter(!!sym(frame) > 0)
  frame_ratio <- set_stats[which(set_stats$code == frame),]$ratio
  frame_freq <- set_stats[which(set_stats$code == frame),]$freq
  #print(frame_ratio)
  #print(sum(subset %>% select(score)))
  frame_score = sum(subset %>% select(score))
  counter_score = subset_counter
  raw_score = frame_score - counter_score
  weighted_frame = frame_score * frame_ratio
  weighted_counter = counter_score * counter_ratio
  value = weighted_frame - weighted_counter
  subs_used = nrow(unique(subset %>% select(subreddit)))
  subs_total = length(unique(set_codes$subreddit))
  subs_ratio =  subs_used / subs_total
  breadth = frame_freq * subs_ratio
  consensus = value * breadth
  #cat(" net score =", score, "\n", "subs used =", subs_used, "\n",
  #          "total subs =", subs_total, "\n")
  return(c(frame_score, counter_score, raw_score, subs_ratio, value, breadth, consensus))
}

left_stats_consensus <- cbind(left_stats, 
                    as.data.frame(t(as.data.frame(lapply(left_stats$code, consensus, set = "left")))))
  #mutate(breadth = freq * V5, .before = V6)
  #%>% mutate(V2 = format(V2, scientific = FALSE), V3 = format(V3, scientific = FALSE))
right_stats_consensus <- cbind(right_stats, 
                     as.data.frame(t(as.data.frame(lapply(right_stats$code, consensus, set = "right")))))
  #mutate(breadth = freq * V5, .before = V6)
  #%>% mutate(V2 = format(V2, scientific = FALSE), V3 = format(V3, scientific = FALSE))
rownames(left_stats_consensus) <- rownames(right_stats_consensus) <- NULL
colnames(left_stats_consensus) <- colnames(right_stats_consensus) <- c("code", "freq", "ratio", "frame score",
                                                                       "counterframe score", "unweighted net score",
                                                                       "subreddit use ratio", "value", 
                                                                       "breadth", "consensus")


#### Correlations ####

discourses <- lm(Antiintellectualism ~ Populism + Christianity, coded_threads_nosubs %>% mutate(Discourses = Discourses - (Populism + Christianity),
                                                                                                Antiintellectualism = Discourses - Frames))
summary(discourses)

disagreement_depth <- lm(Disagreement ~ depth, coded_threads %>% filter(depth > 0))
summary(disagreement_depth)
# doesn't feel super shocking but

left_allcodes <- lm(score ~ . - `thread id` - date - `post?` - link - `post/comment`, left_codes %>% filter(`post?` == 0))
summary(left_allcodes)
right_allcodes <- lm(score ~ . - `thread id` - date - `post?` - link - `post/comment`, right_codes %>% filter(`post?` == 0))
summary(right_allcodes)

left_score_ratio <- lm(value ~ breadth, left_stats_consensus)
summary(left_score_ratio)
right_score_ratio <- lm(value ~ breadth, right_stats_consensus)
summary(right_score_ratio)

sub_freq <- cbind(rbind(left_codes %>% select(-Counterframing, -Disagreement, -`Emotional support`, -Mobilization) %>% 
                          filter(rowSums(.[9:150]) > 0) %>% group_by(subreddit) %>% summarise(freq = n()),
                  right_codes %>% select(-Counterframing, -Disagreement, -`Emotional support`, -Mobilization) %>% 
                    filter(rowSums(.[9:150]) > 0) %>% group_by(subreddit) %>% summarise(freq = n())), 
                  c(113978, 110537, 161937, 244112, 786967, 89940, 5594, 986077, 24500, 27817))
colnames(sub_freq) <- c("subreddit", "freq", "subscribers")

sub_freq_corr <- lm(subscribers ~ freq, data = sub_freq)
summary(sub_freq_corr)

shapiro.test(left_stats_consensus$value[1:21])
shapiro.test(left_stats_consensus$`frame score`[1:21])
shapiro.test(left_stats_consensus$`counterframe score`[1:21])
shapiro.test(left_stats_consensus$freq[1:21])
shapiro.test(left_stats_consensus$`subreddit use ratio`[1:21])
shapiro.test(left_stats_consensus$breadth[1:21])
shapiro.test(left_stats_consensus$consensus[1:21])
shapiro.test(right_stats_consensus$value[1:31])
shapiro.test(right_stats_consensus$`frame score`[1:31])
shapiro.test(right_stats_consensus$`counterframe score`[1:31])
shapiro.test(right_stats_consensus$freq[1:31])
shapiro.test(right_stats_consensus$`subreddit use ratio`[1:31])
shapiro.test(right_stats_consensus$breadth[1:31])
shapiro.test(right_stats_consensus$consensus[1:31])

wilcox.test(left_stats_consensus$value[1:21], right_stats_consensus$value[1:31])
wilcox.test(left_stats_consensus$value[1:52], right_stats_consensus$value[1:48])
wilcox.test(left_stats_consensus$ratio[1:21], right_stats_consensus$ratio[1:31])
wilcox.test(left_stats_consensus$`frame score`[1:21], right_stats_consensus$`frame score`[1:31])
wilcox.test(left_stats_consensus$`counterframe score`[1:21], right_stats_consensus$`counterframe score`[1:31])
wilcox.test(left_stats_consensus$freq[1:21], right_stats_consensus$freq[1:31])
wilcox.test(left_stats_consensus$`subreddit use ratio`[1:21], right_stats_consensus$`subreddit use ratio`[1:31])
wilcox.test(left_stats_consensus$breadth[1:21], right_stats_consensus$breadth[1:31])
wilcox.test(left_stats_consensus$consensus[1:21], right_stats_consensus$consensus[1:31])

left_codes_sig <- data.frame()
right_codes_sig <- data.frame()

for (code in as.vector(left_stats_consensus %>% filter(ratio >= 0.01) %>% select(code))$code){
  p <- wilcox.test(as.vector(left_codes %>% filter(!!sym(code) == 1) %>% select(score))$score, mu = 0, alternative = "greater")$p.value
  pair <- c(code, p)
  left_codes_sig <- rbind(left_codes_sig, pair)
  #print(pair)
}

for (code in as.vector(right_stats_consensus %>% filter(ratio >= 0.01) %>% select(code))$code){
  p <- wilcox.test(as.vector(right_codes %>% filter(!!sym(code) == 1) %>% select(score))$score, mu = 0, alternative = "greater")$p.value
  pair <- c(code, p)
  right_codes_sig <- rbind(right_codes_sig, pair)
  #print(pair)
}

colnames(left_codes_sig) <- colnames(right_codes_sig) <- c("code", "p-value")
  

#### Networks ####

left_codes_net <- left_codes %>% select(-Counterframing, -Disagreement, -`Emotional support`, -Mobilization) %>% 
  filter(rowSums(.[9:150]) > 0)
right_codes_net <- right_codes %>% select(-Counterframing, -Disagreement, -`Emotional support`, -Mobilization) %>% 
  filter(rowSums(.[9:150]) > 0)

left_edges <- data.frame()
right_edges <- data.frame()

for (i in 1:nrow(left_codes_net)) {
  for (j in 9:150) {
    if (left_codes_net[i, j] >= 1) {
      #print(as.character(left_codes_net[i, 1]))
      #print(as.character(colnames(left_codes_net)[j]))
      row <- c(as.character(left_codes_net[i, 1]), as.character(colnames(left_codes_net)[j]))
      left_edges <- rbind(left_edges, row)
    }
  }
}

for (i in 1:nrow(right_codes_net)) {
  for (j in 9:150) {
    if (right_codes_net[i, j] >= 1) {
      #print(as.character(right_codes_net[i, 1]))
      #print(as.character(colnames(right_codes_net)[j]))
      row <- c(as.character(right_codes_net[i, 1]), as.character(colnames(right_codes_net)[j]))
      right_edges <- rbind(right_edges, row)
    }
  }
}

colnames(left_edges) <- colnames(right_edges) <- c("subreddit", "code")

left_graph <- graph_from_edgelist(as.matrix(left_edges), directed = FALSE)
left_matrix <- get.adjacency(left_graph, sparse = FALSE)
left_graph <- graph_from_adjacency_matrix(left_matrix, mode = "undirected", weighted = TRUE)
left_graph <- igraph::simplify(left_graph, edge.attr.comb = list(weight = "sum")
                               )
for (i in 1:115) {
  if (V(left_graph)[i]$name %in% c("LateStageCapitalism", "Anarchy101", "BreadTube",
                                   "BlackLivesMatter", "FragileWhiteRedditor")) {
    left_graph <- set_vertex_attr(left_graph, "type", index = i, TRUE)
  } else {
    left_graph <- set_vertex_attr(left_graph, "type", index = i, FALSE)
  }
}

left_graph_subs <- bipartite_projection(left_graph, which = "true")
ggraph(left_graph_subs, layout = "fr") + 
  geom_edge_link(aes(width = weight), edge_color = "grey50") + 
  geom_node_point(size = 10, shape = 21, fill = "orchid4") + 
  scale_size(range = c(2,15)) +
  geom_node_text(aes(label = name), color = "black", nudge_y = 0.02, repel = TRUE) + 
  theme_graph()

right_graph <- graph_from_edgelist(as.matrix(right_edges), directed = FALSE)
right_matrix <- get.adjacency(right_graph, sparse = FALSE)
right_graph <- graph_from_adjacency_matrix(right_matrix, mode = "undirected", weighted = TRUE)
right_graph <- igraph::simplify(right_graph, edge.attr.comb = list(weight = "sum")
                                )
for (i in 1:119) {
  if (V(right_graph)[i]$name %in% c("Conservative", "AskTrumpSupporters", "kotakuinaction2",
                                    "DrainTheSwamp", "CoincidenceTheorist")) {
    right_graph <- set_vertex_attr(right_graph, "type", index = i, TRUE)
  } else {
    right_graph <- set_vertex_attr(right_graph, "type", index = i, FALSE)
  }
}

right_graph_subs <- bipartite_projection(right_graph, types = V(right_graph)$type, which = "true")
ggraph(right_graph_subs, layout = "fr") + 
  geom_edge_link(aes(width = weight), edge_color = "grey50") + 
  geom_node_point(size = 10, shape = 21, fill = "orchid4") + 
  scale_size(range = c(2,15)) +
  geom_node_text(aes(label = name), color = "black", nudge_y = 0.02, repel = TRUE) + 
  theme_graph()


#### Mods ####

# total mods in each group #

mods_total <- mods %>% slice(-21, -22, -45, -48, -53, -56) %>% rowwise(subreddit) %>% summarise(total = sum(!is.na(c_across(mods:`...53`)))) %>% arrange(-total)

# mods of more than one group #

mods_freq <- as.data.frame(table(as.vector(t(mods[,2:53] %>% slice(-21, -22, -45, -48, -53, -56))))) %>% 
  filter(!grepl("bot|Bot|BOT|Auto", Var1)) %>% arrange(-Freq)

# mod network #

mods_edgelist <- data.frame()
for (i in 1:nrow(mods)){
  sub = as.character(mods[i, 1])
  #print(sub)
  for (j in 2:53){
    if (!is.na(mods[i, j]) & !grepl("bot|Bot|BOT|Auto|private|banhammered", mods[i, j])){
      mod = as.character(mods[i, j])
      row = c(sub, mod)
      #print(mod)
      #print(row)
      mods_edgelist <- rbind(mods_edgelist, row)
    }
  }
}
colnames(mods_edgelist) <- c("subreddit", "mod")

mods_graph <- graph_from_edgelist(as.matrix(mods_edgelist), directed = FALSE)
mods_graph <- delete_vertices(mods_graph, degree(mods_graph) <= 1)
mods_graph <- igraph::simplify(mods_graph, remove.loops = TRUE)
mods_graph <- delete_vertices(mods_graph, degree(mods_graph) == 0)

for (i in 1:101){
  sub = V(mods_graph)[i]$name
  if (sub %in% left_set){
    V(mods_graph)[i]$type <- "left-wing"
  }
  if (sub %in% right_set){
    V(mods_graph)[i]$type <- "right-wing"
  }
  if (sub %in% left_set == FALSE & sub %in% right_set == FALSE){
    V(mods_graph)[i]$type <- "mod"
  }
}

ggraph(mods_graph, layout = "fr") + 
  geom_edge_link(edge_color = "grey50") + 
  geom_node_point(aes(fill = type), size = 10, shape = 21) + 
  scale_size(range = c(2,15)) +
  geom_node_text(aes(label = name), color = "black", nudge_y = 0.02, repel = TRUE) + 
  theme_graph()


#### Coding Reliability ####

codes <- readxl::read_xlsx("methods/sampling/coding reliability check.xlsx") %>% select(`brend code`, `crystal code`) %>% t()
kripp.alpha(as.matrix(codes), method = "nominal")


#### Saving ####

wb <- createWorkbook()
addWorksheet(wb, "All Stats")
addWorksheet(wb, "Subreddit Stats")

writeData(wb, "All Stats", all_stats)
writeData(wb, "Subreddit Stats", sub_stats)
saveWorkbook(wb, file = "quantitative analysis/sub stats.xlsx", overwrite = TRUE)

wb <- createWorkbook()
addWorksheet(wb, "All Codes")
addWorksheet(wb, "No Sub-codes")
addWorksheet(wb, "Content Stats")
addWorksheet(wb, "Left Frames Stats")
addWorksheet(wb, "Right Frames Stats")

writeData(wb, "All Codes", coded_threads)
writeData(wb, "No Sub-codes", coded_threads_nosubs)
writeData(wb, "Content Stats", content)
writeData(wb, "Left Frames Stats", left_stats_consensus)
writeData(wb, "Right Frames Stats", right_stats_consensus)
saveWorkbook(wb, file = "quantitative analysis/code stats.xlsx", overwrite = TRUE)
