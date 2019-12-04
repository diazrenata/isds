library(dplyr)

all_sims <- read.csv(here::here("analysis", "sims_nounif.csv"), stringsAsFactors = F)

sim_scores <- all_sims %>%
  select(time_chunk, sim, source) %>%
  filter(sim %in% c(NA, c(1:50))) %>%
  distinct() %>%
  mutate(computer_unimodal = 0,
         human_unimodal = NA,
         lumps = NA,
         gaps = NA,
         gaps_stringent = NA,
         revisit = 0)

order <- sample.int(nrow(sim_scores), size = nrow(sim_scores), replace = F)

for(i in order) {
  if(any(any(is.na(sim_scores[i, c("human_unimodal", "lumps", "gaps", "gaps_stringent", "revisit")])), sim_scores$revisit[i])) {
  if(sim_scores$source[i] == "empirical") {
    this_isd <- filter(all_sims, source == "empirical", time_chunk == sim_scores$time_chunk[i])
  } else {
    this_isd <- filter(all_sims, source == sim_scores$source[i], time_chunk == sim_scores$time_chunk[i], sim == sim_scores$sim[i])
  }

  if(sum(pastecs::turnpoints(density(this_isd$wgt, bw = 15)$y)$peaks) == 1) {
    sim_scores$computer_unimodal[i] <- 1
  }

  print(plot(density(this_isd$wgt)$y))
  sim_scores$human_unimodal[i] <- as.numeric(readline(prompt = "unimodal?"))
  sim_scores$lumps[i] <- as.numeric(readline(prompt = "how many lumps?"))
  sim_scores$gaps[i] <- as.numeric(readline(prompt = "how many gaps?"))
  sim_scores$gaps_stringent[i] <- as.numeric(readline(prompt = "how many gaps - stringent?"))
  sim_scores$revisit[i] <- as.numeric(readline(prompt = "need to revisit?"))
  }
}

write.csv(sim_scores, file = here::here("analysis", "sim_scores_short.csv"), row.names = F)

sim_scores <- sim_scores %>%
  mutate(computer_overest_unimod = computer_unimodal > human_unimodal)
