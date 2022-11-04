###################
## Web scraping tables from https://baldursgate.fandom.com/wiki/Companions
###################

library(rvest)
library(tidyverse)
library(janitor)
bald <- read_html(x = "https://baldursgate.fandom.com/wiki/Companions")
tabout <- html_table(bald)

## Repeat characters and non-character tables
tabout <- tabout[c(-4, -7, -11, -14, -15, -16)]

## Remove empty columns (these are images)
bad_names <- map(tabout, ~str_length(names(.)) == 0)
tabout <- map2(tabout, bad_names, discard)

map(tabout, names)
tabout2 <- list()
for (i in seq_along(tabout)) {
  tabout[[i]] %>%
    select(Companion, Alignment, Race, Class, Str, Dex, Con, Int, Wis, Cha) %>%
    mutate(Str = parse_number(str_extract(Str, "\\d+")),
           Dex = parse_number(str_extract(Dex, "\\d+")),
           Con = parse_number(str_extract(Con, "\\d+")),
           Int = parse_number(str_extract(Int, "\\d+")),
           Wis = parse_number(str_extract(Wis, "\\d+")),
           Cha = parse_number(str_extract(Cha, "\\d+"))) ->
    tabout2[[i]]
}

baldur <- bind_rows(tabout2)
baldur %>%
  mutate(Alignment = case_when(Alignment == "True Neutral" ~ "Neutral Neutral",
                               Alignment == "Neutral" ~ "Neutral Neutral",
                               Alignment == "Lawful\nEvil" ~ "Lawful Evil",
                               TRUE ~ Alignment)) %>%
  separate(col = "Alignment", into = c("ethics", "morals")) %>%
  mutate(Class = str_remove(Class, "\\(.+\\)"),
         Class = str_replace(Class, "\\>", "\\/"),
         Class = str_squish(Class)) ->
  baldur

baldur$Class[[3]] <- "Cleric"
unique(baldur$Class)

baldur %>%
  mutate(warrior = str_detect(Class, "(Fighter|Paladin|Ranger|Barbarian|Blackguard|Archer|Blade|Inquisitor|Berserker|Stalker)") * 1,
         rogue = str_detect(Class, "(Thief|Bard|Skald|Bounty Hunter)") * 1,
         priest = str_detect(Class, "(Cleric|Druid|Shaman|Monk|Shapeshifter)") * 1,
         wizard = str_detect(Class, "(Mage|Sorcerer|Invoker|Conjurer|Illusionist|Enchanter|Necromancer)") * 1) ->
  baldur

baldur <- clean_names(baldur)

write_csv(x = baldur, file = "./baldur.csv")
