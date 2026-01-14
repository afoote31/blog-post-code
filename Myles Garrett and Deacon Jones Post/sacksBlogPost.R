library(tidyverse)
library(ggrepel)

years <- c(1964,1967,1968,1978,1984,2001,2011,2014,2021,2025)
passingTotals <- read_csv("~/Desktop/Blog Posts/passingTotals.csv",skip=1) %>% 
  mutate(
    label = case_match(Year,
                       1978 ~ 'Baker',2025 ~ 'Garrett',2001 ~ 'Strahan',
                       2021 ~ 'Watt',2011 ~ 'Allen',1984 ~ 'Gastineau',
                       2014 ~ 'Houston',1964 ~ 'Jones',
                       1967 ~ 'Jones',1968 ~ 'Jones'
      ),
    color = ifelse(Year %in% years,'red','black')) %>% 
  filter(Year >= 1960)



# Create the plot
passingTotals %>% 
  ggplot(aes(x = Year,y = Att,label = label)) + 
  geom_point(size = 3, aes(color = color)) + 
  geom_text_repel(label.padding = 4, box.padding = 1.5, size = 4) +
  ylab('Average Passes per Game') + 
  scale_color_manual(values=c("black", "red")) + 
  theme_minimal() + 
  theme(legend.position = 'none')
  
ggsave('~/Desktop/Blog Posts/passingYards.tiff')

# https://nflfootballjournal.blogspot.com/2017/10/al-bubba-bakers-1978-season.html
# Deacon Jones came up with the word sack!
baker1978 <- c(0,3,1,2,0,1,2.5,4,0,2,5,1,0.5,0,1,0)
garrett2025 <- c(2,1.5,0.5,0,0,0,1,5,1,4,3,1,1,1.5,0.5,0,1)
strahan2001 <- c(0,0,3,1.5,4,2,2,1.5,1,0,0.5,1,0.5,1,3.5,1)
watt2021 <- c(2,1,2,0,2,1.5,3,1,0,3.5,0,1.5,0,4,1)
allen2011 <- c(0.5,1,3,2,2,1,2,1,1,0,0,1,3,0,1,3.5)
gastineau1984 <- c(4,2,2,1,0,1,2,1.5,3,1,0,1,0,0.5,1,2)
houston2014 <- c(2,0,1,2,1,1,3,2,0,0,1,1,2,1,1,4)
#bacon1976 <- c(3,2,2,0,2,0,2.5,2,0.5,0,3,1,0,3.5)

bakerCombos <- combn(baker1978,14,sum)
tibble(sacks = bakerCombos,fill = bakerCombos >= 22,1,0) %>%
  ggplot(aes(x = sacks,fill = fill)) + geom_bar() +
    scale_fill_manual(values=c("blue", "red")) + guides(fill="none") + 
  theme_minimal() + labs(y = 'Frequency',x = 'Sacks', title = 'Al Baker (1978)') + 
  theme(plot.title = element_text(hjust = 0.5))
ggsave('~/Desktop/Blog Posts/AlBaker.tiff')

garrettCombos <- combn(garrett2025,14,sum)
tibble(sacks = garrettCombos,fill = garrettCombos >= 22,1,0) %>%
  ggplot(aes(x = sacks,fill = fill)) + geom_bar() +
  scale_fill_manual(values=c("blue", "red")) + guides(fill="none") + 
  theme_minimal() + labs(y = 'Frequency',x = 'Sacks', title = 'Myles Garrett (2025)') + 
  theme(plot.title = element_text(hjust = 0.5))
ggsave('~/Desktop/Blog Posts/MylesGarrett.tiff')

strahanCombos <- combn(strahan2001,14,sum)
tibble(sacks = strahanCombos,fill = strahanCombos >= 22,1,0) %>%
  ggplot(aes(x = sacks,fill = fill)) + geom_bar() +
  scale_fill_manual(values=c("blue", "red")) + guides(fill="none") + 
  theme_minimal() + labs(y = 'Frequency',x = 'Sacks', title = 'Michael Strahan (2001)') + 
  theme(plot.title = element_text(hjust = 0.5))
ggsave('~/Desktop/Blog Posts/MichaelStrahan.tiff')

wattCombos <- combn(watt2021,14,sum)
tibble(sacks = wattCombos,fill = wattCombos >= 22,1,0) %>%
  ggplot(aes(x = sacks,fill = fill)) + geom_bar() +
  scale_fill_manual(values=c("blue", "red")) + guides(fill="none") + 
  theme_minimal() + labs(y = 'Frequency',x = 'Sacks', title = 'TJ Watt (2021)') + 
  theme(plot.title = element_text(hjust = 0.5))
ggsave('~/Desktop/Blog Posts/TJWatt.tiff')

allenCombos <- combn(allen2011,14,sum)
tibble(sacks = allenCombos,fill = allenCombos >= 22,1,0) %>%
  ggplot(aes(x = sacks,fill = fill)) + geom_bar() +
  scale_fill_manual(values=c("blue", "red")) + guides(fill="none") + 
  theme_minimal() + labs(y = 'Frequency',x = 'Sacks', title = 'Jared Allen (2011)') + 
  theme(plot.title = element_text(hjust = 0.5))
ggsave('~/Desktop/Blog Posts/JaredAllen.tiff')

gastineauCombos <- combn(gastineau1984,14,sum)
tibble(sacks = gastineauCombos,fill = gastineauCombos >= 22,1,0) %>%
  ggplot(aes(x = sacks,fill = fill)) + geom_bar() +
  scale_fill_manual(values=c("blue", "red")) + guides(fill="none") + 
  theme_minimal() + labs(y = 'Frequency',x = 'Sacks', title = 'Mark Gastineau (1984)') + 
  theme(plot.title = element_text(hjust = 0.5))
ggsave('~/Desktop/Blog Posts/MarkGastineau.tiff')

houstonCombos <- combn(houston2014,14,sum)
tibble(sacks = houstonCombos,fill = houstonCombos >= 22,1,0) %>%
  ggplot(aes(x = sacks,fill = fill)) + geom_bar() +
  scale_fill_manual(values=c("blue", "red")) + guides(fill="none") + 
  theme_minimal() + labs(y = 'Frequency',x = 'Sacks', title = 'Justin Houston (2021)') + 
  theme(plot.title = element_text(hjust = 0.5))
ggsave('~/Desktop/Blog Posts/JustinHouston.tiff')

sum(bakerCombos >= 22)/length(bakerCombos)
sum(garrettCombos >= 22)/length(garrettCombos)
sum(strahanCombos >= 22)/length(strahanCombos)
sum(wattCombos >= 22)/length(wattCombos)
sum(allenCombos >= 22)/length(allenCombos)
sum(gastineauCombos >= 22)/length(gastineauCombos)
sum(houstonCombos >= 22)/length(houstonCombos)

#' So for plotting this, I could expand it to have the top 15, and then maybe
#' put everyone on one plot. This would be interesting. I can't seem to find
#' the Deacon Jones seasons which is annoying, but I can get everyone else
#' in the top 10. Since Coy Bacon's season is only 14 games, I might sample
#' all the way down to 12. Honestly, I could explore different game amounts.
#' 
#' I need to decide on what exactly I'm trying to measure. Maybe I start out
#' with what I have right now and go from there? I note how Deacon Jones might
#' have the best numbers sack-wise of all time.
#' 
#' 1. Transcribe the other players DONE
#' 2. Decide how to streamline the sampling process
#' 3. Decide on what the narrative is and what I'm measuring DONE
#' 
#' One thing I could try to measure is who had the best season in terms of
#' sacks of all time. But I don't think that this approach is particularly 
#' effective for that, as under that framing I would probably want to include
#' some other variables for conditioning. I think that the real framing should
#' be about the extra weeks.
#' 
#' What we have for each player is a sequence of sacks. I might say that Deacon
#' Jones is the greatest pass rusher of all time, and he only got to play in 14
#' game seasons. Thus, I want to evaluate how valuable the extra games are for
#' the players that had them. However, I don't want to just cut off at 14 games.
#' Rather, I want to consider all possible seasons of 14 games that the player
#' might have had. I do this by considering all possible 14-game subsets of
#' each player, and seeing how often they would have put together a season to
#' top Deacon Jones's record. 
#' 
#' I think one of the players on the list got lots of sacks in a strike-shortened
#' season, and that would give me a chance to replicate this analysis with more
#' players and have richer distributions with more combinations.

