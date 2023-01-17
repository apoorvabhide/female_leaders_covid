rm(list = ls())
library(httr)
library(rvest)
library(dplyr)
library(stringr)
link <- "https://en.wikipedia.org/wiki/List_of_elected_and_appointed_female_heads_of_state_and_government"
covid <- read.csv("/home/apoorva/Documents/Writing/Bracket/Women Leaders in COVID/Data/owid-covid-data.csv",stringsAsFactors = F)
covid$date <- as.Date(covid$date)
# Read in the data (from world bank)
gdp <- read.csv("/home/apoorva/Documents/Writing/Bracket/Women Leaders in COVID/Data/API_NY.GDP.PCAP.CD_DS2_en_csv_v2_4770417/API_NY.GDP.PCAP.CD_DS2_en_csv_v2_4770417.csv",stringsAsFactors = F)
pop <- read.csv("/home/apoorva/Documents/Writing/Bracket/Women Leaders in COVID/Data/population.csv",stringsAsFactors = F)
pop65 <- read.csv("/home/apoorva/Documents/Writing/Bracket/Women Leaders in COVID/Data/pop65.csv",stringsAsFactors = F)
urban <- read.csv("/home/apoorva/Documents/Writing/Bracket/Women Leaders in COVID/Data/urban_pop.csv",stringsAsFactors = F)

# Read in the wiki table with women leaders and their tenure
wiki <- read_html(link)
lead <- wiki %>% html_table()
lead <- lead[[2]]
lead$Portrait <- NULL
colnames(lead) <- c("Name","Country","Office","StartDate","EndDate","TermLength","HeadOfState","ExecutiveOrNot")
leadex <- lead# %>% filter(ExecutiveOrNot == "Executive")
leadexinc <- leadex %>% filter(EndDate == "Incumbent")
leadexend <- leadex %>% filter(EndDate != "Incumbent")
leadexend$EndDate <- gsub("\\(assassinated\\)","",leadexend$EndDate)
leadexend$EndDate <- gsub("Suspended*","",leadexend$EndDate)
leadexend$EndDate <- sapply(leadexend$EndDate,function(x){str_split(x, "Suspended")[[1]][1]})
leadexend$StartDate <- as.Date(leadexend$StartDate,"%e %B %Y")
leadexend$EndDate <- as.Date(leadexend$EndDate,"%e %B %Y")
leadexinc$StartDate <- as.Date(leadexinc$StartDate,"%e %B %Y")

leadexend1 <- leadexend %>% filter(EndDate >= "2020-03-01" & StartDate <= "2020-03-01")

codate <- as.Date("2020-05-19")
l1 <- leadexend1[leadexend1$EndDate >= codate,]
l2 <- leadexinc[leadexinc$StartDate <= codate,]
l1$EndDate <- as.character(l1$EndDate)
leaders <- rbind(l1,l2)
# Removing the locations in the list that aren't countries - continents, groups of countries etc.
cat <- c('North America','South America','Africa','Asia','Europe','European Union','High income','Low income','Lower middle income','Upper middle income','International',
         'World')
covid_date <- covid %>% filter(date <= codate,
                               !location %in% cat) %>% group_by(location,iso_code) %>% dplyr::summarise(TotalCases = sum(new_cases,na.rm = T),
                                                                                           TotalCasesPerMil = sum(new_cases_per_million,na.rm = T),
                                                                                           TotalDeaths = sum(new_deaths,na.rm = T),
                                                                                           TotalDeathsPerMil = sum(new_deaths_per_million,na.rm = T)) %>% 
  mutate(FemaleLeader = ifelse(location %in% unique(leaders$Country),1,0)) %>% inner_join(gdp %>% dplyr::select(-Country.Name),by=c('iso_code'='Country.Code')) %>% 
  inner_join(pop %>% dplyr::select(-Country.Name),by=c('iso_code'='Country.Code')) %>% inner_join(pop65 %>% dplyr::select(-Country.Name),by=c('iso_code'='Country.Code')) %>% 
  inner_join(urban %>% dplyr::select(-Country.Name),by=c('iso_code'='Country.Code'))
covid_date <- covid_date %>% filter(!is.na(GDPPerCapita19))
# Broad summary
covid_date %>% group_by(FemaleLeader) %>% dplyr::summarise(DeathsPerMil = mean(TotalDeathsPerMil),
                                                           TotalDeaths = mean(TotalDeaths),
                                                           TotalCases = mean(TotalCases),
                                                           CasesPerMil = mean(TotalCasesPerMil),
                                                           SDCases = sd(TotalCases,na.rm = T),
                                                           SDDeaths = sd(TotalDeaths,na.rm=T),
                                                           SampleSize = length(unique(location)),
                                                           SDDeathMil = sd(TotalDeathsPerMil),
                                                           SDCasesMil = sd(TotalCasesPerMil))
lmc <- lm(TotalCases ~ FemaleLeader + GDPPerCapita19 + Population19 + Pop65+UrbanPop,data=covid_date)
summary(lmc)
lmd <- lm(TotalDeaths ~ FemaleLeader + GDPPerCapita19 + Population19 + Pop65+UrbanPop,data=covid_date)
summary(lmd)

# Nearest neighbours in terms of those four things
covid_scaled <- covid_date %>% dplyr::select(location,GDPPerCapita19,Population19,Pop65,UrbanPop)
country_sim <- as.matrix(dist(scale(covid_scaled[2:ncol(covid_scaled)])))
rownames(country_sim) <- covid_scaled$location
colnames(country_sim) <- covid_scaled$location

country_sim1 <- as.data.frame(country_sim)
countries <- unique(covid_date$location[covid_date$FemaleLeader==1])
country_sim1 <- country_sim1[,countries]
country_sim1 <- country_sim1[!row.names(country_sim1) %in% countries,]

# covid_scaled2 <- covid_date %>% dplyr::select(location,GDPPerCapita19,Population19,Pop65,UrbanPop)
# country_sim2 <- as.matrix(dist(covid_scaled[2:ncol(covid_scaled)]))
# rownames(country_sim2) <- covid_scaled2$location
# colnames(country_sim2) <- covid_scaled2$location
# 
# country_sim2 <- as.data.frame(country_sim)
# country_sim2 <- country_sim2[,countries]
# country_sim2 <- country_sim2[!row.names(country_sim2) %in% countries,]

comp_countries <- c('Serbia','Belarus')
covid_date %>% filter(location %in% comp_countries)

# For each country: get which country is the most similar
neighbour <- data.frame()
for(c in countries)
{
  country <- country_sim1 %>% dplyr::select(c)
  rowno <- which(country == min(country), arr.ind=TRUE)[1]
  neigh <- rownames(country)[rowno]
  n <- data.frame(Country = c,NearestNeighbour = neigh)
  neighbour <- rbind(neighbour,n)
}

neighbour_data <- neighbour %>% left_join(covid_date %>% dplyr::select(location,TotalCases,TotalDeaths,GDPPerCapita19) %>% 
                                            rename(TotalCasesCountry=TotalCases,
                                                   TotalDeathsCountry = TotalDeaths,
                                                   GDPPerCapitaCountry=GDPPerCapita19),
                                          by = c('Country'='location')) %>% 
  left_join(covid_date %>% dplyr::select(location,TotalCases,TotalDeaths,GDPPerCapita19) %>% rename(TotalCasesNeighbour=TotalCases,
                                                                                     TotalDeathsNeighbour = TotalDeaths,
                                                                                     GDPPerCapitaNeighbour=GDPPerCapita19),
            by = c('NearestNeighbour'='location'))

neighbour_data2 <- neighbour %>% left_join(covid_date %>% dplyr::select(location,TotalCasesPerMil,TotalDeathsPerMil,GDPPerCapita19) %>% 
                                            rename(TotalCasesCountry=TotalCasesPerMil,
                                                   TotalDeathsCountry = TotalDeathsPerMil,
                                                   GDPPerCapitaCountry=GDPPerCapita19),
                                          by = c('Country'='location')) %>% 
  left_join(covid_date %>% dplyr::select(location,TotalCasesPerMil,TotalDeathsPerMil,GDPPerCapita19) %>% rename(TotalCasesNeighbour=TotalCasesPerMil,
                                                                                                    TotalDeathsNeighbour = TotalDeathsPerMil,
                                                                                                    GDPPerCapitaNeighbour=GDPPerCapita19),
            by = c('NearestNeighbour'='location'))
neighbour_data <- neighbour_data %>% mutate(CaseDiff = TotalCasesCountry-TotalCasesNeighbour,
                                            DeathDiff = TotalDeathsCountry - TotalDeathsNeighbour,
                                            GDPDiff = GDPPerCapitaCountry - GDPPerCapitaNeighbour)
neighbour_data %>% dplyr::summarise(MeanCaseDiff = mean(CaseDiff),
                                    SECaseDiff = sd(CaseDiff)/nrow(neighbour_data),
                                    MeanDeathDiff = mean(DeathDiff),
                                    SEDeathDiff = sd(DeathDiff)/nrow(neighbour_data),
                                    MeanGDPDiff = mean(GDPDiff))
# library(BSDA)
# I assume the authors are doing a t-test on the sample to check if the mean difference between nearest neighbours is less than 0.
t.test(x=neighbour_data$CaseDiff,mu=0,alternative = "l")
t.test(x=neighbour_data$DeathDiff,mu=0,alternative = "l")

neighbour_data2 <- neighbour_data2 %>% mutate(CaseDiff = TotalCasesCountry-TotalCasesNeighbour,
                                            DeathDiff = TotalDeathsCountry - TotalDeathsNeighbour,
                                            GDPDiff = GDPPerCapitaCountry - GDPPerCapitaNeighbour)
neighbour_data2 %>% dplyr::summarise(MeanCaseDiff = mean(CaseDiff),
                                    SECaseDiff = sd(CaseDiff)/nrow(neighbour_data),
                                    MeanDeathDiff = mean(DeathDiff),
                                    SEDeathDiff = sd(DeathDiff)/nrow(neighbour_data))
t.test(x=neighbour_data2$CaseDiff,mu=0,alternative = "l")
t.test(x=neighbour_data2$DeathDiff,mu=0,alternative = "l")

# 1. Replicating for 21 countries, for total cases and total deaths
# Cases: t = -0.57573, df = 20, p-value = 0.2856
# Deaths: t = -0.55472, df = 20, p-value = 0.2926
# MeanCaseDiff SECaseDiff
#    -4043.636   857.0388

# 2. Replicating for 13 countries, only where women are in executive roles, for total cases and total deaths
# Cases: t = -1.0553, df = 12, p-value = 0.156
# Deaths: t = -0.7557, df = 12, p-value = 0.2322
# MeanCaseDiff SECaseDiff
#    -7401.615   1945.189
