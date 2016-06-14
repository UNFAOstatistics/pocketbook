d <- read.csv("./input/data/country_profile_indicators_REU.csv", stringsAsFactors = FALSE)
d$SERIES_NAME_SHORT_RU <- "Индикатор на русском языке"
# d$SERIES_NAME_SHORT_RU <- "Indicator in Russian"

d$PART_RU[d$PART %in%  "The setting"] <- "Настройки"
d$PART_RU[d$PART %in%  "Hunger and dietary quality dimensions"] <- "Голод и диетические качества размеры"
d$PART_RU[d$PART %in% "Food Supply"] <- "Обеспечение продовольствием"
d$PART_RU[d$PART %in% "Production indices (2004-06=100)"] <- "Индексы производства (2004-06 = 100)"
d$PART_RU[d$PART %in% "Net trade (mln US\\$)"] <- "Сальдо торгового баланса (млн долл США \\$)"
d$PART_RU[d$PART %in% "Environment"] <- "Окружающая среда"


write.csv(d, file="./input/data/country_profile_indicators_REU.csv", row.names = FALSE)
