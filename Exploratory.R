# Exploratory

# Data structure
View(head(dfMerged, 100))
str(dfMerged)

# Q1 -- lumpiness in years?
dfMerged %>% group_by(year) %>% summarise (count = n())

# >> 1999, 2002, 2005 & 2008 only years

dfSummary <- dfMerged %>% group_by(year) %>% summarise(Emissions = sum(Emissions))
with(dfSummary,
     plot(year,
          Emissions))

with(dfSummary,
     boxplot(Emissions ~ year))

# Q4 - examination of labels
summary(dfMerged$EI.Sector)
summary(dfMerged$SCC.Level.One)
summary(dfMerged$SCC.Level.Two)
summary(dfMerged$SCC.Level.Three)
summary(dfMerged$SCC.Level.Four)
# Lots...

t1 <- dfMerged %>%
  filter(regexpr("*combustion*", SCC.Level.One, ignore.case = T) > -1,
         regexpr("*coal*", SCC.Level.Three, ignore.case = T) > -1)


# Q4 log representations
t2 <- t1 %>% mutate(lgEm = log(1 + Emissions))
t2.sum <- t2 %>% group_by(year) %>% summarise(Tot.Em = sum(lgEm))
ggplot(t2, aes(x = year, y = lgEm)) +
  geom_jitter(alpha = 1/3) +
  geom_boxplot(aes(group=year))

# Q5
#' "Vehicle" in level two
#' CNG in level two -- this MAY include natural gas vehicles
#' "Vehicle", motorcycle, natural gas in level three
#'

# level 2 vehicle
# level 3 vehicle || motorcycle || truck || bus
x1 <- dfMerged %>%
  filter(regexpr("*vehicle*", SCC.Level.Two, ignore.case = T) > -1)

x2 <- dfMerged %>%
  filter(regexpr("*vehicle*|*cng*", SCC.Level.Two, ignore.case = T) > -1)


