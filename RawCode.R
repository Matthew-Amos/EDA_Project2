# ---------- Q1

## Loads data set & project settings
## Code available at https://github.com/equinaut/EDA_Project2
source("LoadProject.R")

if(Use.Prototype) {
# Plots are prototyped on a smaller sampling of
# the data set during the exploratory phase.
# LoadProject.R contains a boolean 'Use.Prototype'
# to control which data set & subsequent plot is outputted.
  q1.lm <- lm(Emissions ~ year, data = dfSub)
  q1.lm.int <- q1.lm$coefficients[[1]]
  q1.lm.slope<- q1.lm$coefficients[[2]]

  q1.dfSummary <- dfSub %>%
    group_by(year) %>%
    summarise(Emissions = sum(Emissions))

  with(q1.dfSummary,
       plot(year,
            Emissions,
            pch = 19,
            cex = 3,
            col = "blue",
            type = "p",
            xlab = "Year",
            ylab = "Total Emissions",
            main = "Total Emissions by Year"
            ))

  mtext(paste("Linear Model Slope= ", percent(q1.lm.slope), sep = ""),
        side = 3)

} else {

  q1.lm <- lm(Emissions ~ year, data = dfMerged)
  q1.lm.int <- q1.lm$coefficients[[1]]
  q1.lm.slope <- q1.lm$coefficients[[2]]

  q1.dfSummary <- dfMerged %>%
    group_by(year) %>%
    summarise(Emissions = sum(Emissions))

# FINAL PLOT CODE
  png(filename = "plot1.png")

  with(q1.dfSummary,
       plot(year,
            Emissions,
            pch = 19,
            cex = 5,
            col = "blue",
            type = "p",
            xlab = "Year",
            ylab = "Total Emissions",
            main = "Total Emissions by Year"
       ))

  mtext(paste("Linear Model Slope= ", percent(q1.lm.slope), sep = ""),
        side = 3)

  dev.off()
}

# ---------- Q2
## Loads data set & project settings
## Code available at https://github.com/equinaut/EDA_Project2
source("LoadProject.R")

q2.filtered <- dfMerged %>%
  filter(fips == "24510")

q2.lm<- lm(Emissions ~ year, data = q2.filtered)
q2.lm.int<- q2.lm$coefficients[[1]]
q2.lm.slope <- q2.lm$coefficients[[2]]

q2.dfSummary <- q2.filtered %>%
  group_by(year) %>%
  summarise(Emissions = sum(Emissions))


# FINAL PLOT CODE
if(!Use.Prototype) {
  png(filename = "plot2.png")

  with(q2.dfSummary,
       plot(year,
            Emissions,
            pch = 19,
            cex = 3,
            col = "green",
            type = "p",
            xlab = "Year",
            ylab = "Total Emissions",
            main = "Total Emissions by Year for Baltimore City, Maryland"
            ))

  mtext(paste("Linear Model Slope= ", percent(q2.lm.slope), sep = ""),
        side = 3)

  dev.off()
}

# ---------- Q3
## Loads data set & project settings
## Code available at https://github.com/equinaut/EDA_Project2
source("LoadProject.R")

# Courtesy of Ramnath & Ricardo Saporta
lm_eqn = function(m) {

  l <- list(a = format(coef(m)[1], digits = 2),
            b = format(abs(coef(m)[2]), digits = 2),
            r2 = format(summary(m)$r.squared, digits = 3));

  if (coef(m)[2] >= 0)  {
    eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,l)
  } else {
    eq <- substitute(italic(y) == a - b %.% italic(x)*","~~italic(r)^2~"="~r2,l)
  }

  as.character(as.expression(eq));
}

if(!Use.Prototype) {

  q3.dfSummary <- dfMerged %>%
    filter(fips == "24510") %>%
    group_by(year, type) %>%
    summarise(Emissions = sum(Emissions)) %>%
    mutate(label = q3.lmlabs[q3.lmlabs$type == type,2],
           type = factor(type))

  # FINAL PLOT CODE
  png(filename = "plot3.png")

  # Linear Model Labels
  q3.lmlabs <- data.frame(
    type = c('NON-ROAD','NONPOINT','ON-ROAD','POINT'),
    labs = c(
    lm_eqn(lm(Emissions ~ year, filter(q2.filtered, type == 'NON-ROAD'))),
    lm_eqn(lm(Emissions ~ year, filter(q2.filtered, type == 'NONPOINT'))),
    lm_eqn(lm(Emissions ~ year, filter(q2.filtered, type == 'ON-ROAD'))),
    lm_eqn(lm(Emissions ~ year, filter(q2.filtered, type == 'POINT')))
  ))

  ggplot(q3.dfSummary, aes(x = year, y = Emissions, colour = type)) +
    geom_point() +
    facet_wrap(~ type) +
    geom_smooth(method = 'lm') +
    geom_text(aes(x = 2004, y = 2250, label = label), parse = T) +
    labs(title = "Total Emissions for Baltimore City by Year & Type",
         x = "Year",
         y = "Total Emissions") +
    theme(legend.position = "none")

  dev.off()
}


# ---------- Q4

## Loads data set & project settings
## Code available at https://github.com/equinaut/EDA_Project2
source("LoadProject.R")

q4.filtered <- dfMerged %>%
  filter(regexpr("*combustion*", SCC.Level.One, ignore.case = T) > -1,
         regexpr("*coal*", SCC.Level.Three, ignore.case = T) > -1)

if(Use.Prototype) {
  # Plots are prototyped on a smaller sampling of
  # the data set during the exploratory phase.
  # LoadProject.R contains a boolean 'Use.Prototype'
  # to control which data set is outputted.
  q4.sub <- sample_n(q4.filtered, 5000)

  q4.dfSummary <- q4.sub %>%
    group_by(year) %>%
    summarise(Emissions = sum(Emissions), Pop = n())

  # Pollution
  g1 <- ggplot(q4.dfSummary, aes(x = year, y = Emissions)) +
    geom_point(size = 5) +
    labs(title = "U.S. COAL COMBUSTION", x = "Year", y = "Emissions")

  # Counts
  g2 <- ggplot(q4.filtered, aes(x = year)) +
    geom_histogram() +
    labs(title = "Sample Size", x = "Year", y = "Count")

  grid.arrange(g1, g2, ncol = 1)

} else {
  q4.dfSummary <- q4.filtered %>%
    group_by(year) %>%
    summarise(Emissions = sum(Emissions), Pop = n())

  # FINAL PLOT CODE
  png(filename = "plot4.png", width = 600, height = 600)

  # Pollution
  g1 <- ggplot(q4.dfSummary, aes(x = year, y = Emissions)) +
    geom_point(size = 5) +
    labs(title = "U.S. COAL COMBUSTION", x = "Year", y = "Emissions")

  # Counts
  g2 <- ggplot(q4.filtered, aes(x = year)) +
    geom_histogram() +
    labs(title = "Sample Size", x = "Year", y = "Count")

  grid.arrange(g1, g2, ncol = 1)

  dev.off()
}


# ---------- Q5

## Loads data set & project settings
## Code available at https://github.com/equinaut/EDA_Project2
source("LoadProject.R")

if(!Use.Prototype) {
  q5.filtered <- dfMerged %>%
    filter(fips == "24510") %>%
    filter(regexpr("*vehicle*|*cng*", SCC.Level.Two, ignore.case = T) > -1,
           regexpr("*vehicle*|*truck*|*motorcycle*|*bus*",
                   SCC.Level.Three, ignore.case = T) > -1)

  q5.dfSummary <- q5.filtered %>%
    group_by(year) %>%
    summarise(Emissions = sum(Emissions))

  q5.lm <- lm(Emissions ~ year, data = q5.filtered)

  # FINAL PLOT CODE
  png(filename = "plot5.png")

  ggplot(q5.dfSummary, aes(x = year, y = Emissions)) +
    geom_point(size = 5) +
    labs(title = "Baltimore City Motor Vehicle Emissions (Including CNG)") +
    geom_smooth(method = 'lm') +
    geom_text(aes(x = 2000, y = -100, label = lm_eqn(q5.lm)), parse = T)

  dev.off()
}


# ---------- Q6

## Loads data set & project settings
## Code available at https://github.com/equinaut/EDA_Project2
source("LoadProject.R")

q6.filtered <- dfMerged %>%
  filter(fips %in% c("24510", "06037")) %>%
  filter(regexpr("*vehicle*|*cng*", SCC.Level.Two, ignore.case = T) > -1,
         regexpr("*vehicle*|*truck*|*motorcycle*|*bus*",
                 SCC.Level.Three, ignore.case = T) > -1) %>%
  mutate(City = sapply(fips, function(x) if(x == "24510") "Baltimore" else "LA"),
         City = factor(City))

q6.dfSummary <- q6.filtered %>%
  group_by(year, City) %>%
  summarise(Emissions = sum(Emissions))

if(!Use.Prototype) {
  # FINAL PLOT CODE
  png(filename = "plot6.png", width = 600, height = 600)

  xyplot(Emissions ~ year | City,
         data = q6.dfSummary,
         panel = function(x, y, ...) {
           panel.xyplot(x, y, ...)
           panel.lmline(x, y, col = "salmon")

           lmfit <- lm(x ~ y)
           if(coef(lmfit)[2] < 0) sign <- "" else sign <- "+"

           lab <- sprintf("%s=%.0f %s %.4f %s",
                          all.vars(formula(lmfit))[1],
                          coef(lmfit)[1],
                          sign,
                          coef(lmfit)[2],
                          all.vars(formula(lmfit))[2])

           panel.text(2003, 3000, labels = lab)
  })

  dev.off()
}
