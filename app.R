#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)

# df <- read_csv('data/translations-export.csv')
coordDf <- read_csv('data/coord.csv')

# 'Azerbaijan', 
nonEuropeanCountries <- c('Brazil', 'Canada', 'United States', 'Argentina', 'Australia', 'China', 'Colombia', 'Costa Rica', 'Cuba', 'India', 'Indonesia', 'Iran', 'Iraq', 'Israel', 'Japan', 'Lebanon', 'Malaysia', 'Mexico', 'New Zealand', 'Nicaragua', 'Singapore', 'South Korea', 'Tanzania', 'Vietnam')

languages <- df %>% count(sourceLanguage) %>% 
  arrange(desc(n)) %>% select(sourceLanguage) %>% unlist(use.names = FALSE)

publicationYears <- df %>% select(publicationYear) %>% 
  filter(!is.na(publicationYear)) %>% distinct(publicationYear) %>% 
  arrange(publicationYear) %>% unlist(use.name = FALSE)

#' Functions ----
filterByDate <- function(.df, yearRange) {
  if (   min(publicationYears) < yearRange[1]
         || max(publicationYears) > yearRange[1]) {
    .df <- .df %>% 
      filter(  !is.na(publicationYear) 
               & publicationYear >= yearRange[1] 
               & publicationYear <= yearRange[2])
  }
  .df
}

# get data
getSourceLanguagesOf <- function(selectedLanguage, yearRange) {
  df %>% 
    filter(targetLanguage == selectedLanguage & !is.na(sourceLanguage)) %>% 
    filterByDate(yearRange) %>% 
    count(sourceLanguage) %>% 
    rename(language = sourceLanguage) %>% 
    arrange(desc(n))
}

getTargetLanguagesOf <- function(selectedLanguage, yearRange) {
  df %>% 
    filter(sourceLanguage == selectedLanguage & !is.na(targetLanguage)) %>% 
    filterByDate(yearRange) %>% 
    count(targetLanguage) %>% 
    rename(language = targetLanguage) %>% 
    arrange(desc(n))
}

getSourceAuthors <- function(selectedLanguage, yearRange) {
  df %>% 
    filter(sourceLanguage == selectedLanguage & !is.na(author)) %>% 
    filterByDate(yearRange) %>% 
    count(author) %>% 
    arrange(desc(n))
}

getSourceCities <- function(selectedLanguage, yearRange) {
  df1 <- df %>% 
    filter(targetLanguage == selectedLanguage 
           & !is.na(publicationPlace)) %>% 
    filterByDate(yearRange) %>% 
    count(publicationPlace) %>% 
    arrange(desc(n))
}

getTargetCities <- function(selectedLanguage, yearRange) {
  .df <- df %>% 
    filter(sourceLanguage == selectedLanguage
           & !is.na(publicationPlace)) %>% 
    filterByDate(yearRange) %>% 
    count(publicationPlace) %>% 
    arrange(desc(n))
}

getTargetAuthors <- function(selectedLanguage, yearRange) {
  df %>% 
    filter(targetLanguage == selectedLanguage & !is.na(author)) %>% 
    filterByDate(yearRange) %>% 
    count(author) %>% 
    arrange(desc(n)) %>% 
    head(30)
}

getSourceByYear <- function(selectedLanguage, yearRange) {
  df %>% 
    filter(sourceLanguage == selectedLanguage & !is.na(publicationYear)) %>% 
    filterByDate(yearRange) %>% 
    count(publicationYear) %>% 
    arrange(publicationYear)
}

getTargetByYear <- function(selectedLanguage, yearRange) {
  df %>% 
    filter(targetLanguage == selectedLanguage & !is.na(publicationYear)) %>% 
    filterByDate(yearRange) %>% 
    count(publicationYear) %>% 
    arrange(publicationYear)
}

displayCities <- function(df1, selectedLanguage, direction) {
  final <- df1 %>% 
    left_join(coordDf, by = join_by(publicationPlace == city)) %>% 
    filter(!is.na(publicationPlace)) %>%
    filter(!(country %in% nonEuropeanCountries)) %>% 
    select(publicationPlace, n, country, lat, long)
  
  map.europe <- map_data("world")
  basemap <- ggplot() +
    geom_polygon(
      data = map.europe,
      aes(x = long, y = lat, group = group),
      fill = '#ffffff',
      colour = '#999999'
    ) +
    # coord_cartesian(xlim = c(minx, maxx), ylim = c(miny, maxy)) +
    coord_cartesian(xlim = c(-21,45), ylim = c(36,66)) +
    theme_bw() +
    theme(
      legend.position = 'none',
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      # legend.title = element_text(size=rel(0.5)), 
      # legend.text = element_text(size=rel(0.5))
    ) +
    labs(
      title = sprintf('Publication places of books translated %s %s', direction, selectedLanguage)
    )

  basemap +
    geom_point(
      data = final,
      aes(x = long, y = lat, size = n),
      color = "red",
      alpha = .8)
}

plotLanguages <- function(df1, direction, selectedLanguage, nature) {
  df1 %>% 
    ggplot(aes(x = fct_rev(
      factor(language,
             levels = language)),
      y = n)) + 
    geom_col() +
    coord_flip() +
    theme_bw() +
    labs(
      title = sprintf('Translations %s %s', direction, selectedLanguage),
      x = sprintf('%s language', nature),
      y = 'number of books'
    )
}

plotByYear <- function(yearlyDf, direction, selectedLanguage) {
  yearlyDf %>% 
    ggplot(aes(x = publicationYear, y = n)) +
    geom_line() +
    theme_bw() +
    labs(
      title = sprintf('Translations %s %s', direction, selectedLanguage),
      x = 'publicaton year',
      y = 'number of books'
    )
}

displaySourceCities <- function(selectedLanguage, yearRange) {
  displayCities(getSourceCities(selectedLanguage, yearRange), selectedLanguage, 'to')
}

displayTargetCities <- function(selectedLanguage, yearRange) {
  displayCities(getTargetCities(selectedLanguage, yearRange), selectedLanguage, 'from')
}

displayTargetLanguagesOf <- function(selectedLanguage, yearRange) {
  plotLanguages(getTargetLanguagesOf(selectedLanguage, yearRange), 'from', selectedLanguage, 'target')
}

displaySourceLanguagesOf <- function(selectedLanguage, yearRange) {
  plotLanguages(getSourceLanguagesOf(selectedLanguage, yearRange), 'to', selectedLanguage, 'source')
}

displaySourceByYear <- function(selectedLanguage, yearRange) {
  plotByYear(getSourceByYear(selectedLanguage, yearRange), 'from', selectedLanguage)
}

displayTargetByYear <- function(selectedLanguage, yearRange) {
  plotByYear(getTargetByYear(selectedLanguage, yearRange), 'to', selectedLanguage)
}

displayBoth <- function(selectedLanguage, yearRange) {
  both <- getSourceByYear(selectedLanguage, yearRange) %>% 
    full_join(
      getTargetByYear(selectedLanguage, yearRange),
      by = "publicationYear") %>% 
    rename(from = n.x, to = n.y) %>% 
    pivot_longer(!publicationYear,
                 names_to = "direction",
                 values_to = "count",
                 values_drop_na = FALSE
    ) %>% 
    mutate(count = ifelse(is.na(count), 0, count))
  
  both %>% 
    ggplot(aes(x = publicationYear, y = count, color = direction)) +
    geom_line() +
    theme_bw() +
    labs(
      title = sprintf('Translations to and from %s', selectedLanguage),
      x = 'publicaton year',
      y = 'number of books'
    )
}

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("ATRIUM TNA — Patterns of Translation in European Literary Bibliography"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput('language', label='Select a language', choices=languages),
      sliderInput(
        "rng", "Select publication year:",
        min = min(publicationYears),
        max = max(publicationYears),
        value = c(min(publicationYears), max(publicationYears)),
        sep = ""
      ),
      
      selectInput(
        'display',
        label='visualisation', 
        choices=c(
          'translated to which languages?' = 'targetLanguages',
          'translated from which languages?' = 'sourceLanguages',
          'as source by year' = 'sourceByYear',
          'as target by year' = 'targetByYear',
          'translations to and from by year' = 'both',
          'translated from where?' = 'sourceCities',
          'translated to where?' = 'targetCities'
        )
      ),
      numericInput("authorCount", "The number of authors to display",
        value = 30)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput('distPlot'),
      textOutput('languages'),
      h3('Authors'),
      textOutput('authors'),
      h3('Publication places'),
      textOutput('cities')
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  output$distPlot <- renderPlot({
    plot <- NULL
    if (input$display == 'targetLanguages') {
      plot <- displayTargetLanguagesOf(input$language, input$rng)
    } else if (input$display == 'sourceLanguages') {
      plot <- displaySourceLanguagesOf(input$language, input$rng)
    } else if (input$display == 'sourceByYear') {
      plot <- displaySourceByYear(input$language, input$rng)
    } else if (input$display == 'targetByYear') {
      plot <- displayTargetByYear(input$language, input$rng)
    } else if (input$display == 'both') {
      plot <- displayBoth(input$language, input$rng)
    } else if (input$display == 'sourceCities') {
      plot <- displaySourceCities(input$language, input$rng)
    } else if (input$display == 'targetCities') {
      plot <- displayTargetCities(input$language, input$rng)
    }
    plot
  }, res = 96)
  
  output$authors <- renderText({
    authors <- NULL
    if (input$display %in% c('targetLanguages', 'sourceByYear', 'targetCities')) {
      authors <- getSourceAuthors(input$language, input$rng)
    } else if (input$display %in% c('sourceLanguages', 'targetByYear', 'sourceCities')) {
      authors <- getTargetAuthors(input$language, input$rng)
    } else {
      print("does not match")
    }
    if (!is.null(authors)) {
      items <- authors %>% 
        mutate(output = sprintf("%s (%d)", author, n)) %>% 
        select(output) %>% 
        unlist(use.names = FALSE)
      if (!is.na(input$authorCount)) {
        items <- items[1:as.numeric(input$authorCount)]
      }
      paste(items, collapse = ' — ')
    }
  })

  output$cities <- renderText({
    cities <- NULL
    if (input$display %in% c('targetLanguages', 'sourceByYear', 'sourceCities')) {
      cities <- getSourceCities(input$language, input$rng)
    } else if (input$display %in% c('sourceLanguages', 'targetByYear', 'targetCities')) {
      cities <- getTargetCities(input$language, input$rng)
    } else {
      print("does not match")
    }
    if (!is.null(cities)) {
      items <- cities %>% 
        mutate(output = sprintf("%s (%d)", publicationPlace, n)) %>% 
        select(output) %>% 
        unlist(use.names = FALSE)
      paste(items, collapse = ' — ')
    }
  })

  output$languages <- renderText({
    cities <- NULL
    if (input$display %in% c('targetLanguages', 'sourceByYear', 'sourceCities')) {
      cities <- getSourceLanguagesOf(input$language, input$rng)
    } else if (input$display %in% c('sourceLanguages', 'targetByYear', 'targetCities')) {
      cities <- getTargetLanguagesOf(input$language, input$rng)
    } else {
      print("does not match")
    }
    if (!is.null(cities)) {
      items <- cities %>% 
        mutate(output = sprintf("%s (%d)", language, n)) %>% 
        select(output) %>% 
        unlist(use.names = FALSE)
      paste(items, collapse = ' — ')
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
