library(shiny)
library(dplyr)

db <- readRDS('db.rds')

ui <- function(request) {
  fluidPage(
   
   titlePanel(HTML(
     paste('Find your most similar',
           a('RePEc', href = 'http://repec.org/'),
           'economists on', a('Twitter', href = 'https://twitter.com/'))
     ), windowTitle = 'Find your most similar RePEc economists on Twitter'),
   
   sidebarLayout(
     sidebarPanel(
       textInput('screen_name', 'Your Twitter nickname:'),
       selectInput('classification', 'Classification:', c('JEL', 'NEP')),
       selectInput('country', 'Country:', c('All', unique(db$jel$country))),
       selectInput('gender', 'Gender:', c('All', 'Female', 'Male')),
       actionButton('go', 'Find users'),
       actionButton('show', 'Bookmark'),
       p(),
       p(a('Tweet', href='https://twitter.com/intent/tweet?text=Find%20similar%20#RePEc%20economist%20on%20Twitter&url=https://io.mongeau.net/repec-twitter-simil/&via=chrmongeau', class='twitter-share-button')),
       p(a('Follow @chrmongeau', href='https://twitter.com/chrmongeau', class='twitter-follow-button')),
       includeScript('http://platform.twitter.com/widgets.js')
     ),
     
     mainPanel(
       uiOutput('most_similar'),
       tableOutput('similar_users'),
       uiOutput('footer_info')
     )
   )
  )
}

server <- function(input, output, session) {
  
  observeEvent(input$show, {
    myurl <-
      paste0(
        'https://io.mongeau.net/repec-twitter-simil/?_inputs_&screen_name="',
        input$screen_name, '"&classification="', input$classification,
        '"&country="', input$country, '"&gender="', input$gender, '"&go="1"')

    showModal(modalDialog(
      title = 'To bookmark this page copy the following link:',
      a(myurl, href = myurl),
      p(),
      p('Note that this app currently lives in',
        a('shinyapps.io', href = 'https://www.shinyapps.io/'),
        ', but I plan to move it to my server. So, use the link shown in this window and not the one in the broswer address bar, as the former will be permanent.')
    ))
  })
  
  observeEvent(input$go, {
    classif <- input$classification
    
     userdata <-
       db[[tolower(input$classification)]] %>%
       filter(tolower(user) == tolower(input$screen_name)) %>%
       arrange(desc(weight))
     
     if (input$gender != 'All') {
       userdata <- filter(userdata, gender == tolower(input$gender))
     }
     
     if (input$country != 'All') {
       userdata <- filter(userdata, country == input$country)
     }
     
     userdata <- slice(userdata, 1:3)
     
     info_most_similar <-
       db[['nick']] %>%
       filter(tolower(screen_name) == tolower(userdata[1,]$friend))
     
     repec_url <-
       paste0('https://ideas.repec.org/',
              info_most_similar$repecurl, '/', info_most_similar$repecid,
              '.html')
       
     output$most_similar <- renderUI({
       if (nrow(userdata) == 0) {
         tagList(
           p('Sorry, the nickname is not in the database.',
             'Add yourself to the list by following the instructions in:'),
           p(a('https://ideas.repec.org/i/etwitter.html',
               href = 'https://ideas.repec.org/i/etwitter.html')),
           p('Then, when the database is rebuilt, you should appear here.')
         )
       } else {
         tagList(
           p('Your most similar user according to', classif, 'is',
             info_most_similar$tw_name, 'known on Twitter as',
             a(userdata[1,]$friend,
               href = paste0('https://twitter.com/', userdata[1,]$friend))),
           p(img(src = info_most_similar$tw_profile_image_url, width = '200px')),
           p('RePEc profile:', a(repec_url, href = repec_url)),
           p('Below you can find the three (max, if available) most similar users given your selection.')
         )
       }
     })
     
     output$footer_info <- renderUI({
       if (nrow(userdata) == 0) {
         similarity_note <- NULL
       } else {
         similarity_note <- 'The similarity score is calculated by building a similarity matrix (method = cosine) on JELs or NEPs fields associated to authors. This information comes from RePEc.'
       }
       
       tagList(
         p(similarity_note),
         p(HTML(paste0('Do you want to give some feedback/suggestion? Is there anything wrong (e.g., you are not here, your gender is incorrect, etc.)? Please, get in touch on Twitter (', a('@chrmongeau', href = 'https://twitter.com/chrmongeau'), ') or by e-mail (', a('christian+repec-twitter-simil@mongeau.net', href = 'mailto:christian+repec-twitter-simil@mongeau.net'), '.')))
       )
     })
     
     output$similar_users <- renderTable({
       if (nrow(userdata) == 0) {
         NULL
       } else {
         userdata %>%
           select(-user) %>%
           rename(user = friend, similarity = weight) %>%
           left_join(
             db$nick %>% select(tw_name, repecid, screen_name, repecurl),
             by = c('user' = 'screen_name')
           ) %>%
           mutate(
             Twitter = paste0('<a href="https://twitter.com/', user, '">', user, '</a>'),
             RePEc   = paste0('<a href="https://ideas.repec.org/', repecurl, '/', repecid, '.html">', repecid, '</a>')
           ) %>%
           select(Name = tw_name, Twitter, RePEc, Similarity = similarity,
                  Country = country, Gender = gender)
       }
     }, sanitize.text.function = function(x) x)
  })
}

enableBookmarking(store = 'url')

shinyApp(ui = ui, server = server)

