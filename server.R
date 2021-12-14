## server.R

# load functions
source('functions/cf_algorithm.R') # collaborative filtering
source('functions/similarity_measures.R') # similarity measures

# define functions
get_user_ratings = function(value_list) {
  dat = data.table(MovieID = sapply(strsplit(names(value_list), "_"), 
                                    function(x) ifelse(length(x) > 1, x[[2]], NA)),
                   Rating = unlist(as.character(value_list)))
  dat = dat[!is.null(Rating) & !is.na(MovieID)]
  dat[Rating == " ", Rating := 0]
  dat[, ':=' (MovieID = as.numeric(MovieID), Rating = as.numeric(Rating))]
  dat = dat[Rating > 0]
}

# read in Movies data
myurl = "https://liangfgithub.github.io/MovieData/"
movies = readLines(paste0(myurl, 'movies.dat?raw=true'))
movies = strsplit(movies, split = "::", fixed = TRUE, useBytes = TRUE)
movies = matrix(unlist(movies), ncol = 3, byrow = TRUE)
movies = data.frame(movies, stringsAsFactors = FALSE)
colnames(movies) = c('MovieID', 'Title', 'Genres')
movies$MovieID = as.integer(movies$MovieID)
movies$Title = iconv(movies$Title, "latin1", "UTF-8")

# read gen ranked data
rank_mat = read.csv("data/genres_ranked.csv")

# read in Ratings data
ratings = read.csv(paste0(myurl, 'ratings.dat?raw=true'), 
                   sep = ':',
                   colClasses = c('integer', 'NULL'), 
                   header = FALSE)
colnames(ratings) = c('UserID', 'MovieID', 'Rating', 'Timestamp')
ratings$Timestamp = NULL
i = paste0('u', ratings$UserID)
j = paste0('m', ratings$MovieID)
x = ratings$Rating
tmp = data.frame(i, j, x, stringsAsFactors = T)

# predetermine genre movie list
genre_list = c("Action", "Adventure", "Animation",
               "Children's", "Comedy", "Crime",
               "Documentary", "Drama", "Fantasy",
               "Film-Noir", "Horror", "Musical",
               "Mystery", "Romance", "Sci-Fi",
               "Thriller", "War", "Western")


small_image_url = "https://liangfgithub.github.io/MovieImages/"
movies$image_url = sapply(movies$MovieID, 
                          function(x) paste0(small_image_url, x, '.jpg?raw=true'))

shinyServer(function(input, output, session) {
  
  # show the movies to be rated
  output$ratings <- renderUI({
    num_rows <- 20
    num_movies <- 6 # movies per row
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        list(box(width = 2,
                 div(style = "text-align:center", img(src = movies$image_url[(i - 1) * num_movies + j], height = 150)),
                 #div(style = "text-align:center; color: #999999; font-size: 80%", books$authors[(i - 1) * num_books + j]),
                 div(style = "text-align:center", strong(movies$Title[(i - 1) * num_movies + j])),
                 div(style = "text-align:center; font-size: 150%; color: #f0ad4e;", ratingInput(paste0("select_", movies$MovieID[(i - 1) * num_movies + j]), label = "", dataStop = 5)))) #00c0ef
      })))
    })
  })
  
  # Calculate recommendations when the 1st button is clicked
  df1 <- eventReactive(input$btn1, {
    withBusyIndicatorServer("btn1", { # showing the busy indicator
      # hide the rating container
      useShinyjs()
      jsCode <- "document.querySelector('[data-widget=collapse]').click();"
      runjs(jsCode)
      
      # get the user's rating data
      gen = input$g
      
      user_results = (1:10)/10
      PredMovieIDs = rank_mat[,gen]
      Titles = rep(0,10)
      for (i in 1:10){
        Titles[i] = movies[(which(movies$MovieID == PredMovieIDs[i])),"Title"]
      }
      recom_results <- data.table(Rank = 1:10,
                                  MovieID = PredMovieIDs,
                                  Title = Titles,
                                  Predicted_rating =  user_results)
      
    }) # still busy
    
  }) # clicked on button
  
  # Genre output
  output$results1 <- renderUI({
    num_rows <- 2
    num_movies <- 5
    recom_result <- df1()
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
            
            div(style = "text-align:center",
                a(img(src = movies$image_url[recom_result$MovieID[(i - 1) * num_movies + j]], height = 150))
            ),
            div(style="text-align:center; font-size: 100%",
                strong(movies$Title[recom_result$MovieID[(i - 1) * num_movies + j]])
            )
            
        )
      }))) # columns
    }) # rows
    
  }) # renderUI function
  
  
  # Calculate recommendations when the 2nd button is clicked
  df <- eventReactive(input$btn2, {
    withBusyIndicatorServer("btn2", { # showing the busy indicator
      # hide the rating container
      useShinyjs()
      jsCode <- "document.querySelector('[data-widget=collapse]').click();"
      runjs(jsCode)
      
      # get the user's rating data
      value_list <- reactiveValuesToList(input)
      user_ratings <- get_user_ratings(value_list)
      user_ratings = cbind("u0",user_ratings)
      colnames(user_ratings) = c("i","j","x")
      user_ratings$j = paste0('m', user_ratings$j)
      # add user's ratings as first row to rating matrix
      
      tmp <- rbind(user_ratings, tmp)

      
      Rmat = sparseMatrix(as.integer(tmp$i), as.integer(tmp$j), x = tmp$x)
      rownames(Rmat) = levels(tmp$i)
      colnames(Rmat) = levels(tmp$j)
      Rmat = new('realRatingMatrix', data = Rmat)
      
      eval_movies <- evaluationScheme(data = Rmat, 
                                      method = "cross-validation", 
                                      k = 10,
                                      given = 3, 
                                      goodRating = 4)
      
      train_movies <- getData(eval_movies, "train")
      
      ubcf = Recommender(train_movies, method = 'UBCF')
      
      recos_ubcf = predict(ubcf, c(1), n = 10)
      final = as(recos_ubcf, "list")
      top_movieIDs = as.integer(sub('m', '', final$u0))
      
      
      user_results = (1:10)/10
      
      Titles2 = rep(0,10)
      for (i in 1:10){
        Titles2[i] = movies[(which(movies$MovieID == top_movieIDs[i])),"Title"]
      }
      recom_results <- data.table(Rank = 1:10,
                                  MovieID = top_movieIDs,
                                  Title = Titles2,
                                  Predicted_rating =  user_results)
    }) # still busy
    
  }) # clicked on button
  
  
  # display the recommendations
  output$results2 <- renderUI({
    num_rows <- 2
    num_movies <- 5
    recom_result <- df()
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
            
            div(style = "text-align:center", 
                a(img(src = movies$image_url[recom_result$MovieID[(i - 1) * num_movies + j]], height = 150))
            ),
            div(style="text-align:center; font-size: 100%", 
                strong(movies$Title[recom_result$MovieID[(i - 1) * num_movies + j]])
            )
            
        )        
      }))) # columns
    }) # rows
    
  }) # renderUI function
  
}) # server function