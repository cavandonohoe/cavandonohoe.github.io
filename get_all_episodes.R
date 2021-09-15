

#' Get All Episodes
#'
#' @param series_imdb_id unique id IMDb uses for series identifier
#'
#' @return data frame with imdb_id, series_name, episode, series_ep, season, season_ep, url, user_rating, and user_votes
#' \itemize{
#'  \item{"imdb_id"}{unique id IMDb uses for series identifier}
#'  \item{"series_name"}{name of series}
#'  \item{"episode"}{episode name}
#'  \item{"series_ep"}{nth episode of entire series}
#'  \item{"season"}{season number}
#'  \item{"season_ep"}{season episode number}
#'  \item{"url"}{url for episode}
#'  \item{"user_rating"}{IMDb rating for episode}
#'  \item{"user_votes"}{number of votes}
#' }
#' @export
#'
#' @examples
get_all_episodes = function(series_imdb_id) {
  
  # Get existing seasons
  title_url = paste0("http://www.imdb.com/title/", series_imdb_id, "/episodes")
  base_page = read_html(title_url) 
  
  # every now and then, we have a show like One Piece that doesn't play by the rules
  # instead of seasons, they have years
  # so let's make an indicator to tell us when one of these shows is a season or a year
  # hopefully those are the only two options...
  # also I am predicting that most of these edge cases will be animes
  # season == TRUE; year == FALSE
  # also true for Naruto: Shippuden
  season_ind = ifelse(!(series_imdb_id %in% c("tt0388629", "tt0988824")), TRUE, FALSE)
  season_node = ifelse(season_ind, "#bySeason", "#byYear")
  season_url = ifelse(season_ind, "season", "year")
  
  # season vector
  seasons = base_page %>% 
    html_node(season_node) %>% 
    html_text() %>%
    gsub(pattern = " ", replacement = "") %>% str_split(pattern = "\n") %>% unlist
  
  seasons = seasons[seasons != ""]
  
  
  
  # Get episodes for each season
  episodes = tibble()
  for (season in seasons) {
    
    # this is a necessary addition to for Fullmetal Alchemist: Brotherhood
    # and Top Gear too
    # speaking of Top Gear, as of now, there are a episodes without votes
    if (season == "Unknown") {
      season_base_page = paste0("https://www.imdb.com/title/", series_imdb_id,
                                "/episodes?season=", -1) %>% 
        read_html()
    }else {
      season_base_page = paste0("https://www.imdb.com/title/", series_imdb_id,
                                "/episodes?", season_url, "=", season) %>% 
        read_html()
    }
    
    # grab season number (may be uneccesary since that is the index in the for loop)
    season_number = season_base_page %>% 
      html_node("#episodes_content > .seasonAndYearNav") %>%
      html_node(season_node) %>% 
      html_node(xpath="//option[@selected]") %>% 
      html_attr("value")
    
    # grab episode number
    episode_number = season_base_page %>% 
      html_node("#episodes_content > .clear > .eplist") %>% 
      html_nodes(".list_item") %>% 
      html_nodes(".info") %>% 
      html_nodes("meta") %>% 
      html_attr("content")
    
    # grab episode name
    episode_name = season_base_page %>% 
      html_node("#episodes_content > .clear > .eplist") %>% 
      html_nodes("div.list_item") %>% 
      html_nodes("div > strong") %>% 
      html_node("a") %>% 
      html_attr("title")
    
    # make a tibble of votes and episode rating
    # since the two numbers may not match up
    episode_rating_votes = season_base_page %>% 
      html_node("#episodes_content > .clear > .eplist") %>% 
      html_nodes("div") %>% 
      html_nodes("div.info") %>% 
      html_nodes("div.ipl-rating-widget") %>% 
      html_nodes("div.ipl-rating-star.small") %>% 
      html_text() %>% 
      as_tibble() %>% 
      mutate(rating_votes = str_squish(value)) %>% 
      separate(col = rating_votes, into = c("rating", "votes"), sep = " \\(") %>%
      mutate(votes = gsub(x = votes, pattern = "\\)", replacement = ""))
    
    # every now and then a the episode votes/ratings will be empty, but there will
    # still be an episode name, so we will just have a 1 row tibble with NAs instead
    if (nrow(episode_rating_votes) == 0) {
      episode_rating_votes = tibble(value = NA_character_, rating = NA_character_,
                                    votes = NA_character_)
    }
    

    # grab season name
    series_name = season_base_page %>% 
      html_node("#main") %>% 
      html_node(".subpage_title_block") %>% 
      html_node(".parent") %>% 
      html_node("h3") %>% 
      html_node("a") %>% 
      html_text()
    
    # grab link for each episode
    season_episodes = season_base_page %>% 
      html_node("#episodes_content > .clear > .eplist") %>% 
      html_nodes("div") %>% 
      html_nodes("div > strong") %>% 
      html_node("a") %>% 
      html_attr("href") %>% 
      as_tibble()
    
    # Filter only episodes that were displayed on TV
    if (nrow(season_episodes) > length(episode_rating)) {
      season_episodes = season_episodes %>% 
        filter(row_number() <= length(episode_rating))
      episode_name = episode_name[1:length(episode_rating)]
      episode_number = episode_number[1:length(episode_rating)]
    }
    
    # collect everything
    season_episodes = season_episodes %>% 
      mutate(
        series_name = series_name,
        episode = episode_name,
        url = paste0("http://www.imdb.com", value),
        season = (season_number),
        season_ep = as.numeric(episode_number),
        user_rating = episode_rating_votes$rating,
        user_votes = episode_rating_votes$votes,
        imdb_id = series_imdb_id
      ) %>% 
      select(-value)
    
    episodes = rbind(episodes, season_episodes)
  }
  
  episodes = episodes %>% 
    arrange(season, season_ep) %>% 
    mutate(series_ep = row_number()) %>% 
    filter(user_votes > 0)
  
  
  episode_ratings = episodes %>% 
    select(imdb_id, series_name, episode, series_ep, season, season_ep, url, user_rating, user_votes)
  
  return(episode_ratings)
}
