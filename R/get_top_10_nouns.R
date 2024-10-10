#' Title
#'
#' @param start_date the start of date(yyyy-mm-dd),or skip
#' @param end_date the end of date(yyyy-mm-dd),or skip
#' @param type_speech "Nej" or skip
#' @param party "c", "l", "kd", "mp", "m", "s", "sd", "v", "-", "nyd" or skip
#' @param member "id of member" or skip
#' @param size (10,50,200,1000,2000,5000,10000)
#' 
#' @return first_10
#' 
#' @export
#' 
#' @import stopwords
#' @import future.apply
#' @import future
#' @importFrom httr GET status_code content
#' @importFrom xml2 read_xml xml_find_all xml_text
#' @importFrom tm VCorpus VectorSource tm_map content_transformer removePunctuation removeNumbers stripWhitespace
#' @importFrom udpipe udpipe_download_model udpipe_load_model udpipe_annotate
#' @importFrom utils head
#'
get_top_10_nouns <-
  function(start_date,end_date,type_speech,party,member,size){
    
    if (start_date != "" && !grepl("^\\d{4}-\\d{2}-\\d{2}$", start_date)) {
      stop("Invalid start_date format. Please use 'yyyy-mm-dd' format.")
    }
    
    if (end_date != "" && !grepl("^\\d{4}-\\d{2}-\\d{2}$", end_date)) {
      stop("Invalid end_date format. Please use 'yyyy-mm-dd' format.")
    }
    
    if (!type_speech %in% c("Nej", "")) {
      stop("Invalid type_speech value. Allowed values are 'Nej' or skip.")
    }
    
    valid_parties <- c("c", "l", "kd", "mp", "m", "s", "sd", "v", "-", "nyd", "")
    if (!party %in% valid_parties) {
      stop("Invalid party value. Allowed values are 'c', 'l', 'kd', 'mp', 'm', 's', 'sd', 'v', '-', 'nyd' or skip.")
    }
    
    if (member != "" && !grepl("^\\d+$", member)) {
      stop("Invalid member ID. It should be a numeric value or skip.")
    }
    
    valid_sizes <- c(10, 50, 200, 1000, 2000, 5000, 10000)
    if (!size %in% valid_sizes) {
      stop("Invalid size value. Allowed values are 10, 50, 200, 1000, 2000, 5000, 10000.")
    }
    
    input_params <- list(
      type_speech = type_speech,
      party = party,
      start_date = start_date,
      end_date = end_date,
      member = member
    )
    
    input_params <- lapply(input_params, function(x) if (x == "") NULL else x)
    
    type_speech <- input_params$type_speech
    party <- input_params$party
    start_date <- input_params$start_date
    end_date <- input_params$end_date
    member <- input_params$member
    
    #add swedish language modle
    
    model_file_path <- "swedish-ud-2.5-191206.udpipe"
    
    if (!file.exists(model_file_path)) {
      ud_model_info <- udpipe::udpipe_download_model(language = "swedish")
      file.copy(ud_model_info$file_model, model_file_path)
      ud_model <- udpipe::udpipe_load_model(model_file_path) 
    } else {
      ud_model <- udpipe::udpipe_load_model(model_file_path)
    }
    
    url <- paste0(
      "https://data.riksdagen.se/anforandelista/?rm=&anftyp=", type_speech,
      "&d=", start_date, "&ts=", end_date,
      "&parti=", party, "&iid=", member,
      "&sz=", size, "&utformat=xml"
    )
    
    # get speech
    response <- httr::GET(url)
    if(httr::status_code(response) != 200){
      stop("failed")
    }
    data <- httr::content(response, as = "text", encoding = "UTF-8")
    parsed_xml <- xml2::read_xml(data)
    anforanden <- xml2::xml_find_all(parsed_xml, "//anforande")
    anforande_count <- length(anforanden)
    #print(paste("the number of speeches:", anforande_count))
    
    urls <- xml2::xml_text(xml2::xml_find_all(anforanden, ".//anforande_url_xml"))
    if(length(urls) == 0){
      stop("no URL")
    }
    
    # get text of speech
    available_cores <- parallel::detectCores()
    workers <- min(14, available_cores - 1)  
    
    plan(multisession, workers = workers)  
    
    get_speech_text <- function(url) {
      response <- httr::GET(url)
      if (httr::status_code(response) == 200) {
        data_text <- httr::content(response, as = "text", encoding = "UTF-8")
        parsed_text <- xml2::read_xml(data_text)
        speech_text <- xml2::xml_text(xml2::xml_find_all(parsed_text, "//anforandetext"))
        return(speech_text)
      } else {
        warning(paste("Can not get speech, URL:", url))
        return(NA)  
      }
    }
    
    speech_texts <- future_lapply(urls, get_speech_text)
    speech_texts <- unlist(speech_texts)
    
    if(length(speech_texts) == 0){
      stop("no text")
    }
    
    docs <- tm::VCorpus(tm::VectorSource(speech_texts))
    
    # tm
    docs <- tm::tm_map(docs, tm::content_transformer(tolower))
    docs <- tm::tm_map(docs, tm::removePunctuation)
    docs <- tm::tm_map(docs, tm::removeNumbers)
    docs <- tm::tm_map(docs, tm::removeWords, stopwords("sv"))
    
    custom_stopwords <- c("dag", "\u00E5r", "regering", "fr\u00E5ga", "del", "talman", 
                          "m\u00F6jlighet", "tid", "fr\u00E5ga", "utredning","fr\u00E5ga ",
                          "exempel","koppling"  
    )
    
    docs <- tm::tm_map(docs, tm::removeWords, custom_stopwords)
    
    text_data <- sapply(docs, as.character)
    
    annotated <- udpipe::udpipe_annotate(ud_model, x = text_data)
    annotated_df <- as.data.frame(annotated)
    
    # extract nouns
    nouns <- annotated_df$lemma[annotated_df$upos == "NOUN"]
    
    # get frequency
    noun_frequency <- table(nouns)
    noun_frequency_sorted <- sort(noun_frequency, decreasing = TRUE)
    
    first_10 <- head(noun_frequency_sorted, 10)
    
    lst <- list(
      anforande_count = anforande_count,
      first_10 = first_10
    )
    return(lst)
  }