#' Title
#'
#' @return first_10
#' @export
#' 
#' @import stopwords
#' @importFrom httr GET status_code content
#' @importFrom xml2 read_xml xml_find_all xml_text
#' @importFrom tm VCorpus VectorSource tm_map content_transformer removePunctuation removeNumbers stripWhitespace
#' @importFrom udpipe udpipe_download_model udpipe_load_model udpipe_annotate
#'
get_top_10_nouns <-
function(start_date,end_date,type_speech,party,member,size){
  #add swedish language modle
  
  model_file_path <- "swedish-ud-2.5-191206.udpipe"
  
  if (!file.exists(model_file_path)) {
    ud_model_info <- udpipe::udpipe_download_model(language = "swedish")
    file.copy(ud_model_info$file_model, model_file_path)
    ud_model <- udpipe::udpipe_load_model(model_file_path) 
  } else {
    ud_model <- udpipe::udpipe_load_model(model_file_path)
  }
  
  if(type_speech == ""){
    type_speech <- NULL
  }
  if(party == ""){
    party <- NULL
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
  print(paste("the number of speeches：", anforande_count))
  
  urls <- xml2::xml_text(xml2::xml_find_all(anforanden, ".//anforande_url_xml"))
  if(length(urls) == 0){
    stop("no URL")
  }
  # get text of speech
  speech_texts <- c()
  for (url in urls) {
    response <- httr::GET(url)
    if (httr::status_code(response) == 200){
      data_text <- httr::content(response, as = "text", encoding = "UTF-8")
      parsed_text <- xml2::read_xml(data_text)
      speech_text <- xml2::xml_text(xml2::xml_find_all(parsed_text, "//anforandetext"))
      speech_texts <- c(speech_texts, speech_text)
    } else {
      print(paste("can not get speech，URL：", url))
    }
  }
  
  if(length(speech_texts) == 0){
    stop("no text")
  }
  
  docs <- tm::VCorpus(tm::VectorSource(speech_texts))
  
  # tm
  docs <- tm::tm_map(docs, tm::content_transformer(tolower))
  docs <- tm::tm_map(docs, tm::removePunctuation)
  docs <- tm::tm_map(docs, tm::removeNumbers)
  docs <- tm::tm_map(docs, tm::removeWords, stopwords("sv"))
  
  custom_stopwords <- c("dag","år","regering","fråga","del","talman","möjlighet",
                        "tid","fråga","utredning")
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
  print(first_10)
  
  return(first_10)
}
