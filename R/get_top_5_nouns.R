#' Title
#'
#' @param start_date 
#' @param end_date 
#' @param type_speech 
#' @param party 
#' @param member 
#' @param size 
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
    #add Swedish language modle
    
    model_file_path <- "swedish-ud-2.5-191206.udpipe"
    
    if (!file.exists(model_file_path)) {
      ud_model_info <- udpipe::udpipe_download_model(language = "swedish")
      #使用udpipe_download_model模型下载瑞典语预训练模型
      file.copy(ud_model_info$file_model, model_file_path)
      #使用file.copy()将下载的模型文件复制到指定路径model_file_path
      ud_model <- udpipe::udpipe_load_model(model_file_path)
      #使用udpipe_load_model()加载该模型
    } else {
      ud_model <- udpipe::udpipe_load_model(model_file_path)
      #如果已存在，直接使用udpipe_load_model1()加载上面下载的模型
    }
    
    if(type_speech == ""){ #如果是空字符串设置为NULL
      type_speech <- NULL
    }
    if(party == ""){
      party <- NULL
    }
    
    url <- paste0(        #paste0拼接多个字符串
      "https://data.riksdagen.se/anforandelista/?rm=&anftyp=", type_speech,
      "&d=", start_date, "&ts=", end_date,
      "&parti=", party, "&iid=", member,
      "&sz=", size, "&utformat=xml"
    )
    #构建动态url
    # get speech
    response <- httr::GET(url)
    if(httr::status_code(response) != 200){
      stop("failed")
    }
    #如果状态不是200提示获取失败
    
    data <- httr::content(response, as = "text", encoding = "UTF-8")
    #content函数返回API响应的原始XML文本
    parsed_xml <- xml2::read_xml(data)
    #xml2::read_xml()函数解析前面提取的XML文本，并将其转换为R中的XML对象
    anforanden <- xml2::xml_find_all(parsed_xml, "//anforande")
    #xml2::xml_find_all() 函数查找解析后的XML对象中所有匹配 XPath 表达式//anforande 的节点
    
    
    anforande_count <- length(anforanden) #保存所有节点的数量也就是演讲的数量
    numner_of_speeches <- paste("the number of speeches：", anforande_count)
    
    urls <- xml2::xml_text(xml2::xml_find_all(anforanden, ".//anforande_url_xml"))
    #查找子节点<anforande_url_xml> 即发言内容
    #.// 的作用是查找当前节点及其所有子节点
    #xml2::xml_text() 的作用是从 XML 节点中提取文本内容，因此返回的结果是字符串
    #xml2::xml_find_all() 可能找到多个节点，xml2::xml_text() 返回的是这些节点的文本构成的字符型向量
    #字符向量包含所有演讲的列表
    
    
    
    if(length(urls) == 0){
      stop("no URL")
    }
    # get text of speech
    speech_texts <- c()
    #创建演讲文本向量
    for (url in urls) { #这句的url看不懂哪来的
      response <- httr::GET(url)
      if (httr::status_code(response) == 200){
        data_text <- httr::content(response, as = "text", encoding = "UTF-8")#原始xml文本
        parsed_text <- xml2::read_xml(data_text) #解析为R中的XML对象
        speech_text <- xml2::xml_text(xml2::xml_find_all(parsed_text, "//anforandetext"))
        #找到演讲内容并存储为text文本
        speech_texts <- c(speech_texts, speech_text)
        #存储所有文本
      } else {
        print(paste("can not get speech，URL：", url))
      }
    }
    
    if(length(speech_texts) == 0){
      stop("no text")
    }
    
    docs <- tm::VCorpus(tm::VectorSource(speech_texts))
    #使用 tm::VCorpus() 创建了一个文本语料库对象，输入是所有演讲文本 speech_texts，它是从前面的步骤中提取的演讲内容构成的字符向量
    #tm::VectorSource() 将 speech_texts 转换为一个可以用于语料库的数据源
    
    # tm预处理
    docs <- tm::tm_map(docs, tm::content_transformer(tolower)) #小写
    docs <- tm::tm_map(docs, tm::removePunctuation)#去除标点符号
    docs <- tm::tm_map(docs, tm::removeNumbers)#去除数字字符
    docs <- tm::tm_map(docs, tm::removeWords, stopwords("sv"))#去除瑞典语停用词
    
    custom_stopwords <- c("dag","år","regering","fråga","del","talman","möjlighet",
                          "tid","fråga","utredning")
    docs <- tm::tm_map(docs, tm::removeWords, custom_stopwords)
    #去除自定义停用词
    
    text_data <- sapply(docs, as.character) #转换为字符型向量
    
    annotated <- udpipe::udpipe_annotate(ud_model, x = text_data)
    #对文本进行词性标注，ud_model是瑞典语模型
    annotated_df <- as.data.frame(annotated)
    #返回数据框，包含每个词的词性词根lemma等
    
    # extract nouns
    nouns <- annotated_df$lemma[annotated_df$upos == "NOUN"]
    #从标注后的数据 annotated_df 中，提取所有被标注为名词的词根（lemma）
    #upos == "NOUN" 用于筛选出词性为名词的词
    
    # get frequency
    noun_frequency <- table(nouns)
    #table函数对提取的名词计数，得到每个名词出现的频率
    noun_frequency_sorted <- sort(noun_frequency, decreasing = TRUE)
    #使用sort函数对名词频率表进行降序排列
    
    first_10 <- head(noun_frequency_sorted, 10)
    #使用head函数获取频率最高的前10个名称
    result <- list(
      numner_of_speeches=numner_of_speeches,
      top_10_nouns=first_10
      
    )
    
    return(result)
  }
get_top_10_nouns("","","","","0538982776628","200")
