get_content <- function(response){
  if (httr::status_code(response) == 200) {
    content_response <- httr::content(response)
    content_response
  } else {
    cat(paste0("Failed: ", httr::status_code(response), "\n"))
  }
}

gen_header <- function(API_TOKEN, short = FALSE){
  if(short){
    HEADER <- c("Content-Type" = "application/json",
                "X-API-TOKEN" = API_TOKEN)
  } else {
    HEADER <- c("Content-Type" = "application/json",
                "Accept" = "application/json",
                "X-API-TOKEN" = API_TOKEN)
  }
  HEADER
}

check_status <- function(response){
  if (httr::status_code(response) == 200) {
    cat("Success")
  } else {
    cat(paste0("Failed: ", httr::status_code(response), "\n"))
  }
}

listoflist_find <- function(lst, field, value) {
  idx <- which(sapply(lst, function(x) !is.null(x[[field]]) && x[[field]] == value))
  idx
}

create_block <- function(DATA_CENTER, SURVEY_ID, API_TOKEN,
                         block_name = NULL) {
  if(is.null(block_name)){
    block_name <- stringi::stri_rand_strings(1,10)
  }

  # Create the payload
  payload_list <- list(
    Description = block_name,
    Type = "Standard"
  )
  payload <- jsonlite::toJSON(payload_list, auto_unbox = TRUE)

  # Send the POST request
  response <- httr::POST(
    url = paste0("https://",DATA_CENTER,".qualtrics.com/API/v3/survey-definitions/",SURVEY_ID,"/blocks"),
    httr::add_headers(gen_header(API_TOKEN)),
    body = payload,
    encode = "json"
  )

  content_response <- get_content(response)
  block_id <- content_response$result$BlockID
  cat(paste0("Successfully created Block: ", block_id, "\n"))
  block_id
}

add_question <- function(block_id, question, answers, tag, DATA_CENTER, SURVEY_ID, API_TOKEN,
                         qtype = "MC", selector = "SAVR", subselector = "TX", forced = "ON"){
  choices_df <- data.frame(
    id = as.character(1:length(answers)),
    Display = answers
  )
  payload_list <- list(
    QuestionType = qtype,
    Selector = selector,
    SubSelector = subselector,
    QuestionText = question,
    DataExportTag = tag,
    ChoiceOrder = as.character(1:length(answers)),
    Choices = stats::setNames(
      lapply(choices_df$Display, function(x) list(Display = x)),
      choices_df$id
    ),
    Validation = list(
      Settings = list(
        ForceResponse = forced,
        ForceResponseType = forced
      )
    ),
    Configuration = list(
      QuestionDescriptionOption = "UseText"
    )
  )
  payload <- jsonlite::toJSON(payload_list, auto_unbox = TRUE, pretty = TRUE)
  response <- httr::POST(
    paste0("https://",DATA_CENTER,".qualtrics.com/API/v3/survey-definitions/",SURVEY_ID, "/questions?blockId=", block_id),
    httr::add_headers(.headers = gen_header(API_TOKEN)),
    body = payload,
    encode = "json"
  )

  content_response <- get_content(response)
  question_id <- content_response$result$QuestionID
  cat(paste0("Successfully created Question: ", block_id, " ", question_id, "\n"))
  question_id
}

add_text <- function(block_id, text, tag, DATA_CENTER, SURVEY_ID, API_TOKEN){
  payload_list <- list(
    QuestionText = text,
    DataExportTag = paste0(tag,"_Article"),
    QuestionType = "DB",
    Selector = "TB",
    Configuration = list(
      QuestionDescriptionOption = "UseText"
    )
  )
  payload <- jsonlite::toJSON(payload_list, auto_unbox = TRUE)
  response <- httr::POST(
    paste0("https://",DATA_CENTER,".qualtrics.com/API/v3/survey-definitions/",SURVEY_ID, "/questions?blockId=", block_id),
    httr::add_headers(.headers = gen_header(API_TOKEN)),
    body = payload,
    encode = "json"
  )

  content_response <- get_content(response)
  question_id <- content_response$result$QuestionID
  cat(paste0("Successfully added text: ", block_id, " ", question_id, "\n"))
  question_id
}

get_blocks <- function(DATA_CENTER,SURVEY_ID,API_TOKEN){
  response <- httr::GET(
    paste0("https://",DATA_CENTER,".qualtrics.com/API/v3/survey-definitions/",SURVEY_ID),
    httr::add_headers(
      "X-API-TOKEN" = API_TOKEN,
      "Content-Type" = "application/json"
    )
  )
  res <-get_content(response)
  blocks <- res$result$Blocks
  names(blocks)
}

get_flow <- function(DATA_CENTER,SURVEY_ID,API_TOKEN){
  response <- httr::GET(
    paste0("https://",DATA_CENTER,".qualtrics.com/API/v3/survey-definitions/",SURVEY_ID,"/flow"),
    httr::add_headers(
      "X-API-TOKEN" = API_TOKEN,
      "Content-Type" = "application/json"
    )
  )
  res <-get_content(response)
  payload <- res$result
  payload
}

create_blocks <- function(x, DATA_CENTER, SURVEY_ID, API_TOKEN){
  ndoc <- length(x)
  blockids <- c()
  for(i in 1:ndoc){
    newid <- create_block(DATA_CENTER, SURVEY_ID, API_TOKEN)
    blockids <- c(blockids, newid)
  }
  blockids
}

get_options <- function(DATA_CENTER,SURVEY_ID,API_TOKEN){
  response <- httr::GET(
    paste0("https://",DATA_CENTER,".qualtrics.com/API/v3/survey-definitions/",SURVEY_ID,"/options"),
    httr::add_headers(
      "X-API-TOKEN" = API_TOKEN,
      "Content-Type" = "application/json"
    )
  )
  res <-get_content(response)
  result <- res$result
  result
}
