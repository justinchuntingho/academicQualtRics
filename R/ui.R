#' Create Text Blocks for Annotation
#'
#' @param x character vector, texts to be annotated
#' @param tags character vector, identifier for texts, needs to be same length as x
#' @param DATA_CENTER string, your Qualtrics data center
#' @param SURVEY_ID string, your Qualtrics survey ID
#' @param API_TOKEN string, your Qualtrics API token
#'
#' @return A character vector of block ids. It is necessary for later steps.
#' @export
#'
#' @examples
#' texts <- c("In a hole in the ground there lived a hobbit.",
#'             "It was a hobbit-hole, and that means comfort.")
#' \dontrun{
#' add_texts(texts,
#'           tags,
#'           "fra1",
#'           "SV_S3A96bzOnfyKMEDCiKhw",
#'           "ZAhIjt6CkPO5FyczlRhJ")
#' }
add_texts <- function(x, tags, DATA_CENTER, SURVEY_ID, API_TOKEN){
  blockids <- create_blocks(x = x,
                            tags = tags,
                            DATA_CENTER = DATA_CENTER,
                            SURVEY_ID = SURVEY_ID,
                            API_TOKEN = API_TOKEN)
  for(i in seq_along(blockids)){
    add_text(blockid = blockids[i],
             text = x[i],
             tag = tags[i],
             DATA_CENTER = DATA_CENTER,
             SURVEY_ID = SURVEY_ID,
             API_TOKEN = API_TOKEN)
  }
  blockids
}


#' Add Questions to Text Blocks
#'
#' @param blockids character vector, ids of text blocks
#' @param question string, question for annotators
#' @param answers character vector, answers for annotators
#' @param tags character vector, identifier for texts, needs to be same length as x
#' @param DATA_CENTER string, your Qualtrics data center
#' @param SURVEY_ID string, your Qualtrics survey ID
#' @param API_TOKEN string, your Qualtrics API token
#'
#' @return NULL
#' @export
#'
#' @examples
#' blockids <- c("BL_v7iQbRtKtAv7mQ10zO1h", "BL_t4VtQCrLDxzJalrgShVx")
#' question <- c("What does a Hobbit hole look like?")
#' answers <- c("nasty","dirty","comfortable")
#' \dontrun{
#' add_questions(blockids,
#'               question,
#'               answers,
#'               "Annotation",
#'               "fra1",
#'               "SV_S3A96bzOnfyKMEDCiKhw",
#'               "ZAhIjt6CkPO5FyczlRhJ")
#' }
add_questions <- function(blockids, question, answers, tags, DATA_CENTER, SURVEY_ID, API_TOKEN){
  for(i in seq_along(blockids)){
    add_question(blockid = blockids[i],
                 question = question,
                 answers = answers,
                 tag = tags[i],
                 DATA_CENTER = DATA_CENTER,
                 SURVEY_ID = SURVEY_ID,
                 API_TOKEN = API_TOKEN)
  }
}

#' Create Randomizer Block
#'
#' @param blockids character vector, ids of text blocks
#' @param DATA_CENTER string, your Qualtrics data center
#' @param SURVEY_ID string, your Qualtrics survey ID
#' @param API_TOKEN string, your Qualtrics API token
#' @param present integer, how many blocks to show to each annotator
#' @param even boolean, should the randomizer distribute text evenly
#'
#' @return NULL
#' @export
#'
#' @examples
#' blockids <- c("BL_v7iQbRtKtAv7mQ10zO1h", "BL_t4VtQCrLDxzJalrgShVx")
#' \dontrun{
#' create_block_randomizer(blockids,
#'                         "fra1",
#'                         "SV_S3A96bzOnfyKMEDCiKhw",
#'                         "ZAhIjt6CkPO5FyczlRhJ")
#' }
create_block_randomizer <- function(blockids,
                                    DATA_CENTER,
                                    SURVEY_ID,
                                    API_TOKEN,
                                    present = 5,
                                    even = TRUE){
  from_block <- blockids[1]
  to_block <- blockids[length(blockids)]
  # Get flow
  payload <- get_flow(DATA_CENTER = DATA_CENTER,
                      SURVEY_ID = SURVEY_ID,
                      API_TOKEN = API_TOKEN)
  flow <- payload$Flow

  fromid <- listoflist_find(flow, "ID", from_block)
  toid <- listoflist_find(flow, "ID", to_block)

  # Replace flow
  block_randomizer <- list(
    Type = "BlockRandomizer",
    FlowID = "FL_999",
    SubSet = present,
    EvenPresentation = TRUE,
    Flow = flow[fromid:toid]
  )
  if(fromid == 1){
    pre <- NULL
  } else {
    pre <- flow[1:(fromid-1)]
  }
  if(toid == length(flow)){
    post <- NULL
  } else {
    post <- flow[(toid+1):length(flow)]
  }
  new_flow <- c(pre,
                list(block_randomizer),
                post)
  payload$Flow <- new_flow

  response <- httr::PUT(
    paste0("https://",DATA_CENTER,".qualtrics.com/API/v3/survey-definitions/",SURVEY_ID,"/flow"),
    httr::add_headers(.headers =  gen_header(API_TOKEN)),
    body = jsonlite::toJSON(payload, auto_unbox = TRUE, pretty = TRUE),
    encode = "json"
  )
  check_status(response)
}
