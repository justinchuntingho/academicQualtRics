add_PROLIFIC_Q <- function(DATA_CENTER, SURVEY_ID, API_TOKEN){
  # Create Block
  block_id <- create_block(DATA_CENTER, SURVEY_ID, API_TOKEN, "Prolific_ID")

  # Add Q
  payload_list <- list_of_lists <- list(
    QuestionText = "What is your Prolific ID?<br>Please note that this response should auto-fill with the correct ID<br>",
    DefaultChoices = list(TEXT = list(Text = "${e://Field/PROLIFIC_PID}")),
    DataExportTag = "Prolific_ID",
    QuestionType = "TE",
    Selector = "SL",
    QuestionDescription = "What is your Prolific ID? Please note that this response should auto-fill with the correct ID",
    Validation = list(Settings = list(
      ForceResponse = "OFF",
      Type = "None"
    )),
    SearchSource = list(AllowFreeResponse = "false"),
    QuestionText_Unsafe = "What is your Prolific ID?<br>Please note that this response should auto-fill with the correct ID<br>"
  )
  payload <- jsonlite::toJSON(payload_list, auto_unbox = TRUE, pretty = TRUE)
  response <- httr::POST(
    paste0("https://",DATA_CENTER,".qualtrics.com/API/v3/survey-definitions/",SURVEY_ID, "/questions?blockId=", block_id),
    httr::add_headers(.headers = gen_header(API_TOKEN)),
    body = payload,
    encode = "json"
  )
  check_status(response)
}

add_PROLIFIC_block <- function(DATA_CENTER,SURVEY_ID,API_TOKEN){
  payload <- get_flow(DATA_CENTER,SURVEY_ID,API_TOKEN)
  flow <- payload$Flow


  # Replace flow
  payload_list <- list(
    list(
      Type = "EmbeddedData",
      FlowID = "FL_00",
      EmbeddedData = list(
        list(
          Description = "PROLIFIC_PID",
          Type = "Recipient",
          Field = "PROLIFIC_PID",
          VariableType = "String",
          DataVisibility = list(),
          AnalyzeText = FALSE
        )
      )
    )
  )
  new_flow <- c(payload_list,
                flow)
  payload$Flow <- new_flow

  response <- httr::PUT(
    paste0("https://",DATA_CENTER,".qualtrics.com/API/v3/survey-definitions/",SURVEY_ID,"/flow"),
    httr::add_headers(.headers =  gen_header(API_TOKEN)),
    body = jsonlite::toJSON(payload, auto_unbox = TRUE, pretty = TRUE),
    encode = "json"
  )
  check_status(response)
}

add_EOSRedirect <- function(url,
                    DATA_CENTER = NULL,
                    SURVEY_ID = NULL,
                    API_TOKEN = NULL,
                    BackButton = FALSE,
                    Footer = "",
                    Header = "",
                    SurveyExpiration = NA,
                    SurveyProtection = "PublicSurvey"){
  payload_list <- list(
    EOSRedirectURL = url,
    SurveyTermination = "Redirect",
    BackButton = BackButton,
    Footer = Footer,
    Header = Header,
    SurveyExpiration = SurveyExpiration,
    SurveyProtection = SurveyProtection
  )
  response <- httr::PUT(
    paste0("https://",DATA_CENTER,".qualtrics.com/API/v3/survey-definitions/",SURVEY_ID,"/options"),
    httr::add_headers(.headers =  gen_header(API_TOKEN)),
    body = jsonlite::toJSON(payload_list, auto_unbox = TRUE, null = "null", pretty = TRUE),
    encode = "json"
  )
  check_status(response)
}

#' Setup Prolific Integration
#'
#' @param url string, url for the Prolific return URL
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
#' setup_prolific("https://app.prolific.com/submissions/complete?cc=A20A223T",
#'               "fra1",
#'               "SV_S3A96bzOnfyKMEDCiKhw",
#'               "ZAhIjt6CkPO5FyczlRhJ")
#' }
setup_prolific <- function(url, DATA_CENTER, SURVEY_ID, API_TOKEN){
  add_PROLIFIC_block(DATA_CENTER = DATA_CENTER, SURVEY_ID = SURVEY_ID, API_TOKEN = API_TOKEN)
  add_PROLIFIC_Q(DATA_CENTER = DATA_CENTER, SURVEY_ID = SURVEY_ID, API_TOKEN = API_TOKEN)
  add_EOSRedirect(url = url, DATA_CENTER = DATA_CENTER, SURVEY_ID = SURVEY_ID, API_TOKEN = API_TOKEN)
}
