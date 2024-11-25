
.onLoad <- function(...) {
  shiny::addResourcePath(
    prefix = "MAST21-assets", # custom prefix that will be used to reference your directory
    directoryPath = system.file("www", package = "MAST21V2-universityOfOttawa") # path to resource in your package
  )
}



#' MAST21 protocol for University of Ottawa
#'
#' @param musicassessr_state
#' @param language
#' @param final_qualtrics_url
#' @param data_collection_method
#'
#' @return
#' @export
#'
#' @examples

deploy_MAST21V2_2024_uOttawa <- function(
    app_name = paste("University of Ottawa ", format(Sys.Date(), "%Y"), " Study"),
    language,
    musicassessr_state = "test",
    setup_pages = TRUE,
    data_collection_method = c("midi", "audio", "key_presses"),
    get_p_id = TRUE,
    absolute_url = "https://musicog.ca/",
    final_qualtrics_url = '',
    opening_and_final_image = "https://img.freepik.com/free-vector/headphone-concept-illustration_114360-2132.jpg?w=826&t=st=1708543460~exp=1708544060~hmac=7a9cba8f7104f82422a55f30f5120598e4ce64dd222a1247c8da79825dddacb6"
) {


  after_tl <- after_setup_uOttawa(
    get_p_id = get_p_id,
    absolute_url = absolute_url,
    data_collection_method = "audio",
    setup_pages = setup_pages,
    app_name = app_name,
    musicassessr_state = musicassessr_state,
    final_qualtrics_url = final_qualtrics_url,
    opening_and_final_image = opening_and_final_image,
    language = language
  )


}


