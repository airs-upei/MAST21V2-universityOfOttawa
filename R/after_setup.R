

after_setup_uOttawa <- function(page_type = "record_midi_page",
                                setup_pages = TRUE,
                                data_collection_method = c("midi", "audio", "key_presses"),
                                get_p_id = TRUE,
                                language,
                                app_name,
                                opening_and_final_image,
                                musicassessr_state,
                                absolute_url = "https://musicog.ca/",
                                final_qualtrics_url = "") {

  data_collection_method <- match.arg(data_collection_method)


  stopifnot(
    page_type %in% c("record_midi_page", "record_audio_page", "record_key_presses_page"),
    is.scalar.logical(setup_pages),
    is.scalar.character(data_collection_method),
    is.scalar.logical(get_p_id)
  )

  # function() {
  psychTestR::make_test(
    psychTestR::join(
      psychTestR::new_timeline(
        psychTestR::join(



          musicassessr::musicassessr_init(),

          welcome_pg <- psychTestR::one_button_page(shiny::tags$div(shiny::tags$h2(paste("Welcome to the University of Ottawa ", 	format(Sys.Date(), "%Y"), " Singing Test")),
                                                                    shiny::tags$img(src = opening_and_final_image, height = 200, width = 200))),




          u_Ottawa_intro(musicassessr_state),

          musicassessr::setup_pages(input = "microphone", absolute_url = absolute_url, SNR_test = TRUE),


          psychTestR::elt_save_results_to_disk(complete = FALSE),


          musicassessr::long_tone_trials(num_items = 6)

        ),

        dict  = musicassessr::dict(NULL),
        default_lang = language

      ), # end timeline (it's not needed from here onwards, and the SAA is embedded in UPEI_extra_questions, so to avoid nesting)



      mast_21_uOttawa(mast_inst = "You will now have another test of short singing examples.
                    There are 21 questions. The first 20 are very short.
                    Like the previous test, you will hear a melody and be asked to imitate.
                    Unlike the previous test, in which you sang along with the example, now you will listen and then sing: you will hear the example and then sing the imitation after it.
                    You will be asked to sing the 21 examples using a syllable: /ta/ (“Taah”).
                    You will also be asked to sing “Happy birthday” at the end."),



      musicassessr::sing_happy_birthday_page(feedback = FALSE, label = "sing_hbd1", text = "Please sing Happy Birthday on the syllable /ta/."),


      psychTestR::elt_save_results_to_disk(complete = TRUE),

      psychTestR::reactive_page(function(state, ... ) {
        p_id <- psychTestR::get_global('p_id', state)
        url <- paste0(final_qualtrics_url, p_id)
        if(grepl("http", final_qualtrics_url)) {
          psychTestR::final_page(shiny::tags$div(shiny::tags$p("Please click on the following link to go to the next session of this study: ",
                                                               shiny::tags$a(" click here", href = url, target = "_blank"), ".")))
        }

        else {
          psychTestR::final_page(shiny::tags$div(
            shiny::tags$p('Thank You!'),
            shiny::tags$p("You have completed the test.")))
        }

      })

    )
    ,
    opt = upei_test_options(musicassessr_state)
  )
  # }
}
