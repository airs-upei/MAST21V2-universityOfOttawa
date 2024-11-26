
daa_low <- function() {
  psychTestR::conditional(
    test = function(state, ...) {
      range <- psychTestR::get_global("range", state)
      range %in% c("Baritone", "Bass", "Tenor")
    },
    logic = MAST_wav(trial_type = "daa", high_or_low = "low")
  )
}

daa_high <- function() {
  psychTestR::conditional(
    test = function(state, ...) {
      range <- psychTestR::get_global("range", state)
      range %in% c("Soprano", "Alto")
    },
    logic = MAST_wav(trial_type = "daa", high_or_low = "high")
  )
}

doo_low <- function() {
  psychTestR::conditional(
    test = function(state, ...) {
      print('doo_low...')
      range <- psychTestR::get_global("range", state)
      print(range)
      range %in% c("Baritone", "Bass", "Tenor")
    },
    logic = MAST_wav(trial_type = "doo", high_or_low = "low")
  )
}

doo_high <- function() {
  psychTestR::conditional(
    test = function(state, ...) {
      print('doo_high...')
      range <- psychTestR::get_global("range", state)
      print(range)
      range %in% c("Soprano", "Alto")
    },
    logic = MAST_wav(trial_type = "doo", high_or_low = "high")
  )
}

condition_one <- function() {
  # # daa then doo
  psychTestR::conditional(test = function(state, ...) {
    psychTestR::get_global("snap", state) == 1
  }, logic = psychTestR::join(
    psychTestR::one_button_page("In the following trials, you will sing back melodies. Please sing with a \"Taah\" sound."),
    daa_low(),
    # OR
    daa_high(),
    psychTestR::elt_save_results_to_disk(complete = FALSE)
  ))
}

condition_two <- function() {
  # # doo then daa
  psychTestR::conditional(test = function(state, ...) {
    psychTestR::get_global("snap", state) == 2
  }, logic = psychTestR::join(
    psychTestR::one_button_page("In the following trials, you will sing back melodies. Please sing with a \"Dooo\" sound."),
    doo_low(),
    # OR
    doo_high(),
    psychTestR::elt_save_results_to_disk(complete = FALSE),

    musicassessr::sing_happy_birthday_page(feedback = FALSE, label = "sing_hbd2", text = "Please sing Happy Birthday."),

    psychTestR::elt_save_results_to_disk(complete = FALSE),

    psychTestR::one_button_page("In the following trials, you will sing back melodies. Please sing with a \"Taah\" sound."),

    daa_low(),
    # OR
    daa_high()

  ))
}

setup_questions <- function() {
  psychTestR::module("setup_questions",

                     psychTestR::one_button_page(shiny::tags$div(shiny::tags$p("For best results please: "),
                                                                 shiny::tags$ul(
                                                                   shiny::tags$li("Close all tabs and windows other than this one."),
                                                                   shiny::tags$li("Quit other apps that are running, and pause any app or file downloads.")))),

                     psychTestR::one_button_page(shiny::tags$p(style = "text-align: left;", "Please note, it is possible that the program will stop working on your computer.  If this happens you may see “Aw Snap” and a “Reload” button.  Press the “Reload” button, and in most cases, the program will start up where it left off. You may be asked to enter your number-letter code again.
          When it says 'Resuming ongoing testing session. Please click OK to confirm.' click OK, and the page should reload where you were.
          If however the “Reload” option is not available,  please e-mail ", shiny::tags$strong("cfmbonu@upei.ca"), "with a copy to ", shiny::tags$strong("nbaya063@uottawa.ca"), " and state that the session could not be completed.  You will be contacted and provided the opportunity to do the test in the research lab space.")),


                     psychTestR::NAFC_page(label = "computer_type",
                                           prompt = "Which type of computer you are using?",
                                           choices = c("Laptop", "Desktop")),


                     psychTestR::NAFC_page(label = "computer_type2",
                                           prompt = "Which type of computer you are using?",
                                           choices = c("Mac",
                                                       "PC  (e.g., Dell, Hewlitt Packard, Lenova, Asus… any non-Mac computer).")),

                     psychTestR::text_input_page(
                       label = "computer_make_model",
                       prompt = "If you know the exact name, and model number of your computer please provide the information."),


                     psychTestR::NAFC_page(label = "headphone_type",
                                           prompt = "Please identify which kind of headphones you are using",
                                           choices = c("Over the ear", "Inserted in the ear", "Not using headphones")),

                     psychTestR::conditional(test = function(state, ...) {
                       psychTestR::answer(state) == "Not using headphones"
                     }, logic = psychTestR::final_page("If you do not have headphones or earbuds, please contact nbaya063@uottawa.ca to obtain headphones from the researchers.")),

                     psychTestR::elt_save_results_to_disk(complete = FALSE),

                     psychTestR::text_input_page(
                       label = "headphone_make_model",
                       prompt = "If you know the exact name, and model number of your headphones please provide the information.")

  ) # end setup_questions module
}

get_uOttawa_id <- function() {

  psychTestR::join(
    psychTestR::get_p_id(prompt = shiny::tags$div(
      shiny::tags$p("Please provide your participation identifier below created from:"),
      shiny::tags$ul(
        shiny::tags$li("1st 3 letters of the first name of your parent, guardian, or relative"),
        shiny::tags$li("Day of your birthday (2 numbers – 01 to 31)"),
        shiny::tags$li("1st 3 letters of the street you lived on growing up"),
        shiny::tags$br(),
        shiny::tags$p("For example: joh11tav")))),


    psychTestR::reactive_page(function(state, ...) {
      p_id <- psychTestR::answer(state)
      psychTestR::set_global("p_id", p_id, state)
      psychTestR::one_button_page(shiny::tags$div(
        shiny::tags$script(paste0('const p_id = \"', p_id, '\";')),
        shiny::tags$p(paste0("Thank you, ", p_id))))
    })

  )

}

u_Ottawa_intro <- function(state, append = NULL) {



  # function() {
  t <-
    # psychTestR::new_timeline(
    psychTestR::join(
      psychTestR::one_button_page(shiny::tags$div(
        shiny::tags$h1(paste("Welcome to the University of Ottawa ", format(Sys.Date(), "%Y"),  " Music Testing" )),
        shiny::tags$p("Vocalization, Music Interests and Music Knowledge Questionnaire")
      )),



      psychTestR::NAFC_page(label = "using_chrome",
                            prompt = "Are you using the most recent version of Google Chrome?",
                            choices = c("Yes", "No"),
                            save_answer = FALSE),

      psychTestR::conditional(test = function(state, answer, ...) {
        psychTestR::answer(state) == "No"
      }, logic = psychTestR::final_page(shiny::tags$div(shiny::tags$p("Please use the following link to access the instructions to download the latest version: ",
                                                                      shiny::tags$a("https://www.google.com/intl/en_uk/chrome/",
                                                                                    href = "https://www.google.com/intl/en_uk/chrome/", target = "_blank")),
                                                        shiny::tags$p("After you have downloaded the latest version simply proceed to  ",
                                                                      shiny::tags$a("https://musicog.ca/uottawa_2024/", href = "https://musicog.ca/uottawa_2024/", target = "_blank"), "to start again.")))),


      return_questions(append),

      get_uOttawa_id(),


      psychTestR::elt_save_results_to_disk(complete = FALSE)


    )
  # )


  if(is.null(append)) {
    t
  } else {
    psychTestR::make_test(
      psychTestR::join(
        t,
        append,
        psychTestR::elt_save_results_to_disk(complete = TRUE),
        psychTestR::final_page("You have finished this section.")
      ), opt = upei_test_options(state))
  }
  # }

}



mast_21_uOttawa <-  function(mast_inst) {
  psychTestR::module("MAST21",

                     psychTestR::one_button_page(mast_inst),

                     musicassessr::get_voice_range_page(with_examples = FALSE),


                     psychTestR::elt_save_results_to_disk(complete = FALSE),


                     psychTestR::code_block(function(state, ...) {
                       snap <- 1
                       psychTestR::set_global("snap", snap, state)
                     }),

                     psychTestR::elt_save_results_to_disk(complete = FALSE),

                     condition_one(),

                     psychTestR::elt_save_results_to_disk(complete = FALSE))


}



### wav stuff

MAST_low_wavs_ordered <-  c("1_F_low.wav",
                            "2_B_low.wav",
                            "3_E_low.wav",
                            "4_C_low.wav",
                            "5_FF_low.wav",
                            "6_FC_low.wav",
                            "7_FE_low.wav",
                            "8_FB_low.wav" ,
                            "9_FAC_low.wav",
                            "10_FAbC_low.wav",
                            "11_FAbCb_low.wav",
                            "12_FACs_low.wav",
                            "13_FACAF_low.wav",
                            "14_FAbCAbF_low.wav",
                            "15_FAbCbAbF_low.wav",
                            "16_FACsAF_low.wav",
                            "17_BJ1_low.wav",
                            "18_BJ2_low.wav",
                            "19_BJ3_low.wav",
                            "20_BJ4_low.wav",
                            "21_BJfull_low.wav")

MAST_high_wavs_ordered <- c("1_F_high.wav",
                            "2_B_high.wav",
                            "3_E_high.wav",
                            "4_C_high.wav",
                            "5_FF_high.wav",
                            "6_FC_high.wav",
                            "7_FE_high.wav",
                            "8_FB_high.wav" ,
                            "9_FAC_high.wav",
                            "10_FAbC_high.wav",
                            "11_FAbCb_high.wav",
                            "12_FACs_high.wav",
                            "13_FACAF_high.wav",
                            "14_FAbCAbF_high.wav",
                            "15_FAbCbAbF_high.wav",
                            "16_FACsAF_high.wav",
                            "17_BJ1_high.wav",
                            "18_BJ2_high.wav",
                            "19_BJ3_high.wav",
                            "20_BJ4_high.wav",
                            "21_BJfull_high.wav")

choose_MAST21_text <- function(file, trial_type) {

  text_note_daa <- "Please sing back the note with a 'Taa' sound then click 'Stop'."
  text_melody_daa <- "Please sing back the melody with a 'Taa' sound then click 'Stop'."
  text_note_doo <- "Please sing back the note with a 'Doo' sound then click 'Stop'."
  text_melody_doo <- "Please sing back the melody with a 'Doo' sound then click 'Stop'."
  text_note <- "Please sing back the note then click 'Stop'."
  text_melody <- "Please sing back the melody then click 'Stop'."

  if(startsWith(file, "1_") | startsWith(file, "2_") |
     startsWith(file, "3_") | startsWith(file, "4_")) {

    if(trial_type == "daa") {
      text <- text_note_daa
    } else if(trial_type == "doo") {
      text <- text_note_doo
    } else {
      text <- text_note
    }

  } else {
    if(trial_type == "daa") {
      text <- text_melody_daa
    } else if(trial_type == "doo") {
      text <- text_melody_doo
    } else {
      text <- text_melody
    }
  }
  text
}


get_MAST_files <- function(high_or_low) {
  if(high_or_low == "high") {
    file_dir <- 'MAST21-assets/MAST21_high/'
    files_list <- MAST_high_wavs_ordered

  } else {
    file_dir <- 'MAST21-assets/MAST21_low/'
    files_list <- MAST_low_wavs_ordered
  }
  list(file_dir, files_list)
}


MAST_wav <- function(trial_type = c("normal", "daa", "doo"),
                     high_or_low = c("high", "low")) {

  file_dir <- get_MAST_files(high_or_low)[[1]]
  files_list <- get_MAST_files(high_or_low)[[2]]

  res <- purrr::map(files_list, function(file) {

    text <- choose_MAST21_text(file, trial_type)

    x <- paste0(file_dir,  file)
    page_lab <- paste0("MAST21_", high_or_low, "_", trial_type, "_", which(files_list == file))
    #page_lab <- paste0(sample(1:9, 10, replace = T), collapse = "")
    musicassessr::present_stimuli(
      stimuli = x,
      stimuli_type = "audio",
      display_modality = "auditory",
      page_type = "record_audio_page",
      get_answer = musicassessr::get_answer_pyin,
      page_text = text,
      hideOnPlay = TRUE,
      auto_next_page = TRUE,
      page_label = page_lab,
      volume = 0.90)
  })

  res <- musicassessr::insert_item_into_every_other_n_position_in_list(res, psychTestR::elt_save_results_to_disk(complete = FALSE))
  res

}



#' deploy the MAST21 as wavs
#'
#' @param musicassessr_state
#'
#' @return
#' @export
#'
#' @examples
deploy_MAST21_wav <- function(musicassessr_state = 'production') {
  psychTestR::make_test(
    psychTestR::join(
      MAST21_wav(include_microphone_calibration_page = TRUE),
      psychTestR::elt_save_results_to_disk(complete = FALSE),
      psychTestR::final_page("The End.")
    ),
    opt = psychTestR::test_options(
      title = "MAST .wav test",
      admin_password = "demo",
      additional_scripts = musicassessr::musicassessr_js(musicassessr_state)
    )
  )
}


get_dob_page <- function(text = "When is your date of birth?") {
  psychTestR::page(
    label = "dob",
    ui = shiny::tags$div(
      shiny::tags$p(text),
      shiny::selectInput(inputId = "day", label = "Day", choices = as.character(1:31), width = "40%"),
      shiny::selectInput(inputId = "month", label = "Month", choices = month.name, width = "40%"),
      shiny::selectInput(inputId = "year", label = "Year", choices = as.character(1900:2021), width = "40%"),
      psychTestR::trigger_button("next", "Next")
    ),
    get_answer = function(input, ...) {
      list(day = input$day,
           month = input$month,
           year = input$year)
    })
}


upei_test_options <- function(state) {
  psychTestR::test_options(title = "University of Ottawa",
                           admin_password = "@irs@irs2021",
                           enable_admin_panel = FALSE,
                           display = psychTestR::display_options(
                             left_margin = 1L,
                             right_margin = 1L,
                             css = system.file('www/css/musicassessr.css', package = "musicassessr")
                           ),
                           additional_scripts = musicassessr::musicassessr_js(state),
                           languages = c("en"))
}


return_questions <- function(append = NULL) {

  if(is.null(append)) {
    setup_questions()
  } else {
    psychTestR::code_block(function(state, ...) { })
  }
}
