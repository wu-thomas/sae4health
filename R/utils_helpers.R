###############################################################
###  Spinner with customized message
###############################################################
#'
#' @description Show a spinner in the middle of the screen with
#' customized message
#'
#' @param message message to display on top of spinner
#'
#' @return NA
#'
#' @noRd

custom_spinner <- function(id, message = "Loading data, please Wait...") {
  div(
    id = id,
    style = "display: none; position: fixed;
    top: 50%; left: 50%; transform: translate(-50%, -50%);
    background-color: rgba(255, 255, 255, 0.8); /* Light background */
    color: #555; /* Darker text color for contrast */
    padding: 20px; border-radius: 10px; text-align: center;
    z-index: 9999",
    tags$h3(message, style = "color: #555;"),
      shiny::icon("spinner", class = "fa-spin fa-3x", style = "color: #555;")  # Dark icon color
  )
}

#rgba(255, 255, 255, 0.2)


###############################################################
###  question mark with pop up information
###############################################################

pop_question_icon <- function(id,
                              title="Information",
                              content,
                              pop.position='right'){
  # Popover for question mark icon
  pop_question <- shinyBS::bsPopover(
    id = id,
    title = title,
    content = content,
    placement = pop.position,
    trigger = "click"  # Popover appears on click
  )
  return(pop_question)

}

###############################################################
###  Panels shows whether data step is success
###############################################################
#'
#' @description a success/error panel
#'
#' @param message message to display on the panel
#'
#' @return NA
#'
#' @noRd

success_wall <- function(successMessage="Survey raw data upload successful") {
  wellPanel(
    style = "margin-top: 5px;margin-bottom: 5px; padding: 1px; max-width: 400px; margin-left: auto; margin-right: auto;",
    tags$h5(successMessage, style = "color: green; text-align: center;")
  )
}


error_wall <- function(errorMessage="Wrong") {
  wellPanel(
    style = "margin-top: 20px; background-color: #f7f7f7; padding: 10px; max-width: 400px; margin-left: auto; margin-right: auto;",
    tags$h5(errorMessage, style = "color: #333; text-align: center;")
  )
}


###############################################################
###  prompt when no data provided
###############################################################
#'
#' @description a pop up window with information
#'
#' @param message message to display on the panel
#'
#' @return NA
#'
#' @noRd

showNoFileSelectedModal <- function() {
  showModal(modalDialog(
    title = "No File Selected",
    "Please upload a file before submitting.",
    easyClose = TRUE,
    footer = modalButton("OK")
  ))
}

showDataCompleteModal <- function() {
  showModal(modalDialog(
    title = "Data Upload Complete",
      "No need to upload additional data.",
    easyClose = TRUE,
    footer = modalButton("OK")
  ))
}

showNoModelModal <- function() {
  showModal(modalDialog(
    title = "No Model Selected",
    "Please select models to fit from the checkbox table.",
    easyClose = TRUE,
    footer = modalButton("OK")
  ))
}

showNoDataModal <- function() {
  showModal(modalDialog(
    title = "Data Upload Incomplete",
    "Please uploaded all required data before conducting analysis.",
    easyClose = TRUE,
    footer = modalButton("OK")
  ))
}

showNoIndicatorModal <- function() {
  showModal(modalDialog(
    title = "Indicator Not Collected in this Country & Year",
    "Please choose a different indicator, year, or country.",
    easyClose = TRUE,
    footer = modalButton("OK")
  ))
}


showNoRecodeModal <- function(recode=NULL,Svy_indicator=NULL) {
  showModal(modalDialog(
    title = "Recode Data Missing",
    paste0('Missing ',concatenate_vector_with_and(recode), ": The country and survey you selected do not support estimation for ",Svy_indicator),
    easyClose = TRUE,
    footer = modalButton("OK")
  ))
}
###############################################################
###  prompt to whether overwrite existing data
###############################################################

overwrite_svy_dat_confirm <- modalDialog(
  "Overwrite file already oploaded?",
  title = "Overwriting files",
  footer = tagList(
    actionButton("cancel", "Cancel"),
    actionButton("ok", "Delete", class = "btn btn-danger")
  )
)




###############################################################
###  concatenate vector
###############################################################

## c('A','B','C') becomes 'A, B and C'

concatenate_vector_with_and <- function(my_vector){

  if(length(my_vector) > 1){
    # Concatenate all items except the last with commas
    string <- paste(my_vector[-length(my_vector)], collapse = ", ")
    # Add the last item with 'and'
    final_string <- sprintf("%s and %s", string, my_vector[length(my_vector)])
  } else {
    # If only one item, just convert it to string
    final_string <- as.character(my_vector)
  }

  return(final_string)
}



###############################################################
###  admin level string, integer conversion
###############################################################

#' Convert Administrative Level String to Numeric Code
#'
#' This function converts administrative level names (e.g., "National", "Admin-1")
#' into corresponding numerical values.
#'
#' @param admin_level A character string representing the administrative level.
#' It can be "National" or "Admin-X" (where X is a positive integer).
#'
#' @return An integer representing the numerical level:
#' - "National" is converted to 0.
#' - "Admin-X" is converted to X as an integer.
#' - Returns NULL if the input is invalid.
#'
#' @examples
#' admin_to_num("National")   # Returns 0
#' admin_to_num("Admin-1")    # Returns 1
#' admin_to_num("Admin-2")    # Returns 2
#' admin_to_num("Invalid")    # Returns NULL
#' @export
#'
admin_to_num <- function(admin_level) {
  if (admin_level == "National") {
    return(0)
  } else {
    # Extracting the number after "Admin-"
    num <- as.numeric(gsub("Admin-", "", admin_level))
    if (!is.na(num)) {
      return(num)
    } else {
      #stop("Invalid Admin-level")
      return(NULL)
    }
  }
}

#' Convert Numeric Code to Administrative Level String
#'
#' This function converts a numerical administrative level into its corresponding
#' string format.
#'
#' @param num A single integer representing the administrative level.
#' The value 0 corresponds to "National", while positive integers correspond to "Admin-X".
#'
#' @return A character string representing the administrative level:
#' - 0 is converted to "National".
#' - Positive integers are converted to "Admin-X".
#' - Returns NULL if the input is invalid.
#'
#' @examples
#' num_to_admin(0)    # Returns "National"
#' num_to_admin(1)    # Returns "Admin-1"
#' num_to_admin(2)    # Returns "Admin-2"
#'
#' @export
#'
num_to_admin <- function(num) {
  if (num == 0) {
    return("National")
  } else if (num > 0) {
    return(paste("Admin", num, sep = "-"))
  } else {
    #stop("Invalid numerical value")
    return(NULL)
  }
}



###############################################################
###  number of columns for arraging multiple plot
###############################################################

calculate_columns <- function(n, height_width_ratio) {
  # Start with the assumption of square root of n as number of columns
  best_cols = floor(sqrt(n))
  min_diff = Inf

  # Define unwanted column configurations based on the number of plots
  unwanted_configs <- list(
    `4` = c(3,4),
    `5` = c(4,5),
    `6` = c(4,5,6),
    `7` = c(5,6,7),
    `8` = c(5,6,7,8)
  )

  # Loop to find the optimal number of columns
  for (cols in 1:n) {
    # Check for unwanted configurations
    if (as.character(n) %in% names(unwanted_configs) && cols %in% unwanted_configs[[as.character(n)]]) {
      next  # Skip this iteration if the config is unwanted
    }

    rows = ceiling(n / cols)
    # Calculate the aspect ratio of the overall panel
    panel_ratio = (rows * height_width_ratio) / cols

    # Calculate the difference from 1 (square aspect ratio)
    diff = abs(panel_ratio - 1)

    # Check if this configuration is closer to a square
    if (diff < min_diff) {
      min_diff = diff
      best_cols = cols
    }
  }

  return(best_cols)
}


