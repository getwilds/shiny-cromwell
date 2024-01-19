proof_loggedin <- function() {
  !identical(Sys.getenv("PROOF_TOKEN"), "") &&
    rlang::is_list(cromwell_version())
}

dataModal <- function(failed = FALSE, error = "Invalid username or password") {
  modalDialog(
    textInput("username", "Username",
      placeholder = "HutchNet username"
    ),
    passwordInput("password", "Password",
      placeholder = "HutchNet password"
    ),
    if (failed) {
      div(tags$b(error, style = "color: red;"))
    },
    footer = tagList(
      modalButton("Cancel"),
      actionButton("submit", "Submit")
    )
  )
}
