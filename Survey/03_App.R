# 00 Dependencies ##############################################################

# Loading packages
library(surveydown)
library(dplyr)
library(readr)
library(glue)
library(here)
library(kableExtra)
library(shinyjs)

# Fetching the design file with candidate profiles
design <- read_csv(here("data", "choice_questions.csv"))

# Database setup
db <- sd_db_connect(ignore = FALSE)

# 01 UI setup ##################################################################

ui <- tagList(
  useShinyjs(),

  # -- Centered loading overlay ---
  tags$div(
    id = "loading_overlay",
    style = "
      position: fixed;
      top: 0; left: 0;
      width: 100vw; height: 100vh;
      background: rgba(0,0,0,0.3);
      z-index: 9999;
      display: flex;
      justify-content: center;
      align-items: center;
    ",
    tags$div(
      style = "
        background: white;
        padding: 40px 60px;
        border-radius: 10px;
        box-shadow: 0 0 20px rgba(0,0,0,0.2);
        font-size: 22px;
        text-align: center;
        display: flex;
        flex-direction: column;
        align-items: center;
        width: 400px;
      ",
      tags$div(
        class = "loader",
        style = "
          border: 6px solid #f3f3f3;
          border-top: 6px solid #3498db;
          border-radius: 50%;
          width: 50px; height: 50px;
          animation: spin 1s linear infinite;
          margin-bottom: 20px;
        "
      ),
      tags$div("Spørgeskemaet indlæses - vent et øjeblik",
               style = "margin-bottom: 20px;"),
      tags$div(
        style = "
          width: 100%;
          background: #f3f3f3;
          border-radius: 10px;
          height: 10px;
          overflow: hidden;
        ",
        tags$div(
          id = "progress_bar",
          style = "
            height: 10px;
            background: #3498db;
            border-radius: 10px;
            width: 0%;
            transition: width 0.3s ease;
          "
        )
      )
    )
  ),

  # --- Survey UI ---
  sd_ui(),

  # --- CSS ---
  tags$style(HTML("
  @keyframes spin {
    0% { transform: rotate(0deg); }
    100% { transform: rotate(360deg); }
  }
  @media (max-width: 768px) {
    .table-condensed {
      font-size: 13px !important;
    }
    .matrix-question {
      font-size: 13px !important;
    }
    .matrix-question th {
      font-size: 13px !important;
    }
  }
")),

  # --- JS ---
  tags$script(HTML("
    // Start progress bar
    var startTime = Date.now();
    var duration = 3500;

    function updateProgress() {
      var elapsed = Date.now() - startTime;
      var pct = Math.min(95, (elapsed / duration) * 95);
      $('#progress_bar').css('width', pct + '%');
      if (pct < 95) {
        requestAnimationFrame(updateProgress);
      }
    }
    requestAnimationFrame(updateProgress);

    // Remove overlay when idle
    $(document).on('shiny:idle', function() {
      $('#progress_bar').css('width', '100%');
      setTimeout(function() {
        $('#loading_overlay').fadeOut(500);
      }, 300);
    });
  "))
)

# 02 Helper functions ##########################################################

# Helper function for the choice experiment
make_cbc_table <- function(df, attr_order) {
  alts <- df |>
    mutate(altID = ifelse(altID == 1, "<b>Kandidat A</b>", "<b>Kandidat B</b>")) |>
    select(
      ` ` = altID,
      `<b>Køn:</b>`                              = Køn,
      `<b>Alder:</b>`                            = Alder,
      `<b>Omdømme:</b>`                          = Kompetence,
      `<b>Økonomisk politik:</b>`                = Okonomi,
      `<b>Udlændingepolitik:</b>`                = Indvandring,
      `<b>Medieomtale:</b>`                      = Skandale
    )
  row.names(alts) <- NULL
  alts <- alts[, c(" ", attr_order)]

  alts_t <- t(alts)
  n_rows <- nrow(alts_t)

  table <- kbl(alts_t, escape = FALSE) |>
    kable_styling(
      bootstrap_options = c("hover", "condensed"),
      full_width = FALSE,
      position = "center"
    ) |>
    row_spec(seq(1, n_rows, 2), background = "white") |>
    row_spec(seq(2, n_rows, 2), background = "#F2F2F2") |>
    column_spec(1, background = "white")

  function() { table }
}

# Helper function for the single profile experiment
make_single_table <- function(df, attr_order) {
  alts <- df |>
    select(
      `<b>Køn:</b>`                  = Køn,
      `<b>Alder:</b>`                = Alder,
      `<b>Omdømme:</b>`              = Kompetence,
      `<b>Økonomisk politik:</b>`    = Okonomi,
      `<b>Udlændingepolitik:</b>`    = Indvandring,
      `<b>Medieomtale:</b>`          = Skandale
    )
  row.names(alts) <- NULL
  alts <- alts[, attr_order]

  alts_t <- t(alts)
  n_rows <- nrow(alts_t)

  table <- kbl(alts_t, escape = FALSE) |>
    kable_styling(
      bootstrap_options = c("hover", "condensed"),
      full_width = FALSE,
      position = "center"
    ) |>
    row_spec(seq(1, n_rows, 2), background = "white") |>
    row_spec(seq(2, n_rows, 2), background = "#F2F2F2") |>
    column_spec(1, background = "white")

  function() { table }
}

# 03 Server setup ##############################################################

server <- function(input, output, session) {

  # Sampling a random respondentID and storing it in the data
  respondentID <- sample(design$respID, 1)
  sd_store_value(respondentID, "respID")

  # Randomizing attribute order per respondent
  set.seed(respondentID)
  attr_order <- c(
    "<b>Køn:</b>",
    "<b>Alder:</b>",
    sample(c(
      "<b>Omdømme:</b>",
      "<b>Økonomisk politik:</b>",
      "<b>Udlændingepolitik:</b>",
      "<b>Medieomtale:</b>"
    ))
  )

  # Filtering for the rows for the chosen respondentID
  df <- design |>
    filter(respID == respondentID)

  output$cbc1_table <- make_cbc_table(df |> filter(qID == 1), attr_order)
  output$cbc2_table <- make_cbc_table(df |> filter(qID == 2), attr_order)
  output$cbc3_table <- make_cbc_table(df |> filter(qID == 3), attr_order)
  output$cbc4_table <- make_cbc_table(df |> filter(qID == 4), attr_order)
  output$cbc5_table <- make_cbc_table(df |> filter(qID == 5), attr_order)

  # Creating the single profiles
  output$single_table_1  <- make_single_table(df |> filter(qID == 6,  altID == 1), attr_order)
  output$single_table_2 <- make_single_table(df |> filter(qID == 7, altID == 1), attr_order)

  # Showing questions conditionally
  sd_show_if(
    input$immigration == "2" ~ "middleim",
    input$econ == "2" ~ "middleecon"
  )

  # --- Remove overlay when app is loaded ---
  observe({
    # Register a loaded resp ID.
    req(respondentID)

    # Remove overlay
    session$sendCustomMessage("hideLoader", 0)
  })


  # Run surveydown server and define database
  sd_server(db = db)
}

# Launching the app
shiny::shinyApp(ui = ui, server = server)
