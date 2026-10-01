# ------------------------------------------------------------------------------
# 1. Packages
# ------------------------------------------------------------------------------

options(repos = BiocManager::repositories())
if (!require("BiocManager", quietly = TRUE)) {
  install.packages("BiocManager")
}

if (!require("shinyWidgets")) {
  warning("Installing missing package 'shinyWidgets'...")
  install.packages("shinyWidgets")
}
if (!require("shinyjs")) {
  warning("Installing missing package 'shinyjs'...")
  install.packages("shinyjs")
}

library(shiny)
library(dplyr)
library(magrittr)
library(tibble)
library(tidyr)
library(ComplexHeatmap)
library(circlize)
library(grid)
library(DT)
library(shinythemes)
library(shinyWidgets)
library(shinyjs)

options(shiny.maxRequestSize = 200 * 1024^2)

# ================================================================================
# 2. CONFIGURATION & DEFAULTS
# ================================================================================

base_path <- "data"

default_meta <- file.path(base_path, "251115_processed_Master_metadata_V2.csv")
default_norm <- file.path(base_path, "data_normalized.csv")
default_ensembl <- file.path(base_path, "ENSEMBL_ID_Covert20240306.csv")

# Helper: build paths under the data folder.
get_full_path <- function(rel_path) file.path(base_path, rel_path)

default_fdr_map <- list(
  "Infection vs Control" = get_full_path("Exp1_ALL-SE_ALL-Control/DEA_full_genes_FDR_1.csv"),
  "Low-Glucose Infection vs Low-Glucose Control" = get_full_path("Exp2_Low-SE_Low-Control/DEA_full_genes_FDR_1.csv"),
  "High-Glucose Infection vs High-Glucose Control" = get_full_path("Exp3_High-SE_High-Control/DEA_full_genes_FDR_1.csv"),
  "High-Glucose Infection vs Low-Glucose Infection" = get_full_path("Exp4_High-SE_Low-SE/DEA_full_genes_FDR_1.csv")
)

default_codes <- c(
  "Infection vs Control" = "SE_v_CON",
  "Low-Glucose Infection vs Low-Glucose Control" = "LowSE_v_LowCON",
  "High-Glucose Infection vs High-Glucose Control" = "HighSE_v_HighCON",
  "High-Glucose Infection vs Low-Glucose Infection" = "HighSE_v_LowSE"
)

default_gsea_map <- list(
  "Infection vs Control" = get_full_path("Exp1_ALL-SE_ALL-Control/Exp1_KEGG_SE_Control_GSEA_Results.csv"),
  "Low-Glucose Infection vs Low-Glucose Control" = get_full_path("Exp2_Low-SE_Low-Control/Exp2_KEGG_Low_SE_Control_GSEA_Results_Thu.csv"),
  "High-Glucose Infection vs High-Glucose Control" = get_full_path("Exp3_High-SE_High-Control/Exp3_KEGG_High_SE_Control_GSEA_Results_Thu.csv"),
  "High-Glucose Infection vs Low-Glucose Infection" = get_full_path("Exp4_High-SE_Low-SE/Exp4_KEGG_HighSE_lowSE_GSEA_Results.csv")
)

level1_display_names <- c("CON" = "Control", "SE" = "Infection")

level2_colors <- c("#9CB5D0", "#203864", "#FFD365", "#fb8500")
names(level2_colors) <- c("Low-Glucose Control", "High-Glucose Control", "Low-Glucose Infection", "High-Glucose Infection")

level2_display_map <- c(
  "Low-CON" = "Low-Glucose Control",
  "High-CON" = "High-Glucose Control",
  "Low-SE" = "Low-Glucose Infection",
  "High-SE" = "High-Glucose Infection"
)

# Helper: safe -log10 transform for FDR values.
safe_neglog10 <- function(x, min_fdr = 1e-20) {
  x[x <= 0 | is.na(x)] <- min_fdr
  -log10(x)
}

# Helper: title-case labels.
to_title_case <- function(x) {
  if (is.null(x) || length(x) == 0 || x == "") {
    return("")
  }
  if (x %in% c("Both", "Other")) {
    return(x)
  }

  parts <- strsplit(x, " ")
  if (length(parts) == 0) {
    return(x)
  }
  s <- parts[[1]]

  paste(toupper(substring(s, 1, 1)), substring(s, 2), collapse = " ", sep = "")
}
# ================================================================================
# 3. UI
# ================================================================================

ui <- navbarPage(
  title = strong("NeoSepPulmoExplorer"),
  theme = shinytheme("cerulean"),
  id = "main_nav",
  header = tagList(
    useShinyjs(),
    tags$head(
      tags$link(rel = "icon", href = "https://pharmacoomics.com/wp-content/uploads/2025/08/cropped-TWU000008-32x32.png"),
      tags$style(HTML("
        .row-flex { display: flex; flex-wrap: wrap; }
        .row-flex > div[data-display-if] { display: contents; }
        .col-flex { display: flex; flex-direction: column; }
        .step-box-stretch { flex-grow: 1; display: flex; flex-direction: column; }
        .step-box { background: #fff; border: 1px solid #ddd; border-radius: 5px; padding: 15px; margin-bottom: 20px; box-shadow: 0 1px 1px rgba(0,0,0,.05); }
        .footer {
          background-color: #f5f5f5;
          padding: 20px;
          text-align: center;
          border-top: 1px solid #dddddd;
          margin-top: 50px;
          font-size: 14px;
          color: #555;
          font-weight: normal;
          width: 100%;
        }

        .footer h4 {
          color: #555;
          font-weight: normal;
        }

        .footer a {
          color: #2FA4E7;
          text-decoration: none;
          font-weight: normal;
          transition: color 0.3s ease;
        }

        .footer a:hover {
          color: #1d6fa5;
          text-decoration: underline;
        }

        #landing-page {
          position: fixed; top: 0; left: 0; width: 100%; height: 100%;
          background: linear-gradient(135deg, #2c3e50 0%, #4ca1af 100%);
          z-index: 10000; display: flex; flex-direction: column; align-items: center; justify-content: center;
          color: white; text-align: center;
        }

        #landing-content {
          background: rgba(255, 255, 255, 0.9); padding: 50px; border-radius: 15px;
          box-shadow: 0 10px 25px rgba(0,0,0,0.2); color: #2c3e50; max-width: 600px;
        }
        .landing-title { font-size: 3em; font-weight: bold; margin-bottom: 10px; color: #2c3e50; }
        .landing-subtitle { font-size: 1.2em; margin-bottom: 30px; color: #555; }
        .btn-start { font-size: 1.5em; padding: 15px 40px; border-radius: 50px; transition: transform 0.2s; }
        .btn-start:hover { transform: scale(1.05); }

  .tab1-layout {
    display: flex;
    flex-wrap: wrap;
  }


  .tab1-layout > [class*='col-'] {
    display: flex;
    flex-direction: column;
  }


  .left-col-stack {
    display: flex;
    flex-direction: column;
    flex: 1;
    width: 100%;
  }


  .box-top {
    flex: 0 0 auto;
    width: 100%;
    margin-bottom: 20px;
  }


  .box-bottom-grow {
    flex: 1 1 auto;
    display: flex;
    flex-direction: column;
    justify-content: center;
    width: 100%;
    margin-bottom: 20px;
  }


  .box-right-full {
    flex: 1 1 auto;
    display: flex;
    flex-direction: column;
    width: 100%;
    margin-bottom: 20px;
  }


  .scroll-content {
    flex: 1;
    overflow-y: auto;
    min-height: 0;
  }
      "))
    ),
    div(
      id = "landing-page",
      div(
        id = "landing-content",
        img(src = "https://static.wixstatic.com/media/3e651e_747995cb050647038344ae7b72b6b481~mv2.png", height = "80px", style = "margin-bottom: 20px;"),
        h1("NeoSepPulmoExplorer", class = "landing-title"),
        p("Interactive Transcriptomics Analysis & Visualization", class = "landing-subtitle"),
        p("Pharmaco-Omics Lab & Cellular and Molecular Pediatrics Lab", style = "font-size: 0.9em; color: #777; margin-bottom: 30px;"),
        actionButton("btn_enter_app", "Click here to start", icon = icon("rocket"), class = "btn-primary btn-start")
      )
    )
  ),

  # ------------------------------------------------------------------------------
  # TAB 1: DATA MANAGEMENT (optional)
  # ------------------------------------------------------------------------------
  tabPanel("1. Manage Data (Optional)",
    icon = icon("database"),
    fluidPage(
      br(),
      h2("Step 1: Manage Data (Optional)", class = "section-title"),
      helpText("You can skip this step and explore our data immediately."),
      fluidRow(
        class = "tab1-layout",
        column(
          width = 5,
          div(
            class = "left-col-stack",
            div(
              class = "step-box box-top",
              h4(icon("upload"), "Base Data (Optional)"),
              helpText("Upload to override default Lung Transcriptomics data. The app currently accepts *.csv files."),
              hr(),
              fileInput("up_meta", "Metadata CSV", width = "100%", placeholder = "Select Metadata..."),
              fileInput("up_norm", "Normalized Data CSV", width = "100%", placeholder = "Select Counts..."),
              fileInput("up_ensembl", "Ensembl ID Mapping CSV", width = "100%", placeholder = "Select ID Map...")
            ),
            div(
              class = "step-box box-bottom-grow",
              h4(icon("cloud-download-alt"), "Download Example Data"),
              helpText("Download the example dataset to view the required format."),
              br(),
              downloadButton("btn_dl_example", "Download Example Data (.zip)",
                class = "btn-success btn-block", icon = icon("download")
              )
            )
          )
        ),
        column(
          width = 7,
          div(
            class = "step-box box-right-full",
            h4(icon("sync-alt"), "Update Comparisons (Optional)"),
            helpText("Select a comparison below and upload new files to update. The app currently accepts *.csv files."),
            selectInput("comp_to_update", "Select Comparison to Update:",
              choices = names(default_fdr_map), width = "100%"
            ),
            fluidRow(
              column(6, fileInput("update_fdr", "New DEA File")),
              column(6, fileInput("update_gsea", "New GSEA File"))
            ),
            actionButton("do_update_comp", "Update Selected Comparison", icon = icon("save"), class = "btn-primary btn-block"),
            hr(),
            h5("Current Status:"),
            div(
              class = "scroll-content",
              DTOutput("comp_status_table")
            )
          )
        )
      )
    )
  ),

  # ------------------------------------------------------------------------------
  # TAB 2: ANALYSIS SETTINGS
  # ------------------------------------------------------------------------------
  tabPanel("2. Configure Analysis",
    icon = icon("cogs"),
    fluidPage(
      br(),
      h2("Step 2: Configure Analysis", class = "section-title"),
      helpText("Configure your analysis settings below and generate the heatmap."),
      div(
        class = "step-box", style = "text-align: center; background-color: #f7f7f7;",
        h4("Choose Analysis Mode"),
        radioGroupButtons(
          inputId = "analysis_mode", label = NULL,
          choices = c("Pathway Analysis" = "pathway", "Manual Gene List" = "manual"),
          justified = TRUE, status = "primary", checkIcon = list(yes = icon("check"))
        )
      ),
      fluidRow(
        class = "row-flex",
        conditionalPanel(
          condition = "input.analysis_mode == 'pathway'",
          column(3,
            class = "col-flex",
            div(
              class = "step-box step-box-stretch",
              h4(icon("bezier-curve"), "Pathway Selection"),
              helpText("Select pathways to visualize on the heatmap."),
              uiOutput("gsea_source_ui"),
              selectizeInput("pathway1", "Pathway 1 (Required):", choices = NULL, options = list(placeholder = "Loading...")),
              selectizeInput("pathway2", "Pathway 2 (Optional):", choices = NULL, options = list(placeholder = "None"))
            )
          ),
          column(3,
            class = "col-flex",
            div(
              class = "step-box step-box-stretch",
              h4(icon("dna"), "Gene Filtering"),
              helpText("Filter genes from the selected pathways."),
              checkboxInput("apply_fdr_filter", strong("Filter by FDR < 0.05"), value = TRUE),
              helpText("Uncheck this if you want to see ALL genes in the pathway."),
              conditionalPanel(
                condition = "input.pathway2 != ''",
                radioButtons("gene_view_mode", "View Mode:", choices = c("Unified List" = "unified", "Split Tables" = "split"), inline = TRUE)
              ),
              uiOutput("gene_selector_ui")
            )
          )
        ),
        conditionalPanel(
          condition = "input.analysis_mode == 'manual'",
          column(3,
            class = "col-flex",
            div(
              class = "step-box step-box-stretch",
              h4(icon("project-diagram"), "Option A: Import from Pathway"),
              helpText("Select a pathway to add genes."),
              uiOutput("manual_gsea_source_ui"),
              selectizeInput("manual_pathway_search", "Select Pathway:",
                choices = NULL,
                options = list(placeholder = "Type to search pathway...")
              ),
              uiOutput("manual_pathway_genes_ui"),
              br(),
              actionButton("btn_add_from_pathway", "Add Selected to List",
                icon = icon("arrow-right"), class = "btn-primary btn-block"
              )
            )
          ),
          column(3,
            class = "col-flex",
            div(
              class = "step-box step-box-stretch",
              h4(icon("keyboard"), "Option B: Type, Paste or Upload"),
              helpText("Type genes, paste a list, or upload a file (.txt) with list of genes."),
              fileInput("manual_file_upload", "Upload Gene List (.txt/.csv):",
                multiple = FALSE,
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv", ".txt")
              ),
              selectizeInput("manual_genes_search", "Type Gene (Auto-suggest):",
                choices = NULL, multiple = TRUE, width = "100%",
                options = list(placeholder = "e.g., TP53", create = TRUE, maxItems = 1)
              ),
              hr(),
              textAreaInput("manual_paste_area", "Final Gene List:",
                height = "200px", placeholder = "List of genes will appear here...", resize = "vertical"
              ),
              div(
                style = "text-align:right; margin-top:5px;",
                actionButton("btn_clear_manual", "Clear List", icon = icon("trash"), class = "btn-xs btn-danger")
              )
            )
          )
        ),
        column(3,
          class = "col-flex",
          div(
            class = "step-box step-box-stretch",
            h4(icon("list-ul"), "Select Samples & Comparisons"),
            helpText("1. Choose groups to display:"),
            checkboxGroupInput("selected_groups",
              label = NULL,
              choices = setNames(names(level2_display_map), level2_display_map),
              selected = names(level2_display_map),
              inline = FALSE
            ),
            hr(),
            helpText("2. Choose comparisons to display FDR:"),
            uiOutput("comparisons_ui")
          )
        ),
        column(3,
          class = "col-flex",
          div(
            class = "step-box step-box-stretch",
            h4(icon("palette"), "Aesthetic Settings"),
            helpText("Customize plot appearance:"),
            br(),
            switchInput(
              inputId = "show_legend",
              label = "Show Legend",
              value = TRUE,
              onLabel = "ON",
              offLabel = "OFF",
              onStatus = "primary"
            ),
            helpText("Toggle to show or hide all legends (Groups, FDR, Z-score)."),
            hr(),
            sliderInput("aes_base_size", "Global Base Size:", min = 8, max = 24, value = 13, step = 1),
            selectInput("aes_elem_select", "Select Element to Customize:",
              choices = c(
                "Gene Names (X/Y)", "Main Title", "Split Titles (Rows)",
                "Annotation Labels", "Legend Titles", "Legend Labels", "Significance Marks"
              )
            ),
            sliderInput("aes_elem_size", "Current Element Size:", min = 1, max = 30, value = 10, step = 1),
            actionButton("aes_apply_size", "Save Custom Size (Offset)", icon = icon("check"), class = "btn-info btn-block btn-sm")
          )
        )
      ),
      br(),
      fluidRow(
        column(4,
          offset = 4,
          div(
            class = "step-box", style = "background-color: #f0f9ff; border-color: #bce8f1; text-align: center; padding: 20px;",
            h4(icon("check-circle"), "Ready to Plot?"),
            actionButton("run_plot", "Generate Heatmap", icon = icon("play"), class = "btn-primary btn-block btn-lg")
          )
        )
      )
    )
  ),

  # ------------------------------------------------------------------------------
  # TAB 3: VISUALIZATION
  # ------------------------------------------------------------------------------
  tabPanel("3. Generate Plot",
    icon = icon("chart-bar"),
    sidebarLayout(
      sidebarPanel(
        width = 3,
        h4(icon("history"), "Result History"),
        selectInput("history_selector", "Select Result:", choices = NULL, width = "100%"),
        helpText("Select a generated heatmap to view or export."),
        hr(),
        h4(icon("download"), "Export"),
        selectInput("export_dpi", "Export Resolution:", choices = c("300 DPI" = 300, "600 DPI" = 600), selected = 600),
        helpText("Export at 300 DPI for standard use or 600 DPI for publication-quality graphics."),
        hr(),
        fluidRow(
          column(6, downloadButton("dl_tiff", "TIFF", icon = icon("layer-group"), class = "btn-primary btn-block", style = "margin-bottom: 10px;")),
          column(6, downloadButton("dl_pdf", "PDF", icon = icon("file-pdf"), class = "btn-danger btn-block", style = "margin-bottom: 10px;"))
        ),
        fluidRow(
          column(6, downloadButton("dl_png", "PNG", icon = icon("camera-retro"), class = "btn-success btn-block", style = "margin-bottom: 10px;")),
          column(6, downloadButton("dl_jpeg", "JPEG", icon = icon("file-image"), class = "btn-warning btn-block", style = "margin-bottom: 10px;"))
        ),
        br(), hr(),
        actionButton("back_to_settings", "Back to Settings", icon = icon("arrow-left"), class = "btn-default btn-block", onclick = "$('a[data-value=\"2. Configure Analysis\"]').tab('show');")
      ),
      mainPanel(
        width = 9,
        tabsetPanel(
          tabPanel("Heatmap",
            icon = icon("th"),
            br(),
            div(
              style = "height: 800px; overflow-y: auto; overflow-x: auto; border: 1px solid #ddd; padding: 5px;",
              plotOutput("heatmap_plot", height = "auto", width = "auto")
            ),
            br(),
            uiOutput("missing_genes_alert")
          )
        )
      )
    )
  ),
  footer = tags$footer(
    class = "footer",
    HTML("
      <h4><a href='https://github.com/Pharmaco-OmicsLab' target='_blank'>Pharmaco-Omics Lab </a> in collaboration with <a href='https://ivh.ku.dk/english/research/comparative-pediatrics-and-nutrition/cellular-and-molecular-pediatrics/' target='_blank'>Cellular and Molecular Pediatrics Lab</a></h4>
      <p>Graduate Institute of Biomedical Sciences, College of Medicine, Chang Gung University, Taiwan</p>
      <p>Contact: <a href='mailto:pharmacoomicslab@gmail.com'>Dat Le</a> (Main developer) or <a href='mailto:pharmacoomicslab@gmail.com'>Nguyen Phuoc Long</a> (PI)</p>
    ")
  )
)

# ================================================================================
# 4. SERVER
# ================================================================================

server <- function(input, output, session) {
  observeEvent(input$btn_enter_app, {
    shinyjs::hide(id = "landing-page", anim = TRUE, animType = "fade", time = 0.5)
  })

  rv <- reactiveValues(
    fdr_map = default_fdr_map,
    gsea_map = default_gsea_map,
    code_map = default_codes,
    meta_path = default_meta,
    norm_path = default_norm,
    ensembl_path = default_ensembl,
    history = list(),
    aes_offsets = list(
      "Gene Names (X/Y)" = -3,
      "Main Title" = 2,
      "Split Titles (Rows)" = 1,
      "Annotation Labels" = 0,
      "Legend Titles" = 1,
      "Legend Labels" = 0,
      "Significance Marks" = -6
    )
  )

  # ----------------------------------------------------------------------------
  # Aesthetic settings
  # ----------------------------------------------------------------------------

  observeEvent(c(input$aes_elem_select, input$aes_base_size), {
    req(input$aes_elem_select, input$aes_base_size)
    elem <- input$aes_elem_select
    offset <- rv$aes_offsets[[elem]]
    if (is.null(offset)) offset <- 0

    current_val <- input$aes_base_size + offset
    updateSliderInput(session, "aes_elem_size", value = current_val)
  })

  observeEvent(input$aes_apply_size, {
    req(input$aes_elem_select, input$aes_elem_size, input$aes_base_size)
    elem <- input$aes_elem_select
    new_val <- input$aes_elem_size
    base <- input$aes_base_size

    rv$aes_offsets[[elem]] <- new_val - base
    showNotification(paste("Saved size for", elem, ":", new_val, "(Offset:", new_val - base, ")"), type = "message")
  })

  # ----------------------------------------------------------------------------
  # Tab 1: data management
  # ----------------------------------------------------------------------------

  observe({
    if (!is.null(input$up_meta)) rv$meta_path <- input$up_meta$datapath
    if (!is.null(input$up_norm)) rv$norm_path <- input$up_norm$datapath
    if (!is.null(input$up_ensembl)) rv$ensembl_path <- input$up_ensembl$datapath
  })

  observeEvent(input$do_update_comp, {
    req(input$comp_to_update)
    target <- input$comp_to_update
    msg <- c()
    if (!is.null(input$update_fdr)) {
      rv$fdr_map[[target]] <- input$update_fdr$datapath
      msg <- c(msg, "FDR")
    }
    if (!is.null(input$update_gsea)) {
      rv$gsea_map[[target]] <- input$update_gsea$datapath
      msg <- c(msg, "GSEA")
    }
    if (length(msg) > 0) {
      showNotification(paste("Updated", paste(msg, collapse = " & "), "for:", target), type = "message")
    } else {
      showNotification("No file selected!", type = "warning")
    }
  })

  output$comp_status_table <- renderDT({
    df <- data.frame(
      Comparison = names(rv$fdr_map),
      FDR_Source = ifelse(unlist(rv$fdr_map) %in% unlist(default_fdr_map), "Default", "Updated"),
      GSEA_Source = ifelse(unlist(rv$gsea_map) %in% unlist(default_gsea_map), "Default", "Updated"),
      stringsAsFactors = FALSE
    )
    datatable(df, selection = "none", rownames = FALSE, options = list(dom = "t", paging = FALSE, ordering = FALSE))
  })

  # ----------------------------------------------------------------------------
  # Dynamic selectors
  # ----------------------------------------------------------------------------

  output$gsea_source_ui <- renderUI({
    choices <- c("All Comparisons", names(rv$gsea_map))
    selectInput("gsea_source", "GSEA Source:", choices = choices)
  })

  output$comparisons_ui <- renderUI({
    choices <- names(rv$fdr_map)
    checkboxGroupInput("selected_comparisons", label = NULL, choices = choices, selected = choices)
  })

  # ----------------------------------------------------------------------------
  # Base data
  # ----------------------------------------------------------------------------
  static_data <- reactive({
    validate(need(file.exists(rv$meta_path), "Metadata missing"))
    validate(need(file.exists(rv$norm_path), "NormData missing"))
    validate(need(file.exists(rv$ensembl_path), "Ensembl missing"))

    meta <- read.csv(rv$meta_path) %>%
      select(Sample = RNAseq_ID, GroupLevel1 = SE_Control, GroupLevel2 = Correct_Group) %>%
      mutate(Sample = gsub("lung_", "Lung_", Sample), GroupLevel1 = level1_display_names[GroupLevel1], GroupLevel2 = gsub(" ", "", GroupLevel2))
    ens <- read.csv(rv$ensembl_path) %>%
      select(EntrezID, Gene.symbol) %>%
      distinct(EntrezID, .keep_all = TRUE) %>%
      mutate(EntrezID = as.character(EntrezID))
    norm <- read.csv(rv$norm_path, row.names = 1, check.names = FALSE) %>% rownames_to_column(var = "EntrezID")

    available_entrez <- norm$EntrezID
    valid_genes_df <- ens %>% filter(EntrezID %in% available_entrez)
    all_genes <- unique(valid_genes_df$Gene.symbol)
    all_genes <- all_genes[all_genes != ""]
    list(meta = meta, ens = ens, norm = norm, all_genes = all_genes)
  })

  observe({
    req(static_data())
    static <- static_data()
    updateSelectizeInput(session, "manual_genes_search", choices = static$all_genes, server = TRUE, options = list(placeholder = "Type gene symbol...", create = TRUE, persist = FALSE, maxItems = 1))
  })

  # ----------------------------------------------------------------------------
  # Pathway mode
  # ----------------------------------------------------------------------------

  gsea_data_reactive <- reactive({
    req(input$gsea_source)

    if (input$gsea_source != "All Comparisons") {
      if (!(input$gsea_source %in% names(rv$gsea_map))) {
        return(NULL)
      }
      fpath <- rv$gsea_map[[input$gsea_source]]
      validate(need(file.exists(fpath), "GSEA file missing"))

      return(read.csv(fpath) %>% mutate(DisplayLabel = paste0(Description, " (NES: ", round(NES, 2), ")")))
    }

    list_dfs <- lapply(names(rv$gsea_map), function(comp_name) {
      fpath <- rv$gsea_map[[comp_name]]
      if (file.exists(fpath)) {
        df <- read.csv(fpath)
        return(df %>% select(Description, core_enrichment))
      } else {
        return(NULL)
      }
    })

    big_df <- do.call(rbind, list_dfs)
    validate(need(nrow(big_df) > 0, "No GSEA data found in any files."))

    # Helper: merge core enrichment strings.
    merge_enrichment <- function(enrich_strs) {
      all_genes <- unlist(strsplit(as.character(enrich_strs), "[,/]"))
      all_genes <- trimws(all_genes)
      all_genes <- unique(all_genes[all_genes != ""])
      paste(all_genes, collapse = "/")
    }

    final_df <- big_df %>%
      group_by(Description) %>%
      summarise(
        core_enrichment = merge_enrichment(core_enrichment),
        .groups = "drop"
      ) %>%
      mutate(DisplayLabel = paste0(Description))

    return(final_df)
  })

  observeEvent(gsea_data_reactive(), {
    gsea_df <- gsea_data_reactive()
    updateSelectizeInput(session, "pathway1", choices = gsea_df$DisplayLabel, server = TRUE)
    updateSelectizeInput(session, "pathway2", choices = c("", gsea_df$DisplayLabel), server = TRUE)
  })

  # Helper: format gene picker choices.
  format_choices <- function(entrez_ids, ens_df) {
    if (length(entrez_ids) == 0) {
      return(NULL)
    }
    gene_map <- ens_df %>%
      filter(EntrezID %in% entrez_ids) %>%
      select(EntrezID, Gene.symbol)
    setNames(gene_map$EntrezID, paste0(gene_map$Gene.symbol, " (", gene_map$EntrezID, ")"))
  }

  output$gene_selector_ui <- renderUI({
    req(input$pathway1)
    req(input$gsea_source)

    gsea_df <- gsea_data_reactive()
    ens_df <- static_data()$ens

    sig_genes_entrez <- NULL

    if (isTRUE(input$apply_fdr_filter)) {
      if (input$gsea_source == "All Comparisons") {
        all_sig_genes <- c()
        for (fpath in rv$fdr_map) {
          if (file.exists(fpath)) {
            dea_tmp <- read.csv(fpath, row.names = 1)
            col_fdr <- if ("adj.P.Val" %in% colnames(dea_tmp)) "adj.P.Val" else "FDR"
            if (col_fdr %in% colnames(dea_tmp)) {
              sigs <- rownames(dea_tmp)[dea_tmp[[col_fdr]] < 0.05]
              all_sig_genes <- c(all_sig_genes, sigs)
            }
          }
        }
        sig_genes_entrez <- unique(all_sig_genes)
      } else {
        fdr_path <- rv$fdr_map[[input$gsea_source]]
        validate(need(file.exists(fdr_path), "DEA/FDR file not found."))
        dea_data <- read.csv(fdr_path, row.names = 1)
        if ("adj.P.Val" %in% colnames(dea_data)) {
          sig_genes_entrez <- rownames(dea_data)[dea_data$adj.P.Val < 0.05]
        } else {
          sig_genes_entrez <- rownames(dea_data)
        }
      }
    }

    # Helper: get pathway genes after optional filtering.
    get_pathway_genes <- function(enrich_str, sig_list) {
      raw <- trimws(unlist(strsplit(as.character(enrich_str), "[,/]")))
      if (!is.null(sig_list)) {
        return(intersect(raw, sig_list))
      } else {
        return(raw)
      }
    }

    p1_row <- gsea_df %>%
      filter(DisplayLabel == input$pathway1) %>%
      slice(1)
    genes_p1 <- get_pathway_genes(p1_row$core_enrichment, sig_genes_entrez)

    if (length(genes_p1) == 0) {
      msg <- if (isTRUE(input$apply_fdr_filter)) "No genes passed FDR filter." else "No genes found."
      return(div(class = "alert alert-warning", msg))
    }

    mode_dual <- (input$pathway2 != "")

    if (!mode_dual) {
      count_p1 <- length(genes_p1)
      prefix <- if (isTRUE(input$apply_fdr_filter)) "Significant genes" else "All genes"
      label_txt <- paste0(prefix, " in ", p1_row$Description, " (", count_p1, ")")

      c_p1 <- format_choices(genes_p1, ens_df)
      return(pickerInput("sel_genes_unified", label_txt,
        choices = c_p1, selected = c_p1, multiple = TRUE,
        options = list(
          `actions-box` = TRUE, `live-search` = TRUE,
          `selected-text-format` = "count > 1",
          `count-selected-text` = "{0} genes selected"
        ), width = "100%"
      ))
    }

    p2_row <- gsea_df %>%
      filter(DisplayLabel == input$pathway2) %>%
      slice(1)
    genes_p2 <- get_pathway_genes(p2_row$core_enrichment, sig_genes_entrez)

    if (input$gene_view_mode == "unified") {
      all_sig <- unique(c(genes_p1, genes_p2))

      count_all <- length(all_sig)
      if (isTRUE(input$apply_fdr_filter)) {
        label_txt <- paste0("Significant genes merged from 2 pathways (FDR<0.05): ", count_all)
      } else {
        label_txt <- paste0("All genes merged from 2 pathways: ", count_all)
      }

      c_all <- format_choices(all_sig, ens_df)
      return(pickerInput("sel_genes_unified", label_txt,
        choices = c_all, selected = c_all, multiple = TRUE,
        options = list(`actions-box` = TRUE, `live-search` = TRUE, `selected-text-format` = "count > 1", `count-selected-text` = "{0} genes selected"), width = "100%"
      ))
    }

    genes_both <- intersect(genes_p1, genes_p2)
    genes_p1_only <- setdiff(genes_p1, genes_both)
    genes_p2_only <- setdiff(genes_p2, genes_both)

    ui_list <- list()
    col_w <- if (length(genes_both) > 0) 4 else 6

    if (length(genes_p1_only) > 0) {
      lbl <- paste0(p1_row$Description, " (", length(genes_p1_only), ")")
      ui_list[[length(ui_list) + 1]] <- column(col_w, pickerInput("sel_genes_p1", lbl, choices = format_choices(genes_p1_only, ens_df), selected = format_choices(genes_p1_only, ens_df), multiple = TRUE, width = "100%", options = list(`actions-box` = TRUE, `live-search` = TRUE, `selected-text-format` = "count > 1", `count-selected-text` = "{0} genes")))
    }

    if (length(genes_both) > 0) {
      lbl <- paste0("Overlap (", length(genes_both), ")")
      ui_list[[length(ui_list) + 1]] <- column(col_w, pickerInput("sel_genes_both", lbl, choices = format_choices(genes_both, ens_df), selected = format_choices(genes_both, ens_df), multiple = TRUE, width = "100%", options = list(`actions-box` = TRUE, `live-search` = TRUE, `selected-text-format` = "count > 1", `count-selected-text` = "{0} genes")))
    }

    if (length(genes_p2_only) > 0) {
      lbl <- paste0(p2_row$Description, " (Unique: ", length(genes_p2_only), ")")
      ui_list[[length(ui_list) + 1]] <- column(col_w, pickerInput("sel_genes_p2", lbl, choices = format_choices(genes_p2_only, ens_df), selected = format_choices(genes_p2_only, ens_df), multiple = TRUE, width = "100%", options = list(`actions-box` = TRUE, `live-search` = TRUE, `selected-text-format` = "count > 1", `count-selected-text` = "{0} genes")))
    }

    tagList(fluidRow(ui_list))
  })

  # ----------------------------------------------------------------------------
  # Manual mode
  # ----------------------------------------------------------------------------

  observeEvent(input$manual_genes_search, {
    new_val <- input$manual_genes_search
    req(new_val)
    current_text <- input$manual_paste_area
    if (is.null(current_text)) current_text <- ""
    updated_text <- if (current_text == "") new_val else paste(current_text, new_val, sep = "\n")
    updateTextAreaInput(session, "manual_paste_area", value = updated_text)
    static <- static_data()
    updateSelectizeInput(session, "manual_genes_search", selected = character(0), choices = static$all_genes, server = TRUE)
  })

  output$manual_gsea_source_ui <- renderUI({
    choices <- names(rv$gsea_map)
    selectInput("manual_gsea_source", "Source Comparison:", choices = choices)
  })

  manual_gsea_data <- reactive({
    req(input$manual_gsea_source)
    if (!(input$manual_gsea_source %in% names(rv$gsea_map))) {
      return(NULL)
    }
    fpath <- rv$gsea_map[[input$manual_gsea_source]]
    validate(need(file.exists(fpath), "GSEA file missing"))
    read.csv(fpath) %>% mutate(DisplayLabel = paste0(Description, " (NES: ", round(NES, 2), ")"))
  })

  observeEvent(manual_gsea_data(), {
    gsea_df <- manual_gsea_data()
    updateSelectizeInput(session, "manual_pathway_search", choices = gsea_df$DisplayLabel, server = TRUE)
  })

  output$manual_pathway_genes_ui <- renderUI({
    req(input$manual_pathway_search)
    gsea_df <- manual_gsea_data()
    req(gsea_df)

    p_row <- gsea_df %>%
      filter(DisplayLabel == input$manual_pathway_search) %>%
      slice(1)
    req(nrow(p_row) > 0)

    genes_entrez <- trimws(unlist(strsplit(as.character(p_row$core_enrichment), "[,/]")))

    ens_df <- static_data()$ens
    gene_map <- ens_df %>%
      filter(EntrezID %in% genes_entrez) %>%
      select(EntrezID, Gene.symbol)

    choices_vec <- setNames(gene_map$Gene.symbol, paste0(gene_map$Gene.symbol, " (", gene_map$EntrezID, ")"))

    pickerInput("manual_pathway_genes_selection",
      label = paste0("Genes in ", p_row$Description),
      choices = choices_vec,
      selected = choices_vec,
      multiple = TRUE,
      options = list(
        `actions-box` = TRUE,
        `live-search` = TRUE,
        `selected-text-format` = "count > 3",
        `count-selected-text` = "{0} genes selected"
      ),
      width = "100%"
    )
  })

  observeEvent(input$btn_add_from_pathway, {
    new_genes <- input$manual_pathway_genes_selection
    if (is.null(new_genes) || length(new_genes) == 0) {
      showNotification("No genes selected to add!", type = "warning")
      return()
    }

    current_text <- input$manual_paste_area
    if (is.null(current_text)) current_text <- ""

    old_genes <- unique(trimws(unlist(strsplit(current_text, "\n"))))
    old_genes <- old_genes[old_genes != ""]

    combined_genes <- unique(c(old_genes, new_genes))

    updateTextAreaInput(session, "manual_paste_area", value = paste(combined_genes, collapse = "\n"))

    showNotification(paste("Added", length(new_genes), "genes from pathway."), type = "message")
  })

  observeEvent(input$btn_clear_manual, {
    updateTextAreaInput(session, "manual_paste_area", value = "")
  })

  # ----------------------------------------------------------------------------
  # Heatmap generation
  # ----------------------------------------------------------------------------
  observeEvent(input$run_plot, {
    req(input$selected_comparisons)
    static <- static_data()
    ens_df <- static$ens
    genes_sel_entrez <- c()
    plot_title <- ""
    missing_genes_list <- NULL
    p1_name <- ""
    p2_name <- NULL

    if (input$analysis_mode == "pathway") {
      req(input$pathway1)
      mode_dual <- (input$pathway2 != "")
      if (!mode_dual) {
        genes_sel_entrez <- input$sel_genes_unified
        if (length(genes_sel_entrez) == 0) {
          showNotification("No genes selected!", type = "error")
          return()
        }
        gsea_df <- gsea_data_reactive()
        p1_row <- gsea_df %>%
          filter(DisplayLabel == input$pathway1) %>%
          slice(1)
        p1_name <- p1_row$Description
        plot_title <- paste("DEGs in", to_title_case(p1_name))
      } else {
        if (input$gene_view_mode == "unified") genes_sel_entrez <- input$sel_genes_unified else genes_sel_entrez <- c(input$sel_genes_p1, input$sel_genes_both, input$sel_genes_p2)
        if (length(genes_sel_entrez) == 0) {
          showNotification("No genes selected!", type = "error")
          return()
        }
        gsea_df <- gsea_data_reactive()
        p1_row <- gsea_df %>%
          filter(DisplayLabel == input$pathway1) %>%
          slice(1)
        p2_row <- gsea_df %>%
          filter(DisplayLabel == input$pathway2) %>%
          slice(1)
        p1_name <- p1_row$Description
        p2_name <- p2_row$Description
        plot_title <- paste("DEGs in", to_title_case(p1_name), "and", to_title_case(p2_name))
      }
    } else {
      raw_text <- input$manual_paste_area
      if (is.null(raw_text) || raw_text == "") {
        showNotification("Please enter at least one gene!", type = "error")
        return()
      }
      raw_genes <- unique(trimws(unlist(strsplit(raw_text, "\n"))))
      raw_genes <- raw_genes[raw_genes != ""]
      if (length(raw_genes) == 0) {
        showNotification("List is empty!", type = "error")
        return()
      }
      mapped_genes <- ens_df %>% filter(Gene.symbol %in% raw_genes)
      available_in_norm <- static$norm$EntrezID
      valid_genes_df <- mapped_genes %>% filter(EntrezID %in% available_in_norm)
      genes_sel_entrez <- valid_genes_df$EntrezID
      missing_genes_list <- setdiff(raw_genes, valid_genes_df$Gene.symbol)
      if (length(genes_sel_entrez) == 0) {
        showNotification("None of the entered genes exist in the NORMALIZED data!", type = "error")
        return()
      }
      p1_name <- "Manual Gene List"
      plot_title <- "Heatmap of Selected Genes"
    }

    plot_data <- static$norm %>%
      filter(EntrezID %in% genes_sel_entrez) %>%
      left_join(static$ens, by = "EntrezID") %>%
      mutate(Features = ifelse(is.na(Gene.symbol) | Gene.symbol == "", EntrezID, Gene.symbol))

    if (input$analysis_mode == "pathway" && !is.null(p2_name)) {
      gsea_df <- gsea_data_reactive()
      p1_r <- gsea_df %>%
        filter(DisplayLabel == input$pathway1) %>%
        slice(1)
      g1 <- trimws(unlist(strsplit(as.character(p1_r$core_enrichment), "[,/]")))
      p2_r <- gsea_df %>%
        filter(DisplayLabel == input$pathway2) %>%
        slice(1)
      g2 <- trimws(unlist(strsplit(as.character(p2_r$core_enrichment), "[,/]")))
      both <- intersect(g1, g2)
      plot_data <- plot_data %>% mutate(Pathway = case_when(EntrezID %in% both ~ "Mutual genes", EntrezID %in% g1 ~ p1_name, EntrezID %in% g2 ~ p2_name, TRUE ~ "Other"))
      lvls <- c(setdiff(unique(plot_data$Pathway), "Mutual genes"), "Mutual genes")
      lvls <- lvls[lvls %in% unique(plot_data$Pathway)]
      plot_data <- plot_data %>% arrange(factor(Pathway, levels = lvls))
      split_row <- factor(plot_data$Pathway, levels = lvls)
    } else {
      plot_data <- plot_data %>%
        mutate(Pathway = p1_name) %>%
        arrange(Features)
      split_row <- NULL
    }

    fdr_info <- list()
    idx <- 1
    for (cname in input$selected_comparisons) {
      if (cname %in% names(rv$fdr_map)) {
        fpath <- rv$fdr_map[[cname]]
        if (file.exists(fpath)) {
          tmp <- read.csv(fpath, row.names = 1) %>%
            rownames_to_column("EntrezID") %>%
            select(EntrezID, adj.P.Val) %>%
            mutate(EntrezID = as.character(EntrezID))
          col_id <- paste0("FDR_", idx)
          tmp <- tmp %>% rename(!!col_id := adj.P.Val)
          plot_data <- plot_data %>% left_join(tmp, by = "EntrezID")
          fdr_info[[col_id]] <- cname
          idx <- idx + 1
        }
      }
    }

    req(input$selected_groups)

    meta_processed <- static$meta %>%
      mutate(GroupLevel2_Display = level2_display_map[GroupLevel2])

    meta_sub <- meta_processed %>%
      filter(GroupLevel2 %in% input$selected_groups) %>%
      arrange(
        factor(GroupLevel1, levels = c("Control", "Infection")),
        factor(GroupLevel2_Display, levels = names(level2_colors))
      )

    if (nrow(meta_sub) == 0) {
      showNotification("No samples found for the selected groups!", type = "error")
      return()
    }

    valid_samples <- intersect(names(plot_data), meta_sub$Sample)
    if (length(valid_samples) == 0) {
      showNotification("Error: No matching samples in data.", type = "error")
      return()
    }

    meta_sub <- meta_sub %>% filter(Sample %in% valid_samples)
    mat_expr <- plot_data %>% select(all_of(meta_sub$Sample))
    mat_scaled <- t(scale(t(mat_expr)))

    active_colors <- level2_colors[names(level2_colors) %in% unique(meta_sub$GroupLevel2_Display)]

    # Helper: resolve plot font sizes.
    fs <- function(key) {
      base <- input$aes_base_size
      off <- rv$aes_offsets[[key]]
      if (is.null(off)) off <- 0
      return(base + off)
    }

    ha_col <- HeatmapAnnotation(
      Group = anno_simple(as.character(meta_sub$GroupLevel2_Display), col = active_colors),
      show_annotation_name = FALSE,
      annotation_name_side = "left"
    )

    fdr_colors <- colorRamp2(c(0, 1.3, 10), c("#CAB2D6", "white", "#FDBF6F"))
    fdr_annos <- list()
    for (fc in names(fdr_info)) {
      pv <- plot_data[[fc]]
      idx_s <- gsub("FDR_", "", fc)
      fdr_annos[[idx_s]] <- anno_simple(safe_neglog10(pv), col = fdr_colors, pch = ifelse(pv < 0.05, "*", "ns"), pt_gp = gpar(fontsize = fs("Significance Marks") + 3, col = "black"), pt_size = unit(fs("Significance Marks"), "pt"), which = "row")
    }
    ha_left <- do.call(rowAnnotation, c(fdr_annos, list(annotation_name_gp = gpar(fontsize = fs("Annotation Labels")), annotation_name_side = "top", annotation_name_rot = 0, show_annotation_name = TRUE)))

    ha_right <- rowAnnotation(Genes = anno_text(plot_data$Features, which = "row", gp = gpar(fontsize = fs("Gene Names (X/Y)"), fontface = "italic")))

    lgd_grp <- Legend(title = "Groups", legend_gp = gpar(fill = active_colors), labels = names(active_colors), title_gp = gpar(fontsize = fs("Legend Titles"), fontface = "bold"), labels_gp = gpar(fontsize = fs("Legend Labels")), ncol = 1)

    lgd_fdr <- Legend(title = "FDR", col_fun = colorRamp2(c(1, 0.5, 0), c("#CAB2D6", "white", "#FDBF6F")), at = c(1, 0), labels = c("FDR=1", "FDR=0"), title_gp = gpar(fontsize = fs("Legend Titles"), fontface = "bold"), labels_gp = gpar(fontsize = fs("Legend Labels")), direction = "horizontal", legend_width = unit(6, "cm"))
    lgd_cmp <- Legend(title = "Comparison", labels = unlist(fdr_info), pch = gsub("FDR_", "", names(fdr_info)), title_gp = gpar(fontsize = fs("Legend Titles"), fontface = "bold"), labels_gp = gpar(fontsize = fs("Legend Labels")), type = "points", size = unit(5, "mm"), background = "white", border = "black")
    lgd_sig <- Legend(title = "Significance", labels = c("FDR<0.05", "Not Significant"), type = "points", pch = c(NA, NA), size = unit(8, "mm"), title_gp = gpar(fontsize = fs("Legend Titles"), fontface = "bold"), labels_gp = gpar(fontsize = fs("Legend Labels")), graphics = list(function(x, y, w, h) grid.text("*", x, y, gp = gpar(fontsize = fs("Significance Marks"), fontface = "bold")), function(x, y, w, h) grid.text("ns", x, y, gp = gpar(fontsize = fs("Significance Marks"), fontface = "bold"))))

    pd <- packLegend(lgd_grp, packLegend(lgd_fdr, lgd_sig, direction = "vertical", row_gap = unit(2, "mm")), lgd_cmp, direction = "horizontal", column_gap = unit(20, "mm"))

    col_split_vec <- factor(meta_sub$GroupLevel1, levels = c("Control", "Infection"))
    if (length(unique(col_split_vec[!is.na(col_split_vec)])) < 2) col_split_vec <- NULL

    ht <- Heatmap(mat_scaled,
      name = "Z-score",
      cluster_rows = TRUE, show_row_dend = FALSE,
      cluster_columns = FALSE, cluster_row_slices = FALSE,
      top_annotation = ha_col, left_annotation = ha_left, right_annotation = ha_right,
      row_split = split_row,
      row_title_gp = gpar(fontsize = fs("Split Titles (Rows)"), fontface = "bold"), row_gap = unit(2, "mm"),
      column_split = col_split_vec,
      column_title = plot_title, column_title_gp = gpar(fontsize = fs("Main Title"), fontface = "bold"), column_gap = unit(1, "mm"),
      col = colorRamp2(c(-2, 0, 2), c("#2166AC", "white", "#B2182B")), border = TRUE, show_column_names = FALSE,
      heatmap_legend_param = list(legend_direction = "horizontal", title_gp = gpar(fontsize = fs("Legend Titles"), fontface = "bold"), labels_gp = gpar(fontsize = fs("Legend Labels")), legend_width = unit(6, "cm"))
    )

    gsea_short_code <- rv$code_map[input$gsea_source]

    if (is.na(gsea_short_code) || is.null(gsea_short_code)) {
      if (input$gsea_source == "All Comparisons") {
        gsea_short_code <- "All_Comp"
      } else {
        gsea_short_code <- "Custom"
      }
    }
    if (input$analysis_mode == "pathway") {
      pathway_str <- if (!is.null(p2_name) && p2_name != "") paste(p1_name, "&", p2_name) else p1_name
      base_name <- paste0(gsea_short_code, ": ", pathway_str)
    } else {
      base_name <- "Manual Gene List"
    }

    final_name <- base_name
    count <- 1
    while (final_name %in% names(rv$history)) {
      final_name <- paste0(base_name, " (", count, ")")
      count <- count + 1
    }

    rv$history[[final_name]] <- list(ht = ht, legend = pd, data = plot_data, missing = missing_genes_list)
    updateSelectInput(session, "history_selector", choices = names(rv$history), selected = final_name)
    updateNavbarPage(session, "main_nav", selected = "3. Generate Plot")
    showNotification(paste("Generated:", final_name), type = "message")
  })

  output$group_selector_ui <- renderUI({
    req(static_data())
    meta <- static_data()$meta

    available_groups <- unique(meta$GroupLevel2)

    pickerInput("selected_groups",
      label = NULL,
      choices = available_groups,
      selected = available_groups,
      multiple = TRUE,
      options = list(`actions-box` = TRUE, `selected-text-format` = "count > 2"),
      width = "100%"
    )
  })

  observeEvent(input$manual_file_upload, {
    req(input$manual_file_upload)
    infile <- input$manual_file_upload

    content <- tryCatch(
      {
        ext <- tools::file_ext(infile$name)
        if (ext == "csv") {
          tbl <- read.csv(infile$datapath, header = FALSE, stringsAsFactors = FALSE)
          unlist(tbl)
        } else {
          readLines(infile$datapath)
        }
      },
      error = function(e) {
        NULL
      }
    )

    if (!is.null(content)) {
      new_genes <- unique(trimws(content))
      new_genes <- new_genes[new_genes != ""]

      current_text <- input$manual_paste_area
      if (is.null(current_text)) current_text <- ""

      old_genes <- unique(trimws(unlist(strsplit(current_text, "\n"))))
      old_genes <- old_genes[old_genes != ""]

      combined_genes <- unique(c(old_genes, new_genes))

      updateTextAreaInput(session, "manual_paste_area", value = paste(combined_genes, collapse = "\n"))

      showNotification(paste("Success! Added", length(new_genes), "genes from file."), type = "message")

      shinyjs::reset("manual_file_upload")
    } else {
      showNotification("Could not read file. Please check the format (CSV or TXT).", type = "error")
    }
  })

  # ----------------------------------------------------------------------------
  # Tab 3: display and export
  # ----------------------------------------------------------------------------

  current_obj <- reactive({
    req(input$history_selector)
    rv$history[[input$history_selector]]
  })

  output$heatmap_plot <- renderPlot(
    {
      obj <- current_obj()
      req(obj)
      if (isTRUE(input$show_legend)) {
        draw(obj$ht,
          heatmap_legend_list = obj$legend,
          heatmap_legend_side = "bottom",
          annotation_legend_side = "bottom",
          merge_legend = TRUE
        )
      } else {
        draw(obj$ht, show_heatmap_legend = FALSE, show_annotation_legend = FALSE)
      }
    },
    height = function() {
      obj <- current_obj()
      if (is.null(obj)) {
        return(800)
      }
      n_genes <- nrow(obj$data)
      calc_height <- 500 + (n_genes * 10)
      max(800, calc_height)
    },
    width = function() {
      obj <- current_obj()
      if (is.null(obj)) {
        return("100%")
      }

      n_cols <- ncol(obj$data)

      calc_width <- (n_cols * 25)

      if (calc_width < 1000) {
        return("100%")
      } else {
        return(calc_width)
      }
    }
  )

  # Helper: export filename generator.
  fname_gen <- function(ext) {
    function() {
      raw_name <- input$history_selector
      safe_name <- gsub("[: ]+", "_", raw_name)
      safe_name <- gsub("[^a-zA-Z0-9_]", "_", safe_name)
      safe_name <- gsub("__+", "_", safe_name)
      dpi <- input$export_dpi
      paste0("Heatmap_", safe_name, "_", dpi, "dpi.", ext)
    }
  }

  # Helper: export content writer.
  content_gen <- function(format) {
    function(file) {
      obj <- current_obj()
      req(obj)
      dpi <- as.numeric(input$export_dpi)

      n_genes <- nrow(obj$data)
      calc_h_inches <- 8 + (n_genes * 0.12)
      base_w_inches <- 14

      w_px <- base_w_inches * dpi
      h_px <- calc_h_inches * dpi

      if (format == "tiff") {
        tiff(file, width = w_px, height = h_px, res = dpi, compression = "lzw")
      } else if (format == "pdf") {
        pdf(file, width = base_w_inches, height = calc_h_inches)
      } else if (format == "png") {
        png(file, width = w_px, height = h_px, res = dpi)
      } else if (format == "jpeg") jpeg(file, width = w_px, height = h_px, res = dpi, quality = 100)
      if (isTRUE(input$show_legend)) {
        draw(obj$ht, heatmap_legend_list = obj$legend, heatmap_legend_side = "bottom", annotation_legend_side = "bottom", merge_legend = TRUE)
      } else {
        draw(obj$ht, show_heatmap_legend = FALSE, show_annotation_legend = FALSE)
      }
      dev.off()
    }
  }

  output$dl_tiff <- downloadHandler(filename = fname_gen("tiff"), content = content_gen("tiff"))
  output$dl_pdf <- downloadHandler(filename = fname_gen("pdf"), content = content_gen("pdf"))
  output$dl_png <- downloadHandler(filename = fname_gen("png"), content = content_gen("png"))
  output$dl_jpeg <- downloadHandler(filename = fname_gen("jpeg"), content = content_gen("jpeg"))

  output$missing_genes_alert <- renderUI({
    obj <- current_obj()
    req(obj)
    if (!is.null(obj$missing) && length(obj$missing) > 0) {
      tagList(div(
        style = "background-color: #f8d7da; color: #721c24; padding: 15px; border: 1px solid #f5c6cb; border-radius: 5px; margin-top: 20px;",
        h4(icon("exclamation-triangle"), "Warning: The following genes were not found in the database and are excluded:"),
        p(style = "font-family: monospace; word-break: break-all;", paste(obj$missing, collapse = ", "))
      ))
    }
  })


  # Example data download
  output$btn_dl_example <- downloadHandler(
    filename = function() {
      "NeoSepPulmoExplorer_Example_Dataset.zip"
    },
    content = function(file) {
      src <- file.path(base_path, "NeoSepPulmoExplorer_Example_Dataset.zip")

      showNotification("Downloading example data...", type = "message", duration = 2)

      if (!file.exists(src)) {
        showNotification("Error: Example data file not found.", type = "error")
        stop("Missing example dataset file.", call. = FALSE)
      }
      ok <- file.copy(src, file, overwrite = TRUE)
      if (!isTRUE(ok)) {
        showNotification("Error: Could not prepare example data for download.", type = "error")
        stop("Failed to prepare example dataset download.", call. = FALSE)
      }
    }
  )
}

shinyApp(ui, server)
