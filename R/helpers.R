`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) {
    return(y)
  }
  x
}

COLORS <- c(
  navy = "#16324F",
  ink = "#243B53",
  slate = "#4F6D7A",
  blue = "#5FA8D3",
  mist = "#CAE9FF",
  sky = "#DCEEF8",
  sage = "#7BAE7F",
  gold = "#D5A44B",
  coral = "#C97C5D",
  rose = "#B56576",
  plum = "#8E6C8A",
  sand = "#F5E6D3",
  cloud = "#F4F8FB",
  charcoal = "#495057"
)

resolve_results_location <- function() {
  project_dir <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  candidate_dirs <- c(
    file.path(project_dir, "data"),
    file.path(tempdir(), "diplom_project_results")
  )
  candidate_labels <- c("project", "temp")

  for (i in seq_along(candidate_dirs)) {
    dir_path <- candidate_dirs[[i]]
    dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)

    probe_file <- tempfile(pattern = "write_probe_", tmpdir = dir_path, fileext = ".tmp")
    writable <- tryCatch(
      {
        cat("ok", file = probe_file)
        file.exists(probe_file)
      },
      error = function(e) FALSE
    )

    if (isTRUE(writable)) {
      unlink(probe_file, force = TRUE)
      return(list(
        dir = dir_path,
        file = file.path(dir_path, "experiment_results.csv"),
        source = candidate_labels[[i]]
      ))
    }
  }

  stop("No writable directory is available for saving experiment results.", call. = FALSE)
}

RESULTS_LOCATION <- resolve_results_location()
RESULTS_DIR <- RESULTS_LOCATION$dir
RESULTS_FILE <- RESULTS_LOCATION$file
RESULTS_LOCK_DIR <- paste0(RESULTS_FILE, ".lock")
RESULTS_STORAGE_LABEL <- if (identical(RESULTS_LOCATION$source, "project")) {
  "Жоба бумасы"
} else {
  "Уақытша бума"
}

GESTALT_PRINCIPLES <- c("Closure", "Similarity", "Proximity", "Symmetry", "Continuity")
ANALYTIC_FAMILY <- "Analytics"
VALID_RENDERERS <- c("single_plot", "plot_grid", "multi_panel_individual")
TASK_PREFIXES <- c(
  Closure = "CLO_",
  Similarity = "SIM_",
  Proximity = "PROX_",
  Symmetry = "SYM_",
  Continuity = "CONT_"
)

RESULT_COLUMNS <- c(
  "participant_id",
  "age",
  "gender",
  "specialization",
  "experiment_started_at",
  "started_at",
  "submitted_at",
  "displayed_at_client_ms",
  "submitted_at_client_ms",
  "task_order_position",
  "task_id",
  "question_id",
  "task_family",
  "gestalt_principle",
  "visualization_type",
  "question_type",
  "question_text",
  "options_shown",
  "correct_answer",
  "selected_answer",
  "is_correct",
  "reaction_time_sec",
  "chart_count",
  "ease_rating",
  "confidence",
  "parent_task_id",
  "panel_id",
  "panel_label",
  "panel_order",
  "panel_selected_answer",
  "panel_is_correct",
  "panel_reaction_time_sec",
  "panel_submitted_at",
  "panel_displayed_at_client_ms",
  "session_id"
)

DT_LANGUAGE <- list(
  decimal = ".",
  emptyTable = "Кестеде дерек жоқ",
  info = "_START_-тен _END_-ке дейін көрсетілді, барлығы _TOTAL_ жазба",
  infoEmpty = "0-ден 0-ге дейін көрсетілді, барлығы 0 жазба",
  infoFiltered = "(_MAX_ жазбаның ішінен сүзілді)",
  thousands = ",",
  lengthMenu = "_MENU_ жазбаны көрсету",
  loadingRecords = "Жүктелуде...",
  processing = "Өңделуде...",
  search = "Іздеу:",
  zeroRecords = "Сәйкес жазбалар табылмады",
  paginate = list(
    first = "Бірінші",
    last = "Соңғы",
    "next" = "Келесі",
    previous = "Алдыңғы"
  )
)

timestamp_string <- function(x) {
  format(x, "%Y-%m-%d %H:%M:%OS3", tz = Sys.timezone())
}

client_ms_to_posix <- function(client_ms) {
  client_ms <- suppressWarnings(as.numeric(client_ms))
  if (length(client_ms) != 1 || is.na(client_ms)) {
    return(as.POSIXct(NA))
  }
  as.POSIXct(client_ms / 1000, origin = "1970-01-01", tz = Sys.timezone())
}

trim_or_na <- function(x) {
  x <- trimws(x %||% "")
  if (!nzchar(x)) {
    return(NA_character_)
  }
  x
}

empty_results_df <- function() {
  data.frame(
    participant_id = character(),
    age = numeric(),
    gender = character(),
    specialization = character(),
    experiment_started_at = character(),
    started_at = character(),
    submitted_at = character(),
    displayed_at_client_ms = numeric(),
    submitted_at_client_ms = numeric(),
    task_order_position = integer(),
    task_id = character(),
    question_id = character(),
    task_family = character(),
    gestalt_principle = character(),
    visualization_type = character(),
    question_type = character(),
    question_text = character(),
    options_shown = character(),
    correct_answer = character(),
    selected_answer = character(),
    is_correct = logical(),
    reaction_time_sec = numeric(),
    chart_count = numeric(),
    ease_rating = numeric(),
    confidence = numeric(),
    parent_task_id = character(),
    panel_id = character(),
    panel_label = character(),
    panel_order = integer(),
    panel_selected_answer = character(),
    panel_is_correct = logical(),
    panel_reaction_time_sec = numeric(),
    panel_submitted_at = character(),
    panel_displayed_at_client_ms = numeric(),
    session_id = character(),
    stringsAsFactors = FALSE
  )
}

normalize_results_row <- function(row_df) {
  missing_cols <- setdiff(RESULT_COLUMNS, names(row_df))
  for (col_name in missing_cols) {
    row_df[[col_name]] <- NA
  }

  row_df <- row_df[, RESULT_COLUMNS, drop = FALSE]

  character_cols <- c(
    "participant_id", "gender", "specialization", "experiment_started_at",
    "started_at", "submitted_at", "task_id", "question_id", "task_family",
    "gestalt_principle", "visualization_type", "question_type",
    "question_text", "options_shown", "correct_answer", "selected_answer",
    "parent_task_id", "panel_id", "panel_label", "panel_selected_answer",
    "panel_submitted_at", "session_id"
  )
  numeric_cols <- c(
    "age", "displayed_at_client_ms", "submitted_at_client_ms",
    "task_order_position", "reaction_time_sec", "chart_count",
    "ease_rating", "confidence", "panel_order", "panel_reaction_time_sec",
    "panel_displayed_at_client_ms"
  )
  logical_cols <- c("is_correct", "panel_is_correct")

  for (col_name in intersect(character_cols, names(row_df))) {
    row_df[[col_name]] <- enc2utf8(as.character(row_df[[col_name]]))
  }

  for (col_name in intersect(numeric_cols, names(row_df))) {
    row_df[[col_name]] <- suppressWarnings(as.numeric(row_df[[col_name]]))
  }

  for (col_name in intersect(logical_cols, names(row_df))) {
    row_df[[col_name]] <- as.logical(row_df[[col_name]])
  }

  row_df
}

normalize_results_table <- function(results_df) {
  if (is.null(results_df) || nrow(results_df) == 0) {
    return(empty_results_df())
  }
  normalize_results_row(as.data.frame(results_df, stringsAsFactors = FALSE))
}

acquire_results_lock <- function(lock_dir, timeout_sec = 10, poll_sec = 0.05, stale_after_sec = 120) {
  start_time <- Sys.time()

  repeat {
    dir.create(dirname(lock_dir), recursive = TRUE, showWarnings = FALSE)

    if (dir.create(lock_dir, showWarnings = FALSE)) {
      return(invisible(TRUE))
    }

    lock_info <- suppressWarnings(file.info(lock_dir))
    if (nrow(lock_info) == 1 && !is.na(lock_info$ctime)) {
      lock_age <- as.numeric(difftime(Sys.time(), lock_info$ctime, units = "secs"))
      if (!is.na(lock_age) && lock_age > stale_after_sec) {
        unlink(lock_dir, recursive = TRUE, force = TRUE)
        next
      }
    }

    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    if (!is.na(elapsed) && elapsed >= timeout_sec) {
      stop("Could not acquire the results file lock within the timeout window.")
    }

    Sys.sleep(poll_sec)
  }
}

wait_for_results_unlock <- function(lock_dir, timeout_sec = 10, poll_sec = 0.05) {
  start_time <- Sys.time()
  while (dir.exists(lock_dir)) {
    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    if (!is.na(elapsed) && elapsed >= timeout_sec) {
      break
    }
    Sys.sleep(poll_sec)
  }
}

read_results_file <- function(file_path, n = NULL, wait_for_lock = TRUE, fail_on_error = FALSE) {
  if (wait_for_lock) {
    wait_for_results_unlock(paste0(file_path, ".lock"))
  }

  if (!file.exists(file_path) || file.info(file_path)$size == 0) {
    return(empty_results_df())
  }

  out <- tryCatch(
    readr::read_csv(file_path, show_col_types = FALSE, progress = FALSE),
    error = function(e) {
      if (isTRUE(fail_on_error)) {
        stop(
          sprintf("The existing results file could not be read safely: %s", e$message),
          call. = FALSE
        )
      }
      empty_results_df()
    }
  )

  out <- normalize_results_table(out)

  if (!is.null(n) && nrow(out) > n) {
    out <- utils::tail(out, n)
  }

  out
}

replace_results_file <- function(temp_file, file_path, attempts = 4, delay_sec = 0.15) {
  dir.create(dirname(file_path), recursive = TRUE, showWarnings = FALSE)

  for (attempt in seq_len(attempts)) {
    if (file.exists(file_path)) {
      suppressWarnings(unlink(file_path, force = TRUE))
    }

    renamed <- suppressWarnings(file.rename(temp_file, file_path))
    if (isTRUE(renamed) && file.exists(file_path)) {
      return(invisible(TRUE))
    }

    copied <- file.copy(temp_file, file_path, overwrite = TRUE, copy.mode = TRUE, copy.date = TRUE)
    if (isTRUE(copied) && file.exists(file_path)) {
      return(invisible(TRUE))
    }

    Sys.sleep(delay_sec)
  }

  stop("The result file could not be updated after multiple write attempts.", call. = FALSE)
}

excel_column_name <- function(index) {
  index <- as.integer(index)
  if (is.na(index) || index < 1) {
    stop("Excel column index must be a positive integer.", call. = FALSE)
  }

  name <- character()
  while (index > 0) {
    rem <- (index - 1) %% 26
    name <- c(intToUtf8(65 + rem), name)
    index <- (index - 1) %/% 26
  }
  paste(name, collapse = "")
}

xml_escape <- function(x) {
  x <- enc2utf8(as.character(x))
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  x <- gsub("'", "&apos;", x, fixed = TRUE)
  x
}

sanitize_sheet_name <- function(x, existing = character()) {
  base <- trimws(enc2utf8(as.character(x %||% "Парақ")))
  if (!nzchar(base)) {
    base <- "Парақ"
  }

  base <- gsub("[\\\\/:*?\\[\\]]", "_", base)
  base <- substr(base, 1, 31)
  candidate <- base
  counter <- 1L

  while (candidate %in% existing) {
    suffix <- paste0("_", counter)
    candidate <- paste0(substr(base, 1, max(1, 31 - nchar(suffix))), suffix)
    counter <- counter + 1L
  }

  candidate
}

build_xlsx_cell_xml <- function(value, row_index, col_index) {
  cell_ref <- paste0(excel_column_name(col_index), row_index)

  if (length(value) == 0 || is.na(value)) {
    return(sprintf('<c r="%s"/>', cell_ref))
  }

  if (inherits(value, "POSIXt")) {
    value <- format(value, "%Y-%m-%d %H:%M:%S")
  }

  if (is.numeric(value) && is.finite(value)) {
    return(sprintf('<c r="%s"><v>%s</v></c>', cell_ref, format(value, scientific = FALSE, trim = TRUE)))
  }

  text_value <- xml_escape(value)
  sprintf('<c r="%s" t="inlineStr"><is><t xml:space="preserve">%s</t></is></c>', cell_ref, text_value)
}

build_xlsx_sheet_xml <- function(data) {
  data <- as.data.frame(data, stringsAsFactors = FALSE, check.names = FALSE)
  headers <- names(data)

  rows_xml <- character()
  header_cells <- vapply(seq_along(headers), function(i) {
    build_xlsx_cell_xml(headers[[i]], 1L, i)
  }, character(1))
  rows_xml[[1]] <- sprintf('<row r="1">%s</row>', paste(header_cells, collapse = ""))

  if (nrow(data) > 0) {
    for (row_index in seq_len(nrow(data))) {
      values <- data[row_index, , drop = TRUE]
      row_cells <- vapply(seq_along(values), function(col_index) {
        build_xlsx_cell_xml(values[[col_index]], row_index + 1L, col_index)
      }, character(1))
      rows_xml[[length(rows_xml) + 1L]] <- sprintf('<row r="%s">%s</row>', row_index + 1L, paste(row_cells, collapse = ""))
    }
  }

  paste0(
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
    '<worksheet xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main">',
    '<sheetData>',
    paste(rows_xml, collapse = ""),
    '</sheetData>',
    '</worksheet>'
  )
}

build_xlsx_workbook_xml <- function(sheet_names) {
  sheet_nodes <- vapply(seq_along(sheet_names), function(i) {
    sprintf(
      '<sheet name="%s" sheetId="%s" r:id="rId%s"/>',
      xml_escape(sheet_names[[i]]),
      i,
      i
    )
  }, character(1))

  paste0(
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
    '<workbook xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main" ',
    'xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships">',
    '<sheets>',
    paste(sheet_nodes, collapse = ""),
    '</sheets>',
    '</workbook>'
  )
}

build_xlsx_workbook_rels_xml <- function(sheet_names) {
  rel_nodes <- vapply(seq_along(sheet_names), function(i) {
    sprintf(
      '<Relationship Id="rId%s" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/worksheet" Target="worksheets/sheet%s.xml"/>',
      i,
      i
    )
  }, character(1))

  paste0(
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
    '<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">',
    paste(rel_nodes, collapse = ""),
    '</Relationships>'
  )
}

build_xlsx_content_types_xml <- function(sheet_count) {
  sheet_overrides <- vapply(seq_len(sheet_count), function(i) {
    sprintf(
      '<Override PartName="/xl/worksheets/sheet%s.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml"/>',
      i
    )
  }, character(1))

  paste0(
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
    '<Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">',
    '<Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/>',
    '<Default Extension="xml" ContentType="application/xml"/>',
    '<Override PartName="/xl/workbook.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml"/>',
    paste(sheet_overrides, collapse = ""),
    '</Types>'
  )
}

build_xlsx_root_rels_xml <- function() {
  paste0(
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
    '<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">',
    '<Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument" Target="xl/workbook.xml"/>',
    '</Relationships>'
  )
}

compress_directory_to_zip <- function(source_dir, destination_zip) {
  source_dir <- normalizePath(source_dir, winslash = "\\", mustWork = TRUE)
  destination_zip <- normalizePath(destination_zip, winslash = "\\", mustWork = FALSE)

  ps_quote <- function(x) {
    paste0("'", gsub("'", "''", x, fixed = TRUE), "'")
  }

  command <- paste(
    "$ErrorActionPreference='Stop';",
    "Add-Type -AssemblyName System.IO.Compression.FileSystem;",
    sprintf("if (Test-Path %s) { Remove-Item %s -Force }", ps_quote(destination_zip), ps_quote(destination_zip)),
    sprintf("[System.IO.Compression.ZipFile]::CreateFromDirectory(%s, %s)", ps_quote(source_dir), ps_quote(destination_zip))
  )

  result <- suppressWarnings(
    system2("powershell", c("-NoProfile", "-Command", command), stdout = TRUE, stderr = TRUE)
  )

  if (!file.exists(destination_zip)) {
    stop(
      paste(c("The Excel archive could not be created.", result), collapse = "\n"),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

write_simple_xlsx <- function(sheets, path) {
  if (!length(sheets)) {
    stop("At least one worksheet is required for Excel export.", call. = FALSE)
  }

  root_dir <- tempfile(pattern = "xlsx_export_")
  dir.create(root_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(root_dir, recursive = TRUE, force = TRUE), add = TRUE)

  dir.create(file.path(root_dir, "_rels"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(root_dir, "xl", "_rels"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(root_dir, "xl", "worksheets"), recursive = TRUE, showWarnings = FALSE)

  sheet_names <- character()
  sheet_values <- vector("list", length(sheets))

  for (i in seq_along(sheets)) {
    sheet_name <- names(sheets)[i] %||% paste0("Парақ ", i)
    sheet_name <- sanitize_sheet_name(sheet_name, existing = sheet_names)
    sheet_names[[i]] <- sheet_name
    sheet_values[[i]] <- as.data.frame(sheets[[i]], stringsAsFactors = FALSE, check.names = FALSE)
  }

  writeLines(build_xlsx_content_types_xml(length(sheet_names)), file.path(root_dir, "[Content_Types].xml"), useBytes = TRUE)
  writeLines(build_xlsx_root_rels_xml(), file.path(root_dir, "_rels", ".rels"), useBytes = TRUE)
  writeLines(build_xlsx_workbook_xml(sheet_names), file.path(root_dir, "xl", "workbook.xml"), useBytes = TRUE)
  writeLines(build_xlsx_workbook_rels_xml(sheet_names), file.path(root_dir, "xl", "_rels", "workbook.xml.rels"), useBytes = TRUE)

  for (i in seq_along(sheet_values)) {
    sheet_xml <- build_xlsx_sheet_xml(sheet_values[[i]])
    writeLines(sheet_xml, file.path(root_dir, "xl", "worksheets", paste0("sheet", i, ".xml")), useBytes = TRUE)
  }

  compress_directory_to_zip(root_dir, path)
  invisible(TRUE)
}

append_results_csv <- function(row_df, file_path, lock_dir, timeout_sec = 10, poll_sec = 0.05) {
  dir.create(dirname(file_path), recursive = TRUE, showWarnings = FALSE)
  row_df <- normalize_results_table(row_df)

  acquire_results_lock(lock_dir, timeout_sec = timeout_sec, poll_sec = poll_sec)
  on.exit(unlink(lock_dir, recursive = TRUE, force = TRUE), add = TRUE)

  existing_rows <- read_results_file(file_path, wait_for_lock = FALSE, fail_on_error = TRUE)
  combined_rows <- normalize_results_table(dplyr::bind_rows(existing_rows, row_df))
  temp_file <- tempfile(pattern = "results_", tmpdir = dirname(file_path), fileext = ".csv")
  on.exit(unlink(temp_file, force = TRUE), add = TRUE)

  readr::write_csv(combined_rows, temp_file)
  replace_results_file(temp_file, file_path)

  invisible(TRUE)
}

rescale_values <- function(x, to = c(8, 16)) {
  x <- as.numeric(x)
  rng <- range(x, na.rm = TRUE)
  if (!all(is.finite(rng)) || diff(rng) == 0) {
    return(rep(mean(to), length(x)))
  }
  (x - rng[1]) / diff(rng) * diff(to) + to[1]
}

point_df <- function(
  x,
  y,
  color = COLORS["blue"],
  size = 4.2,
  outline = COLORS["navy"],
  label = NA_character_,
  panel = NULL,
  path_group = NULL
) {
  n <- length(x)
  data.frame(
    x = x,
    y = y,
    color = if (length(color) == 1) rep(unname(color), n) else unname(color),
    size = if (length(size) == 1) rep(unname(size), n) else unname(size),
    outline = if (length(outline) == 1) rep(unname(outline), n) else unname(outline),
    label = if (length(label) == 1) rep(label, n) else label,
    panel = if (is.null(panel)) rep("Сурет", n) else if (length(panel) == 1) rep(panel, n) else panel,
    path_group = if (is.null(path_group)) rep(NA_character_, n) else if (length(path_group) == 1) rep(path_group, n) else path_group,
    stringsAsFactors = FALSE
  )
}

curve_points <- function(x, y, color = COLORS["blue"], size = 4.2, panel = NULL, path_group = NULL) {
  point_df(x, y, color = color, size = size, panel = panel, path_group = path_group)
}

segment_points <- function(x1, y1, x2, y2, n = 6, color = COLORS["blue"], size = 4.2, panel = NULL, path_group = NULL) {
  curve_points(
    seq(x1, x2, length.out = n),
    seq(y1, y2, length.out = n),
    color = color,
    size = size,
    panel = panel,
    path_group = path_group
  )
}

arc_points <- function(center_x, center_y, radius, start_deg, end_deg, n = 16, color = COLORS["blue"], size = 4.2, panel = NULL, path_group = NULL) {
  angles <- seq(start_deg, end_deg, length.out = n) * pi / 180
  point_df(
    center_x + radius * cos(angles),
    center_y + radius * sin(angles),
    color = color,
    size = size,
    panel = panel,
    path_group = path_group
  )
}

experiment_theme <- function(base_size = 13) {
  ggplot2::theme_minimal(base_size = base_size, base_family = "sans") +
    ggplot2::theme(
      plot.title = ggplot2::element_blank(),
      plot.subtitle = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(fill = "#FFFFFF", colour = NA),
      panel.background = ggplot2::element_rect(fill = "#FBFDFF", colour = NA),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(color = "#E3EBF2", linewidth = 0.45),
      axis.text = ggplot2::element_text(color = COLORS["ink"]),
      axis.title = ggplot2::element_text(color = COLORS["slate"], face = "bold", size = 10),
      strip.background = ggplot2::element_rect(fill = "#ECF4FA", colour = NA),
      strip.text = ggplot2::element_text(face = "bold", color = COLORS["navy"], size = 12),
      legend.position = "none",
      plot.margin = ggplot2::margin(10, 10, 10, 10)
    )
}

plot_scatter_generic <- function(data, xlim = c(0, 10), ylim = c(0, 10), show_axes = FALSE, show_labels = FALSE, connect = FALSE, augment_fn = NULL) {
  if (!"color" %in% names(data)) data$color <- COLORS["blue"]
  if (!"outline" %in% names(data)) data$outline <- COLORS["navy"]
  if (!"size" %in% names(data)) data$size <- 4.2
  if (!"panel" %in% names(data)) data$panel <- "Сурет"
  if (!"label" %in% names(data)) data$label <- NA_character_
  if (!"path_group" %in% names(data)) data$path_group <- paste0("g", seq_len(nrow(data)))

  p <- ggplot2::ggplot(data, ggplot2::aes(x = x, y = y))

  if (connect) {
    p <- p +
      ggplot2::geom_path(
        ggplot2::aes(group = path_group, color = color),
        linewidth = 1.1,
        alpha = 0.9,
        lineend = "round"
      )
  }

  p <- p +
    ggplot2::geom_point(
      ggplot2::aes(size = size, fill = color, color = outline),
      shape = 21,
      stroke = 0.9,
      alpha = 0.98
    ) +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_color_identity() +
    ggplot2::scale_size_identity() +
    ggplot2::coord_fixed(ratio = 1, xlim = xlim, ylim = ylim, expand = FALSE, clip = "off") +
    experiment_theme()

  if (length(unique(data$panel)) > 1) {
    p <- p + ggplot2::facet_wrap(~panel)
  }

  if (show_labels && any(!is.na(data$label))) {
    p <- p +
      ggplot2::geom_text(
        ggplot2::aes(label = label),
        color = COLORS["navy"],
        fontface = "bold",
        size = 4,
        vjust = -1
      )
  }

  if (!show_axes) {
    p <- p +
      ggplot2::theme(
        axis.text = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        panel.grid = ggplot2::element_blank()
      )
  }

  if (!is.null(augment_fn)) {
    p <- augment_fn(p, data)
  }

  p
}

plot_bar_generic <- function(data, compact = FALSE, show_value_labels = TRUE, y_label = "Мән", augment_fn = NULL) {
  if (!"fill" %in% names(data)) {
    data$fill <- COLORS["blue"]
  }
  if (!"x_pos" %in% names(data)) {
    data$x_pos <- seq_len(nrow(data))
  }

  data <- data[order(data$x_pos), , drop = FALSE]
  data$value <- suppressWarnings(as.numeric(data$value))

  y_min_raw <- min(c(0, data$value), na.rm = TRUE)
  y_max_raw <- max(c(0, data$value), na.rm = TRUE)
  span <- y_max_raw - y_min_raw
  if (!is.finite(span) || span == 0) {
    span <- max(abs(c(y_min_raw, y_max_raw, 1)), na.rm = TRUE)
  }

  pad <- span * if (compact) 0.08 else 0.12
  label_pad <- span * if (compact) 0.03 else 0.045
  y_lower <- if (y_min_raw < 0) y_min_raw - pad else 0
  y_upper <- if (y_max_raw > 0) y_max_raw + pad else 0
  if (y_lower == y_upper) {
    y_lower <- y_lower - 1
    y_upper <- y_upper + 1
  }

  data$label_y <- ifelse(data$value >= 0, data$value + label_pad, data$value - label_pad)
  data$label_vjust <- ifelse(data$value >= 0, 0, 1)
  data$label_text <- format(round(data$value, 0), big.mark = " ", scientific = FALSE, trim = TRUE)

  x_labels <- data$category
  if (compact && nrow(data) > 5) {
    keep_idx <- unique(c(seq(1, nrow(data), by = 2), nrow(data)))
    x_labels[!seq_along(x_labels) %in% keep_idx] <- ""
  }

  p <- ggplot2::ggplot(data, ggplot2::aes(x = x_pos, y = value, fill = fill)) +
    ggplot2::geom_hline(yintercept = 0, color = "#9FB3C2", linewidth = 0.45) +
    ggplot2::geom_col(width = 0.72, color = "#FFFFFF", linewidth = 0.45, alpha = 0.96) +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_x_continuous(
      breaks = data$x_pos,
      labels = x_labels,
      expand = ggplot2::expansion(mult = c(0.04, 0.06))
    ) +
    ggplot2::scale_y_continuous(
      limits = c(y_lower, y_upper),
      expand = ggplot2::expansion(mult = c(0, 0))
    ) +
    experiment_theme() +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(x = NULL, y = y_label) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        size = if (compact) 8 else 10,
        angle = if (compact) 0 else 18,
        hjust = 0.5
      ),
      axis.text.y = ggplot2::element_text(size = if (compact) 8 else 10)
    )

  if (show_value_labels) {
    p <- p +
      ggplot2::geom_text(
        data = data,
        ggplot2::aes(x = x_pos, y = label_y, label = label_text, vjust = label_vjust),
        inherit.aes = FALSE,
        color = COLORS["navy"],
        fontface = "bold",
        size = if (compact) 3.1 else 3.8
      )
  }

  if (!is.null(augment_fn)) {
    p <- augment_fn(p, data)
  }

  p
}

plot_heatmap_generic <- function(data, compact = FALSE, show_values = TRUE, augment_fn = NULL) {
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  if (!is.factor(data$row)) {
    data$row <- factor(data$row, levels = rev(unique(data$row)))
  }
  if (!is.factor(data$col)) {
    data$col <- factor(data$col, levels = unique(data$col))
  }

  rng <- range(data$value, na.rm = TRUE)
  midpoint <- mean(rng)
  data$text_color <- ifelse(data$value >= midpoint, "#FFFFFF", COLORS["navy"])

  p <- ggplot2::ggplot(data, ggplot2::aes(x = col, y = row, fill = value)) +
    ggplot2::geom_tile(color = "#FFFFFF", linewidth = 1)

  if (all(is.finite(rng)) && diff(rng) > 0) {
    p <- p + ggplot2::scale_fill_gradient2(
      low = COLORS["sky"],
      mid = COLORS["blue"],
      high = COLORS["navy"],
      midpoint = midpoint
    )
  } else {
    p <- p + ggplot2::scale_fill_gradient(low = COLORS["mist"], high = COLORS["blue"])
  }

  p <- p +
    experiment_theme() +
    ggplot2::labs(x = NULL, y = NULL, fill = NULL) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(size = if (compact) 8 else 10),
      axis.text.y = ggplot2::element_text(size = if (compact) 8 else 10),
      legend.position = "none"
    )

  if (show_values) {
    p <- p +
      ggplot2::geom_text(
        ggplot2::aes(label = round(value, 0), color = text_color),
        fontface = "bold",
        size = if (compact) 3 else 3.6
      ) +
      ggplot2::scale_color_identity()
  }

  if (!is.null(augment_fn)) {
    p <- augment_fn(p, data)
  }

  p
}

plot_line_generic <- function(data, compact = FALSE, x_breaks = NULL, x_labels = NULL, y_label = "Мән", augment_fn = NULL) {
  if (!"display_color" %in% names(data)) data$display_color <- COLORS["blue"]
  if (!"series" %in% names(data)) data$series <- "S"
  if (!"panel" %in% names(data)) data$panel <- "Сурет"

  if (is.null(x_breaks)) x_breaks <- sort(unique(data$x))
  if (is.null(x_labels)) x_labels <- x_breaks

  p <- ggplot2::ggplot(data, ggplot2::aes(x = x, y = y, group = series)) +
    ggplot2::geom_line(
      ggplot2::aes(color = display_color),
      linewidth = 1.35,
      lineend = "round"
    ) +
    ggplot2::geom_point(
      ggplot2::aes(fill = display_color),
      shape = 21,
      size = if (compact) 3 else 3.5,
      stroke = 0.9,
      color = COLORS["navy"]
    ) +
    ggplot2::scale_color_identity() +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_x_continuous(
      breaks = x_breaks,
      labels = x_labels,
      expand = ggplot2::expansion(mult = c(0.02, 0.05))
    ) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.05, 0.12))) +
    ggplot2::labs(x = NULL, y = y_label) +
    experiment_theme() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        angle = if (compact) 0 else 35,
        hjust = 0.5,
        size = if (compact) 8 else 9
      ),
      axis.text.y = ggplot2::element_text(size = if (compact) 8 else 10)
    )

  if (length(unique(data$panel)) > 1) {
    p <- p + ggplot2::facet_wrap(~panel)
  }

  if (!is.null(augment_fn)) {
    p <- augment_fn(p, data)
  }

  p
}
