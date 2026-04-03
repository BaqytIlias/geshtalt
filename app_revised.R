# ---- Package setup ----
required_packages <- c(
  "shiny",
  "ggplot2",
  "dplyr",
  "readr",
  "treemapify",
  "DT"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    paste(
      "Please install the required packages before running the app:",
      paste(missing_packages, collapse = ", ")
    ),
    call. = FALSE
  )
}

invisible(lapply(required_packages, library, character.only = TRUE))

# ---- Small helpers and global constants ----
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
  cloud = "#F4F8FB"
)

RESULTS_DIR <- file.path(getwd(), "data")
RESULTS_FILE <- file.path(RESULTS_DIR, "experiment_results.csv")
RESULTS_LOCK_DIR <- paste0(RESULTS_FILE, ".lock")
SUPERSTORE_FILE <- file.path(RESULTS_DIR, "superstore.csv")
GESTALT_PRINCIPLES <- c("Closure", "Similarity", "Proximity", "Symmetry", "Continuity")
VALID_RENDERERS <- c("single_plot", "plot_grid", "closure_triptych")
BASE_VIS_TYPES <- c("Bar chart", "Scatterplot", "Heatmap", "Bubble chart", "Treemap", "Line chart")
TASK_PREFIXES <- c(
  Closure = "CLO_",
  Similarity = "SIM_",
  Proximity = "PROX_",
  Symmetry = "SYM_",
  Continuity = "CONT_"
)
PRINCIPLE_BRIEFS <- c(
  Closure = "Толық емес шекаралар тұтас пішін ретінде қабылданады.",
  Similarity = "Ортақ түс, өлшем немесе пішін элементтерді бір топ ретінде қабылдатуға ықпал етеді.",
  Proximity = "Бір-біріне жақын орналасқан элементтер бір топқа жатады деп қабылданады.",
  Symmetry = "Теңгерімді симметриялы құрылымдар тұрақты әрі реттелген болып көрінеді.",
  Continuity = "Бір сызық бойымен орналасқан элементтер үзілмейтін жол ретінде қабылданады."
)
PRINCIPLE_LABELS <- c(
  Closure = "Тұйықталу",
  Similarity = "Ұқсастық",
  Proximity = "Жақындық",
  Symmetry = "Симметрия",
  Continuity = "Үздіксіздік"
)
VIS_LABELS <- c(
  "Bar chart" = "Бағанды диаграмма",
  "Scatterplot" = "Нүктелік диаграмма",
  "Heatmap" = "Жылу картасы",
  "Bubble chart" = "Көпіршік диаграммасы",
  "Treemap" = "Ағаш картасы",
  "Line chart" = "Сызықтық диаграмма",
  "Scatterplot + Bar chart + Treemap" = "Нүктелік диаграмма + Бағанды диаграмма + Ағаш картасы",
  "Comparison grid" = "Салыстыру панелі"
)
QUESTION_TYPE_LABELS <- c(
  A = "A. Ең үлкен мәнді табу",
  B = "B. Ең кіші мәнді табу",
  C = "C. Екі санатты салыстыру",
  D = "D. Айырманы шамамен бағалау",
  E = "E. Реттеу",
  F = "F. Аномалияны немесе ауытқуды анықтау",
  G = "G. Аралықты анықтау",
  H = "H. Ұқсас мәндерді анықтау",
  I = "I. Топты қабылдау",
  J = "J. Құрылым мен үрдісті қабылдау"
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
  "gestalt_principle",
  "visualization_type",
  "question_type",
  "question_text",
  "options_shown",
  "correct_answer",
  "selected_answer",
  "is_correct",
  "reaction_time_sec",
  "confidence",
  "session_id"
)
DT_LANGUAGE <- list(
  decimal = ".",
  emptyTable = "Кестеде деректер жоқ",
  info = "_START_-тен _END_-ке дейін көрсетілуде, барлығы _TOTAL_ жазба",
  infoEmpty = "0-ден 0-ге дейін көрсетілуде, барлығы 0 жазба",
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

label_principle <- function(x) {
  unname(PRINCIPLE_LABELS[[x]] %||% x)
}

label_visualization <- function(x) {
  unname(VIS_LABELS[[x]] %||% x)
}

label_question_type <- function(x) {
  code <- substring(x %||% "", 1, 1)
  unname(QUESTION_TYPE_LABELS[[code]] %||% x)
}

trim_or_na <- function(x) {
  x <- trimws(x %||% "")
  if (!nzchar(x)) {
    return(NA_character_)
  }
  x
}

as_heatmap_df <- function(mat) {
  df <- as.data.frame(as.table(mat), stringsAsFactors = FALSE)
  names(df) <- c("row", "col", "value")
  df$row <- factor(df$row, levels = rev(rownames(mat)))
  df$col <- factor(df$col, levels = colnames(mat))
  df
}

arc_points <- function(center_x, center_y, radius, start_deg, end_deg, n = 16, color = COLORS["blue"], size = 4.4) {
  angles <- seq(start_deg, end_deg, length.out = n) * pi / 180
  data.frame(
    x = center_x + radius * cos(angles),
    y = center_y + radius * sin(angles),
    color = rep(unname(color), length(angles)),
    outline = rep(unname(COLORS["navy"]), length(angles)),
    size = rep(unname(size), length(angles)),
    row.names = NULL,
    stringsAsFactors = FALSE
  )
}

curve_points <- function(x, y, color = COLORS["blue"], size = 4.2) {
  n_points <- length(x)
  data.frame(
    x = x,
    y = y,
    color = if (length(color) == 1) rep(unname(color), n_points) else unname(color),
    outline = rep(unname(COLORS["navy"]), n_points),
    size = if (length(size) == 1) rep(unname(size), n_points) else unname(size),
    row.names = NULL,
    stringsAsFactors = FALSE
  )
}

# Pre-create a zero-row results frame so downloads and previews always use
# the same column order, even before the first submission is saved.
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
    gestalt_principle = character(),
    visualization_type = character(),
    question_type = character(),
    question_text = character(),
    options_shown = character(),
    correct_answer = character(),
    selected_answer = character(),
    is_correct = logical(),
    reaction_time_sec = numeric(),
    confidence = numeric(),
    session_id = character(),
    stringsAsFactors = FALSE
  )
}

experiment_theme <- function(base_size = 13) {
  ggplot2::theme_minimal(base_size = base_size, base_family = "Segoe UI") +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 16, color = COLORS["navy"]),
      plot.subtitle = ggplot2::element_text(size = 11, color = COLORS["slate"]),
      plot.title.position = "plot",
      plot.background = ggplot2::element_rect(fill = "#FFFFFF", colour = NA),
      panel.background = ggplot2::element_rect(fill = "#FBFDFF", colour = NA),
      axis.title = ggplot2::element_text(size = 10.5, face = "bold", color = COLORS["slate"]),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 10)),
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 10)),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(color = "#E3EBF2", linewidth = 0.45),
      axis.text = ggplot2::element_text(color = COLORS["ink"]),
      axis.line = ggplot2::element_line(color = "#A8BBC9", linewidth = 0.35),
      strip.background = ggplot2::element_rect(fill = "#EAF3F9", colour = NA),
      strip.text = ggplot2::element_text(face = "bold", color = COLORS["navy"]),
      legend.position = "none",
      plot.margin = ggplot2::margin(12, 14, 12, 12)
    )
}

plot_scatter_generic <- function(
  data,
  title,
  subtitle = NULL,
  xlim = c(0, 10),
  ylim = c(0, 10),
  show_axes = FALSE,
  show_labels = FALSE,
  connect = FALSE,
  augment_fn = NULL
) {
  if (!"color" %in% names(data)) {
    data$color <- COLORS["blue"]
  }
  if (!"outline" %in% names(data)) {
    data$outline <- COLORS["navy"]
  }
  if (!"size" %in% names(data)) {
    data$size <- 4.2
  }
  if (!"panel" %in% names(data)) {
    data$panel <- "View"
  }
  if (!"label" %in% names(data)) {
    data$label <- NA_character_
  }
  if (!"path_group" %in% names(data)) {
    data$path_group <- paste0("point_", seq_len(nrow(data)))
  } else {
    missing_path <- is.na(data$path_group) | data$path_group == ""
    data$path_group[missing_path] <- paste0("point_", seq_len(sum(missing_path)))
  }

  p <- ggplot2::ggplot(data, ggplot2::aes(x = x, y = y))

  if (connect) {
    p <- p +
      ggplot2::geom_path(
        ggplot2::aes(group = path_group, color = color),
        linewidth = 1.05,
        alpha = 0.8,
        lineend = "round"
      )
  }

  p <- p +
    ggplot2::geom_point(
      ggplot2::aes(size = size, fill = color, color = outline),
      shape = 21,
      stroke = 0.95,
      alpha = 0.98
    ) +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_color_identity() +
    ggplot2::scale_size_identity() +
    ggplot2::coord_fixed(ratio = 1, xlim = xlim, ylim = ylim, expand = FALSE, clip = "off") +
    ggplot2::labs(title = NULL, subtitle = NULL) +
    experiment_theme()

  if (length(unique(data$panel)) > 1) {
    p <- p + ggplot2::facet_wrap(~panel)
  }

  if (show_labels && any(!is.na(data$label))) {
    p <- p +
      ggplot2::geom_text(
        ggplot2::aes(label = label),
        color = COLORS["navy"],
        family = "Segoe UI",
        fontface = "bold",
        size = 4.1,
        vjust = -1
      )
  }

  if (!show_axes) {
    p <- p +
      ggplot2::theme(
        axis.text = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        panel.grid = ggplot2::element_blank()
      )
  }

  if (!is.null(augment_fn)) {
    p <- augment_fn(p, data)
  }

  p
}

plot_bubble_generic <- function(...) {
  plot_scatter_generic(...)
}

plot_bar_generic <- function(data, title, subtitle = NULL, y_max = NULL, augment_fn = NULL) {
  if (!"fill" %in% names(data)) {
    data$fill <- COLORS["blue"]
  }
  if (!"x_pos" %in% names(data)) {
    data$x_pos <- seq_len(nrow(data))
  }

  data <- data[order(data$x_pos), , drop = FALSE]
  y_limit <- y_max %||% (max(data$value) * 1.18)

  p <- ggplot2::ggplot(data, ggplot2::aes(x = x_pos, y = value, fill = fill)) +
    ggplot2::geom_col(width = 0.72, color = "#FFFFFF", linewidth = 0.45, alpha = 0.95) +
    ggplot2::geom_text(
      ggplot2::aes(label = value),
      vjust = -0.35,
      color = COLORS["navy"],
      family = "Segoe UI",
      fontface = "bold",
      size = 4
    ) +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_x_continuous(
      breaks = data$x_pos,
      labels = data$category,
      expand = ggplot2::expansion(mult = c(0.03, 0.05))
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, y_limit),
      expand = ggplot2::expansion(mult = c(0, 0.02))
    ) +
    ggplot2::labs(title = NULL, subtitle = NULL, x = NULL, y = "Мән") +
    experiment_theme() +
    ggplot2::coord_cartesian(clip = "off")

  if (!is.null(augment_fn)) {
    p <- augment_fn(p, data)
  }

  p
}

plot_heatmap_generic <- function(data, title, subtitle = NULL, show_values = TRUE, augment_fn = NULL) {
  midpoint <- stats::median(data$value, na.rm = TRUE)
  data$text_color <- ifelse(data$value >= midpoint, "#FFFFFF", COLORS["navy"])

  p <- ggplot2::ggplot(data, ggplot2::aes(x = col, y = row, fill = value)) +
    ggplot2::geom_tile(color = "#FFFFFF", linewidth = 1) +
    ggplot2::scale_fill_gradient2(
      low = COLORS["sky"],
      mid = COLORS["blue"],
      high = COLORS["navy"],
      midpoint = midpoint
    ) +
    ggplot2::labs(title = NULL, subtitle = NULL, x = NULL, y = NULL, fill = "Мән") +
    experiment_theme() +
    ggplot2::theme(panel.grid = ggplot2::element_blank())

  if (show_values) {
    p <- p +
      ggplot2::geom_text(
        ggplot2::aes(label = round(value, 0), color = text_color),
        family = "Segoe UI",
        fontface = "bold",
        size = 3.7
      ) +
      ggplot2::scale_color_identity()
  }

  if (!is.null(augment_fn)) {
    p <- augment_fn(p, data)
  }

  p
}

plot_treemap_generic <- function(data, title, subtitle = NULL) {
  if (!"fill" %in% names(data)) {
    data$fill <- COLORS["blue"]
  }
  if (!"group" %in% names(data)) {
    data$group <- "All"
  }
  has_subgroups <- length(unique(data$group)) > 1
  base_mapping <- if (has_subgroups) {
    ggplot2::aes(area = value, fill = fill, label = label, subgroup = group)
  } else {
    ggplot2::aes(area = value, fill = fill, label = label)
  }

  p <- ggplot2::ggplot(data, base_mapping) +
    treemapify::geom_treemap(color = "#FFFFFF", size = 1)

  if (has_subgroups) {
    p <- p +
      treemapify::geom_treemap_subgroup_border(color = COLORS["navy"], size = 2) +
      treemapify::geom_treemap_subgroup_text(
        place = "centre",
        alpha = 0.7,
        colour = COLORS["navy"],
        grow = TRUE,
        family = "Segoe UI",
        min.size = 8
      )
  }

  p +
    treemapify::geom_treemap_text(
      place = "centre",
      colour = "#FFFFFF",
      grow = TRUE,
      reflow = TRUE,
      family = "Segoe UI",
      min.size = 7
    ) +
    ggplot2::scale_fill_identity() +
    ggplot2::labs(title = NULL, subtitle = NULL) +
    ggplot2::theme_void(base_family = "Segoe UI") +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "#FFFFFF", colour = NA),
      plot.title = ggplot2::element_text(face = "bold", size = 16, color = COLORS["navy"]),
      plot.subtitle = ggplot2::element_text(size = 11, color = COLORS["slate"]),
      plot.title.position = "plot",
      plot.margin = ggplot2::margin(12, 14, 12, 12)
    )
}

plot_line_generic <- function(data, title, subtitle = NULL, augment_fn = NULL) {
  if (!"display_color" %in% names(data)) {
    data$display_color <- COLORS["blue"]
  }
  if (!"series" %in% names(data)) {
    data$series <- "Series"
  }
  if (!"panel" %in% names(data)) {
    data$panel <- "View"
  }

  p <- ggplot2::ggplot(data, ggplot2::aes(x = x, y = y, group = series)) +
    ggplot2::geom_line(
      ggplot2::aes(color = display_color),
      linewidth = 1.35,
      na.rm = FALSE,
      lineend = "round"
    ) +
    ggplot2::geom_point(
      ggplot2::aes(fill = display_color),
      shape = 21,
      size = 3.6,
      stroke = 0.95,
      color = COLORS["navy"],
      na.rm = TRUE
    ) +
    ggplot2::scale_color_identity() +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_x_continuous(
      breaks = sort(unique(data$x)),
      expand = ggplot2::expansion(mult = c(0.02, 0.05))
    ) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.05, 0.12))) +
    ggplot2::labs(title = NULL, subtitle = NULL, x = "Индекс", y = "Мән") +
    experiment_theme()

  if (length(unique(data$panel)) > 1) {
    p <- p + ggplot2::facet_wrap(~panel)
  }

  if (!is.null(augment_fn)) {
    p <- augment_fn(p, data)
  }

  p
}

# Normalize result rows before saving so CSV files always preserve a stable
# schema, predictable types, and UTF-8 text output.
normalize_results_row <- function(row_df) {
  missing_cols <- setdiff(RESULT_COLUMNS, names(row_df))
  for (col_name in missing_cols) {
    row_df[[col_name]] <- NA
  }

  row_df <- row_df[, RESULT_COLUMNS, drop = FALSE]

  character_cols <- c(
    "participant_id", "gender", "specialization", "experiment_started_at",
    "started_at", "submitted_at", "task_id", "question_id",
    "gestalt_principle", "visualization_type", "question_type",
    "question_text", "options_shown", "correct_answer", "selected_answer",
    "session_id"
  )
  numeric_cols <- c(
    "age", "displayed_at_client_ms", "submitted_at_client_ms",
    "task_order_position", "reaction_time_sec", "confidence"
  )

  for (col_name in intersect(character_cols, names(row_df))) {
    row_df[[col_name]] <- enc2utf8(as.character(row_df[[col_name]]))
  }

  for (col_name in intersect(numeric_cols, names(row_df))) {
    row_df[[col_name]] <- suppressWarnings(as.numeric(row_df[[col_name]]))
  }

  row_df$is_correct <- as.logical(row_df$is_correct)

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
    wait_for_results_unlock(RESULTS_LOCK_DIR)
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

replace_results_file <- function(temp_file, file_path, attempts = 3, delay_sec = 0.1) {
  for (attempt in seq_len(attempts)) {
    copied <- file.copy(temp_file, file_path, overwrite = TRUE, copy.mode = TRUE, copy.date = TRUE)
    if (isTRUE(copied)) {
      return(invisible(TRUE))
    }
    Sys.sleep(delay_sec)
  }

  stop("The results file could not be updated after multiple write attempts.", call. = FALSE)
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

legacy_make_task <- function(
  task_id,
  principle,
  visualization_type,
  question_type,
  question_text,
  answer_choices,
  correct_answer,
  renderer = "single_plot",
  plot_fun = NULL,
  scatter_fun = NULL,
  bar_fun = NULL,
  treemap_fun = NULL
) {
  list(
    task_id = task_id,
    question_id = paste0("Q_", task_id),
    principle = principle,
    visualization_type = visualization_type,
    question_type = question_type,
    question_text = question_text,
    answer_choices = answer_choices,
    correct_answer = correct_answer,
    renderer = renderer,
    plot_fun = plot_fun,
    scatter_fun = scatter_fun,
    bar_fun = bar_fun,
    treemap_fun = treemap_fun
  )
}

legacy_build_task_bank <- function() {
  clr <- COLORS
  tasks <- list()

  # ---- Task definitions ----
  # Every task is declared explicitly so the experiment remains auditable and
  # easy to extend for diploma-project review.
  # ---- Closure ----

  tasks[[length(tasks) + 1]] <- make_task(
    task_id = "CLO_01",
    principle = "Closure",
    visualization_type = "Scatterplot + Bar chart + Treemap",
    question_type = "I. Group perception based on closure",
    question_text = "Қай визуализация топтық құрылымды тұйық біртұтас ретінде ең оңай қабылдауға мүмкіндік береді?",
    answer_choices = c("Нүктелік диаграмма", "Бағанды диаграмма", "Ағаш картасы"),
    correct_answer = "Ағаш картасы",
    renderer = "closure_triptych",
    scatter_fun = function() {
      data <- dplyr::bind_rows(
        arc_points(2.2, 5.0, 1.1, 15, 320, n = 14, color = clr["blue"], size = 4.5),
        arc_points(5.1, 5.0, 1.0, 25, 330, n = 14, color = clr["blue"], size = 4.5),
        arc_points(8.0, 5.0, 1.1, 5, 300, n = 13, color = clr["blue"], size = 4.5)
      )
      plot_scatter_generic(
        data,
        title = "Нүктелік диаграмма",
        subtitle = "Бірдей логикалық топтар ішінара контурлар түрінде көрсетілген",
        xlim = c(0.5, 9.5),
        ylim = c(3.2, 6.8),
        show_axes = FALSE
      )
    },
    bar_fun = function() {
      data <- data.frame(
        category = LETTERS[1:9],
        value = c(10, 12, 11, 14, 13, 15, 12, 11, 13),
        x_pos = c(1, 2, 3, 5, 6, 7, 9, 10, 11),
        fill = rep(clr["blue"], 9),
        stringsAsFactors = FALSE
      )
      plot_bar_generic(
        data,
        title = "Бағанды диаграмма",
        subtitle = "Аралықтар мен тұспалды шекаралар топтарды аңғартады",
        y_max = 18,
        augment_fn = function(p, df) {
          p +
            ggplot2::annotate("segment", x = 0.45, xend = 0.45, y = 0.7, yend = 16.6, color = clr["navy"], linewidth = 0.9) +
            ggplot2::annotate("segment", x = 0.45, xend = 3.55, y = 0.7, yend = 0.7, color = clr["navy"], linewidth = 0.9) +
            ggplot2::annotate("segment", x = 3.55, xend = 3.55, y = 0.7, yend = 16.6, color = clr["navy"], linewidth = 0.9) +
            ggplot2::annotate("segment", x = 4.45, xend = 4.45, y = 0.7, yend = 17.1, color = clr["navy"], linewidth = 0.9) +
            ggplot2::annotate("segment", x = 4.45, xend = 7.55, y = 0.7, yend = 0.7, color = clr["navy"], linewidth = 0.9) +
            ggplot2::annotate("segment", x = 7.55, xend = 7.55, y = 0.7, yend = 17.1, color = clr["navy"], linewidth = 0.9) +
            ggplot2::annotate("segment", x = 8.45, xend = 8.45, y = 0.7, yend = 16.8, color = clr["navy"], linewidth = 0.9) +
            ggplot2::annotate("segment", x = 8.45, xend = 11.55, y = 0.7, yend = 0.7, color = clr["navy"], linewidth = 0.9) +
            ggplot2::annotate("segment", x = 11.55, xend = 11.55, y = 0.7, yend = 16.8, color = clr["navy"], linewidth = 0.9)
        }
      )
    },
    treemap_fun = function() {
      data <- data.frame(
        group = rep(c("1-топ", "2-топ", "3-топ"), each = 3),
        label = c("A", "B", "C", "D", "E", "F", "G", "H", "I"),
        value = c(10, 12, 11, 14, 13, 15, 12, 11, 13),
        fill = rep(c(clr["blue"], clr["sage"], clr["coral"]), each = 3),
        stringsAsFactors = FALSE
      )
      plot_treemap_generic(
        data,
        title = "Ағаш картасы",
        subtitle = "Тұйық контейнерлер топтастыруды айқын көрсетеді"
      )
    }
  )

  tasks[[length(tasks) + 1]] <- make_task(
    task_id = "CLO_02",
    principle = "Closure",
    visualization_type = "Treemap",
    question_type = "I. Group perception based on closure",
    question_text = "Ағаш картасында неше логикалық топ көріп тұрсыз?",
    answer_choices = c("2", "3", "4", "5"),
    correct_answer = "3",
    plot_fun = function() {
      data <- data.frame(
        group = rep(c("Солтүстік", "Орталық", "Оңтүстік"), each = 3),
        label = c("A1", "A2", "A3", "B1", "B2", "B3", "C1", "C2", "C3"),
        value = c(13, 11, 9, 14, 12, 10, 15, 8, 7),
        fill = rep(c(clr["blue"], clr["sage"], clr["gold"]), each = 3),
        stringsAsFactors = FALSE
      )
      plot_treemap_generic(
        data,
        title = "Тұспалды топтары бар ағаш картасы",
        subtitle = "Әр ішкі топ ортақ ата-аналық шекарамен қоршалған"
      )
    }
  )

  tasks[[length(tasks) + 1]] <- make_task(
    task_id = "CLO_03",
    principle = "Closure",
    visualization_type = "Scatterplot",
    question_type = "J. Subjective organization and structure perception",
    question_text = "Белгіленген нүктелердің қай жиыны ең тұтас пішін ретінде қабылданады?",
    answer_choices = c("A тобы", "B тобы", "C тобы", "Ешқайсысы"),
    correct_answer = "B тобы",
    plot_fun = function() {
      data <- dplyr::bind_rows(
        dplyr::mutate(arc_points(2.1, 5.0, 1.2, 45, 245, n = 10, color = clr["mist"], size = 4.4), group = "A"),
        dplyr::mutate(arc_points(5.1, 5.0, 1.15, 10, 335, n = 16, color = clr["blue"], size = 4.6), group = "B"),
        dplyr::mutate(curve_points(c(7.3, 7.9, 8.5, 8.0, 7.6), c(4.1, 4.9, 5.1, 5.8, 6.2), color = clr["sand"], size = 4.6), group = "C")
      )
      plot_scatter_generic(
        data,
        title = "Фрагменттелген нүктелердегі тұйықталу",
        subtitle = "Бір кластер басқаларына қарағанда күштірек тұспалды контур құрайды",
        xlim = c(0.8, 9.2),
        ylim = c(3.2, 6.8),
        show_axes = FALSE,
        augment_fn = function(p, df) {
          p +
            ggplot2::annotate("text", x = 2.1, y = 6.55, label = "A", family = "Segoe UI", size = 5, color = clr["navy"], fontface = "bold") +
            ggplot2::annotate("text", x = 5.1, y = 6.55, label = "B", family = "Segoe UI", size = 5, color = clr["navy"], fontface = "bold") +
            ggplot2::annotate("text", x = 8.0, y = 6.55, label = "C", family = "Segoe UI", size = 5, color = clr["navy"], fontface = "bold")
        }
      )
    }
  )

  tasks[[length(tasks) + 1]] <- make_task(
    task_id = "CLO_04",
    principle = "Closure",
    visualization_type = "Bar chart",
    question_type = "I. Group perception based on closure",
    question_text = "Қай санаттар жұбы біртұтас бірлік ретінде ең оңай қабылданады?",
    answer_choices = c("A және B", "B және C", "C және D", "D және E"),
    correct_answer = "A және B",
    plot_fun = function() {
      data <- data.frame(
        category = LETTERS[1:5],
        value = c(14, 16, 11, 13, 12),
        x_pos = c(1, 2, 4, 6, 8),
        fill = c(clr["blue"], clr["blue"], clr["mist"], clr["mist"], clr["mist"]),
        stringsAsFactors = FALSE
      )
      plot_bar_generic(
        data,
        title = "Тұспалды қоршауы бар бағанды диаграмма",
        subtitle = "Толық емес шекара бір топталған блокты аңғартады",
        y_max = 19,
        augment_fn = function(p, df) {
          p +
            ggplot2::annotate("segment", x = 0.45, xend = 0.45, y = 0.6, yend = 17.2, color = clr["navy"], linewidth = 1) +
            ggplot2::annotate("segment", x = 0.45, xend = 2.55, y = 0.6, yend = 0.6, color = clr["navy"], linewidth = 1) +
            ggplot2::annotate("segment", x = 2.55, xend = 2.55, y = 0.6, yend = 17.2, color = clr["navy"], linewidth = 1) +
            ggplot2::annotate("segment", x = 0.75, xend = 1.45, y = 17.2, yend = 17.2, color = clr["navy"], linewidth = 1) +
            ggplot2::annotate("segment", x = 1.85, xend = 2.25, y = 17.2, yend = 17.2, color = clr["navy"], linewidth = 1)
        }
      )
    }
  )

  tasks[[length(tasks) + 1]] <- make_task(
    task_id = "CLO_05",
    principle = "Closure",
    visualization_type = "Bubble chart",
    question_type = "I. Group perception based on closure",
    question_text = "Бос жерлерге қарамастан, неше тұтас құрылымды қабылдайсыз?",
    answer_choices = c("1", "2", "3", "Ешқайсысы"),
    correct_answer = "2",
    plot_fun = function() {
      data <- dplyr::bind_rows(
        dplyr::mutate(arc_points(3.1, 5.0, 1.25, 20, 330, n = 14, color = clr["blue"], size = 10), path_group = "left"),
        dplyr::mutate(arc_points(7.1, 5.0, 1.25, 35, 335, n = 14, color = clr["sage"], size = 10), path_group = "right"),
        dplyr::mutate(curve_points(c(5.0, 5.4, 5.9), c(4.0, 5.2, 4.3), color = clr["sand"], size = 7), path_group = "noise")
      )
      plot_bubble_generic(
        data,
        title = "Толық емес контурлары бар көпіршік диаграммасы",
        subtitle = "Әрқайсысы бөлшектенген болса да, екіге жуық тұйық ілмек көрінеді",
        xlim = c(0.8, 9.3),
        ylim = c(3.2, 6.8),
        show_axes = FALSE
      )
    }
  )

  # ---- Similarity ----
  tasks[[length(tasks) + 1]] <- make_task(
    task_id = "SIM_01",
    principle = "Similarity",
    visualization_type = "Scatterplot",
    question_type = "J. Subjective organization and trend perception",
    question_text = "Бұл нүктелерді топтастырудағы басым белгі қандай?",
    answer_choices = c("Түс", "Орналасу", "Өлшем", "Айқын белгі жоқ"),
    correct_answer = "Түс",
    plot_fun = function() {
      data <- data.frame(
        x = c(1.3, 2.5, 3.8, 5.0, 6.2, 7.4, 2.0, 3.2, 4.4, 5.7, 6.9, 8.1),
        y = c(6.4, 4.5, 6.1, 4.1, 6.3, 4.4, 5.5, 3.7, 5.8, 3.8, 5.7, 3.9),
        color = c(rep(clr["blue"], 4), rep(clr["coral"], 4), rep(clr["sage"], 4)),
        outline = unname(clr["navy"]),
        size = 4.5,
        row.names = NULL,
        stringsAsFactors = FALSE
      )
      plot_scatter_generic(
        data,
        title = "Қайталанатын түс арқылы ұқсастық",
        subtitle = "Кеңістіктегі орындары әртүрлі болғанымен, ортақ реңк топтастыруға итермелейді",
        xlim = c(0.8, 8.6),
        ylim = c(3.0, 7.0),
        show_axes = FALSE
      )
    }
  )

  tasks[[length(tasks) + 1]] <- make_task(
    task_id = "SIM_02",
    principle = "Similarity",
    visualization_type = "Bubble chart",
    question_type = "I. Group perception based on similarity",
    question_text = "Ұқсастыққа сүйеніп неше топты қабылдайсыз?",
    answer_choices = c("2", "3", "4", "5"),
    correct_answer = "3",
    plot_fun = function() {
      data <- data.frame(
        x = c(1.3, 2.1, 3.1, 4.2, 5.0, 5.9, 6.8, 7.7, 8.5),
        y = c(5.8, 4.1, 6.2, 4.7, 6.0, 4.0, 5.7, 4.3, 6.1),
        color = c(rep(clr["blue"], 3), rep(clr["sage"], 3), rep(clr["coral"], 3)),
        outline = unname(clr["navy"]),
        size = c(9, 12, 15, 8, 14, 10, 12, 9, 15),
        row.names = NULL,
        stringsAsFactors = FALSE
      )
      plot_bubble_generic(
        data,
        title = "Ортақ көрнекі белгілері бар көпіршік диаграммасы",
        subtitle = "Көпіршік өлшемдері әртүрлі, бірақ түс ұқсастығы үш айқын топ құрайды",
        xlim = c(0.8, 9.0),
        ylim = c(3.2, 6.8),
        show_axes = FALSE
      )
    }
  )

  tasks[[length(tasks) + 1]] <- make_task(
    task_id = "SIM_03",
    principle = "Similarity",
    visualization_type = "Bar chart",
    question_type = "A. Finding the maximum value",
    question_text = "Көк бағандардың ішінде ең үлкен мән қай санатта?",
    answer_choices = c("A", "B", "C", "E"),
    correct_answer = "C",
    plot_fun = function() {
      data <- data.frame(
        category = LETTERS[1:5],
        value = c(18, 24, 27, 20, 21),
        fill = c(clr["blue"], clr["coral"], clr["blue"], clr["coral"], clr["blue"]),
        stringsAsFactors = FALSE
      )
      plot_bar_generic(
        data,
        title = "Ортақ түс бойынша топталған бағанды диаграмма",
        subtitle = "Бірдей көрнекі кодты бөлісетін бағандарды ғана салыстырыңыз"
      )
    }
  )

  tasks[[length(tasks) + 1]] <- make_task(
    task_id = "SIM_04",
    principle = "Similarity",
    visualization_type = "Heatmap",
    question_type = "H. Detecting similar values",
    question_text = "Қай екі жолдағы мәндер үлгісі ең ұқсас?",
    answer_choices = c("R1 және R2", "R2 және R3", "R3 және R5", "R4 және R5"),
    correct_answer = "R2 және R3",
    plot_fun = function() {
      mat <- matrix(
        c(
          12, 30, 14, 29, 16,
          40, 42, 41, 43, 42,
          39, 41, 40, 44, 41,
          18, 20, 23, 19, 21,
          28, 14, 27, 15, 29
        ),
        nrow = 5,
        byrow = TRUE,
        dimnames = list(paste0("R", 1:5), paste0("C", 1:5))
      )
      plot_heatmap_generic(
        as_heatmap_df(mat),
        title = "Жауап профильдерінің жылу картасы",
        subtitle = "Түстік үлгісі ең ұқсас жолдар мәндер үлгісінің де жақын екенін көрсетеді"
      )
    }
  )

  tasks[[length(tasks) + 1]] <- make_task(
    task_id = "SIM_05",
    principle = "Similarity",
    visualization_type = "Treemap",
    question_type = "E. Ranking",
    question_text = "Қай реттілік түстік топтарды жиынтық ауданы бойынша үлкеннен кішіге дұрыс орналастырады?",
    answer_choices = c(
      "Көк > Жасыл > Қызғылт сары",
      "Жасыл > Көк > Қызғылт сары",
      "Көк > Қызғылт сары > Жасыл",
      "Қызғылт сары > Жасыл > Көк"
    ),
    correct_answer = "Көк > Жасыл > Қызғылт сары",
    plot_fun = function() {
      data <- data.frame(
        group = c(rep("Көк", 3), rep("Жасыл", 3), rep("Қызғылт сары", 3)),
        label = c("B1", "B2", "B3", "G1", "G2", "G3", "O1", "O2", "O3"),
        value = c(16, 14, 12, 11, 10, 9, 7, 6, 5),
        fill = c(rep(clr["blue"], 3), rep(clr["sage"], 3), rep(clr["coral"], 3)),
        stringsAsFactors = FALSE
      )
      plot_treemap_generic(
        data,
        title = "Түс ұқсастығы бойынша ұйымдастырылған ағаш картасы",
        subtitle = "Әр түспен анықталған топ алып жатқан жалпы ауданды салыстырыңыз"
      )
    }
  )

  # ---- Proximity ----
  tasks[[length(tasks) + 1]] <- make_task(
    task_id = "PROX_01",
    principle = "Proximity",
    visualization_type = "Scatterplot",
    question_type = "I. Group perception based on proximity",
    question_text = "Неше топ көріп тұрсыз?",
    answer_choices = c("3", "4", "5", "6"),
    correct_answer = "4",
    plot_fun = function() {
      data <- dplyr::bind_rows(
        curve_points(c(1.5, 1.9, 2.2, 2.4), c(6.5, 6.0, 6.4, 5.8), color = clr["blue"], size = 4.5),
        curve_points(c(6.1, 6.5, 6.8, 7.2), c(6.2, 5.7, 6.1, 5.6), color = clr["blue"], size = 4.5),
        curve_points(c(2.0, 2.3, 2.6, 2.9), c(3.0, 2.5, 3.2, 2.8), color = clr["blue"], size = 4.5),
        curve_points(c(6.5, 6.8, 7.1, 7.4), c(2.8, 3.3, 2.6, 3.1), color = clr["blue"], size = 4.5)
      )
      plot_scatter_generic(
        data,
        title = "Бөлінген кластерлері бар нүктелік диаграмма",
        subtitle = "Тек қашықтықтың өзі топтау құрылымын жасайды",
        xlim = c(0.8, 8.2),
        ylim = c(2.0, 7.0),
        show_axes = FALSE
      )
    }
  )

  tasks[[length(tasks) + 1]] <- make_task(
    task_id = "PROX_02",
    principle = "Proximity",
    visualization_type = "Bubble chart",
    question_type = "F. Detecting anomaly or outlier",
    question_text = "Қай кластер басқаларынан ең анық бөлінген?",
    answer_choices = c("Жоғарғы оң жақтағы кластер", "Орталық кластер", "Төменгі сол жақтағы кластер", "Төменгі оң жақтағы кластер"),
    correct_answer = "Жоғарғы оң жақтағы кластер",
    plot_fun = function() {
      data <- dplyr::bind_rows(
        data.frame(x = c(2.2, 2.8, 3.1), y = c(2.4, 3.0, 2.7), color = unname(clr["blue"]), outline = unname(clr["navy"]), size = c(10, 13, 11), row.names = NULL),
        data.frame(x = c(4.7, 5.3, 5.8), y = c(4.4, 5.0, 4.7), color = unname(clr["sage"]), outline = unname(clr["navy"]), size = c(14, 12, 10), row.names = NULL),
        data.frame(x = c(6.0, 6.5, 7.0), y = c(2.5, 3.2, 2.7), color = unname(clr["coral"]), outline = unname(clr["navy"]), size = c(10, 14, 11), row.names = NULL),
        data.frame(x = c(8.0, 8.4), y = c(6.8, 7.3), color = unname(clr["gold"]), outline = unname(clr["navy"]), size = c(9, 12), row.names = NULL)
      )
      plot_bubble_generic(
        data,
        title = "Кластерленген орналасуы бар көпіршік диаграммасы",
        subtitle = "Бір кластер қалғандарынан едәуір алшақ орналасқан",
        xlim = c(1.2, 9.1),
        ylim = c(1.8, 7.8),
        show_axes = FALSE
      )
    }
  )

  tasks[[length(tasks) + 1]] <- make_task(
    task_id = "PROX_03",
    principle = "Proximity",
    visualization_type = "Heatmap",
    question_type = "G. Determining a range",
    question_text = "Орталық кластердегі мәндердің көбі қай аралыққа жатады?",
    answer_choices = c("10-20", "20-30", "30-40", "40-50"),
    correct_answer = "40-50",
    plot_fun = function() {
      mat <- matrix(
        c(
          18, 21, 19, 22, 17,
          20, 43, 47, 44, 21,
          18, 45, 48, 46, 19,
          17, 42, 44, 43, 18,
          16, 19, 20, 18, 15
        ),
        nrow = 5,
        byrow = TRUE,
        dimnames = list(paste0("R", 1:5), paste0("C", 1:5))
      )
      plot_heatmap_generic(
        as_heatmap_df(mat),
        title = "Ықшам орталық блогы бар жылу картасы",
        subtitle = "Бір-біріне жақын жоғары мәнді ұяшықтар орталық кластер құрайды"
      )
    }
  )

  tasks[[length(tasks) + 1]] <- make_task(
    task_id = "PROX_04",
    principle = "Proximity",
    visualization_type = "Bar chart",
    question_type = "I. Group perception based on proximity",
    question_text = "Қай санаттар арақашықтығына байланысты бір топқа жататын сияқты көрінеді?",
    answer_choices = c("A және B", "B және C", "C және D", "D және E"),
    correct_answer = "C және D",
    plot_fun = function() {
      data <- data.frame(
        category = LETTERS[1:5],
        value = c(17, 14, 15, 16, 13),
        x_pos = c(1.0, 3.0, 6.2, 6.9, 10.0),
        fill = rep(clr["blue"], 5),
        stringsAsFactors = FALSE
      )
      plot_bar_generic(
        data,
        title = "Аралығы біркелкі емес бағанды диаграмма",
        subtitle = "Бір жұп қалғандарына қарағанда әлдеқайда жақын орналасқан"
      )
    }
  )

  tasks[[length(tasks) + 1]] <- make_task(
    task_id = "PROX_05",
    principle = "Proximity",
    visualization_type = "Bubble chart",
    question_type = "B. Finding the minimum value",
    question_text = "Сол жақ кластер ішіндегі ең кішкентай белгіленген көпіршік қайсысы?",
    answer_choices = c("A", "B", "C", "D"),
    correct_answer = "C",
    plot_fun = function() {
      data <- data.frame(
        x = c(2.0, 2.7, 1.6, 6.6, 7.3),
        y = c(5.8, 5.0, 4.5, 5.5, 4.8),
        color = c(clr["blue"], clr["blue"], clr["blue"], clr["sage"], clr["sage"]),
        outline = unname(clr["navy"]),
        size = c(18, 12, 6, 14, 10),
        label = c("A", "B", "C", "D", "E"),
        row.names = NULL,
        stringsAsFactors = FALSE
      )
      plot_bubble_generic(
        data,
        title = "Сол жақтағы кластері бар көпіршік диаграммасы",
        subtitle = "Алдымен кеңістіктегі жақындыққа сүйеніп, содан кейін сол топ ішіндегі көпіршіктердің өлшемін салыстырыңыз",
        xlim = c(0.8, 8.1),
        ylim = c(3.8, 6.6),
        show_axes = FALSE,
        show_labels = TRUE
      )
    }
  )

  # ---- Symmetry ----
  tasks[[length(tasks) + 1]] <- make_task(
    task_id = "SYM_01",
    principle = "Symmetry",
    visualization_type = "Scatterplot",
    question_type = "J. Subjective organization and trend perception",
    question_text = "Қай үлгі тұрақтырақ және симметриялырақ көрінеді?",
    answer_choices = c("A үлгісі", "B үлгісі", "Екі үлгі де", "Екеуі де емес"),
    correct_answer = "A үлгісі",
    plot_fun = function() {
      pattern_a <- data.frame(
        x = c(3.0, 4.0, 5.0, 6.0, 7.0, 4.2, 5.0, 5.8),
        y = c(4.0, 5.6, 6.4, 5.6, 4.0, 3.0, 2.6, 3.0),
        color = unname(clr["blue"]),
        outline = unname(clr["navy"]),
        size = 4.4,
        panel = "A үлгісі",
        row.names = NULL,
        stringsAsFactors = FALSE
      )
      pattern_b <- data.frame(
        x = c(3.0, 3.8, 5.0, 6.4, 7.5, 4.3, 5.4, 6.4),
        y = c(4.0, 5.8, 6.6, 5.2, 3.7, 2.8, 2.4, 3.1),
        color = unname(clr["coral"]),
        outline = unname(clr["navy"]),
        size = 4.4,
        panel = "B үлгісі",
        row.names = NULL,
        stringsAsFactors = FALSE
      )
      plot_scatter_generic(
        dplyr::bind_rows(pattern_a, pattern_b),
        title = "Екі нүктелік орналасу",
        subtitle = "Олардың жалпы тепе-теңдігі мен айна тәрізді реттілігін салыстырыңыз",
        xlim = c(2.2, 8.0),
        ylim = c(2.0, 7.0),
        show_axes = FALSE
      )
    }
  )

  tasks[[length(tasks) + 1]] <- make_task(
    task_id = "SYM_02",
    principle = "Symmetry",
    visualization_type = "Heatmap",
    question_type = "J. Subjective organization and trend perception",
    question_text = "Түс матрицасы негізгі диагональ бойынша симметриялы ма?",
    answer_choices = c("Иә", "Жоқ", "Тек ортасына қатысты", "Анықтау қиын"),
    correct_answer = "Иә",
    plot_fun = function() {
      mat <- matrix(
        c(
          50, 32, 24, 18, 12,
          32, 48, 30, 22, 18,
          24, 30, 46, 30, 24,
          18, 22, 30, 48, 32,
          12, 18, 24, 32, 50
        ),
        nrow = 5,
        byrow = TRUE,
        dimnames = list(LETTERS[1:5], LETTERS[1:5])
      )
      plot_heatmap_generic(
        as_heatmap_df(mat),
        title = "Симметриялы матрицаның жылу картасы",
        subtitle = "Мәндер негізгі диагональ бойынша айнадай қайталанады"
      )
    }
  )

  tasks[[length(tasks) + 1]] <- make_task(
    task_id = "SYM_03",
    principle = "Symmetry",
    visualization_type = "Bar chart",
    question_type = "H. Detecting similar values",
    question_text = "Симметрияға байланысты қай шеткі санаттар жұбының биіктігі бірдей?",
    answer_choices = c("A және E", "A және B", "B және C", "C және E"),
    correct_answer = "A және E",
    plot_fun = function() {
      data <- data.frame(
        category = LETTERS[1:5],
        value = c(12, 18, 24, 18, 12),
        fill = c(clr["blue"], clr["mist"], clr["gold"], clr["mist"], clr["blue"]),
        stringsAsFactors = FALSE
      )
      plot_bar_generic(
        data,
        title = "Айнадай қайталанған баған биіктіктері",
        subtitle = "Шеткі бағандар ортасына қатысты бір-бірін қайталайды"
      )
    }
  )

  tasks[[length(tasks) + 1]] <- make_task(
    task_id = "SYM_04",
    principle = "Symmetry",
    visualization_type = "Bubble chart",
    question_type = "D. Estimating the difference",
    question_text = "Орталық көпіршік x = -1 және x = 1 нүктелеріндегі көпіршіктерден шамамен қаншаға үлкен?",
    answer_choices = c("4", "6", "8", "10"),
    correct_answer = "8",
    plot_fun = function() {
      data <- data.frame(
        x = c(-2, -1, 0, 1, 2),
        y = c(5, 5, 5, 5, 5),
        color = c(clr["mist"], clr["blue"], clr["gold"], clr["blue"], clr["mist"]),
        outline = unname(clr["navy"]),
        size = c(8, 12, 20, 12, 8),
        label = c("8", "12", "20", "12", "8"),
        row.names = NULL,
        stringsAsFactors = FALSE
      )
      plot_bubble_generic(
        data,
        title = "Симметриялы көпіршік өлшемдері",
        subtitle = "Көпіршіктердің ішіндегі сандық мәндерді оқыңыз",
        xlim = c(-2.8, 2.8),
        ylim = c(4.0, 6.0),
        show_axes = TRUE,
        show_labels = FALSE,
        augment_fn = function(p, df) {
          p +
            ggplot2::geom_text(
              ggplot2::aes(label = label),
              color = clr["navy"],
              family = "Segoe UI",
              fontface = "bold",
              size = 4.1,
              vjust = 0.35
            ) +
            ggplot2::scale_x_continuous(breaks = c(-2, -1, 0, 1, 2)) +
            ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
        }
      )
    }
  )

  tasks[[length(tasks) + 1]] <- make_task(
    task_id = "SYM_05",
    principle = "Symmetry",
    visualization_type = "Line chart",
    question_type = "C. Comparing two categories",
    question_text = "Қай жақ жоғары шекті мәнге жетеді?",
    answer_choices = c("Сол жақ", "Оң жақ", "Екі жақта да бірдей", "Екі жақта да шек жоқ"),
    correct_answer = "Екі жақта да бірдей",
    plot_fun = function() {
      data <- data.frame(
        x = 1:6,
        y = c(4, 8, 10, 10, 8, 4),
        series = "Айнадай үрдіс",
        display_color = unname(clr["blue"]),
        row.names = NULL,
        stringsAsFactors = FALSE
      )
      plot_line_generic(
        data,
        title = "Симметриялы сызықтық профиль",
        subtitle = "Сол және оң жартысы бір-бірін айнадай қайталайды"
      )
    }
  )

  # ---- Continuity ----
  tasks[[length(tasks) + 1]] <- make_task(
    task_id = "CONT_01",
    principle = "Continuity",
    visualization_type = "Line chart",
    question_type = "J. Subjective organization and trend perception",
    question_text = "Қай тізбек бір үздіксіз үрдіс ретінде қабылданады?",
    answer_choices = c("Көк тізбек", "Қызғылт сары тізбек", "Екі тізбек те", "Ешқайсысы"),
    correct_answer = "Көк тізбек",
    plot_fun = function() {
      data <- data.frame(
        x = rep(1:6, 2),
        y = c(2, 4, 6, NA, 10, 12, 5, 7, 3, 8, 4, 7),
        series = rep(c("Көк тізбек", "Қызғылт сары тізбек"), each = 6),
        display_color = c(rep(clr["blue"], 6), rep(clr["coral"], 6)),
        stringsAsFactors = FALSE
      )
      plot_line_generic(
        data,
        title = "Үзілген және тұрақсыз тізбектер",
        subtitle = "Бір тізбек үзіліске қарамастан біртұтас жоғарылайтын ағым ретінде оқылады"
      )
    }
  )

  tasks[[length(tasks) + 1]] <- make_task(
    task_id = "CONT_02",
    principle = "Continuity",
    visualization_type = "Scatterplot",
    question_type = "J. Subjective organization and trend perception",
    question_text = "Белгіленген қай топ бір үздіксіз жолмен жүреді?",
    answer_choices = c("A тобы", "B тобы", "C тобы", "Топтардың ешқайсысы"),
    correct_answer = "B тобы",
    plot_fun = function() {
      group_a <- dplyr::mutate(curve_points(c(1.5, 2.0, 2.4, 2.8), c(6.2, 5.1, 6.4, 4.8), color = clr["mist"], size = 4.4), group = "A")
      group_b <- dplyr::mutate(curve_points(c(4.0, 4.6, 5.1, 5.7, 6.3), c(3.1, 3.8, 4.7, 5.5, 6.1), color = clr["blue"], size = 4.6), group = "B", path_group = "trend")
      group_c <- dplyr::mutate(curve_points(c(7.1, 7.5, 7.9, 8.3), c(6.0, 5.2, 4.4, 5.6), color = clr["sand"], size = 4.4), group = "C")
      data <- dplyr::bind_rows(group_a, group_b, group_c)
      plot_scatter_generic(
        data,
        title = "Құрылымы әртүрлі нүктелер топтары",
        subtitle = "Бір жиын басқаларына қарағанда әлдеқайда күшті үздіксіз траектория құрайды",
        xlim = c(1.0, 8.8),
        ylim = c(2.5, 6.8),
        show_axes = FALSE,
        connect = TRUE,
        augment_fn = function(p, df) {
          p +
            ggplot2::annotate("text", x = 2.1, y = 6.7, label = "A", family = "Segoe UI", size = 5, color = clr["navy"], fontface = "bold") +
            ggplot2::annotate("text", x = 5.2, y = 6.7, label = "B", family = "Segoe UI", size = 5, color = clr["navy"], fontface = "bold") +
            ggplot2::annotate("text", x = 7.7, y = 6.7, label = "C", family = "Segoe UI", size = 5, color = clr["navy"], fontface = "bold")
        }
      )
    }
  )

  tasks[[length(tasks) + 1]] <- make_task(
    task_id = "CONT_03",
    principle = "Continuity",
    visualization_type = "Line chart",
    question_type = "F. Detecting anomaly or outlier",
    question_text = "Жалпы үздіксіз үлгіні айқын бұзатын нүкте бар ма?",
    answer_choices = c("Жоқ", "Иә, A сериясында", "Иә, B сериясында", "Иә, екі серияда да"),
    correct_answer = "Иә, B сериясында",
    plot_fun = function() {
      data <- data.frame(
        x = rep(1:6, 2),
        y = c(3, 4, 5, 6, 7, 8, 2, 3, 4, 9, 5, 6),
        series = rep(c("A сериясы", "B сериясы"), each = 6),
        display_color = c(rep(clr["blue"], 6), rep(clr["coral"], 6)),
        stringsAsFactors = FALSE
      )
      plot_line_generic(
        data,
        title = "Екі уақыттық тізбек",
        subtitle = "Бір тізбекте жергілікті үздіксіздікті айқын бұзатын үзіліс бар",
        augment_fn = function(p, df) {
          p +
            ggplot2::annotate("text", x = 6.1, y = 8.1, label = "A сериясы", color = clr["blue"], family = "Segoe UI", size = 4.1, hjust = 0) +
            ggplot2::annotate("text", x = 6.1, y = 6.0, label = "B сериясы", color = clr["coral"], family = "Segoe UI", size = 4.1, hjust = 0)
        }
      )
    }
  )

  tasks[[length(tasks) + 1]] <- make_task(
    task_id = "CONT_04",
    principle = "Continuity",
    visualization_type = "Heatmap",
    question_type = "J. Subjective organization and trend perception",
    question_text = "Қай жолды бір үздіксіз жолақ ретінде ең оңай қадағалауға болады?",
    answer_choices = c("Негізгі диагональ", "Сыртқы жиек", "Ортаңғы баған", "Кездейсоқ бөліктер"),
    correct_answer = "Негізгі диагональ",
    plot_fun = function() {
      mat <- matrix(
        c(
          48, 40, 18, 12, 10, 8,
          39, 49, 41, 18, 12, 10,
          17, 40, 50, 42, 18, 11,
          11, 18, 41, 50, 39, 16,
          9, 12, 18, 41, 49, 38,
          8, 10, 12, 17, 40, 48
        ),
        nrow = 6,
        byrow = TRUE,
        dimnames = list(paste0("R", 1:6), paste0("C", 1:6))
      )
      plot_heatmap_generic(
        as_heatmap_df(mat),
        title = "Диагональды жолағы бар жылу картасы",
        subtitle = "Жоғары қарқынды ұяшықтар біртұтас байланысқан ағын құрайды"
      )
    }
  )

  tasks[[length(tasks) + 1]] <- make_task(
    task_id = "CONT_05",
    principle = "Continuity",
    visualization_type = "Line chart",
    question_type = "E. Ranking",
    question_text = "Қай реттілік үздіксіз үрдістің соңғы үш нүктесін үлкеннен кішіге дұрыс орналастырады?",
    answer_choices = c("6 > 5 > 4", "4 > 5 > 6", "5 > 6 > 4", "6 > 4 > 5"),
    correct_answer = "6 > 5 > 4",
    plot_fun = function() {
      data <- data.frame(
        x = 1:6,
        y = c(10, 12, 14, 17, 19, 21),
        series = "Өсуші үрдіс",
        display_color = unname(clr["blue"]),
        row.names = NULL,
        stringsAsFactors = FALSE
      )
      plot_line_generic(
        data,
        title = "Уақыт бойынша үздіксіз өсу",
        subtitle = "Соңғы бөлік сол монотонды бағытты жалғастырады"
      )
    }
  )

  tasks
}

# Validate the task bank early so incomplete task definitions are caught at
# startup instead of during a live participant session.
legacy_validate_task_bank <- function(task_bank) {
  issues <- character()

  if (length(task_bank) != 25) {
    issues <- c(issues, sprintf("Expected 25 tasks, found %s.", length(task_bank)))
  }

  task_ids <- vapply(task_bank, function(task) task$task_id, character(1))
  question_ids <- vapply(task_bank, function(task) task$question_id, character(1))
  principles <- vapply(task_bank, function(task) task$principle, character(1))
  viz_types <- vapply(task_bank, function(task) task$visualization_type, character(1))
  question_codes <- substring(vapply(task_bank, function(task) task$question_type, character(1)), 1, 1)

  if (anyDuplicated(task_ids)) {
    issues <- c(issues, "Task IDs must be unique.")
  }

  if (anyDuplicated(question_ids)) {
    issues <- c(issues, "Question IDs must be unique.")
  }

  principle_counts <- table(factor(principles, levels = GESTALT_PRINCIPLES))
  wrong_counts <- names(principle_counts)[principle_counts != 5]
  if (length(wrong_counts) > 0) {
    issues <- c(
      issues,
      paste(
        "Each Gestalt principle must contain exactly 5 tasks.",
        paste(sprintf("%s=%s", names(principle_counts), as.integer(principle_counts)), collapse = ", ")
      )
    )
  }

  if (!all(vapply(BASE_VIS_TYPES, function(type_name) any(grepl(type_name, viz_types, fixed = TRUE)), logical(1)))) {
    issues <- c(issues, "All required visualization types must appear at least once in the task bank.")
  }

  if (!any(vapply(task_bank, function(task) identical(task$renderer, "closure_triptych"), logical(1)))) {
    issues <- c(issues, "At least one closure task must use the three-chart comparison screen.")
  }

  if (!setequal(sort(unique(question_codes)), LETTERS[1:10])) {
    issues <- c(issues, "Question types A-J must all be represented in the task bank.")
  }

  for (task in task_bank) {
    if (!nzchar(task$task_id) || !nzchar(task$principle) || !nzchar(task$question_text)) {
      issues <- c(issues, paste("Task has missing required text fields:", task$task_id))
    }

    expected_prefix <- TASK_PREFIXES[[task$principle]]
    if (!is.null(expected_prefix) && !startsWith(task$task_id, expected_prefix)) {
      issues <- c(issues, paste("Task ID prefix does not match its Gestalt principle:", task$task_id))
    }

    if (!task$renderer %in% VALID_RENDERERS) {
      issues <- c(issues, paste("Task uses an unsupported renderer:", task$task_id))
    }

    if (!grepl("^[A-J]\\.", task$question_type)) {
      issues <- c(issues, paste("Task question type must use the A.-J. coding convention:", task$task_id))
    }

    if (length(task$answer_choices) < 2) {
      issues <- c(issues, paste("Task must have at least two answer choices:", task$task_id))
    }

    if (length(unique(task$answer_choices)) != length(task$answer_choices)) {
      issues <- c(issues, paste("Task answer choices must be unique:", task$task_id))
    }

    if (!task$correct_answer %in% task$answer_choices) {
      issues <- c(issues, paste("Correct answer is missing from answer choices:", task$task_id))
    }

    if (identical(task$renderer, "single_plot") && !is.function(task$plot_fun)) {
      issues <- c(issues, paste("Single-plot task is missing plot_fun:", task$task_id))
    }

    if (identical(task$renderer, "closure_triptych")) {
      renderer_checks <- c(
        is.function(task$scatter_fun),
        is.function(task$bar_fun),
        is.function(task$treemap_fun)
      )
      if (!all(renderer_checks)) {
        issues <- c(issues, paste("Closure comparison task is missing one or more plot functions:", task$task_id))
      }
    }

    if (identical(task$renderer, "single_plot") && is.function(task$plot_fun)) {
      plot_candidate <- tryCatch(task$plot_fun(), error = function(e) e)
      if (inherits(plot_candidate, "error") || !inherits(plot_candidate, "ggplot")) {
        issues <- c(issues, paste("Single-plot task does not return a valid ggplot object:", task$task_id))
      }
    }

    if (identical(task$renderer, "closure_triptych")) {
      plot_candidates <- list(
        scatter = tryCatch(task$scatter_fun(), error = function(e) e),
        bar = tryCatch(task$bar_fun(), error = function(e) e),
        treemap = tryCatch(task$treemap_fun(), error = function(e) e)
      )
      invalid_components <- names(plot_candidates)[!vapply(plot_candidates, inherits, logical(1), "ggplot")]
      if (length(invalid_components) > 0) {
        issues <- c(
          issues,
          paste(
            "Closure comparison task has invalid plot components:",
            task$task_id,
            paste(invalid_components, collapse = ", ")
          )
        )
      }
    }
  }

  if (length(issues) > 0) {
    stop(paste(c("Task bank validation failed:", issues), collapse = "\n"), call. = FALSE)
  }

  invisible(TRUE)
}

legacy_task_bank <- NULL
legacy_task_ids <- character()

# ---- Revised dataset/task architecture ----
clean_superstore_names <- function(nm) {
  nm <- gsub("([a-z0-9])([A-Z])", "\\1_\\2", nm)
  nm <- tolower(gsub("[^a-zA-Z0-9]+", "_", nm))
  nm <- gsub("^_+|_+$", "", nm)
  gsub("_+", "_", nm)
}

kz_month_label <- function(x) {
  month_lookup <- c(
    "01" = "Қаң", "02" = "Ақп", "03" = "Нау", "04" = "Сәу",
    "05" = "Мам", "06" = "Мау", "07" = "Шіл", "08" = "Там",
    "09" = "Қыр", "10" = "Қаз", "11" = "Қар", "12" = "Жел"
  )
  paste(unname(month_lookup[format(x, "%m")]), format(x, "%Y"))
}

DISPLAY_TRANSLATIONS <- c(
  "Central" = "Орталық",
  "East" = "Шығыс",
  "South" = "Оңтүстік",
  "West" = "Батыс",
  "Furniture" = "Жиһаз",
  "Office Supplies" = "Кеңсе тауарлары",
  "Technology" = "Технология",
  "Consumer" = "Тұтынушы",
  "Corporate" = "Корпоративтік",
  "Home Office" = "Үй кеңсесі",
  "Bookcases" = "Кітап сөрелері",
  "Chairs" = "Орындықтар",
  "Furnishings" = "Интерьер бұйымдары",
  "Tables" = "Үстелдер",
  "Appliances" = "Тұрмыстық техника",
  "Art" = "Кеңсе өнері",
  "Binders" = "Бумажкалар",
  "Envelopes" = "Конверттер",
  "Fasteners" = "Бекіткіштер",
  "Labels" = "Жапсырмалар",
  "Paper" = "Қағаз",
  "Storage" = "Сақтау бұйымдары",
  "Supplies" = "Жабдықтар",
  "Accessories" = "Аксессуарлар",
  "Copiers" = "Көшірме құрылғылары",
  "Machines" = "Жабдық машиналары",
  "Phones" = "Телефондар"
)

translate_display_values <- function(x) {
  values <- as.character(x)
  translated <- unname(DISPLAY_TRANSLATIONS[values])
  translated[is.na(translated)] <- values[is.na(translated)]
  translated
}

build_demo_superstore <- function() {
  subcategory_map <- data.frame(
    category = c(
      rep("Furniture", 4),
      rep("Office Supplies", 9),
      rep("Technology", 4)
    ),
    sub_category = c(
      "Bookcases", "Chairs", "Furnishings", "Tables",
      "Appliances", "Art", "Binders", "Envelopes", "Fasteners",
      "Paper", "Storage", "Supplies", "Labels",
      "Accessories", "Copiers", "Machines", "Phones"
    ),
    base_sales = c(220, 310, 180, 260, 240, 120, 290, 90, 70, 160, 210, 95, 110, 260, 420, 330, 390),
    base_margin = c(0.16, 0.18, 0.20, 0.11, 0.15, 0.24, 0.19, 0.22, 0.13, 0.25, 0.17, 0.10, 0.18, 0.21, 0.27, 0.12, 0.23),
    stringsAsFactors = FALSE
  )

  grid <- expand.grid(
    order_month = seq(as.Date("2021-01-01"), as.Date("2023-12-01"), by = "month"),
    region = c("West", "East", "Central", "South"),
    segment = c("Consumer", "Corporate", "Home Office"),
    sub_category = subcategory_map$sub_category,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  grid <- dplyr::left_join(grid, subcategory_map, by = "sub_category")
  row_id <- seq_len(nrow(grid))

  region_mult <- c(West = 1.18, East = 1.08, Central = 0.94, South = 0.89)
  segment_mult <- c(Consumer = 1.10, Corporate = 1.04, `Home Office` = 0.93)
  seasonal_mult <- c(1.02, 1.00, 1.03, 1.05, 1.06, 1.08, 1.07, 1.06, 1.09, 1.12, 1.15, 1.18)
  year_mult <- c("2021" = 0.94, "2022" = 1.00, "2023" = 1.08)
  month_idx <- as.integer(format(grid$order_month, "%m"))
  year_idx <- format(grid$order_month, "%Y")
  wave <- sin((row_id %% 12) / 12 * pi * 2) * 0.06 + cos((row_id %% 7) / 7 * pi * 2) * 0.04

  sales <- round(
    grid$base_sales *
      unname(region_mult[grid$region]) *
      unname(segment_mult[grid$segment]) *
      seasonal_mult[month_idx] *
      unname(year_mult[year_idx]) *
      (1 + wave),
    2
  )
  quantity <- pmax(1L, as.integer(round((grid$base_sales / 55) + (row_id %% 5) + month_idx / 4)))
  discount <- pmin(
    0.35,
    pmax(
      0,
      round(
        ifelse(grid$segment == "Consumer", 0.08, ifelse(grid$segment == "Corporate", 0.05, 0.03)) +
          ifelse(grid$region == "South", 0.03, ifelse(grid$region == "Central", 0.02, 0)) +
          ((row_id %% 4) - 1) * 0.015,
        2
      )
    )
  )
  profit <- round(sales * (grid$base_margin - discount * 0.85) - quantity * 4.5, 2)

  data.frame(
    row_id = row_id,
    order_id = sprintf("CA-%04d-%05d", 2021 + (as.integer(year_idx) - 2021), row_id),
    order_date = grid$order_month + ((row_id %% 21) + 1),
    ship_date = grid$order_month + ((row_id %% 21) + 4),
    ship_mode = c("Standard Class", "Second Class", "First Class", "Same Day")[(row_id %% 4) + 1],
    customer_id = sprintf("C-%04d", (row_id %% 420) + 1),
    customer_name = sprintf("Customer %03d", (row_id %% 420) + 1),
    segment = grid$segment,
    country = "United States",
    city = c("Seattle", "New York", "Chicago", "Houston")[match(grid$region, c("West", "East", "Central", "South"))],
    state = c("Washington", "New York", "Illinois", "Texas")[match(grid$region, c("West", "East", "Central", "South"))],
    postal_code = c(98101, 10001, 60601, 77001)[match(grid$region, c("West", "East", "Central", "South"))],
    region = grid$region,
    product_id = sprintf("P-%03d", match(grid$sub_category, subcategory_map$sub_category)),
    category = grid$category,
    sub_category = grid$sub_category,
    product_name = paste(grid$sub_category, "Model", (row_id %% 9) + 1),
    sales = sales,
    quantity = quantity,
    discount = discount,
    profit = profit,
    stringsAsFactors = FALSE
  )
}

load_experiment_dataset <- function(file_path = SUPERSTORE_FILE) {
  dir.create(dirname(file_path), recursive = TRUE, showWarnings = FALSE)

  if (file.exists(file_path)) {
    df <- readr::read_csv(file_path, show_col_types = FALSE, progress = FALSE)
    names(df) <- clean_superstore_names(names(df))
  } else {
    df <- build_demo_superstore()
  }

  required_cols <- c("order_date", "region", "segment", "category", "sub_category", "sales", "profit", "quantity", "discount")
  if (!all(required_cols %in% names(df))) {
    df <- build_demo_superstore()
  }

  df$order_date <- as.Date(df$order_date)
  if (all(is.na(df$order_date))) {
    df <- build_demo_superstore()
  }

  for (col_name in intersect(c("sales", "profit", "quantity", "discount"), names(df))) {
    df[[col_name]] <- suppressWarnings(as.numeric(df[[col_name]]))
  }

  dplyr::filter(
    df,
    !is.na(order_date),
    !is.na(region),
    !is.na(segment),
    !is.na(category),
    !is.na(sub_category),
    !is.na(sales),
    !is.na(profit),
    !is.na(quantity),
    !is.na(discount)
  )
}

summarise_one <- function(df, group_col, value_col, descending = TRUE) {
  out <- dplyr::summarise(
    dplyr::group_by(df, .data[[group_col]]),
    value = sum(.data[[value_col]], na.rm = TRUE),
    .groups = "drop"
  )
  names(out)[1] <- "label"
  out$label <- translate_display_values(out$label)
  dplyr::arrange(out, if (descending) dplyr::desc(value) else value)
}

summarise_two <- function(df, row_col, col_col, value_col, descending = TRUE) {
  out <- dplyr::summarise(
    dplyr::group_by(df, .data[[row_col]], .data[[col_col]]),
    value = sum(.data[[value_col]], na.rm = TRUE),
    .groups = "drop"
  )
  names(out)[1:2] <- c("row", "col")
  out$row <- translate_display_values(out$row)
  out$col <- translate_display_values(out$col)
  out$label <- paste(out$row, out$col, sep = " / ")
  dplyr::arrange(out, if (descending) dplyr::desc(value) else value)
}

prepare_experiment_data <- function(df) {
  df$order_month <- as.Date(format(df$order_date, "%Y-%m-01"))

  month_summary <- dplyr::summarise(
    dplyr::group_by(df, order_month),
    sales = sum(sales, na.rm = TRUE),
    profit = sum(profit, na.rm = TRUE),
    quantity = sum(quantity, na.rm = TRUE),
    .groups = "drop"
  )
  month_summary <- dplyr::arrange(month_summary, order_month)
  month_summary <- dplyr::mutate(month_summary, index = dplyr::row_number(), label = kz_month_label(order_month))
  month_last12 <- utils::tail(month_summary, 12)
  month_last12$index <- seq_len(nrow(month_last12))

  month_region_sales <- dplyr::summarise(
    dplyr::group_by(df, order_month, region),
    value = sum(sales, na.rm = TRUE),
    .groups = "drop"
  )
  month_region_sales <- dplyr::filter(month_region_sales, order_month %in% month_last12$order_month)
  month_region_sales <- dplyr::left_join(month_region_sales, dplyr::select(month_last12, order_month, index, label), by = "order_month")
  month_region_sales$region <- translate_display_values(month_region_sales$region)
  month_region_sales$series <- month_region_sales$region
  month_region_sales$panel <- month_region_sales$region

  month_region_profit <- dplyr::summarise(
    dplyr::group_by(df, order_month, region),
    value = sum(profit, na.rm = TRUE),
    .groups = "drop"
  )
  month_region_profit <- dplyr::filter(month_region_profit, order_month %in% month_last12$order_month)
  month_region_profit <- dplyr::left_join(month_region_profit, dplyr::select(month_last12, order_month, index, label), by = "order_month")
  month_region_profit$region <- translate_display_values(month_region_profit$region)
  month_region_profit$series <- month_region_profit$region
  month_region_profit$panel <- month_region_profit$region

  scatter_subcategory <- dplyr::summarise(
    dplyr::group_by(df, sub_category),
    sales = sum(sales, na.rm = TRUE),
    profit = sum(profit, na.rm = TRUE),
    quantity = sum(quantity, na.rm = TRUE),
    .groups = "drop"
  )
  scatter_subcategory <- dplyr::slice_max(scatter_subcategory, order_by = sales, n = 8, with_ties = FALSE)
  scatter_subcategory$sub_category <- translate_display_values(scatter_subcategory$sub_category)

  list(
    raw = df,
    region_sales = summarise_one(df, "region", "sales", TRUE),
    region_profit = summarise_one(df, "region", "profit", FALSE),
    category_sales = summarise_one(df, "category", "sales", TRUE),
    category_profit = summarise_one(df, "category", "profit", FALSE),
    segment_sales = summarise_one(df, "segment", "sales", TRUE),
    segment_profit = summarise_one(df, "segment", "profit", FALSE),
    subcategory_sales_top = dplyr::slice_max(summarise_one(df, "sub_category", "sales", TRUE), order_by = value, n = 8, with_ties = FALSE),
    subcategory_profit_low = dplyr::slice_min(summarise_one(df, "sub_category", "profit", FALSE), order_by = value, n = 8, with_ties = FALSE),
    region_category_sales = summarise_two(df, "region", "category", "sales", TRUE),
    region_category_profit = summarise_two(df, "region", "category", "profit", FALSE),
    category_segment_sales = summarise_two(df, "category", "segment", "sales", TRUE),
    region_segment_sales = summarise_two(df, "region", "segment", "sales", TRUE),
    month_last12 = month_last12,
    month_region_sales_last12 = month_region_sales,
    month_region_profit_last12 = month_region_profit,
    scatter_subcategory = scatter_subcategory
  )
}

plot_bar_generic <- function(
  data,
  title,
  subtitle = NULL,
  y_max = NULL,
  compact = FALSE,
  show_value_labels = TRUE,
  augment_fn = NULL
) {
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

  if (!is.null(y_max) && is.finite(y_max)) {
    if (y_max >= 0) {
      y_max_raw <- max(y_max_raw, y_max)
    }
    if (y_max <= 0) {
      y_min_raw <- min(y_min_raw, y_max)
    }
  }

  span <- y_max_raw - y_min_raw
  if (!is.finite(span) || span == 0) {
    span <- max(abs(c(y_min_raw, y_max_raw, 1)), na.rm = TRUE)
  }

  pad <- span * if (compact) 0.08 else 0.12
  label_pad <- span * if (compact) 0.025 else 0.04
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
  if (compact && nrow(data) > 4) {
    keep_idx <- unique(c(seq(1, nrow(data), by = 2), nrow(data)))
    x_labels[!seq_along(x_labels) %in% keep_idx] <- ""
  }

  p <- ggplot2::ggplot(data, ggplot2::aes(x = x_pos, y = value, fill = fill)) +
    ggplot2::geom_hline(yintercept = 0, color = "#9FB3C2", linewidth = 0.45) +
    ggplot2::geom_col(width = 0.72, color = "#FFFFFF", linewidth = 0.45, alpha = 0.95) +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_x_continuous(
      breaks = data$x_pos,
      labels = x_labels,
      expand = ggplot2::expansion(mult = c(0.03, 0.05))
    ) +
    ggplot2::scale_y_continuous(
      limits = c(y_lower, y_upper),
      expand = ggplot2::expansion(mult = c(0, 0))
    ) +
    ggplot2::labs(title = NULL, subtitle = NULL, x = NULL, y = "Мән") +
    experiment_theme() +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        size = if (compact) 8 else 10,
        angle = if (compact) 0 else 18,
        hjust = 0.5
      ),
      axis.text.y = ggplot2::element_text(size = if (compact) 8 else 10),
      axis.title.y = ggplot2::element_text(size = if (compact) 9 else 10.5)
    )

  if (show_value_labels) {
    p <- p +
      ggplot2::geom_text(
        data = data,
        ggplot2::aes(x = x_pos, y = label_y, label = label_text, vjust = label_vjust),
        inherit.aes = FALSE,
        color = COLORS["navy"],
        family = "Segoe UI",
        fontface = "bold",
        size = if (compact) 3.1 else 4
      )
  }

  if (!is.null(augment_fn)) {
    p <- augment_fn(p, data)
  }

  p
}

bar_plot_from_summary <- function(df, fill_color = COLORS["blue"], compact = FALSE, show_value_labels = !compact) {
  plot_bar_generic(
    data.frame(
      category = df$label,
      value = round(df$value, 0),
      fill = rep(unname(fill_color), nrow(df)),
      stringsAsFactors = FALSE
    ),
    title = NULL,
    subtitle = NULL,
    y_max = if (all(df$value <= 0, na.rm = TRUE)) min(df$value) * 1.15 else max(df$value) * 1.15,
    compact = compact,
    show_value_labels = show_value_labels
  )
}

bubble_plot_from_summary <- function(df, show_labels = FALSE) {
  plot_bubble_generic(
    data.frame(
      x = seq_len(nrow(df)),
      y = df$value,
      color = rep(unname(COLORS["blue"]), nrow(df)),
      outline = rep(unname(COLORS["navy"]), nrow(df)),
      size = scales::rescale(abs(df$value), to = c(9, 18)),
      label = if (show_labels) df$label else NA_character_,
      stringsAsFactors = FALSE
    ),
    title = NULL,
    subtitle = NULL,
    xlim = c(0.5, nrow(df) + 0.5),
    ylim = c(min(df$value) * 0.9, max(df$value) * 1.08),
    show_axes = FALSE,
    show_labels = show_labels
  )
}

treemap_plot_from_summary <- function(df) {
  safe_df <- dplyr::filter(df, is.finite(value), value > 0)
  if (nrow(safe_df) == 0) {
    safe_df <- dplyr::mutate(df, value = abs(value) + 1)
  }

  plot_treemap_generic(
    data.frame(
      group = rep("Dataset", nrow(safe_df)),
      label = safe_df$label,
      value = safe_df$value,
      fill = rep(c(COLORS["blue"], COLORS["sage"], COLORS["gold"], COLORS["coral"], COLORS["plum"], COLORS["slate"], COLORS["rose"], COLORS["mist"]), length.out = nrow(safe_df)),
      stringsAsFactors = FALSE
    ),
    title = NULL,
    subtitle = NULL
  )
}

heatmap_plot_from_summary <- function(df, compact = FALSE, show_values = !compact) {
  plot_heatmap_generic(
    data.frame(row = df$row, col = df$col, value = df$value, stringsAsFactors = FALSE),
    title = NULL,
    subtitle = NULL,
    show_values = show_values,
    augment_fn = function(p, plot_df) {
      p + ggplot2::theme(
        axis.text.x = ggplot2::element_text(size = if (compact) 8 else 10),
        axis.text.y = ggplot2::element_text(size = if (compact) 8 else 10)
      )
    }
  )
}

line_plot_from_monthly <- function(df, value_col = "sales", compact = FALSE) {
  breaks <- if (compact) df$index[df$index %% 2 == 1 | df$index == max(df$index)] else df$index
  labels <- df$label[match(breaks, df$index)]

  plot_line_generic(
    data.frame(
      x = df$index,
      y = df[[value_col]],
      series = "Series",
      display_color = unname(COLORS["blue"]),
      stringsAsFactors = FALSE
    ),
    title = NULL,
    subtitle = NULL,
    augment_fn = function(p, plot_df) {
      p +
        ggplot2::scale_x_continuous(
          breaks = breaks,
          labels = labels,
          expand = ggplot2::expansion(mult = c(0.02, 0.05))
        ) +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(
            angle = if (compact) 0 else 35,
            hjust = 0.5,
            size = if (compact) 7.5 else 9
          ),
          axis.text.y = ggplot2::element_text(size = if (compact) 8 else 10),
          axis.title.x = ggplot2::element_blank(),
          axis.title.y = ggplot2::element_text(size = if (compact) 9 else 10.5)
        )
    }
  )
}

facet_line_plot_from_region <- function(df, compact = FALSE) {
  breaks <- sort(unique(df$index))
  if (compact && length(breaks) > 6) {
    breaks <- breaks[breaks %% 2 == 1 | breaks == max(breaks)]
  }
  labels <- df$label[match(breaks, df$index)]

  plot_line_generic(
    data.frame(
      x = df$index,
      y = df$value,
      series = df$series,
      panel = df$panel,
      display_color = unname(COLORS["blue"]),
      stringsAsFactors = FALSE
    ),
    title = NULL,
    subtitle = NULL,
    augment_fn = function(p, plot_df) {
      p +
        ggplot2::scale_x_continuous(
          breaks = breaks,
          labels = labels,
          expand = ggplot2::expansion(mult = c(0.02, 0.05))
        ) +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(
            angle = if (compact) 0 else 35,
            hjust = 0.5,
            size = if (compact) 7 else 9
          ),
          axis.text.y = ggplot2::element_text(size = if (compact) 8 else 10),
          axis.title.x = ggplot2::element_blank(),
          strip.text = ggplot2::element_text(size = if (compact) 9 else 11)
        )
    }
  )
}

scatter_plot_from_subcategory <- function(df) {
  plot_scatter_generic(
    data.frame(
      x = df$sales,
      y = df$profit,
      color = rep(unname(COLORS["blue"]), nrow(df)),
      outline = rep(unname(COLORS["navy"]), nrow(df)),
      size = scales::rescale(df$quantity, to = c(8, 16)),
      label = df$sub_category,
      stringsAsFactors = FALSE
    ),
    title = NULL,
    subtitle = NULL,
    xlim = c(min(df$sales) * 0.9, max(df$sales) * 1.08),
    ylim = c(min(df$profit) * 1.08, max(df$profit) * 1.08),
    show_axes = TRUE,
    show_labels = TRUE,
    augment_fn = function(p, plot_df) {
      p + ggplot2::labs(x = "Сату көлемі", y = "Пайда") +
        ggplot2::theme(panel.grid.major.x = ggplot2::element_line(color = "#E3EBF2", linewidth = 0.45))
    }
  )
}

monthly_heatmap_plot <- function(df, value_col = "sales", compact = FALSE, show_value_labels = !compact) {
  stopifnot(nrow(df) >= 12)

  short_month <- vapply(strsplit(as.character(df$label), " "), function(x) x[[1]], character(1))
  grid_df <- data.frame(
    row = factor(rep(c("3", "2", "1"), each = 4), levels = c("3", "2", "1")),
    col = factor(rep(c("1", "2", "3", "4"), times = 3), levels = c("1", "2", "3", "4")),
    value = df[[value_col]][1:12],
    month_label = short_month[1:12],
    stringsAsFactors = FALSE
  )

  plot_heatmap_generic(
    grid_df,
    title = NULL,
    subtitle = NULL,
    show_values = show_value_labels && !compact,
    augment_fn = function(p, plot_df) {
      p +
        ggplot2::geom_text(
          data = plot_df,
          ggplot2::aes(label = month_label),
          inherit.aes = TRUE,
          color = COLORS["navy"],
          family = "Segoe UI",
          fontface = "bold",
          size = if (compact) 3 else 3.3
        ) +
        ggplot2::labs(x = NULL, y = NULL) +
        ggplot2::theme(
          axis.text.x = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_blank(),
          axis.ticks = ggplot2::element_blank(),
          legend.position = if (compact) "none" else "right"
        )
    }
  )
}

pick_choices <- function(df, label_col = "label", order_col = "value", direction = c("max", "min"), n = 4) {
  direction <- match.arg(direction)
  ord <- if (identical(direction, "max")) order(df[[order_col]], decreasing = TRUE) else order(df[[order_col]], decreasing = FALSE)
  unique(df[[label_col]][ord])[seq_len(min(n, length(unique(df[[label_col]][ord]))))]
}

make_task <- function(
  task_id,
  principle,
  visualization_type,
  question_type,
  question_text,
  answer_choices,
  correct_answer,
  renderer = "single_plot",
  plot_fun = NULL,
  plot_funs = NULL,
  panel_labels = NULL
) {
  list(
    task_id = task_id,
    question_id = paste0("Q_", task_id),
    principle = principle,
    visualization_type = visualization_type,
    question_type = question_type,
    question_text = question_text,
    answer_choices = answer_choices,
    correct_answer = correct_answer,
    renderer = renderer,
    plot_fun = plot_fun,
    plot_funs = plot_funs,
    panel_labels = panel_labels
  )
}

build_task_bank <- function(data_store = prepare_experiment_data(load_experiment_dataset())) {
  d <- data_store
  tasks <- list()

  comparison_task <- function(task_id, principle, question_type, question_text, correct_answer, plot_funs) {
    make_task(
      task_id = task_id,
      principle = principle,
      visualization_type = "Comparison grid",
      question_type = question_type,
      question_text = question_text,
      answer_choices = LETTERS[seq_along(plot_funs)],
      correct_answer = correct_answer,
      renderer = "plot_grid",
      plot_funs = plot_funs,
      panel_labels = LETTERS[seq_along(plot_funs)]
    )
  }

  single_task <- function(task_id, principle, visualization_type, question_type, question_text, answer_choices, correct_answer, plot_fun) {
    make_task(
      task_id = task_id,
      principle = principle,
      visualization_type = visualization_type,
      question_type = question_type,
      question_text = question_text,
      answer_choices = answer_choices,
      correct_answer = correct_answer,
      renderer = "single_plot",
      plot_fun = plot_fun
    )
  }

  top_sales_cells <- pick_choices(d$region_category_sales, direction = "max")
  low_profit_cells <- pick_choices(d$region_category_profit, direction = "min")
  top_cat_seg_cells <- pick_choices(d$category_segment_sales, direction = "max")
  top_region_seg_cells <- pick_choices(d$region_segment_sales, direction = "max")
  top_month_sales <- pick_choices(d$month_last12, order_col = "sales", direction = "max")
  low_month_profit <- pick_choices(d$month_last12, order_col = "profit", direction = "min")
  top_month_quantity <- pick_choices(d$month_last12, order_col = "quantity", direction = "max")
  top_scatter_profit <- pick_choices(d$scatter_subcategory, label_col = "sub_category", order_col = "profit", direction = "max")

  tasks[[length(tasks) + 1]] <- comparison_task("CLO_01", "Closure", "J. Panel comparison", "Қай нұсқада ең жоғары мәнді табу жеңілірек?", "A", list(
    function() bar_plot_from_summary(d$region_sales, compact = TRUE, show_value_labels = FALSE),
    function() bubble_plot_from_summary(d$region_sales),
    function() treemap_plot_from_summary(d$region_sales)
  ))
  tasks[[length(tasks) + 1]] <- single_task("CLO_02", "Closure", "Bar chart", "A. Maximum", "Қай аймақ жоғары?", d$region_sales$label, d$region_sales$label[which.max(d$region_sales$value)], function() bar_plot_from_summary(d$region_sales))
  tasks[[length(tasks) + 1]] <- single_task("CLO_03", "Closure", "Treemap", "A. Maximum", "Қай санат үлкен?", d$category_sales$label, d$category_sales$label[which.max(d$category_sales$value)], function() treemap_plot_from_summary(d$category_sales))
  tasks[[length(tasks) + 1]] <- single_task("CLO_04", "Closure", "Heatmap", "C. Comparison", "Қай ұяшық жоғары?", top_sales_cells, top_sales_cells[1], function() heatmap_plot_from_summary(d$region_category_sales))
  tasks[[length(tasks) + 1]] <- single_task("CLO_05", "Closure", "Line chart", "A. Maximum", "Қай ай жоғары?", top_month_sales, top_month_sales[1], function() line_plot_from_monthly(d$month_last12, "sales"))

  tasks[[length(tasks) + 1]] <- comparison_task("SIM_01", "Similarity", "C. Comparison", "Қай нұсқада топтарды салыстыру жеңілірек?", "A", list(
    function() bar_plot_from_summary(d$segment_sales, COLORS["sage"], compact = TRUE, show_value_labels = FALSE),
    function() bubble_plot_from_summary(d$segment_sales),
    function() treemap_plot_from_summary(d$segment_sales)
  ))
  tasks[[length(tasks) + 1]] <- single_task("SIM_02", "Similarity", "Bar chart", "A. Maximum", "Қай сегмент жоғары?", d$segment_sales$label, d$segment_sales$label[which.max(d$segment_sales$value)], function() bar_plot_from_summary(d$segment_sales, COLORS["sage"]))
  tasks[[length(tasks) + 1]] <- single_task("SIM_03", "Similarity", "Bar chart", "B. Minimum", "Қай санат төмен?", d$category_profit$label, d$category_profit$label[which.min(d$category_profit$value)], function() bar_plot_from_summary(d$category_profit, COLORS["coral"]))
  tasks[[length(tasks) + 1]] <- single_task("SIM_04", "Similarity", "Scatterplot", "F. Outlier", "Қай белгіде пайда жоғары?", top_scatter_profit, top_scatter_profit[1], function() scatter_plot_from_subcategory(d$scatter_subcategory))
  tasks[[length(tasks) + 1]] <- single_task("SIM_05", "Similarity", "Heatmap", "C. Comparison", "Қай ұяшық жоғары?", top_cat_seg_cells, top_cat_seg_cells[1], function() heatmap_plot_from_summary(d$category_segment_sales))

  tasks[[length(tasks) + 1]] <- comparison_task("PROX_01", "Proximity", "D. Difference", "Қай нұсқада айырмашылық тез байқалады?", "A", list(
    function() bar_plot_from_summary(d$category_sales, compact = TRUE, show_value_labels = FALSE),
    function() treemap_plot_from_summary(d$category_sales),
    function() bubble_plot_from_summary(d$category_sales)
  ))
  tasks[[length(tasks) + 1]] <- single_task("PROX_02", "Proximity", "Bar chart", "B. Minimum", "Қай аймақ төмен?", d$region_profit$label, d$region_profit$label[which.min(d$region_profit$value)], function() bar_plot_from_summary(d$region_profit, COLORS["gold"]))
  tasks[[length(tasks) + 1]] <- single_task("PROX_03", "Proximity", "Bar chart", "B. Minimum", "Қай кіші санат төмен?", d$subcategory_profit_low$label[1:4], d$subcategory_profit_low$label[1], function() bar_plot_from_summary(d$subcategory_profit_low[1:6, ], COLORS["coral"]))
  tasks[[length(tasks) + 1]] <- single_task("PROX_04", "Proximity", "Heatmap", "F. Outlier", "Қай ұяшықта шығын бар?", low_profit_cells, low_profit_cells[1], function() heatmap_plot_from_summary(d$region_category_profit))
  tasks[[length(tasks) + 1]] <- single_task("PROX_05", "Proximity", "Bubble chart", "A. Maximum", "Қай аймақ жоғары?", d$region_sales$label, d$region_sales$label[which.max(d$region_sales$value)], function() bubble_plot_from_summary(d$region_sales, TRUE))

  tasks[[length(tasks) + 1]] <- comparison_task("SYM_01", "Symmetry", "C. Comparison", "Қай нұсқада ұяшықтарды салыстыру жеңілірек?", "A", list(
    function() heatmap_plot_from_summary(d$region_category_sales, compact = TRUE, show_values = FALSE),
    function() treemap_plot_from_summary(d$region_category_sales),
    function() bubble_plot_from_summary(dplyr::slice_max(d$region_category_sales, order_by = value, n = 8, with_ties = FALSE))
  ))
  tasks[[length(tasks) + 1]] <- single_task("SYM_02", "Symmetry", "Bar chart", "A. Maximum", "Қай санат жоғары?", d$category_sales$label, d$category_sales$label[which.max(d$category_sales$value)], function() bar_plot_from_summary(d$category_sales))
  tasks[[length(tasks) + 1]] <- single_task("SYM_03", "Symmetry", "Treemap", "A. Maximum", "Қай сегмент үлкен?", d$segment_sales$label, d$segment_sales$label[which.max(d$segment_sales$value)], function() treemap_plot_from_summary(d$segment_sales))
  tasks[[length(tasks) + 1]] <- single_task("SYM_04", "Symmetry", "Line chart", "B. Minimum", "Қай ай төмен?", low_month_profit, low_month_profit[1], function() line_plot_from_monthly(d$month_last12, "profit"))
  tasks[[length(tasks) + 1]] <- single_task("SYM_05", "Symmetry", "Heatmap", "C. Comparison", "Қай ұяшық жоғары?", top_region_seg_cells, top_region_seg_cells[1], function() heatmap_plot_from_summary(d$region_segment_sales))

  tasks[[length(tasks) + 1]] <- comparison_task("CONT_01", "Continuity", "J. Trend", "Қай нұсқада үрдісті көру жеңілірек?", "A", list(
    function() line_plot_from_monthly(d$month_last12, "sales", compact = TRUE),
    function() bar_plot_from_summary(dplyr::select(d$month_last12, label, value = sales), compact = TRUE, show_value_labels = FALSE),
    function() monthly_heatmap_plot(d$month_last12, "sales", compact = TRUE, show_value_labels = FALSE)
  ))
  tasks[[length(tasks) + 1]] <- single_task("CONT_02", "Continuity", "Line chart", "B. Minimum", "Қай ай төмен?", low_month_profit, low_month_profit[1], function() line_plot_from_monthly(d$month_last12, "profit"))
  tasks[[length(tasks) + 1]] <- single_task("CONT_03", "Continuity", "Line chart", "A. Maximum", "Қай ай жоғары?", top_month_quantity, top_month_quantity[1], function() line_plot_from_monthly(d$month_last12, "quantity"))
  tasks[[length(tasks) + 1]] <- single_task("CONT_04", "Continuity", "Line chart", "E. Ranking", "Қай аймақта шек жоғары?", unique(d$month_region_sales_last12$region), d$month_region_sales_last12$region[which.max(d$month_region_sales_last12$value)], function() facet_line_plot_from_region(d$month_region_sales_last12))
  region_last <- dplyr::slice_max(d$month_region_profit_last12, order_by = index, n = 1, by = region)
  tasks[[length(tasks) + 1]] <- single_task("CONT_05", "Continuity", "Line chart", "B. Minimum", "Қай аймақта соңғы мән төмен?", region_last$region, region_last$region[which.min(region_last$value)], function() facet_line_plot_from_region(d$month_region_profit_last12))

  tasks
}

validate_task_bank <- function(task_bank) {
  issues <- character()
  if (length(task_bank) != 25) {
    issues <- c(issues, sprintf("Expected 25 tasks, found %s.", length(task_bank)))
  }

  task_ids <- vapply(task_bank, function(task) task$task_id, character(1))
  question_ids <- vapply(task_bank, function(task) task$question_id, character(1))
  principles <- vapply(task_bank, function(task) task$principle, character(1))

  if (anyDuplicated(task_ids)) {
    issues <- c(issues, "Task IDs must be unique.")
  }
  if (anyDuplicated(question_ids)) {
    issues <- c(issues, "Question IDs must be unique.")
  }

  principle_counts <- table(factor(principles, levels = GESTALT_PRINCIPLES))
  if (any(principle_counts != 5)) {
    issues <- c(issues, "Each internal principle bucket must contain exactly 5 tasks.")
  }

  if (!any(vapply(task_bank, function(task) identical(task$renderer, "plot_grid"), logical(1)))) {
    issues <- c(issues, "At least one plot_grid task is required.")
  }

  for (task in task_bank) {
    if (!task$renderer %in% VALID_RENDERERS) {
      issues <- c(issues, paste("Unsupported renderer:", task$task_id))
    }
    if (!task$correct_answer %in% task$answer_choices) {
      issues <- c(issues, paste("Correct answer missing from choices:", task$task_id))
    }
    if (identical(task$renderer, "single_plot") && !is.function(task$plot_fun)) {
      issues <- c(issues, paste("single_plot task missing plot_fun:", task$task_id))
    }
    if (identical(task$renderer, "plot_grid")) {
      if (is.null(task$plot_funs) || length(task$plot_funs) < 2 || length(task$plot_funs) > 4) {
        issues <- c(issues, paste("plot_grid task must have 2-4 plots:", task$task_id))
      }
      if (length(task$panel_labels) != length(task$plot_funs)) {
        issues <- c(issues, paste("plot_grid labels mismatch:", task$task_id))
      }
    }
  }

  if (length(issues) > 0) {
    stop(paste(c("Task bank validation failed:", issues), collapse = "\n"), call. = FALSE)
  }

  invisible(TRUE)
}

EXPERIMENT_DATA <- prepare_experiment_data(load_experiment_dataset())
TASK_BANK <- build_task_bank(EXPERIMENT_DATA)
TASK_IDS <- vapply(TASK_BANK, function(task) task$task_id, character(1))
validate_task_bank(TASK_BANK)

# ---- UI helpers ----
soft_card <- function(..., class = "") {
  shiny::div(class = trimws(paste("soft-card", class)), ...)
}

chip <- function(text, tone = "default") {
  shiny::div(class = paste("chip", tone), text)
}

principle_tile <- function(title, description) {
  shiny::div(
    class = "principle-tile",
    shiny::tags$div(class = "principle-name", title),
    shiny::tags$div(class = "principle-description", description)
  )
}

legacy_welcome_screen_ui <- function() {
  shiny::div(
    class = "page-wrap",
    shiny::div(
      class = "hero-grid",
      soft_card(
        class = "hero-card",
        shiny::tags$h1("Гештальт принциптері бойынша визуалды қабылдау эксперименті"),
        shiny::tags$p(
          class = "lead-text",
          "Бұл Shiny қосымшасында деректерді визуализациялаудың қабылдау, топтастыру және түсіндіруге әсерін зерттеуге арналған 25 эксперименттік тапсырма бар."
        ),
        shiny::tags$div(
          class = "hero-highlights",
          chip("5 Гештальт принципі"),
          chip("25 кездейсоқ реттелген тапсырма"),
          chip("Реакция уақыты мен сенімділік"),
          chip("CSV-ге автоматты сақтау")
        ),
        shiny::tags$ul(
          class = "brief-list",
          shiny::tags$li("Әр визуализацияны мұқият қарап, ең дұрыс жауапты таңдаңыз."),
          shiny::tags$li("Әр тапсырма дәлдікті, реакция уақытын және сенімділік деңгейін тіркейді."),
          shiny::tags$li("Толық эксперимент әдетте компьютерде 10-15 минуттай уақыт алады.")
        ),
        shiny::div(
          class = "principle-grid",
          lapply(names(PRINCIPLE_BRIEFS), function(principle_name) {
            principle_tile(label_principle(principle_name), PRINCIPLE_BRIEFS[[principle_name]])
          })
        ),
        shiny::tags$p(
          class = "results-note",
          paste("Нәтижелер жергілікті түрде мына жерде сақталады:", normalizePath(RESULTS_FILE, winslash = "/", mustWork = FALSE))
        )
      ),
      soft_card(
        class = "form-card",
        shiny::tags$h2("Қатысушы туралы ақпарат"),
        shiny::textInput("participant_id", "Қатысушы ID-і", placeholder = "мысалы, P-001"),
        shiny::numericInput("age", "Жасы (міндетті емес)", value = NA, min = 16, max = 99, step = 1),
        shiny::selectInput(
          "gender",
          "Жынысы (міндетті емес)",
          choices = c(
            "Таңдаңыз..." = "",
            "Әйел" = "Әйел",
            "Ер" = "Ер",
            "Бинарлы емес" = "Бинарлы емес",
            "Жауап бермеуді жөн көремін" = "Жауап бермеуді жөн көремін"
          )
        ),
        shiny::textInput("specialization", "Мамандығы (міндетті емес)", placeholder = "мысалы, Ақпараттық жүйелер"),
        shiny::actionButton("start_experiment", "Экспериментті бастау", class = "action-main")
      )
    )
  )
}

legacy_experiment_screen_ui <- function(task, position, total, participant_id) {
  progress_pct <- round(100 * position / total)

  shiny::div(
    class = "page-wrap",
    shiny::div(
      class = "top-strip",
      shiny::div(
        class = "progress-shell",
        shiny::div(
          class = "progress-track",
          shiny::div(class = "progress-fill", style = paste0("width:", progress_pct, "%;"))
        ),
        shiny::div(class = "progress-text", paste("Тапсырма", position, "/", total))
      ),
      shiny::div(class = "progress-text", paste("Қатысушы ID-і:", participant_id))
    ),
    shiny::div(
      class = "experiment-grid",
      soft_card(
        class = "plot-card",
        shiny::uiOutput("task_visual_ui")
      ),
      soft_card(
        class = "question-card",
        shiny::tags$h2(task$question_text),
        shiny::uiOutput("answer_choices_ui"),
        shiny::sliderInput(
          "confidence",
          "Жауабыңызға қаншалықты сенімдісіз?",
          min = 1,
          max = 5,
          value = 3,
          step = 1,
          ticks = FALSE
        ),
        shiny::div(class = "confidence-scale", "1 = сенімділігі төмен, 5 = сенімділігі жоғары"),
        shiny::tags$button(
          id = "submit_answer",
          type = "button",
          class = "action-main action-submit",
          disabled = "disabled",
          "Жауапты жіберу"
        )
      )
    )
  )
}

completion_screen_ui <- function() {
  shiny::div(
    class = "page-wrap",
    soft_card(
      class = "completion-card",
      shiny::tags$h1("Эксперимент аяқталды"),
      shiny::tags$p(
        class = "lead-text",
        "Рақмет. Сіздің жауаптарыңыз сәтті сақталды және кейінгі талдауға дайын."
      ),
      shiny::uiOutput("completion_summary_ui"),
      shiny::div(class = "download-row", shiny::uiOutput("download_buttons_ui")),
      shiny::div(class = "results-note", shiny::textOutput("results_path_text", inline = TRUE)),
      shiny::actionButton("reset_experiment", "Жаңа қатысушы сессиясын бастау", class = "action-secondary")
    ),
    shiny::div(
      class = "completion-grid",
      soft_card(
        class = "table-card",
        shiny::tags$h2("Ағымдағы қатысушының нәтижелері"),
        DT::DTOutput("participant_results_table")
      ),
      soft_card(
        class = "table-card",
        shiny::tags$h2("Соңғы сақталған жазбалар"),
        DT::DTOutput("recent_results_table")
      )
    )
  )
}

welcome_screen_ui <- function() {
  shiny::div(
    class = "page-wrap",
    shiny::div(
      class = "hero-grid",
      soft_card(
        class = "hero-card",
        shiny::tags$h1("Деректер визуализациясын қабылдауды эксперименттік бағалау"),
        shiny::tags$p(
          class = "lead-text",
          "Бұл қосымша визуалды деректерді қабылдауды бағалауға арналған. Қатысушы әр бетте берілген диаграмманы немесе диаграммалар жиынын қарап, қысқа сұраққа жауап береді."
        ),
        shiny::tags$div(
          class = "hero-highlights",
          chip("25 тапсырма"),
          chip("Кездейсоқ реттік өту"),
          chip("Жауап уақыты мен сенімділік"),
          chip("Нәтижелер CSV форматына сақталады")
        ),
        shiny::tags$ul(
          class = "brief-list",
          shiny::tags$li("Қатысушы туралы мәліметті енгізіп, экспериментті бастаңыз."),
          shiny::tags$li("Әр тапсырмада бір ғана жауап нұсқасын таңдаңыз."),
          shiny::tags$li("Эксперимент әдетте 10-15 минут уақыт алады.")
        ),
        shiny::tags$p(
          class = "results-note",
          paste("Нәтижелер осы бумаға сақталады:", normalizePath(RESULTS_FILE, winslash = "/", mustWork = FALSE))
        )
      ),
      soft_card(
        class = "form-card",
        shiny::tags$h2("Қатысушы туралы ақпарат"),
        shiny::textInput("participant_id", "Қатысушы ID-і", placeholder = "мысалы, P-001"),
        shiny::numericInput("age", "Жасы (міндетті емес)", value = NA, min = 16, max = 99, step = 1),
        shiny::selectInput(
          "gender",
          "Жынысы (міндетті емес)",
          choices = c(
            "Таңдаңыз..." = "",
            "Әйел" = "Әйел",
            "Ер" = "Ер",
            "Бинарлы емес" = "Бинарлы емес",
            "Жауап бермеуді жөн көремін" = "Жауап бермеуді жөн көремін"
          )
        ),
        shiny::textInput("specialization", "Мамандығы (міндетті емес)", placeholder = "мысалы, Ақпараттық жүйелер"),
        shiny::actionButton("start_experiment", "Экспериментті бастау", class = "action-main")
      )
    )
  )
}

experiment_screen_ui <- function(task, position, total, participant_id) {
  progress_pct <- round(100 * position / total)

  shiny::div(
    class = "page-wrap",
    shiny::div(
      class = "top-strip",
      shiny::div(
        class = "progress-shell",
        shiny::div(
          class = "progress-track",
          shiny::div(class = "progress-fill", style = paste0("width:", progress_pct, "%;"))
        ),
        shiny::div(class = "progress-text", paste("Тапсырма", position, "/", total))
      ),
      shiny::div(class = "progress-text", paste("Қатысушы ID-і:", participant_id))
    ),
    shiny::div(
      class = "experiment-grid",
      soft_card(
        class = "plot-card",
        shiny::uiOutput("task_visual_ui")
      ),
      soft_card(
        class = "question-card",
        shiny::tags$h2(task$question_text),
        shiny::uiOutput("answer_choices_ui"),
        shiny::sliderInput(
          "confidence",
          "Жауабыңызға қаншалықты сенімдісіз?",
          min = 1,
          max = 5,
          value = 3,
          step = 1,
          ticks = FALSE
        ),
        shiny::div(class = "confidence-scale", "1 = сенімділігі төмен, 5 = сенімділігі жоғары"),
        shiny::tags$button(
          id = "submit_answer",
          type = "button",
          class = "action-main action-submit",
          disabled = "disabled",
          "Жауапты жіберу"
        )
      )
    )
  )
}

# ---- Shiny UI ----
ui <- shiny::fluidPage(
  shiny::tags$head(
    shiny::tags$style(
      shiny::HTML(
        paste(
          ":root {",
          "  --bg-1: #f3f8fb;",
          "  --bg-2: #ebf3f8;",
          "  --ink: #243B53;",
          "  --muted: #5A7184;",
          "  --line: #dce7ef;",
          "  --surface: rgba(255, 255, 255, 0.94);",
          "  --surface-strong: #ffffff;",
          "  --accent: #16324F;",
          "  --accent-soft: #5FA8D3;",
          "}",
          "body {",
          "  min-height: 100vh;",
          "  background:",
          "    radial-gradient(circle at top left, rgba(95, 168, 211, 0.20), transparent 28%),",
          "    radial-gradient(circle at top right, rgba(213, 164, 75, 0.14), transparent 24%),",
          "    linear-gradient(180deg, var(--bg-1) 0%, var(--bg-2) 100%);",
          "  color: var(--ink);",
          "  font-family: 'Segoe UI', 'Noto Sans', 'Helvetica Neue', sans-serif;",
          "}",
          ".container-fluid { max-width: 1480px; padding: 28px 24px 40px; }",
          ".page-wrap { display: flex; flex-direction: column; gap: 24px; }",
          ".hero-grid, .experiment-grid, .completion-grid {",
          "  display: grid;",
          "  grid-template-columns: minmax(0, 1.2fr) minmax(320px, 0.8fr);",
          "  gap: 24px;",
          "}",
          ".completion-grid { grid-template-columns: repeat(2, minmax(0, 1fr)); }",
          ".soft-card {",
          "  background: var(--surface);",
          "  border: 1px solid rgba(22, 50, 79, 0.08);",
          "  border-radius: 24px;",
          "  box-shadow: 0 20px 45px rgba(24, 50, 79, 0.08);",
          "  backdrop-filter: blur(8px);",
          "  padding: 26px 28px;",
          "}",
          ".hero-card { background: linear-gradient(180deg, rgba(255,255,255,0.98) 0%, rgba(247,251,254,0.95) 100%); }",
          ".hero-card h1, .completion-card h1 { margin-top: 0; margin-bottom: 10px; font-weight: 800; letter-spacing: -0.02em; }",
          ".form-card h2, .question-card h2, .table-card h2 { margin-top: 0; color: #16324F; }",
          ".card-eyebrow {",
          "  display: inline-flex;",
          "  margin-bottom: 14px;",
          "  font-size: 11px;",
          "  font-weight: 800;",
          "  text-transform: uppercase;",
          "  letter-spacing: 0.12em;",
          "  color: #6E879A;",
          "}",
          ".lead-text { font-size: 16px; line-height: 1.7; color: #4F6D7A; max-width: 66ch; }",
          ".hero-highlights, .meta-chip-row, .download-row { display: flex; flex-wrap: wrap; gap: 10px; }",
          ".brief-list { margin: 18px 0 0; padding-left: 18px; line-height: 1.8; }",
          ".principle-grid { display: grid; grid-template-columns: repeat(2, minmax(0, 1fr)); gap: 12px; margin-top: 20px; }",
          ".principle-tile { background: rgba(245, 250, 252, 0.9); border: 1px solid #e2ecf3; border-radius: 18px; padding: 14px 16px; }",
          ".principle-name { font-weight: 700; color: #16324F; margin-bottom: 4px; }",
          ".principle-description { font-size: 13px; color: #5A7184; line-height: 1.55; }",
          ".chip {",
          "  display: inline-flex;",
          "  align-items: center;",
          "  border-radius: 999px;",
          "  padding: 8px 14px;",
          "  font-size: 12px;",
          "  font-weight: 700;",
          "  letter-spacing: 0.02em;",
          "  background: #e7f0f7;",
          "  color: #16324F;",
          "}",
          ".chip.principle { background: #dceef8; }",
          ".chip.viz { background: #f5e6d3; }",
          ".chip.question { background: #e8f1e8; }",
          ".chip.participant { background: #f2edf7; }",
          ".top-strip { display: flex; flex-direction: column; gap: 14px; }",
          ".progress-shell { display: flex; flex-direction: column; gap: 10px; }",
          ".progress-track { height: 14px; border-radius: 999px; background: #dbe7f0; overflow: hidden; box-shadow: inset 0 1px 2px rgba(22,50,79,0.08); }",
          ".progress-fill { height: 100%; border-radius: 999px; background: linear-gradient(90deg, #5FA8D3 0%, #16324F 100%); }",
          ".progress-text { font-size: 13px; font-weight: 700; color: #4F6D7A; }",
          ".plot-card { min-height: 640px; }",
          ".plot-card .shiny-plot-output, .closure-panel .shiny-plot-output {",
          "  background: linear-gradient(180deg, #fbfdff 0%, #f4f9fc 100%);",
          "  border: 1px solid #e4edf4;",
          "  border-radius: 18px;",
          "  padding: 8px;",
          "}",
          ".question-card { display: flex; flex-direction: column; gap: 12px; position: sticky; top: 18px; align-self: start; }",
          ".side-note { margin-top: -4px; color: #61798B; font-size: 13px; line-height: 1.6; }",
          ".question-card .radio {",
          "  background: #f7fbfd;",
          "  border: 1px solid #dde8ef;",
          "  border-radius: 14px;",
          "  margin-top: 0;",
          "  margin-bottom: 10px;",
          "  padding: 12px 14px;",
          "  transition: border-color 0.2s ease, box-shadow 0.2s ease;",
          "}",
          ".question-card .radio:hover { border-color: #b8cfdf; box-shadow: 0 8px 18px rgba(95,168,211,0.10); }",
          ".question-card .radio label { width: 100%; font-weight: 600; color: #243B53; display: block; }",
          ".question-card .shiny-input-container { width: 100%; margin-bottom: 4px; }",
          ".irs--shiny .irs-bar, .irs--shiny .irs-single { background: #16324F; border-color: #16324F; }",
          ".irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single { font-weight: 700; }",
          ".action-main, .action-secondary, .btn-default, .btn-primary {",
          "  border-radius: 14px !important;",
          "  padding: 12px 18px !important;",
          "  font-weight: 700 !important;",
          "  border: none !important;",
          "}",
          ".action-main { background: #16324F !important; color: #ffffff !important; }",
          ".action-main:hover { background: #22476d !important; color: #ffffff !important; }",
          ".action-secondary { background: #dbe7f0 !important; color: #16324F !important; }",
          ".action-submit { width: 100%; box-shadow: 0 10px 22px rgba(22, 50, 79, 0.16); }",
          ".action-submit.is-disabled, .action-submit:disabled { opacity: 0.56; cursor: wait; box-shadow: none; }",
          ".confidence-scale, .hint-text, .results-note { color: #5A7184; font-size: 13px; line-height: 1.6; }",
          ".metric-grid { display: grid; grid-template-columns: repeat(4, minmax(0, 1fr)); gap: 12px; margin: 18px 0 10px; }",
          ".metric-card {",
          "  background: linear-gradient(180deg, #f8fbfd 0%, #f3f8fb 100%);",
          "  border: 1px solid #dbe7f0;",
          "  border-radius: 16px;",
          "  padding: 14px 16px;",
          "}",
          ".metric-label { font-size: 12px; text-transform: uppercase; letter-spacing: 0.08em; color: #6B7C8F; }",
          ".metric-value { margin-top: 6px; font-size: 24px; font-weight: 700; color: #16324F; }",
          ".plot-caption { margin-top: 12px; color: #5A7184; font-size: 13px; }",
          ".closure-grid { display: grid; grid-template-columns: repeat(3, minmax(0, 1fr)); gap: 14px; }",
          ".closure-panel { background: #f8fbfd; border: 1px solid #e3ebf2; border-radius: 20px; padding: 10px; }",
          ".closure-label { font-weight: 700; color: #16324F; margin: 2px 0 10px; text-align: center; }",
          ".dataTables_wrapper .dataTables_paginate .paginate_button.current { background: #16324F !important; color: #ffffff !important; border-color: #16324F !important; }",
          ".table-card table { font-size: 12px; }",
          "@media (max-width: 1080px) {",
          "  .hero-grid, .experiment-grid, .completion-grid, .metric-grid, .closure-grid, .principle-grid { grid-template-columns: 1fr; }",
          "  .question-card { position: static; }",
          "  .plot-card { min-height: auto; }",
          "}",
          sep = "\n"
        )
      )
    ),
    shiny::tags$script(
      shiny::HTML(
        "
        window.gestaltTaskClock = {
          taskId: null,
          shownPerf: null,
          shownAtMs: null,
          observer: null,
          fallbackTimer: null
        };

        window.disableTaskSubmit = function() {
          var button = document.getElementById('submit_answer');
          if (button) {
            button.disabled = true;
            button.classList.add('is-disabled');
          }
        };

        window.unlockTaskSubmit = function() {
          var button = document.getElementById('submit_answer');
          if (button) {
            button.disabled = false;
            button.classList.remove('is-disabled');
          }
        };

        window.clearTaskDisplayObserver = function() {
          if (window.gestaltTaskClock.observer) {
            window.gestaltTaskClock.observer.disconnect();
            window.gestaltTaskClock.observer = null;
          }
          if (window.gestaltTaskClock.fallbackTimer) {
            window.clearTimeout(window.gestaltTaskClock.fallbackTimer);
            window.gestaltTaskClock.fallbackTimer = null;
          }
        };

        window.markTaskDisplayed = function(taskId) {
          if (window.gestaltTaskClock.taskId !== taskId || window.gestaltTaskClock.shownPerf !== null) {
            return;
          }

          window.clearTaskDisplayObserver();
          window.gestaltTaskClock.shownPerf = window.performance.now();
          window.gestaltTaskClock.shownAtMs = Date.now();
          window.unlockTaskSubmit();

          if (window.Shiny && window.Shiny.setInputValue) {
            Shiny.setInputValue('task_display_event', {
              task_id: taskId,
              displayed_at_ms: window.gestaltTaskClock.shownAtMs,
              nonce: Math.random()
            }, { priority: 'event' });
          }
        };

        window.initTaskDisplayObserver = function(taskId) {
          window.clearTaskDisplayObserver();
          window.gestaltTaskClock = {
            taskId: taskId,
            shownPerf: null,
            shownAtMs: null,
            observer: null,
            fallbackTimer: null
          };

          window.disableTaskSubmit();
          var container = document.getElementById('task-visual-container');

          if (!container) {
            window.gestaltTaskClock.fallbackTimer = window.setTimeout(function() {
              window.markTaskDisplayed(taskId);
            }, 250);
            return;
          }

          var arePlotsReady = function() {
            var plotImages = container.querySelectorAll('.shiny-plot-output img');
            if (!plotImages.length) {
              return false;
            }

            return Array.prototype.every.call(plotImages, function(img) {
              return img.complete && img.naturalWidth > 0;
            });
          };

          var checkPlots = function() {
            if (window.gestaltTaskClock.taskId !== taskId) {
              return;
            }

            if (arePlotsReady()) {
              window.requestAnimationFrame(function() {
                window.markTaskDisplayed(taskId);
              });
              return;
            }

            var plotImages = container.querySelectorAll('.shiny-plot-output img');
            Array.prototype.forEach.call(plotImages, function(img) {
              if (img.complete && img.naturalWidth > 0) {
                return;
              }

              var onDone = function() {
                window.requestAnimationFrame(checkPlots);
              };
              img.addEventListener('load', onDone, { once: true });
              img.addEventListener('error', onDone, { once: true });
            });
          };

          window.gestaltTaskClock.observer = new MutationObserver(function() {
            window.requestAnimationFrame(checkPlots);
          });
          window.gestaltTaskClock.observer.observe(container, {
            childList: true,
            subtree: true,
            attributes: true
          });

          window.gestaltTaskClock.fallbackTimer = window.setTimeout(function() {
            window.markTaskDisplayed(taskId);
          }, 5000);

          window.requestAnimationFrame(checkPlots);
        };

        document.addEventListener('click', function(event) {
  var button = event.target.closest('#submit_answer');
  if (!button || button.disabled) {
    return;
  }

  var selectedOption = document.querySelector('input[name=\"answer_choice\"]:checked');
  if (!selectedOption) {
    if (window.Shiny && window.Shiny.setInputValue) {
      Shiny.setInputValue('submit_validation_event', {
        reason: 'missing_answer',
        nonce: Math.random()
      }, { priority: 'event' });
    }
    return;
  }

  var tracker = window.gestaltTaskClock || {};
  var nowPerf = window.performance.now();
  var nowMs = Date.now();
  var reactionTimeMs = tracker.shownPerf === null ? null : Math.max(0, nowPerf - tracker.shownPerf);

  window.disableTaskSubmit();

  if (window.Shiny && window.Shiny.setInputValue) {
    Shiny.setInputValue('task_submit_event', {
      task_id: tracker.taskId,
      selected_answer: selectedOption.value,
      submitted_at_ms: nowMs,
      reaction_time_ms: reactionTimeMs,
      nonce: Math.random()
    }, { priority: 'event' });
  } else {
    window.unlockTaskSubmit();
  }
});

        if (window.Shiny && window.Shiny.addCustomMessageHandler) {
          Shiny.addCustomMessageHandler('initTaskObserver', function(message) {
            if (message && message.task_id) {
              window.initTaskDisplayObserver(message.task_id);
            }
          });
          Shiny.addCustomMessageHandler('unlockSubmit', function() {
            window.unlockTaskSubmit();
          });
          Shiny.addCustomMessageHandler('disableSubmit', function() {
            window.disableTaskSubmit();
          });
        }
        "
      )
    )
  ),
  shiny::uiOutput("app_body")
)

# ---- Shiny server ----
server <- function(input, output, session) {
  # Runtime state for the current participant session.
  state <- shiny::reactiveValues(
    screen = "welcome",
    task_sequence = NULL,
    current_index = 0,
    task_started_at = NULL,
    task_started_at_client_ms = NA_real_,
    experiment_started_at = NULL,
    participant_run_id = NULL,
    session_results = empty_results_df(),
    is_submitting = FALSE
  )

  current_task <- shiny::reactive({
    shiny::req(!is.null(state$task_sequence))
    shiny::req(state$current_index >= 1)
    shiny::req(state$current_index <= length(state$task_sequence))
    state$task_sequence[[state$current_index]]
  })

  reset_task_display_state <- function() {
    state$task_started_at <- NULL
    state$task_started_at_client_ms <- NA_real_
  }

  participant_id_current <- shiny::reactive({
    trimws(input$participant_id %||% "")
  })

  arm_task_observer <- function(task_id) {
    force(task_id)
    task_id <- as.character(task_id)[1]
    session$sendCustomMessage("disableSubmit", list())
    session$onFlushed(function() {
      shiny::updateRadioButtons(session, "answer_choice", selected = character(0))
      session$sendCustomMessage("initTaskObserver", list(task_id = task_id))
    }, once = TRUE)
  }

  output$app_body <- shiny::renderUI({
    if (identical(state$screen, "welcome")) {
      return(welcome_screen_ui())
    }

    if (identical(state$screen, "experiment")) {
      participant_label <- participant_id_current()
      if (!nzchar(participant_label)) {
        participant_label <- "Қатысушы"
      }
      return(experiment_screen_ui(current_task(), state$current_index, length(state$task_sequence), participant_label))
    }

    completion_screen_ui()
  })

  # Build the current stimulus UI. The closure task uses three synchronized
  # visualizations on one screen, while the others use a single plot.
  output$task_visual_ui <- shiny::renderUI({
    shiny::req(identical(state$screen, "experiment"))
    task <- current_task()

    if (identical(task$renderer, "plot_grid")) {
      n_plots <- length(task$plot_funs)
      grid_cols <- if (n_plots <= 2) n_plots else 2
      plot_height <- if (n_plots == 2) "320px" else if (n_plots == 3) "290px" else "250px"

      return(
        shiny::div(
          id = "task-visual-container",
          shiny::div(
            class = "closure-grid",
            style = paste0(
              "grid-template-columns: repeat(",
              grid_cols,
              ", minmax(0, 1fr));",
              "align-items: start;"
            ),
            lapply(seq_len(n_plots), function(i) {
              shiny::div(
                class = "closure-panel",
                shiny::div(class = "closure-label", task$panel_labels[[i]]),
                shiny::plotOutput(paste0("grid_plot_", i), height = plot_height)
              )
            })
          )
        )
      )
    }

    if (identical(task$renderer, "closure_triptych")) {
      return(
        shiny::div(
          id = "task-visual-container",
          shiny::div(
            class = "closure-grid",
            shiny::div(
              class = "closure-panel",
              shiny::plotOutput("closure_scatter_plot", height = "280px")
            ),
            shiny::div(
              class = "closure-panel",
              shiny::plotOutput("closure_bar_plot", height = "280px")
            ),
            shiny::div(
              class = "closure-panel",
              shiny::plotOutput("closure_treemap_plot", height = "280px")
            )
          )
        )
      )
    }

    shiny::div(
      id = "task-visual-container",
      shiny::plotOutput("task_plot", height = "520px")
    )
  })

  output$answer_choices_ui <- shiny::renderUI({
    shiny::req(identical(state$screen, "experiment"))
    task <- current_task()
    shiny::radioButtons(
      "answer_choice",
      label = NULL,
      choices = task$answer_choices,
      selected = character(0)
    )
  })

  output$task_plot <- shiny::renderPlot({
    task <- current_task()
    shiny::req(identical(task$renderer, "single_plot"))
    task$plot_fun()
  }, res = 110)

  render_grid_plot <- function(index) {
    shiny::renderPlot({
      task <- current_task()
      shiny::req(identical(task$renderer, "plot_grid"))
      shiny::req(length(task$plot_funs) >= index)
      task$plot_funs[[index]]()
    }, res = 110)
  }

  output$grid_plot_1 <- render_grid_plot(1)
  output$grid_plot_2 <- render_grid_plot(2)
  output$grid_plot_3 <- render_grid_plot(3)
  output$grid_plot_4 <- render_grid_plot(4)

  output$closure_scatter_plot <- shiny::renderPlot({
    task <- current_task()
    shiny::req(identical(task$renderer, "closure_triptych"))
    task$scatter_fun()
  }, res = 110)

  output$closure_bar_plot <- shiny::renderPlot({
    task <- current_task()
    shiny::req(identical(task$renderer, "closure_triptych"))
    task$bar_fun()
  }, res = 110)

  output$closure_treemap_plot <- shiny::renderPlot({
    task <- current_task()
    shiny::req(identical(task$renderer, "closure_triptych"))
    task$treemap_fun()
  }, res = 110)

  output$completion_summary_ui <- shiny::renderUI({
    df <- state$session_results
    if (nrow(df) == 0) {
      return(NULL)
    }

    is_correct_vec <- as.logical(df$is_correct)
    reaction_time_vec <- suppressWarnings(as.numeric(df$reaction_time_sec))
    confidence_vec <- suppressWarnings(as.numeric(df$confidence))

    accuracy <- if (all(is.na(is_correct_vec))) NA_real_ else round(mean(is_correct_vec, na.rm = TRUE) * 100, 1)
    median_rt <- if (all(is.na(reaction_time_vec))) NA_real_ else round(stats::median(reaction_time_vec, na.rm = TRUE), 2)
    avg_conf <- if (all(is.na(confidence_vec))) NA_real_ else round(mean(confidence_vec, na.rm = TRUE), 2)
    correct_n <- sum(is_correct_vec, na.rm = TRUE)

    shiny::div(
      class = "metric-grid",
      shiny::div(
        class = "metric-card",
        shiny::div(class = "metric-label", "Дәлдік"),
        shiny::div(class = "metric-value", paste0(accuracy, "%"))
      ),
      shiny::div(
        class = "metric-card",
        shiny::div(class = "metric-label", "Дұрыс жауаптар"),
        shiny::div(class = "metric-value", paste(correct_n, "/", nrow(df)))
      ),
      shiny::div(
        class = "metric-card",
        shiny::div(class = "metric-label", "Реакция уақытының медианасы"),
        shiny::div(class = "metric-value", paste0(median_rt, " сек"))
      ),
      shiny::div(
        class = "metric-card",
        shiny::div(class = "metric-label", "Орташа сенімділік"),
        shiny::div(class = "metric-value", avg_conf)
      )
    )
  })

  output$download_buttons_ui <- shiny::renderUI({
    buttons <- list(
      shiny::downloadButton("download_my_results", "Менің нәтижелерімді жүктеу", class = "action-main")
    )

    if (file.exists(RESULTS_FILE)) {
      buttons <- c(
        buttons,
        list(shiny::downloadButton("download_all_results", "Толық CSV файлын жүктеу", class = "action-secondary"))
      )
    }

    shiny::tagList(buttons)
  })

  output$results_path_text <- shiny::renderText({
    paste("Нәтижелер файлы:", normalizePath(RESULTS_FILE, winslash = "/", mustWork = FALSE))
  })

  output$participant_results_table <- DT::renderDT({
    df <- state$session_results
    if (nrow(df) == 0) {
      return(
        DT::datatable(
          data.frame(Хабарлама = "Әзірге сақталған жолдар жоқ."),
          options = list(dom = "t", language = DT_LANGUAGE),
          rownames = FALSE
        )
      )
    }

    view_df <- dplyr::select(
      df,
      task_order_position,
      task_id,
      selected_answer,
      correct_answer,
      is_correct,
      reaction_time_sec,
      confidence
    )
    names(view_df) <- c(
      "Тапсырма реті",
      "Тапсырма ID-і",
      "Таңдалған жауап",
      "Дұрыс жауап",
      "Дұрыс",
      "Реакция уақыты, сек",
      "Сенімділік"
    )
    view_df$Дұрыс <- ifelse(view_df$Дұрыс, "Иә", "Жоқ")

    DT::datatable(
      view_df,
      rownames = FALSE,
      options = list(pageLength = 10, autoWidth = TRUE, scrollX = TRUE, language = DT_LANGUAGE)
    )
  })

  output$recent_results_table <- DT::renderDT({
    df <- read_results_file(RESULTS_FILE, n = 12)
    if (nrow(df) == 0) {
      return(
        DT::datatable(
          data.frame(Хабарлама = "Әзірге сақталған жолдар жоқ."),
          options = list(dom = "t", language = DT_LANGUAGE),
          rownames = FALSE
        )
      )
    }

    view_df <- dplyr::select(
      df,
      participant_id,
      task_id,
      selected_answer,
      is_correct,
      reaction_time_sec,
      submitted_at
    )
    names(view_df) <- c(
      "Қатысушы ID-і",
      "Тапсырма ID-і",
      "Таңдалған жауап",
      "Дұрыс",
      "Реакция уақыты, сек",
      "Жіберілген уақыты"
    )
    view_df$Дұрыс <- ifelse(view_df$Дұрыс, "Иә", "Жоқ")

    DT::datatable(
      view_df,
      rownames = FALSE,
      options = list(pageLength = 10, autoWidth = TRUE, scrollX = TRUE, language = DT_LANGUAGE)
    )
  })

  output$download_my_results <- shiny::downloadHandler(
    filename = function() {
      paste0("participant_results_", trimws(input$participant_id %||% "participant"), "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      readr::write_excel_csv(normalize_results_table(state$session_results), file)
    }
  )

  output$download_all_results <- shiny::downloadHandler(
    filename = function() {
      paste0("experiment_results_", Sys.Date(), ".csv")
    },
    content = function(file) {
      readr::write_excel_csv(read_results_file(RESULTS_FILE), file)
    }
  )

  shiny::observeEvent(input$start_experiment, {
    participant_id <- trimws(input$participant_id %||% "")
    if (!nzchar(participant_id)) {
      shiny::showNotification("Экспериментті бастамас бұрын қатысушы ID-ін енгізіңіз.", type = "error")
      return()
    }

    state$screen <- "experiment"
    state$task_sequence <- unname(TASK_BANK[sample.int(length(TASK_BANK))])
    state$current_index <- 1
    state$experiment_started_at <- Sys.time()
    state$participant_run_id <- paste(session$token, format(Sys.time(), "%Y%m%d%H%M%OS3"), sep = "_")
    state$session_results <- empty_results_df()
    reset_task_display_state()
    first_task_id <- state$task_sequence[[1]]$task_id
    arm_task_observer(first_task_id)
  }, ignoreInit = TRUE)

  # The client sends this event only after all plots on the task screen finish
  # rendering, which makes the captured start time much closer to what the
  # participant actually sees.
  shiny::observeEvent(input$task_display_event, {
    shiny::req(identical(state$screen, "experiment"))
    payload <- input$task_display_event
    task <- current_task()

    if (is.null(payload$task_id) || !identical(payload$task_id, task$task_id)) {
      return()
    }

    if (!is.null(state$task_started_at)) {
      return()
    }

    state$task_started_at_client_ms <- suppressWarnings(as.numeric(payload$displayed_at_ms))
    displayed_at <- client_ms_to_posix(state$task_started_at_client_ms)
    if (is.na(displayed_at)) {
      displayed_at <- Sys.time()
    }
    state$task_started_at <- displayed_at
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$submit_validation_event, {
    shiny::req(identical(state$screen, "experiment"))
    shiny::showNotification("Тапсырманы жібермес бұрын жауап нұсқасын таңдаңыз.", type = "error")
  }, ignoreInit = TRUE)

  # Save one response per task submission, then immediately advance to the
  # next randomized task only after the CSV write succeeds.
  shiny::observeEvent(input$task_submit_event, {
    shiny::req(identical(state$screen, "experiment"))

    if (isTRUE(state$is_submitting)) {
      return()
    }

    task <- current_task()
    payload <- input$task_submit_event
    if (is.null(payload$task_id) || !identical(payload$task_id, task$task_id)) {
      session$sendCustomMessage("unlockSubmit", list())
      return()
    }

    selected_answer <- trimws(payload$selected_answer %||% input$answer_choice %||% "")

    if (!nzchar(selected_answer)) {
      shiny::showNotification("Тапсырманы жібермес бұрын жауап нұсқасын таңдаңыз.", type = "error")
      session$sendCustomMessage("unlockSubmit", list())
      return()
    }

    state$is_submitting <- TRUE
    on.exit({
      state$is_submitting <- FALSE
    }, add = TRUE)

    submitted_at_client_ms <- suppressWarnings(as.numeric(payload$submitted_at_ms))
    submitted_at <- client_ms_to_posix(submitted_at_client_ms)
    if (is.na(submitted_at)) {
      submitted_at <- Sys.time()
    }

    started_at <- state$task_started_at %||% submitted_at
    reaction_time <- suppressWarnings(as.numeric(payload$reaction_time_ms) / 1000)
    if (is.na(reaction_time) || reaction_time < 0) {
      reaction_time <- round(as.numeric(difftime(submitted_at, started_at, units = "secs")), 3)
    } else {
      reaction_time <- round(reaction_time, 3)
    }
    age_value <- suppressWarnings(as.numeric(input$age))
    if (is.na(age_value) || age_value <= 0) {
      age_value <- NA_real_
    }

    result_row <- normalize_results_row(data.frame(
      participant_id = participant_id_current(),
      age = age_value,
      gender = trim_or_na(input$gender),
      specialization = trim_or_na(input$specialization),
      experiment_started_at = timestamp_string(state$experiment_started_at),
      started_at = timestamp_string(started_at),
      submitted_at = timestamp_string(submitted_at),
      displayed_at_client_ms = state$task_started_at_client_ms,
      submitted_at_client_ms = submitted_at_client_ms,
      task_order_position = state$current_index,
      task_id = task$task_id,
      question_id = task$question_id,
      gestalt_principle = task$principle,
      visualization_type = task$visualization_type,
      question_type = task$question_type,
      question_text = task$question_text,
      options_shown = paste(task$answer_choices, collapse = " | "),
      correct_answer = task$correct_answer,
      selected_answer = selected_answer,
      is_correct = identical(selected_answer, task$correct_answer),
      reaction_time_sec = reaction_time,
      confidence = input$confidence,
      session_id = state$participant_run_id,
      stringsAsFactors = FALSE
    ))

    save_ok <- tryCatch(
      {
        append_results_csv(result_row, RESULTS_FILE, RESULTS_LOCK_DIR)
        TRUE
      },
      error = function(e) {
        shiny::showNotification(
          paste("Жауапты сақтау мүмкін болмады:", e$message),
          type = "error",
          duration = 7
        )
        session$sendCustomMessage("unlockSubmit", list())
        FALSE
      }
    )

    if (!save_ok) {
      return()
    }

    state$session_results <- normalize_results_table(dplyr::bind_rows(state$session_results, result_row))

    if (state$current_index < length(state$task_sequence)) {
      next_index <- state$current_index + 1
      next_task_id <- state$task_sequence[[next_index]]$task_id
      reset_task_display_state()
      state$current_index <- next_index
      arm_task_observer(next_task_id)
    } else {
      state$screen <- "complete"
    }
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$reset_experiment, {
    state$screen <- "welcome"
    state$task_sequence <- NULL
    state$current_index <- 0
    reset_task_display_state()
    state$experiment_started_at <- NULL
    state$participant_run_id <- NULL
    state$session_results <- empty_results_df()

    session$onFlushed(function() {
      shiny::updateTextInput(session, "participant_id", value = "")
      shiny::updateNumericInput(session, "age", value = NA)
      shiny::updateSelectInput(session, "gender", selected = "")
      shiny::updateTextInput(session, "specialization", value = "")
    }, once = TRUE)
  }, ignoreInit = TRUE)
}

shiny::shinyApp(ui = ui, server = server)
