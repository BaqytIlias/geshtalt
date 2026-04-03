make_task <- function(
  task_id,
  task_family,
  principle = NA_character_,
  visualization_type,
  question_type,
  question_text,
  answer_choices = NULL,
  correct_answer = NA_character_,
  renderer = "single_plot",
  chart_count = 1L,
  plot_fun = NULL,
  plot_funs = NULL,
  panel_labels = NULL,
  panels = NULL
) {
  list(
    task_id = task_id,
    question_id = paste0("Q_", task_id),
    task_family = task_family,
    principle = principle,
    visualization_type = visualization_type,
    question_type = question_type,
    question_text = question_text,
    answer_choices = answer_choices,
    correct_answer = correct_answer,
    renderer = renderer,
    chart_count = chart_count,
    plot_fun = plot_fun,
    plot_funs = plot_funs,
    panel_labels = panel_labels,
    panels = panels
  )
}

validate_task_bank <- function(task_bank) {
  issues <- character()

  if (length(task_bank) != 30) {
    issues <- c(issues, sprintf("Expected 30 tasks, found %s.", length(task_bank)))
  }

  task_ids <- vapply(task_bank, function(task) task$task_id, character(1))
  question_ids <- vapply(task_bank, function(task) task$question_id, character(1))
  families <- vapply(task_bank, function(task) task$task_family, character(1))
  principles <- vapply(task_bank, function(task) task$principle %||% NA_character_, character(1))

  if (anyDuplicated(task_ids)) {
    issues <- c(issues, "Task IDs must be unique.")
  }
  if (anyDuplicated(question_ids)) {
    issues <- c(issues, "Question IDs must be unique.")
  }

  analytic_n <- sum(families == ANALYTIC_FAMILY)
  if (analytic_n != 15) {
    issues <- c(issues, sprintf("Expected 15 analytic tasks, found %s.", analytic_n))
  }

  gestalt_counts <- table(factor(principles[principles %in% GESTALT_PRINCIPLES], levels = GESTALT_PRINCIPLES))
  if (any(gestalt_counts != 3)) {
    issues <- c(
      issues,
      paste(
        "Each gestalt family must contain exactly 3 tasks.",
        paste(sprintf("%s=%s", names(gestalt_counts), as.integer(gestalt_counts)), collapse = ", ")
      )
    )
  }

  multi_panel_n <- sum(vapply(task_bank, function(task) identical(task$renderer, "multi_panel_individual"), logical(1)))
  if (multi_panel_n != 2) {
    issues <- c(issues, sprintf("Expected exactly 2 multi_panel_individual tasks, found %s.", multi_panel_n))
  }

  for (task in task_bank) {
    if (!task$renderer %in% VALID_RENDERERS) {
      issues <- c(issues, paste("Unsupported renderer:", task$task_id))
    }
    if (!nzchar(task$question_text)) {
      issues <- c(issues, paste("Missing question text:", task$task_id))
    }

    if (identical(task$renderer, "single_plot")) {
      if (length(task$answer_choices) < 2) {
        issues <- c(issues, paste("Task needs at least two answer choices:", task$task_id))
      }
      if (!task$correct_answer %in% task$answer_choices) {
        issues <- c(issues, paste("Correct answer missing from choices:", task$task_id))
      }
      if (!is.function(task$plot_fun)) {
        issues <- c(issues, paste("single_plot task missing plot_fun:", task$task_id))
      }
    }

    if (identical(task$renderer, "multi_panel_individual")) {
      if (is.null(task$panels) || length(task$panels) != 4) {
        issues <- c(issues, paste("multi_panel_individual task must have exactly 4 panels:", task$task_id))
      } else {
        for (panel in task$panels) {
          if (!all(c("panel_id", "panel_label", "answer_choices", "correct_answer", "plot_fun", "panel_order") %in% names(panel))) {
            issues <- c(issues, paste("Panel definition is incomplete:", task$task_id))
            next
          }
          if (length(panel$answer_choices) < 2) {
            issues <- c(issues, paste("Panel needs at least two answer choices:", task$task_id, panel$panel_id))
          }
          if (!panel$correct_answer %in% panel$answer_choices) {
            issues <- c(issues, paste("Panel correct answer missing from choices:", task$task_id, panel$panel_id))
          }
          if (!is.function(panel$plot_fun)) {
            issues <- c(issues, paste("Panel plot_fun missing:", task$task_id, panel$panel_id))
          }
        }
      }
    }
  }

  if (length(issues) > 0) {
    stop(paste(c("Task bank validation failed:", issues), collapse = "\n"), call. = FALSE)
  }

  invisible(TRUE)
}

plot_shape_scatter <- function(
  data,
  xlim = c(0, 10),
  ylim = c(0, 10),
  show_axes = FALSE,
  show_labels = TRUE,
  augment_fn = NULL
) {
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  if (!"shape_id" %in% names(data)) data$shape_id <- 21
  if (!"fill" %in% names(data)) data$fill <- COLORS["blue"]
  if (!"outline" %in% names(data)) data$outline <- COLORS["navy"]
  if (!"size" %in% names(data)) data$size <- 5
  if (!"label" %in% names(data)) data$label <- NA_character_
  if (!"panel" %in% names(data)) data$panel <- "Сурет"

  p <- ggplot2::ggplot(data, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_point(
      ggplot2::aes(shape = shape_id, size = size, fill = fill, color = outline),
      stroke = 0.9,
      alpha = 0.96
    ) +
    ggplot2::scale_shape_identity() +
    ggplot2::scale_size_identity() +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_color_identity() +
    ggplot2::coord_fixed(ratio = 1, xlim = xlim, ylim = ylim, expand = FALSE, clip = "off") +
    experiment_theme()

  if (length(unique(data$panel)) > 1) {
    p <- p + ggplot2::facet_wrap(~panel)
  }

  if (isTRUE(show_labels)) {
    label_df <- data[!is.na(data$label) & nzchar(data$label), , drop = FALSE]
    if (nrow(label_df) > 0) {
      p <- p +
        ggplot2::geom_text(
          data = label_df,
          ggplot2::aes(label = label),
          inherit.aes = FALSE,
          color = COLORS["navy"],
          fontface = "bold",
          size = 3.8,
          vjust = -1
        )
    }
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

plot_bubble_generic <- function(
  data,
  xlim = c(0, 10),
  ylim = c(0, 10),
  show_axes = FALSE,
  augment_fn = NULL
) {
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  if (!"fill" %in% names(data)) data$fill <- COLORS["blue"]
  if (!"label" %in% names(data)) data$label <- NA_character_
  if (!"outline" %in% names(data)) data$outline <- COLORS["navy"]
  if (!"panel" %in% names(data)) data$panel <- "Сурет"
  data$plot_size <- rescale_values(data$value, to = c(10, 24))

  p <- ggplot2::ggplot(data, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_point(
      ggplot2::aes(size = plot_size, fill = fill, color = outline),
      shape = 21,
      stroke = 1,
      alpha = 0.95
    ) +
    ggplot2::scale_size_identity() +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_color_identity() +
    ggplot2::coord_fixed(ratio = 1, xlim = xlim, ylim = ylim, expand = FALSE, clip = "off") +
    experiment_theme()

  label_df <- data[!is.na(data$label) & nzchar(data$label), , drop = FALSE]
  if (nrow(label_df) > 0) {
    p <- p +
      ggplot2::geom_text(
        data = label_df,
        ggplot2::aes(label = label),
        inherit.aes = FALSE,
        color = COLORS["navy"],
        fontface = "bold",
        size = 3.6
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

plot_treemap_generic <- function(data, show_values = FALSE, augment_fn = NULL) {
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  if (!"fill" %in% names(data)) data$fill <- COLORS["blue"]
  if (!"label_color" %in% names(data)) data$label_color <- "#FFFFFF"

  data$x_mid <- (data$xmin + data$xmax) / 2
  data$y_mid <- (data$ymin + data$ymax) / 2
  data$label_text <- if (show_values) {
    paste0(data$category, "\n", data$value)
  } else {
    data$category
  }

  p <- ggplot2::ggplot(data) +
    ggplot2::geom_rect(
      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill),
      color = "#FFFFFF",
      linewidth = 1.1
    ) +
    ggplot2::geom_text(
      ggplot2::aes(x = x_mid, y = y_mid, label = label_text, color = label_color),
      fontface = "bold",
      lineheight = 0.95,
      size = 4
    ) +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_color_identity() +
    ggplot2::coord_equal(expand = FALSE, clip = "off") +
    ggplot2::theme_void(base_size = 13) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "#FFFFFF", colour = NA),
      panel.background = ggplot2::element_rect(fill = "#FBFDFF", colour = NA),
      plot.margin = ggplot2::margin(10, 10, 10, 10)
    )

  if (!is.null(augment_fn)) {
    p <- augment_fn(p, data)
  }

  p
}

build_task_bank <- function() {
  tasks <- list()

  single_task <- function(
    task_id,
    task_family,
    principle = NA_character_,
    visualization_type,
    question_type,
    question_text,
    answer_choices,
    correct_answer,
    plot_fun
  ) {
    make_task(
      task_id = task_id,
      task_family = task_family,
      principle = principle,
      visualization_type = visualization_type,
      question_type = question_type,
      question_text = question_text,
      answer_choices = answer_choices,
      correct_answer = correct_answer,
      renderer = "single_plot",
      chart_count = 1L,
      plot_fun = plot_fun
    )
  }

  multi_panel_task <- function(
    task_id,
    task_family,
    principle,
    visualization_type,
    question_type,
    question_text,
    panels
  ) {
    make_task(
      task_id = task_id,
      task_family = task_family,
      principle = principle,
      visualization_type = visualization_type,
      question_type = question_type,
      question_text = question_text,
      answer_choices = panels[[1]]$answer_choices,
      correct_answer = NA_character_,
      renderer = "multi_panel_individual",
      chart_count = length(panels),
      panels = panels
    )
  }

  incomplete_box <- function(xmin, xmax, ymin, ymax, color = COLORS["blue"], panel = NULL, prefix = "box") {
    x_span <- xmax - xmin
    dplyr::bind_rows(
      segment_points(xmin, ymax, xmax, ymax, n = 6, color = color, panel = panel, path_group = paste0(prefix, "_top")),
      segment_points(xmin, ymin, xmin, ymax - 0.35, n = 6, color = color, panel = panel, path_group = paste0(prefix, "_left")),
      segment_points(xmax, ymin + 0.35, xmax, ymax, n = 6, color = color, panel = panel, path_group = paste0(prefix, "_right")),
      segment_points(xmin, ymin, xmin + x_span * 0.34, ymin, n = 4, color = color, panel = panel, path_group = paste0(prefix, "_bottom_a")),
      segment_points(xmin + x_span * 0.60, ymin, xmax, ymin, n = 4, color = color, panel = panel, path_group = paste0(prefix, "_bottom_b"))
    )
  }

  # ---- Closure: 3 ----
  tasks[[length(tasks) + 1]] <- single_task(
    "CLO_01", "Closure", "Closure", "Structured fragments", "Closure region count",
    "Неше логикалық аймақ байқалады?",
    c("2", "3", "4", "5"),
    "3",
    function() {
      data <- dplyr::bind_rows(
        incomplete_box(1.0, 3.0, 4.1, 6.2, color = COLORS["blue"], prefix = "clo1_a"),
        incomplete_box(4.0, 6.1, 3.7, 5.9, color = COLORS["sage"], prefix = "clo1_b"),
        incomplete_box(7.0, 9.1, 4.0, 6.1, color = COLORS["coral"], prefix = "clo1_c"),
        segment_points(4.6, 1.8, 5.5, 2.6, n = 5, color = COLORS["mist"], path_group = "clo1_s1"),
        segment_points(5.8, 1.9, 6.5, 2.4, n = 4, color = COLORS["mist"], path_group = "clo1_s2")
      )
      plot_scatter_generic(data, xlim = c(0.5, 9.6), ylim = c(1.3, 6.8), connect = TRUE)
    }
  )

  tasks[[length(tasks) + 1]] <- single_task(
    "CLO_02", "Closure", "Closure", "Facet comparison", "Most complete region",
    "Қай аймақ ең тұтас пішін сияқты көрінеді?",
    c("Сол жақ аймақ", "Ортаңғы аймақ", "Оң жақ аймақ"),
    "Ортаңғы аймақ",
    function() {
      data <- dplyr::bind_rows(
        arc_points(5.0, 5.0, 1.2, 30, 120, n = 9, color = COLORS["mist"], panel = "Сол жақ аймақ", path_group = "a1"),
        arc_points(5.0, 5.0, 1.2, 210, 290, n = 8, color = COLORS["mist"], panel = "Сол жақ аймақ", path_group = "a2"),
        arc_points(5.0, 5.0, 1.15, 15, 125, n = 10, color = COLORS["blue"], panel = "Ортаңғы аймақ", path_group = "b1"),
        arc_points(5.0, 5.0, 1.15, 145, 255, n = 10, color = COLORS["blue"], panel = "Ортаңғы аймақ", path_group = "b2"),
        arc_points(5.0, 5.0, 1.15, 285, 350, n = 7, color = COLORS["blue"], panel = "Ортаңғы аймақ", path_group = "b3"),
        segment_points(3.7, 4.2, 4.4, 5.4, n = 5, color = COLORS["sand"], panel = "Оң жақ аймақ", path_group = "c1"),
        segment_points(5.0, 3.9, 5.8, 4.6, n = 5, color = COLORS["sand"], panel = "Оң жақ аймақ", path_group = "c2"),
        segment_points(5.5, 5.5, 6.3, 6.2, n = 4, color = COLORS["sand"], panel = "Оң жақ аймақ", path_group = "c3")
      )
      plot_scatter_generic(data, xlim = c(2.8, 7.2), ylim = c(3.3, 6.8), connect = TRUE)
    }
  )

  tasks[[length(tasks) + 1]] <- single_task(
    "CLO_03", "Closure", "Closure", "Facet comparison", "Whole form identification",
    "Қай топ бір бүтін белгі сияқты қабылданады?",
    c("Сол жақ топ", "Орталық топ", "Жоғарғы топ", "Оң жақ топ"),
    "Орталық топ",
    function() {
      data <- dplyr::bind_rows(
        segment_points(3.4, 4.0, 4.1, 4.8, n = 5, color = COLORS["mist"], panel = "Сол жақ топ", path_group = "s1"),
        segment_points(4.6, 5.3, 5.3, 6.0, n = 4, color = COLORS["mist"], panel = "Сол жақ топ", path_group = "s2"),
        segment_points(3.3, 6.1, 4.2, 6.1, n = 4, color = COLORS["mist"], panel = "Сол жақ топ", path_group = "s3"),
        incomplete_box(3.6, 6.4, 3.8, 6.2, color = COLORS["blue"], panel = "Орталық топ", prefix = "mid"),
        segment_points(3.1, 5.8, 3.9, 5.2, n = 4, color = COLORS["sand"], panel = "Жоғарғы топ", path_group = "t1"),
        segment_points(4.8, 4.9, 5.6, 5.8, n = 5, color = COLORS["sand"], panel = "Жоғарғы топ", path_group = "t2"),
        segment_points(6.0, 4.1, 6.5, 4.7, n = 4, color = COLORS["sand"], panel = "Жоғарғы топ", path_group = "t3"),
        segment_points(3.5, 4.0, 4.2, 4.7, n = 5, color = COLORS["coral"], panel = "Оң жақ топ", path_group = "r1"),
        segment_points(5.8, 4.1, 6.4, 5.0, n = 5, color = COLORS["coral"], panel = "Оң жақ топ", path_group = "r2"),
        segment_points(4.2, 6.0, 5.0, 5.3, n = 5, color = COLORS["coral"], panel = "Оң жақ топ", path_group = "r3")
      )
      plot_scatter_generic(data, xlim = c(2.8, 7.1), ylim = c(3.4, 6.7), connect = TRUE)
    }
  )

  # ---- Similarity: 3 ----
  tasks[[length(tasks) + 1]] <- single_task(
    "SIM_01", "Similarity", "Similarity", "Scatterplot", "Group count by color",
    "Түске қарай неше топ көрінеді?",
    c("2", "3", "4", "5"),
    "3",
    function() {
      data <- point_df(
        x = c(1.3, 2.6, 4.0, 5.4, 6.8, 2.0, 3.4, 4.8, 6.2, 1.8, 5.0, 7.3),
        y = c(6.4, 5.0, 6.1, 5.3, 6.2, 3.9, 4.3, 4.0, 4.6, 2.4, 2.7, 2.5),
        color = c(
          COLORS["blue"], COLORS["coral"], COLORS["gold"], COLORS["blue"],
          COLORS["coral"], COLORS["gold"], COLORS["blue"], COLORS["coral"],
          COLORS["gold"], COLORS["blue"], COLORS["coral"], COLORS["gold"]
        ),
        size = 5
      )
      plot_scatter_generic(data, xlim = c(0.8, 7.8), ylim = c(1.8, 6.9))
    }
  )

  tasks[[length(tasks) + 1]] <- single_task(
    "SIM_02", "Similarity", "Similarity", "Scatterplot", "Grouping criterion",
    "Бұл суретте топтасу қай белгі арқылы байқалады?",
    c("Түс", "Өлшем", "Пішін", "Арақашықтық"),
    "Өлшем",
    function() {
      data <- point_df(
        x = rep(c(1.6, 3.2, 4.8, 6.4), 3),
        y = c(rep(6.2, 4), rep(4.6, 4), rep(3.0, 4)),
        color = c(
          COLORS["coral"], COLORS["gold"], COLORS["blue"], COLORS["sage"],
          COLORS["gold"], COLORS["blue"], COLORS["sage"], COLORS["coral"],
          COLORS["blue"], COLORS["sage"], COLORS["coral"], COLORS["gold"]
        ),
        size = c(rep(8, 4), rep(6, 4), rep(4.2, 4))
      )
      plot_scatter_generic(data, xlim = c(0.8, 7.2), ylim = c(2.2, 6.9))
    }
  )

  tasks[[length(tasks) + 1]] <- single_task(
    "SIM_03", "Similarity", "Similarity", "Shape scatter", "Objects in same group",
    "Қай нысандар бір топқа жатады?",
    c("1 және 4", "2 және 5", "3 және 6", "1 және 6"),
    "1 және 4",
    function() {
      data <- data.frame(
        x = c(2.0, 4.2, 6.4, 2.2, 4.4, 6.2),
        y = c(6.0, 6.2, 6.0, 3.4, 3.2, 3.5),
        shape_id = c(22, 21, 24, 22, 21, 24),
        fill = c(COLORS["blue"], COLORS["coral"], COLORS["gold"], COLORS["sage"], COLORS["gold"], COLORS["coral"]),
        outline = COLORS["navy"],
        size = 6,
        label = c("1", "2", "3", "4", "5", "6"),
        stringsAsFactors = FALSE
      )
      plot_shape_scatter(data, xlim = c(1.2, 7.0), ylim = c(2.6, 6.8))
    }
  )

  # ---- Proximity: 3 ----
  tasks[[length(tasks) + 1]] <- single_task(
    "PROX_01", "Proximity", "Proximity", "Scatterplot", "Cluster count",
    "Неше жеке шоғыр көрінеді?",
    c("4", "5", "6", "7"),
    "5",
    function() {
      data <- dplyr::bind_rows(
        point_df(c(1.6, 2.0, 2.3), c(6.5, 6.0, 6.3), color = COLORS["blue"], size = 4.8),
        point_df(c(5.0, 5.4, 5.8), c(6.4, 6.0, 6.3), color = COLORS["coral"], size = 4.8),
        point_df(c(8.0, 8.3, 8.6), c(5.8, 6.2, 5.7), color = COLORS["gold"], size = 4.8),
        point_df(c(2.1, 2.4, 2.7), c(2.7, 3.2, 2.8), color = COLORS["sage"], size = 4.8),
        point_df(c(6.2, 6.5, 6.9), c(2.5, 3.0, 2.6), color = COLORS["plum"], size = 4.8)
      )
      plot_scatter_generic(data, xlim = c(0.8, 9.2), ylim = c(2.0, 7.0))
    }
  )

  tasks[[length(tasks) + 1]] <- single_task(
    "PROX_02", "Proximity", "Proximity", "Scatterplot", "Central cluster",
    "Қай шоғыр ортасында орналасқан?",
    c("Орталық топ", "Сол жақ топ", "Оң жақ топ", "Төменгі топ"),
    "Орталық топ",
    function() {
      data <- dplyr::bind_rows(
        point_df(c(2.0, 2.4, 2.6), c(5.8, 5.2, 5.6), color = COLORS["blue"], size = 5),
        point_df(c(7.4, 7.8, 8.0), c(5.9, 5.4, 5.7), color = COLORS["coral"], size = 5),
        point_df(c(4.5, 4.9, 5.2, 4.8), c(4.4, 4.8, 4.3, 3.9), color = COLORS["gold"], size = 5),
        point_df(c(4.8, 5.2, 5.6), c(2.3, 2.7, 2.4), color = COLORS["sage"], size = 5)
      )
      plot_scatter_generic(
        data,
        xlim = c(1.2, 8.8),
        ylim = c(1.8, 6.5),
        augment_fn = function(p, df) {
          p +
            ggplot2::annotate("text", x = 2.3, y = 6.35, label = "Сол жақ", color = COLORS["navy"], fontface = "bold") +
            ggplot2::annotate("text", x = 7.7, y = 6.35, label = "Оң жақ", color = COLORS["navy"], fontface = "bold") +
            ggplot2::annotate("text", x = 4.9, y = 5.5, label = "Орталық", color = COLORS["navy"], fontface = "bold") +
            ggplot2::annotate("text", x = 5.2, y = 3.2, label = "Төменгі", color = COLORS["navy"], fontface = "bold")
        }
      )
    }
  )

  tasks[[length(tasks) + 1]] <- single_task(
    "PROX_03", "Proximity", "Proximity", "Labeled scatter", "Distance-based grouping",
    "Қай нүктелер арақашықтығына қарай бір топ болып көрінеді?",
    c("1 және 2", "3 және 4", "5 және 7", "6 және 8"),
    "3 және 4",
    function() {
      data <- point_df(
        x = c(1.8, 3.1, 5.4, 5.8, 7.8, 2.2, 7.1, 8.6),
        y = c(6.0, 5.2, 5.8, 5.5, 5.9, 2.5, 2.4, 2.9),
        color = c(COLORS["blue"], COLORS["gold"], COLORS["coral"], COLORS["coral"], COLORS["sage"], COLORS["gold"], COLORS["plum"], COLORS["plum"]),
        size = 5.3,
        label = as.character(1:8)
      )
      plot_scatter_generic(data, xlim = c(1.0, 9.2), ylim = c(1.8, 6.6), show_labels = TRUE)
    }
  )

  # ---- Symmetry: 3 ----
  tasks[[length(tasks) + 1]] <- multi_panel_task(
    "SYM_01", "Symmetry", "Symmetry", "4-panel comparison", "Vertical symmetry check",
    "Әр панельдегі құрылым тік ось бойынша симметриялы ма?",
    panels = list(
      list(
        panel_id = "sym_tl",
        panel_label = "Жоғарғы сол",
        panel_order = 1L,
        answer_choices = c("Иә", "Жоқ"),
        correct_answer = "Иә",
        plot_fun = function() {
          data <- point_df(
            x = c(3.6, 4.4, 5.6, 6.4, 4.3, 5.7, 5.0),
            y = c(4.0, 5.6, 5.6, 4.0, 3.0, 3.0, 6.6),
            color = COLORS["blue"],
            size = 4.8
          )
          plot_scatter_generic(
            data,
            xlim = c(2.8, 7.2),
            ylim = c(2.5, 7.0),
            augment_fn = function(p, df) {
              p + ggplot2::annotate("segment", x = 5, xend = 5, y = 2.6, yend = 6.9, color = "#A8BCCB", linetype = "dashed")
            }
          )
        }
      ),
      list(
        panel_id = "sym_tr",
        panel_label = "Жоғарғы оң",
        panel_order = 2L,
        answer_choices = c("Иә", "Жоқ"),
        correct_answer = "Жоқ",
        plot_fun = function() {
          data <- point_df(
            x = c(3.5, 4.1, 5.3, 6.6, 4.0, 5.9, 6.1),
            y = c(4.1, 5.9, 6.2, 3.8, 2.8, 3.1, 5.0),
            color = COLORS["coral"],
            size = 4.8
          )
          plot_scatter_generic(
            data,
            xlim = c(2.8, 7.2),
            ylim = c(2.5, 7.0),
            augment_fn = function(p, df) {
              p + ggplot2::annotate("segment", x = 5, xend = 5, y = 2.6, yend = 6.9, color = "#A8BCCB", linetype = "dashed")
            }
          )
        }
      ),
      list(
        panel_id = "sym_bl",
        panel_label = "Төменгі сол",
        panel_order = 3L,
        answer_choices = c("Иә", "Жоқ"),
        correct_answer = "Иә",
        plot_fun = function() {
          data <- point_df(
            x = c(3.7, 4.2, 5.8, 6.3, 4.0, 6.0, 5.0),
            y = c(4.3, 5.4, 5.4, 4.3, 3.2, 3.2, 6.2),
            color = COLORS["sage"],
            size = 4.6
          )
          plot_scatter_generic(
            data,
            xlim = c(2.8, 7.2),
            ylim = c(2.7, 6.7),
            augment_fn = function(p, df) {
              p + ggplot2::annotate("segment", x = 5, xend = 5, y = 2.8, yend = 6.6, color = "#A8BCCB", linetype = "dashed")
            }
          )
        }
      ),
      list(
        panel_id = "sym_br",
        panel_label = "Төменгі оң",
        panel_order = 4L,
        answer_choices = c("Иә", "Жоқ"),
        correct_answer = "Жоқ",
        plot_fun = function() {
          data <- point_df(
            x = c(3.5, 4.6, 5.5, 6.4, 4.0, 5.9, 6.0),
            y = c(4.0, 5.5, 4.1, 5.7, 3.0, 3.0, 4.8),
            color = COLORS["gold"],
            size = 4.6
          )
          plot_scatter_generic(
            data,
            xlim = c(2.8, 7.2),
            ylim = c(2.7, 6.7),
            augment_fn = function(p, df) {
              p + ggplot2::annotate("segment", x = 5, xend = 5, y = 2.8, yend = 6.6, color = "#A8BCCB", linetype = "dashed")
            }
          )
        }
      )
    )
  )

  tasks[[length(tasks) + 1]] <- single_task(
    "SYM_02", "Symmetry", "Symmetry", "Facet comparison", "Mirrored half identification",
    "Қай нұсқада оң бөлік сол жақтың айнасы?",
    c("Сол жақ нұсқа", "Ортаңғы нұсқа", "Оң жақ нұсқа"),
    "Ортаңғы нұсқа",
    function() {
      left_half <- point_df(
        x = c(3.4, 4.1, 4.5),
        y = c(4.1, 5.7, 3.3),
        color = COLORS["navy"],
        size = 4.8
      )

      data <- dplyr::bind_rows(
        dplyr::mutate(left_half, panel = "Сол жақ нұсқа"),
        point_df(c(5.5, 6.1, 6.6), c(5.5, 4.2, 3.9), color = COLORS["coral"], size = 4.8, panel = "Сол жақ нұсқа"),
        dplyr::mutate(left_half, panel = "Ортаңғы нұсқа"),
        point_df(c(5.5, 5.9, 6.6), c(3.3, 5.7, 4.1), color = COLORS["blue"], size = 4.8, panel = "Ортаңғы нұсқа"),
        dplyr::mutate(left_half, panel = "Оң жақ нұсқа"),
        point_df(c(5.5, 6.0, 6.4), c(4.8, 3.5, 5.9), color = COLORS["sage"], size = 4.8, panel = "Оң жақ нұсқа")
      )

      plot_scatter_generic(
        data,
        xlim = c(2.8, 7.0),
        ylim = c(2.8, 6.4),
        augment_fn = function(p, df) {
          p + ggplot2::geom_vline(xintercept = 5, linetype = "dashed", color = "#A8BCCB")
        }
      )
    }
  )

  tasks[[length(tasks) + 1]] <- single_task(
    "SYM_03", "Symmetry", "Symmetry", "Bar chart", "Mirror pair equality",
    "Қай екі баған айна жұбы ретінде тең тұр?",
    c("1 және 5", "1 және 3", "2 және 4", "3 және 5"),
    "1 және 5",
    function() {
      plot_bar_generic(
        data.frame(
          category = c("1", "2", "3", "4", "5"),
          value = c(14, 22, 29, 22, 14),
          fill = c(COLORS["blue"], COLORS["mist"], COLORS["gold"], COLORS["mist"], COLORS["blue"]),
          stringsAsFactors = FALSE
        )
      )
    }
  )

  # ---- Continuity: 3 ----
  tasks[[length(tasks) + 1]] <- single_task(
    "CONT_01", "Continuity", "Continuity", "Crossing lines", "Natural continuation",
    "A сызығы қай нүктеге табиғи жалғасады?",
    c("1", "2"),
    "2",
    function() {
      path_a <- data.frame(
        x = seq(1, 9, length.out = 9),
        y = c(7.7, 7.1, 6.6, 6.0, 5.5, 4.9, 4.3, 3.8, 3.2),
        group = "A",
        col = COLORS["blue"]
      )
      path_b <- data.frame(
        x = seq(1, 9, length.out = 9),
        y = c(3.2, 3.8, 4.3, 4.9, 5.5, 6.0, 6.6, 7.1, 7.7),
        group = "B",
        col = COLORS["coral"]
      )
      endpoints <- data.frame(
        x = c(1, 1, 9, 9),
        y = c(7.7, 3.2, 7.7, 3.2),
        label = c("A", "B", "1", "2"),
        nudges = c(0.35, -0.35, 0.35, -0.35),
        stringsAsFactors = FALSE
      )

      ggplot2::ggplot(dplyr::bind_rows(path_a, path_b), ggplot2::aes(x = x, y = y, group = group)) +
        ggplot2::geom_path(ggplot2::aes(color = col), linewidth = 1.5, lineend = "round") +
        ggplot2::geom_point(ggplot2::aes(color = col), size = 2.8) +
        ggplot2::geom_text(
          data = endpoints,
          ggplot2::aes(x = x, y = y + nudges, label = label),
          inherit.aes = FALSE,
          color = COLORS["navy"],
          fontface = "bold",
          size = 4.6
        ) +
        ggplot2::scale_color_identity() +
        ggplot2::coord_fixed(xlim = c(0.6, 9.4), ylim = c(2.5, 8.3), expand = FALSE, clip = "off") +
        experiment_theme() +
        ggplot2::theme(
          axis.text = ggplot2::element_blank(),
          axis.ticks = ggplot2::element_blank(),
          axis.title = ggplot2::element_blank(),
          panel.grid = ggplot2::element_blank()
        )
    }
  )

  tasks[[length(tasks) + 1]] <- single_task(
    "CONT_02", "Continuity", "Continuity", "Interwoven paths", "Count main lines",
    "Неше негізгі сызық көрінеді?",
    c("2", "3", "4", "5"),
    "3",
    function() {
      data <- dplyr::bind_rows(
        curve_points(c(1.2, 2.0, 2.8, 3.7, 4.6, 5.4, 6.3), c(6.5, 6.1, 5.6, 5.2, 4.9, 4.5, 4.0), color = COLORS["blue"], path_group = "l1"),
        curve_points(c(1.4, 2.2, 3.0, 3.8, 4.8, 5.8, 6.8), c(3.0, 3.5, 4.0, 4.6, 5.1, 5.7, 6.2), color = COLORS["blue"], path_group = "l2"),
        curve_points(c(2.0, 2.6, 3.4, 4.2, 5.0, 5.8, 6.6), c(1.8, 2.4, 3.1, 3.8, 4.4, 5.1, 5.8), color = COLORS["blue"], path_group = "l3")
      )
      plot_scatter_generic(data, xlim = c(0.8, 7.2), ylim = c(1.2, 6.9), connect = TRUE)
    }
  )

  tasks[[length(tasks) + 1]] <- single_task(
    "CONT_03", "Continuity", "Continuity", "Facet comparison", "Unified line",
    "Қай қатар біртұтас сызық ретінде оқылады?",
    c("Жоғарғы қатар", "Ортаңғы қатар", "Төменгі қатар"),
    "Ортаңғы қатар",
    function() {
      data <- dplyr::bind_rows(
        curve_points(c(3.1, 3.8, 4.5, 5.2, 5.9), c(5.8, 4.4, 5.7, 4.3, 5.6), color = COLORS["mist"], panel = "Жоғарғы қатар", path_group = "u1"),
        curve_points(c(3.0, 3.8, 4.6, 5.4, 6.2), c(3.2, 3.9, 4.5, 5.2, 5.9), color = COLORS["blue"], panel = "Ортаңғы қатар", path_group = "u2"),
        curve_points(c(3.2, 4.0, 4.7, 5.5, 6.1), c(5.7, 5.0, 4.1, 5.4, 4.4), color = COLORS["sand"], panel = "Төменгі қатар", path_group = "u3")
      )
      plot_scatter_generic(data, xlim = c(2.6, 6.7), ylim = c(3.0, 6.2), connect = TRUE)
    }
  )

  # ---- Analytics: 15 ----
  tasks[[length(tasks) + 1]] <- multi_panel_task(
    "ANL_01", ANALYTIC_FAMILY, NA_character_, "4-panel bar comparison", "Maximum in each panel",
    "Әр панельде ең жоғары баған қай нөмірде?",
    panels = list(
      list(
        panel_id = "anl_tl",
        panel_label = "Жоғарғы сол",
        panel_order = 1L,
        answer_choices = c("1", "2", "3", "4"),
        correct_answer = "2",
        plot_fun = function() {
          plot_bar_generic(
            data.frame(category = c("1", "2", "3", "4"), value = c(16, 28, 19, 21), fill = COLORS["blue"], stringsAsFactors = FALSE),
            compact = TRUE,
            show_value_labels = FALSE
          )
        }
      ),
      list(
        panel_id = "anl_tr",
        panel_label = "Жоғарғы оң",
        panel_order = 2L,
        answer_choices = c("1", "2", "3", "4"),
        correct_answer = "4",
        plot_fun = function() {
          plot_bar_generic(
            data.frame(category = c("1", "2", "3", "4"), value = c(11, 17, 22, 29), fill = COLORS["sage"], stringsAsFactors = FALSE),
            compact = TRUE,
            show_value_labels = FALSE
          )
        }
      ),
      list(
        panel_id = "anl_bl",
        panel_label = "Төменгі сол",
        panel_order = 3L,
        answer_choices = c("1", "2", "3", "4"),
        correct_answer = "1",
        plot_fun = function() {
          plot_bar_generic(
            data.frame(category = c("1", "2", "3", "4"), value = c(30, 21, 18, 24), fill = COLORS["gold"], stringsAsFactors = FALSE),
            compact = TRUE,
            show_value_labels = FALSE
          )
        }
      ),
      list(
        panel_id = "anl_br",
        panel_label = "Төменгі оң",
        panel_order = 4L,
        answer_choices = c("1", "2", "3", "4"),
        correct_answer = "3",
        plot_fun = function() {
          plot_bar_generic(
            data.frame(category = c("1", "2", "3", "4"), value = c(13, 20, 27, 23), fill = COLORS["coral"], stringsAsFactors = FALSE),
            compact = TRUE,
            show_value_labels = FALSE
          )
        }
      )
    )
  )

  tasks[[length(tasks) + 1]] <- single_task(
    "ANL_02", ANALYTIC_FAMILY, NA_character_, "Bar chart", "Compare two categories",
    "Алма мен Шиенің қайсысы жоғары?",
    c("Алма", "Шие"),
    "Шие",
    function() {
      plot_bar_generic(
        data.frame(
          category = c("Алма", "Алмұрт", "Өрік", "Шие", "Жүзім"),
          value = c(18, 25, 14, 22, 16),
          fill = c(COLORS["blue"], COLORS["mist"], COLORS["gold"], COLORS["coral"], COLORS["sage"]),
          stringsAsFactors = FALSE
        )
      )
    }
  )

  tasks[[length(tasks) + 1]] <- single_task(
    "ANL_03", ANALYTIC_FAMILY, NA_character_, "Bar chart", "Second highest category",
    "Екінші ең жоғары санат қайсы?",
    c("Солтүстік", "Шығыс", "Орталық", "Оңтүстік"),
    "Орталық",
    function() {
      plot_bar_generic(
        data.frame(
          category = c("Солтүстік", "Шығыс", "Батыс", "Орталық", "Оңтүстік"),
          value = c(35, 41, 28, 38, 24),
          fill = c(COLORS["blue"], COLORS["coral"], COLORS["gold"], COLORS["sage"], COLORS["plum"]),
          stringsAsFactors = FALSE
        )
      )
    }
  )

  tasks[[length(tasks) + 1]] <- single_task(
    "ANL_04", ANALYTIC_FAMILY, NA_character_, "Heatmap", "Exact value lookup",
    "B-3 ұяшығындағы нақты мән қандай?",
    c("21", "24", "27", "30"),
    "27",
    function() {
      plot_heatmap_generic(
        data.frame(
          row = rep(c("A", "B", "C", "D"), each = 4),
          col = rep(c("1", "2", "3", "4"), times = 4),
          value = c(
            12, 18, 16, 14,
            20, 24, 27, 22,
            15, 19, 21, 17,
            9, 13, 18, 11
          ),
          stringsAsFactors = FALSE
        ),
        show_values = TRUE
      )
    }
  )

  tasks[[length(tasks) + 1]] <- single_task(
    "ANL_05", ANALYTIC_FAMILY, NA_character_, "Heatmap", "Minimum cell",
    "Қай ұяшықтағы мән ең төмен?",
    c("A-4", "B-2", "C-1", "D-3"),
    "C-1",
    function() {
      plot_heatmap_generic(
        data.frame(
          row = rep(c("A", "B", "C", "D"), each = 4),
          col = rep(c("1", "2", "3", "4"), times = 4),
          value = c(
            16, 20, 18, 11,
            14, 17, 19, 15,
            6, 12, 10, 13,
            22, 25, 24, 21
          ),
          stringsAsFactors = FALSE
        ),
        show_values = TRUE
      )
    }
  )

  tasks[[length(tasks) + 1]] <- single_task(
    "ANL_06", ANALYTIC_FAMILY, NA_character_, "Heatmap", "Closest row averages",
    "Орташа мәні бір-біріне ең жақын екі қатар қайсы?",
    c("A және B", "B және C", "C және D", "A және D"),
    "B және C",
    function() {
      plot_heatmap_generic(
        data.frame(
          row = rep(c("A", "B", "C", "D"), each = 4),
          col = rep(c("1", "2", "3", "4"), times = 4),
          value = c(
            12, 14, 16, 18,
            23, 24, 25, 24,
            24, 23, 22, 25,
            31, 32, 30, 29
          ),
          stringsAsFactors = FALSE
        ),
        show_values = TRUE
      )
    }
  )

  tasks[[length(tasks) + 1]] <- single_task(
    "ANL_07", ANALYTIC_FAMILY, NA_character_, "Treemap", "Largest category",
    "Қай санаттың ауданы ең үлкен?",
    c("Сату", "Қызмет", "Логистика", "Маркетинг"),
    "Сату",
    function() {
      plot_treemap_generic(
        data.frame(
          category = c("Сату", "Қызмет", "Логистика", "Маркетинг", "Қолдау"),
          value = c(55, 28, 10, 4, 3),
          xmin = c(0, 55, 55, 80, 80),
          xmax = c(55, 100, 80, 100, 100),
          ymin = c(0, 45, 0, 20, 0),
          ymax = c(100, 100, 45, 45, 20),
          fill = c(COLORS["blue"], COLORS["sage"], COLORS["gold"], COLORS["coral"], COLORS["plum"]),
          label_color = c("#FFFFFF", "#FFFFFF", COLORS["navy"], "#FFFFFF", "#FFFFFF"),
          stringsAsFactors = FALSE
        ),
        show_values = FALSE
      )
    }
  )

  tasks[[length(tasks) + 1]] <- single_task(
    "ANL_08", ANALYTIC_FAMILY, NA_character_, "Treemap", "Value range estimation",
    "«Қызмет» блогының шамасы қай аралықта?",
    c("20-30", "30-40", "40-50", "50-60"),
    "30-40",
    function() {
      plot_treemap_generic(
        data.frame(
          category = c("Өнім", "Қызмет", "Инфрақұрылым", "Қолдау"),
          value = c(42, 35, 15, 8),
          xmin = c(0, 52, 52, 80),
          xmax = c(52, 100, 80, 100),
          ymin = c(0, 35, 0, 0),
          ymax = c(100, 100, 35, 35),
          fill = c(COLORS["coral"], COLORS["blue"], COLORS["gold"], COLORS["sage"]),
          label_color = c("#FFFFFF", "#FFFFFF", COLORS["navy"], COLORS["navy"]),
          stringsAsFactors = FALSE
        ),
        show_values = FALSE
      )
    }
  )

  tasks[[length(tasks) + 1]] <- single_task(
    "ANL_09", ANALYTIC_FAMILY, NA_character_, "Treemap", "Closest pair of areas",
    "Аудандары бір-біріне ең жақын екі санат қайсы?",
    c("Солтүстік және Орталық", "Шығыс және Батыс", "Батыс және Онлайн", "Орталық және Онлайн"),
    "Солтүстік және Орталық",
    function() {
      plot_treemap_generic(
        data.frame(
          category = c("Солтүстік", "Орталық", "Шығыс", "Батыс", "Онлайн"),
          value = c(28, 26, 17, 12, 9),
          xmin = c(0, 50, 50, 76, 76),
          xmax = c(50, 100, 76, 100, 100),
          ymin = c(0, 48, 0, 18, 0),
          ymax = c(100, 100, 48, 48, 18),
          fill = c(COLORS["blue"], COLORS["sage"], COLORS["gold"], COLORS["coral"], COLORS["plum"]),
          label_color = c("#FFFFFF", "#FFFFFF", COLORS["navy"], "#FFFFFF", "#FFFFFF"),
          stringsAsFactors = FALSE
        ),
        show_values = FALSE
      )
    }
  )

  tasks[[length(tasks) + 1]] <- single_task(
    "ANL_10", ANALYTIC_FAMILY, NA_character_, "Bubble chart", "Outlier identification",
    "Қай нысан көлемі бойынша айқын ауытқу көрсетеді?",
    c("Нысан 1", "Нысан 3", "Нысан 5", "Нысан 6"),
    "Нысан 6",
    function() {
      plot_bubble_generic(
        data.frame(
          x = c(1.8, 3.4, 5.1, 6.8, 4.6, 8.1),
          y = c(6.0, 4.6, 6.3, 4.1, 2.6, 2.9),
          value = c(22, 26, 24, 25, 23, 52),
          label = c("1", "2", "3", "4", "5", "6"),
          fill = c(COLORS["blue"], COLORS["sage"], COLORS["gold"], COLORS["coral"], COLORS["mist"], COLORS["plum"]),
          outline = COLORS["navy"],
          stringsAsFactors = FALSE
        ),
        xlim = c(1.0, 9.0),
        ylim = c(2.0, 6.9)
      )
    }
  )

  tasks[[length(tasks) + 1]] <- single_task(
    "ANL_11", ANALYTIC_FAMILY, NA_character_, "Bubble chart", "Compare bubble sizes",
    "Солт. пен Бат. қайсысының көпіршігі үлкен?",
    c("Солт.", "Бат."),
    "Бат.",
    function() {
      plot_bubble_generic(
        data.frame(
          x = c(2.0, 4.0, 6.0, 8.0, 5.0),
          y = c(5.8, 5.4, 5.9, 5.2, 2.8),
          value = c(18, 24, 20, 29, 22),
          label = c("Солт.", "Орт.", "Шығ.", "Бат.", "Оңт."),
          fill = c(COLORS["blue"], COLORS["gold"], COLORS["sage"], COLORS["coral"], COLORS["plum"]),
          outline = COLORS["navy"],
          stringsAsFactors = FALSE
        ),
        xlim = c(1.2, 8.8),
        ylim = c(2.0, 6.6)
      )
    }
  )

  tasks[[length(tasks) + 1]] <- single_task(
    "ANL_12", ANALYTIC_FAMILY, NA_character_, "Bubble chart", "Typical range",
    "Көпшілік көпіршіктің мөлшері қай аралықта?",
    c("10-20", "20-30", "30-40", "40-50"),
    "20-30",
    function() {
      plot_bubble_generic(
        data.frame(
          x = c(1.8, 3.1, 4.4, 5.7, 7.0, 8.2),
          y = c(5.9, 4.2, 5.4, 3.1, 5.8, 3.8),
          value = c(23, 25, 24, 26, 22, 47),
          label = c("1", "2", "3", "4", "5", "6"),
          fill = c(COLORS["blue"], COLORS["sage"], COLORS["gold"], COLORS["coral"], COLORS["mist"], COLORS["plum"]),
          outline = COLORS["navy"],
          stringsAsFactors = FALSE
        ),
        xlim = c(1.0, 9.0),
        ylim = c(2.4, 6.5)
      )
    }
  )

  tasks[[length(tasks) + 1]] <- single_task(
    "ANL_13", ANALYTIC_FAMILY, NA_character_, "Line chart", "Largest increase interval",
    "Ең үлкен өсім қай аралықта болды?",
    c("1-2", "2-3", "3-4", "5-6"),
    "3-4",
    function() {
      plot_line_generic(
        data.frame(
          x = 1:6,
          y = c(12, 14, 13, 20, 21, 19),
          display_color = COLORS["blue"],
          stringsAsFactors = FALSE
        ),
        x_labels = c("1", "2", "3", "4", "5", "6")
      )
    }
  )

  tasks[[length(tasks) + 1]] <- single_task(
    "ANL_14", ANALYTIC_FAMILY, NA_character_, "Line chart", "Largest decrease interval",
    "Ең үлкен төмендеу қай аралықта болды?",
    c("1-2", "2-3", "4-5", "5-6"),
    "4-5",
    function() {
      plot_line_generic(
        data.frame(
          x = 1:6,
          y = c(11, 15, 18, 23, 16, 14),
          display_color = COLORS["coral"],
          stringsAsFactors = FALSE
        ),
        x_labels = c("1", "2", "3", "4", "5", "6")
      )
    }
  )

  tasks[[length(tasks) + 1]] <- single_task(
    "ANL_15", ANALYTIC_FAMILY, NA_character_, "Line chart", "Exact value on line",
    "5-күндегі мән неше?",
    c("11", "12", "13", "14"),
    "13",
    function() {
      plot_line_generic(
        data.frame(
          x = 1:6,
          y = c(7, 9, 8, 11, 13, 12),
          display_color = COLORS["gold"],
          stringsAsFactors = FALSE
        ),
        x_labels = c("1", "2", "3", "4", "5", "6")
      )
    }
  )

  tasks
}
