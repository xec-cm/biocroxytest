test_build_exclusions <- function(path) {
  file <- file.path(path, ".Rbuildignore")
  if (!file.exists(file) && !is_symlink(file)) { return(character()) }
  check_test_input_types(file)
  patterns <- readLines(file, warn = FALSE)
  patterns[nzchar(patterns)]
}

test_build_ignored <- function(paths, patterns, ignore_case = TRUE) {
  ignored <- rep(FALSE, length(paths))
  for (pattern in patterns) {
    ignored <- ignored | grepl(pattern, paths, perl = TRUE, ignore.case = ignore_case)
  }
  ignored
}

test_r_sources <- function(path) {
  files <- list.files(file.path(path, "R"), pattern = "[.][Rr]$")
  paste0("R/", files, recycle0 = TRUE)
}

test_ignored_sources <- function(sources, patterns, legacy = FALSE) {
  excluded <- test_build_ignored(sources, patterns, ignore_case = !legacy)
  if (legacy) { return(excluded) }
  parents <- dirname(sources)
  while (any(parents != ".")) {
    active <- parents != "."
    excluded[active] <- excluded[active] | test_build_ignored(parents[active], patterns)
    parents <- dirname(parents)
  }
  excluded
}

test_collate_sources <- function(path, fields = "Collate") {
  description <- file.path(path, "DESCRIPTION")
  if (!file.exists(description)) { return(character()) }
  values <- read.dcf(description, fields = fields)[1L, ]
  listed <- unlist(lapply(values[!is.na(values)], function(value) {
    scan(text = value, what = character(), quiet = TRUE)
  }), use.names = FALSE)
  paste0("R/", as.character(listed), recycle0 = TRUE)
}

select_test_blocks <- function(blocks, path, env) {
  patterns <- test_build_exclusions(path)
  if (!length(patterns)) { return(blocks) }
  sources <- unique(c(test_collate_sources(path), sort(test_r_sources(path), method = "radix")))
  excluded <- test_ignored_sources(sources, patterns)
  # Roxygen can report the resolved target of a source link as the block origin.
  origins <- normalizePath(file.path(path, sources), winslash = "/", mustWork = FALSE)
  omitted <- setdiff(origins[excluded], origins[!excluded])
  block_files <- vapply(blocks, function(block) {
    if (length(block$file)) normalizePath(block$file, winslash = "/", mustWork = FALSE) else ""
  }, character(1))
  keep <- !block_files %in% omitted
  blocks <- blocks[keep]

  # A case-sensitive negated pattern can make roxygen omit a retained source.
  missed <- !excluded & test_build_ignored(sources, patterns, ignore_case = FALSE) &
    !origins %in% block_files & file.exists(origins)
  if (!any(missed)) { return(blocks) }
  recovered <- lapply(origins[missed], roxygen2::parse_file, env = env)
  files <- c(block_files[keep], rep(origins[missed], lengths(recovered)))
  blocks <- c(blocks, unlist(recovered, recursive = FALSE))
  # Match roxygen's @order, then Collate or C-sorted source order.
  order_tag <- vapply(blocks, function(block) {
    tag <- roxygen2::block_get_tag(block, "order")
    if (is.null(tag)) Inf else as.double(tag$val)
  }, numeric(1))
  blocks[order(order_tag, match(files, origins))]
}

test_package_manifest <- function(path, legacy = FALSE) {
  metadata <- c("DESCRIPTION", "NAMESPACE", ".Rbuildignore", ".BBSoptions", ".Rinstignore")
  suites <- c("tests", "longtests")
  roots <- c(metadata, suites, "R", "src", "data", "inst", "man", "vignettes",
             "demo", "po", "tools", "exec", "configure", "configure.win", "configure.ucrt",
             "cleanup", "cleanup.win", "cleanup.ucrt", "LICENSE", "LICENSE.md", "LICENCE")
  patterns <- extra <- retained <- ancestors <- character()
  if (!legacy) {
    artifacts <- c("(^|/)\\.DS_Store$", "~$", "\\.bak$", "\\.sw.$", "(^|/)\\.#[^/]*$",
                   "(^|/)#[^/]*#$", "^src/.*\\.(d|o|obj|so|dll|dylib)$",
                   "^src/Makedeps$", "^src/so_locations$")
    patterns <- c(test_build_exclusions(path), artifacts)
    extra <- extra_test_inputs(path)
    roxygen_inputs <- c("man-roxygen", "man/roxygen")
    retained <- c(roxygen_inputs, extra)
    ancestors <- unique(unlist(lapply(strsplit(retained, "/", fixed = TRUE), function(parts) {
      vapply(seq_len(length(parts) - 1L), function(i) {
        paste(parts[seq_len(i)], collapse = "/")
      }, character(1))
    }), use.names = FALSE))
  }

  walk <- function(relative = "", include_tree = legacy, declared_only = FALSE) {
    directory <- if (nzchar(relative)) file.path(path, relative) else path
    entries <- basename(fs::dir_ls(directory, all = TRUE))
    entries <- entries[entries != ".git"]
    paths <- if (nzchar(relative)) paste0(relative, "/", entries, recycle0 = TRUE) else entries
    paths <- enc2utf8(paths)
    ignored <- test_build_ignored(paths, patterns)
    # The R tree follows build exclusions even when declared explicitly.
    excluded_code <- grepl("^R(/|$)", paths) & ignored
    if (!include_tree) {
      keep <- if (!nzchar(relative)) entries %in% roots else rep(!declared_only, length(entries))
      keep <- (keep & !ignored) |
        paths %in% c(metadata, suites, retained, ancestors)
      paths <- paths[keep & !excluded_code]
    } else {
      paths <- paths[!excluded_code]
    }
    files <- file.path(path, paths)
    types <- check_test_input_types(files)
    directories <- paths[types == "directory"]
    children <- lapply(directories, function(directory) {
      next_declared <- declared_only || test_build_ignored(directory, patterns) ||
        (!nzchar(relative) && !directory %in% roots)
      complete <- include_tree || directory %in% suites ||
        (directory %in% retained && !grepl("^R(/|$)", directory))
      walk(directory, complete, next_declared)
    })
    do.call(rbind, c(list(data.frame(path = paths, directory = types == "directory")), children))
  }

  manifest <- walk()
  if (!legacy) {
    missing <- setdiff(extra, manifest$path)
    if (length(missing)) {
      cli::cli_abort("Declared test inputs are missing or exclude R source: {.file {missing}}.",
                     class = "biocroxytest_test_check_input", call = NULL)
    }
  }
  manifest[order(manifest$path, method = "radix"), , drop = FALSE]
}

extra_test_inputs <- function(path) {
  field <- "Config/biocroxytest/inputs"
  description <- file.path(path, "DESCRIPTION")
  check_test_input_types(description)
  value <- read.dcf(description, fields = field, keep.white = field)[1L, 1L]
  if (is.na(value)) { return(character()) }
  inputs <- trimws(strsplit(value, "\n", fixed = TRUE)[[1L]])
  inputs <- unique(enc2utf8(inputs[nzchar(inputs)]))
  invalid <- grepl("^[~/]|/$|//|[:\\\\[:cntrl:]]|(^|/)\\.{1,2}(/|$)|(^|/)\\.git(/|$)",
                   inputs, ignore.case = TRUE)
  if (any(invalid)) {
    cli::cli_abort("{.field {field}} needs relative package paths: {.file {inputs[invalid]}}.",
                   class = "biocroxytest_test_check_input", call = NULL)
  }
  inputs
}

check_test_input_types <- function(paths) {
  types <- fs::file_info(paths, follow = FALSE)$type
  unsupported <- which(!types %in% c("file", "directory"))
  if (length(unsupported)) {
    entry <- paths[unsupported[1L]]
    cli::cli_abort(
      "Cannot copy linked or special file {.file {entry}}.",
      class = "biocroxytest_test_check_input", call = NULL
    )
  }
  types
}

copy_test_package <- function(from, to, manifest = test_package_manifest(from)) {
  fs::dir_create(c(to, file.path(to, manifest$path[manifest$directory])))
  files <- manifest$path[!manifest$directory]
  if (length(files)) { fs::file_copy(file.path(from, files), file.path(to, files)) }
}
