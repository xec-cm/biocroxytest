test_build_exclusions <- function(path) {
  file <- file.path(path, ".Rbuildignore")
  if (!file.exists(file) && !is_symlink(file)) { return(character()) }
  check_test_input_types(file)
  patterns <- readLines(file, warn = FALSE)
  patterns[nzchar(patterns)]
}

test_build_ignored <- function(paths, patterns) {
  ignored <- rep(FALSE, length(paths))
  for (pattern in patterns) {
    ignored <- ignored | grepl(pattern, paths, perl = TRUE, ignore.case = TRUE)
  }
  ignored
}

filter_test_blocks <- function(blocks, path) {
  patterns <- test_build_exclusions(path)
  if (!length(patterns)) { return(blocks) }
  files <- vapply(blocks, function(block) {
    if (length(block$file)) {
      as.character(fs::path_rel(block$file, start = path))
    } else {
      ""
    }
  }, character(1))
  excluded <- test_build_ignored(files, patterns) | test_build_ignored("R", patterns)
  blocks[!(startsWith(files, "R/") & excluded)]
}

test_package_manifest <- function(path, legacy = FALSE) {
  metadata <- c("DESCRIPTION", "NAMESPACE", ".Rbuildignore", ".BBSoptions", ".Rinstignore")
  suites <- c("tests", "longtests")
  roots <- c(metadata, suites, "R", "src", "data", "inst", "man", "vignettes",
             "demo", "po", "tools", "exec", "configure", "configure.win",
             "cleanup", "cleanup.win", "LICENSE", "LICENSE.md", "LICENCE")
  patterns <- extra <- ancestors <- character()
  if (!legacy) {
    patterns <- test_build_exclusions(path)
    extra <- extra_test_inputs(path)
    ancestors <- unique(unlist(lapply(strsplit(extra, "/", fixed = TRUE), function(parts) {
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
        paths %in% c(metadata, suites, extra, ancestors)
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
        (directory %in% extra && !grepl("^R(/|$)", directory))
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
