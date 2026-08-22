# The full-matrix path is exercised here without any real network access:
# a refused localhost connection stands in for "offline", and a file:// URL
# with a wrong checksum stands in for "the resource changed". Nothing is
# written outside tempdir().

tmp_dirs <- function() {
  root <- tempfile("mlcvi-cache-")
  list(persistent = file.path(root, "persistent"),
       session    = file.path(root, "session"))
}

test_that("a dead URL fails gracefully: message, NULL, no warning or error", {
  skip_on_cran()
  dirs <- tmp_dirs()
  expect_no_warning(
    msgs <- capture_messages(
      out <- .mlcvi_read_full(cache = "session", url = "http://127.0.0.1:9/x.rds",
                              md5 = NULL, bundled = "", dirs = dirs)
    )
  )
  expect_match(msgs, "Could not download", all = FALSE)
  expect_null(out)
  expect_false(file.exists(file.path(dirs$session, .mlcvi_train_file)))
  expect_length(list.files(dirs$session, all.files = TRUE, no.. = TRUE), 0)
})

test_that("a changed resource (bad checksum) fails gracefully and leaves no file", {
  skip_on_cran()
  dirs <- tmp_dirs()
  src <- tempfile(fileext = ".rds")
  saveRDS(matrix(1:4, 2), src)
  url <- paste0("file://", normalizePath(src, winslash = "/"))
  expect_no_warning(
    msgs <- capture_messages(
      out <- .mlcvi_read_full(cache = "session", url = url,
                              md5 = "0000deadbeef", bundled = "", dirs = dirs)
    )
  )
  expect_match(msgs, "checksum", all = FALSE)
  expect_null(out)
  expect_length(list.files(dirs$session, all.files = TRUE, no.. = TRUE), 0)
})

test_that("a matching checksum is cached in the resolved directory and reused", {
  skip_on_cran()
  dirs <- tmp_dirs()
  src <- tempfile(fileext = ".rds")
  saveRDS(matrix(as.numeric(1:6), 2), src)
  url <- paste0("file://", normalizePath(src, winslash = "/"))
  md5 <- unname(tools::md5sum(src))
  msgs <- capture_messages(
    out <- .mlcvi_read_full(cache = "session", url = url, md5 = md5,
                            bundled = "", dirs = dirs)
  )
  expect_match(msgs, "Downloading", all = FALSE)
  expect_true(is.matrix(out))
  expect_true(file.exists(file.path(dirs$session, .mlcvi_train_file)))
  expect_false(dir.exists(dirs$persistent))
  # second call reads the cache silently
  expect_silent(
    again <- .mlcvi_read_full(cache = "session", url = url, md5 = md5,
                              bundled = "", dirs = dirs)
  )
  expect_identical(again, out)
  # a stale cached copy is replaced
  writeBin(as.raw(1:10), file.path(dirs$session, .mlcvi_train_file))
  msgs <- capture_messages(
    .mlcvi_read_full(cache = "session", url = url, md5 = md5,
                     bundled = "", dirs = dirs)
  )
  expect_match(msgs, "does not match the expected checksum; replacing",
               all = FALSE)
})

test_that("non-interactive 'ask' resolves to the session cache", {
  dirs <- tmp_dirs()
  withr::local_options(list(mlcvi.cache_dir = NULL))
  withr::local_envvar(MLCVI_CACHE_DIR = "")
  expect_identical(.mlcvi_resolve_cache("ask", dirs, quiet = TRUE), "session")
  expect_identical(.mlcvi_resolve_cache("persistent", dirs, quiet = TRUE),
                   "persistent")
  withr::local_options(list(mlcvi.cache_dir = dirs$persistent))
  expect_identical(.mlcvi_resolve_cache("ask", dirs, quiet = TRUE), "persistent")
})

test_that("cache directories honour the override option and env var", {
  withr::local_options(list(mlcvi.cache_dir = "/custom/dir"))
  expect_identical(.mlcvi_cache_dirs()$persistent, "/custom/dir")
  withr::local_options(list(mlcvi.cache_dir = NULL))
  withr::local_envvar(MLCVI_CACHE_DIR = "/env/dir")
  expect_identical(.mlcvi_cache_dirs()$persistent, "/env/dir")
  expect_identical(.mlcvi_cache_dirs()$session, file.path(tempdir(), "mlcvi"))
})

test_that("mlcvi_clear_cache removes cached files and reports them", {
  dirs <- tmp_dirs()
  withr::local_options(list(mlcvi.cache_dir = dirs$persistent))
  dir.create(dirs$persistent, recursive = TRUE)
  f <- file.path(dirs$persistent, .mlcvi_train_file)
  writeBin(as.raw(1:3), f)
  removed <- mlcvi_clear_cache()
  expect_true(f %in% removed)
  expect_false(file.exists(f))
  expect_length(mlcvi_clear_cache(), 0)
})

test_that("no download location configured returns NULL with a message", {
  dirs <- tmp_dirs()
  expect_message(
    out <- .mlcvi_read_full(cache = "session", url = "", md5 = NULL,
                            bundled = "", dirs = dirs),
    "No download location"
  )
  expect_null(out)
})

test_that("functions needing the full matrix stop clearly when it is unavailable", {
  dirs <- tmp_dirs()
  withr::local_options(list(mlcvi.train_url = "http://127.0.0.1:9/x.rds"))
  skip_on_cran()
  # bypass the bundled copy present in source checkouts
  local_mocked_bindings(
    .mlcvi_read_full = function(...) NULL
  )
  rm(list = intersect("full", ls(.mlcvi_cache)), envir = .mlcvi_cache)
  codes <- sub("^s003_", "", colnames(train_output_matrix))
  scores <- data.frame(s003 = codes, values = 1)
  expect_error(mlcvi_extend(scores, lambda = 0.1, repeats = 1L, verbose = FALSE),
               "not available")
  expect_error(mlcvi_build_matrix("a001"), "not available")
})

test_that("the OSF resource is reachable (HEAD only; skipped on CRAN and offline)", {
  skip_on_cran()
  skip_if_offline("osf.io")
  skip_if_not_installed("curl")
  h <- curl::new_handle(nobody = TRUE, followlocation = TRUE)
  res <- curl::curl_fetch_memory(.mlcvi_train_url_default, handle = h)
  expect_equal(res$status_code, 200L)
  len <- curl::parse_headers_list(res$headers)[["content-length"]]
  if (!is.null(len)) expect_equal(as.numeric(len), 280617660)
})
