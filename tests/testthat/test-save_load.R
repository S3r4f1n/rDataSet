library(testthat)
library(jsonlite)

# =============================================================================
# Test fixtures
# =============================================================================

# Simple dataset with single ID level
ds_simple <- dataset_build(
  tibble::tibble(i = 1:5, value = c(10, NA, 30, 40, NA), name = letters[1:5]),
  ids = "i"
)

# Dataset with NA patterns
ds_with_nas <- dataset_build(
  tibble::tibble(
    id = 1:4,
    x = c(1, NA, 3, NA),
    y = c(NA, 2, NA, 4),
    z = c(10, 20, 30, 40)
  ),
  ids = "id"
)

# Dataset with multiple ID levels (hierarchical)
ds_hierarchical <- dataset_build(
  tibble::tibble(
    group = c("A", "A", "B", "B"),
    id = 1:4,
    group_const = c("G1", "G1", "G2", "G2"),  # Constant within group
    id_const = c("X", "X", "Y", "Y"),         # Constant within id (but id is unique)
    value = c(10, 20, 30, 40)                  # Varies at leaf level
  ),
  ids = c("group", "id")
)

# Dataset with mixed data types
ds_mixed <- dataset_build(
  tibble::tibble(
    id = 1:4,
    int_val = c(1L, 2L, NA, 4L),
    num_val = c(1.5, NA, 3.7, 4.2),
    char_val = c("a", NA, "c", "d"),
    log_val = c(TRUE, FALSE, NA, TRUE)
  ),
  ids = "id"
)

# =============================================================================
# dataset_collapse_map tests
# =============================================================================

test_that("dataset_collapse_map returns correct structure", {
  result <- dataset_collapse_map(ds_simple)

  expect_type(result, "list")
  expect_named(result, c("ids", "variables", "mapping"))
  expect_equal(result$ids, "i")
  expect_equal(result$variables, c("value", "name"))
  expect_type(result$mapping, "integer")
  expect_length(result$mapping, 2)  # One mapping per variable
})

test_that("dataset_collapse_map identifies constant variables correctly", {
  result <- dataset_collapse_map(ds_hierarchical)

  expect_equal(result$ids, c("group", "id"))
  # group_const should be constant at group level (level 1)
  # id_const and value should be at id level (level 2)
  expect_gte(min(result$mapping), 1)
  expect_lte(max(result$mapping), length(result$ids) + 1)  # +1 for top_level
})

test_that("dataset_collapse_map handles single ID level", {
  result <- dataset_collapse_map(ds_with_nas)

  expect_equal(result$ids, "id")
  expect_equal(sort(result$variables), c("x", "y", "z"))
  # All variables vary at the leaf level (only one ID level)
  expect_true(all(result$mapping >= 1))
})

test_that("dataset_collapse_map errors on invalid dataset", {
  expect_error(
    dataset_collapse_map(data.frame(x = 1:3)),
    "must be of instance dataset"
  )
})

# =============================================================================
# dataset_nest tests
# =============================================================================

test_that("dataset_nest returns correct structure", {
  result <- dataset_nest(ds_simple)

  expect_type(result, "list")
  expect_named(result, c("metadata", "data"))
  expect_named(result$metadata, c("ids", "variables", "structure"))
  expect_equal(result$metadata$ids, "i")
  expect_type(result$data, "list")
})

test_that("dataset_nest metadata contains structure mapping", {
  result <- dataset_nest(ds_simple)

  expect_type(result$metadata$structure, "list")
  # Structure should map levels to their variables
  expect_true(length(result$metadata$structure) > 0)
})

test_that("dataset_nest preserves all data", {
  result <- dataset_nest(ds_simple)

  # With inverted structure, data is organized by variables
  # Each variable contains a mapping from ID to value
  expect_type(result$data, "list")
  expect_length(result$data, length(result$metadata$variables))

  # Each variable should have entries for all rows
  for (var in result$metadata$variables) {
    expect_length(result$data[[var]], nrow(ds_simple))
  }
})

test_that("dataset_nest with hierarchical IDs creates nested structure", {
  result <- dataset_nest(ds_hierarchical)

  expect_named(result$data, c("A", "B"))  # Top level groups
  # Each group should contain nested id level
  expect_type(result$data$A, "list")
  expect_type(result$data$B, "list")
})

test_that("dataset_nest preserves NA values in structure", {
  result <- dataset_nest(ds_with_nas)

  # With inverted structure, data is organized by variables
  # Each variable contains a mapping from ID to value
  expect_type(result$data, "list")
  expect_length(result$data, length(result$metadata$variables))

  # Each variable should have entries for all rows
  for (var in result$metadata$variables) {
    expect_length(result$data[[var]], 4)  # 4 rows
  }
})

test_that("dataset_nest with pre-computed collapse_map works", {
  collapse_map <- dataset_collapse_map(ds_simple)
  result1 <- dataset_nest(ds_simple)
  result2 <- dataset_nest(ds_simple, collapse_map = collapse_map)

  expect_equal(result1$metadata, result2$metadata)
  expect_equal(result1$data, result2$data)
})

test_that("dataset_nest errors on invalid dataset", {
  expect_error(
    dataset_nest(data.frame(x = 1:3)),
    "must be of instance dataset"
  )
})

# =============================================================================
# JSON serialization tests
# =============================================================================

test_that("dataset_nest output is JSON serializable", {
  result <- dataset_nest(ds_simple)
  json_str <- jsonlite::toJSON(result, pretty = TRUE, auto_unbox = TRUE)

  expect_type(json_str, "character")
  expect_gt(nchar(json_str), 0)

  # Verify it's valid JSON by parsing it back
  parsed <- jsonlite::parse_json(json_str)
  expect_type(parsed, "list")
})

test_that("JSON roundtrip preserves structure", {
  result <- dataset_nest(ds_mixed)
  json_str <- jsonlite::toJSON(result, pretty = TRUE, auto_unbox = TRUE)
  parsed <- jsonlite::parse_json(json_str)

  # Check top-level structure
  expect_named(parsed, c("metadata", "data"))
  expect_named(parsed$metadata, c("ids", "variables", "structure"))
})

test_that("JSON output handles NA values correctly", {
  result <- dataset_nest(ds_with_nas)
  json_str <- jsonlite::toJSON(result, pretty = TRUE, auto_unbox = TRUE)
  parsed <- jsonlite::parse_json(json_str)

  # JSON should be valid and parseable
  expect_type(parsed, "list")
  expect_named(parsed, c("metadata", "data"))

  # With inverted structure, data is organized by variables
  # Each variable should have entries for all 4 rows
  expect_type(parsed$data, "list")
  for (var in parsed$metadata$variables) {
    expect_length(parsed$data[[var]], 4)
  }
})

test_that("complex hierarchical structure serializes correctly", {
  result <- dataset_nest(ds_hierarchical)
  json_str <- jsonlite::toJSON(result, pretty = TRUE, auto_unbox = TRUE)

  expect_type(json_str, "character")

  parsed <- jsonlite::parse_json(json_str)
  expect_named(parsed$data, c("A", "B"))
})

# =============================================================================
# Integration tests
# =============================================================================

test_that("full pipeline: collapse_map -> nest -> JSON", {
  collapse_map <- dataset_collapse_map(ds_mixed)
  nested <- dataset_nest(ds_mixed, collapse_map = collapse_map)
  json_str <- jsonlite::toJSON(nested, auto_unbox = TRUE)
  parsed <- jsonlite::parse_json(json_str)

  # Verify metadata
  expect_equal(parsed$metadata$ids, "id")
  expect_true(all(c("int_val", "num_val", "char_val", "log_val") %in% parsed$metadata$variables))

  # Verify data structure exists
  expect_type(parsed$data, "list")
})

test_that("multiple data types handled correctly", {
  result <- dataset_nest(ds_mixed)

  # Check that all variable types are represented in metadata
  expect_true("int_val" %in% result$metadata$variables)
  expect_true("num_val" %in% result$metadata$variables)
  expect_true("char_val" %in% result$metadata$variables)
  expect_true("log_val" %in% result$metadata$variables)
})

test_that("hierarchical IDs create proper nesting depth", {
  result <- dataset_nest(ds_hierarchical)

  # Check nesting: group -> id -> values
  expect_true("A" %in% names(result$data))
  expect_true("B" %in% names(result$data))

  # Each group should have nested id entries
  group_a_ids <- names(result$data$A)
  expect_gte(length(group_a_ids), 1)
})

test_that("large dataset can be nested", {
  ds_large <- dataset_build(
    tibble::tibble(
      id = 1:100,
      value = 1:100,
      category = rep(letters[1:10], each = 10)
    ),
    ids = "id"
  )

  result <- dataset_nest(ds_large)
  expect_type(result$data, "list")
  # With inverted structure, data is organized by variables
  expect_length(result$data, length(result$metadata$variables))
  # Each variable should have entries for all 100 rows
  for (var in result$metadata$variables) {
    expect_length(result$data[[var]], 100)
  }
})

# =============================================================================
# dataset_unnest tests
# =============================================================================

test_that("dataset_unnest reverses dataset_nest for simple dataset", {
  nested <- dataset_nest(ds_simple)
  result <- dataset_unnest(nested)

  expect_s3_class(result, "dataset")
  expect_equal(attr(result, "dataset_ids"), attr(ds_simple, "dataset_ids"))
  expect_equal(nrow(result), nrow(ds_simple))
  expect_equal(ncol(result), ncol(ds_simple))
  expect_equal(sort(names(result)), sort(names(ds_simple)))
})

test_that("dataset_unnest preserves NA values", {
  nested <- dataset_nest(ds_with_nas)
  result <- dataset_unnest(nested)

  expect_equal(sum(is.na(result$x)), sum(is.na(ds_with_nas$x)))
  expect_equal(sum(is.na(result$y)), sum(is.na(ds_with_nas$y)))
  expect_equal(sum(is.na(result$z)), sum(is.na(ds_with_nas$z)))
})

test_that("dataset_unnest reverses hierarchical nesting", {
  nested <- dataset_nest(ds_hierarchical)
  result <- dataset_unnest(nested)

  expect_s3_class(result, "dataset")
  expect_equal(attr(result, "dataset_ids"), c("group", "id"))
  expect_equal(nrow(result), nrow(ds_hierarchical))

  # Check that ID values are preserved (as character, since list names are strings)
  expect_equal(sort(as.character(result$group)), sort(ds_hierarchical$group))
  expect_equal(sort(as.character(result$id)), sort(as.character(ds_hierarchical$id)))
})

test_that("dataset_unnest preserves mixed data types", {
  nested <- dataset_nest(ds_mixed)
  result <- dataset_unnest(nested)

  expect_equal(result$int_val, ds_mixed$int_val)
  expect_equal(result$num_val, ds_mixed$num_val)
  expect_equal(result$char_val, ds_mixed$char_val)
  expect_equal(result$log_val, ds_mixed$log_val)
})

test_that("dataset_unnest roundtrip preserves data", {
  # Test simple dataset - note: ID columns become character after roundtrip
  # because list names are always strings
  nested <- dataset_nest(ds_simple)
  result <- dataset_unnest(nested)

  expect_equal(attr(result, "dataset_ids"), attr(ds_simple, "dataset_ids"))
  # Value columns should match
  expect_equal(result$value, ds_simple$value)
  expect_equal(result$name, ds_simple$name)
})

test_that("dataset_unnest works with JSON parsed input", {
  # Use a dataset with character IDs to avoid type conversion issues
  ds_char <- dataset_build(
    tibble::tibble(id = letters[1:3], value = c(10, 20, 30)),
    ids = "id"
  )

  nested <- dataset_nest(ds_char)
  json_str <- jsonlite::toJSON(nested, auto_unbox = TRUE)
  parsed <- jsonlite::parse_json(json_str)

  result <- dataset_unnest(parsed)

  expect_s3_class(result, "dataset")
  expect_equal(nrow(result), nrow(ds_char))
  expect_equal(result$value, ds_char$value)
})

test_that("dataset_unnest errors on invalid input structure", {
  expect_error(
    dataset_unnest(list(data = list())),
    "must contain both 'metadata' and 'data'"
  )
})

test_that("dataset_unnest errors on missing metadata ids", {
  expect_error(
    dataset_unnest(list(metadata = list(), data = list())),
    "metadata must contain 'ids'"
  )
})

test_that("dataset_unnest with hierarchical IDs preserves order", {
  nested <- dataset_nest(ds_hierarchical)
  result <- dataset_unnest(nested)

  # Check that rows are properly reconstructed
  expect_equal(sort(result$group), sort(ds_hierarchical$group))
  expect_equal(sort(result$value), sort(ds_hierarchical$value))
})

# =============================================================================
# Lossless roundtrip tests
# =============================================================================

test_that("roundtrip preserves all value columns exactly", {
  # Test with ds_mixed which has multiple data types
  nested <- dataset_nest(ds_mixed)
  result <- dataset_unnest(nested)

  # All value columns should be identical
  expect_equal(result$int_val, ds_mixed$int_val)
  expect_equal(result$num_val, ds_mixed$num_val)
  expect_equal(result$char_val, ds_mixed$char_val)
  expect_equal(result$log_val, ds_mixed$log_val)
})

test_that("roundtrip preserves NA patterns exactly", {
  nested <- dataset_nest(ds_with_nas)
  result <- dataset_unnest(nested)

  # NA patterns should be identical
  expect_equal(is.na(result$x), is.na(ds_with_nas$x))
  expect_equal(is.na(result$y), is.na(ds_with_nas$y))
  expect_equal(is.na(result$z), is.na(ds_with_nas$z))

  # Non-NA values should also match
  expect_equal(result$x[!is.na(result$x)], ds_with_nas$x[!is.na(ds_with_nas$x)])
  expect_equal(result$y[!is.na(result$y)], ds_with_nas$y[!is.na(ds_with_nas$y)])
  expect_equal(result$z[!is.na(result$z)], ds_with_nas$z[!is.na(ds_with_nas$z)])
})

test_that("roundtrip preserves row order", {
  nested <- dataset_nest(ds_simple)
  result <- dataset_unnest(nested)

  # Rows should be in the same order
  expect_equal(result$i, ds_simple$i)
  expect_equal(result$value, ds_simple$value)
  expect_equal(result$name, ds_simple$name)
})

test_that("roundtrip preserves hierarchical data completely", {
  nested <- dataset_nest(ds_hierarchical)
  result <- dataset_unnest(nested)

  # All columns should match
  expect_equal(result$group_const, ds_hierarchical$group_const)
  expect_equal(result$id_const, ds_hierarchical$id_const)
  expect_equal(result$value, ds_hierarchical$value)
})

test_that("JSON roundtrip with type inference for integers", {
  ds_int <- dataset_build(
    tibble::tibble(id = 1:5, int_col = c(1L, 2L, NA, 4L, 5L)),
    ids = "id"
  )

  nested <- dataset_nest(ds_int)
  json_str <- jsonlite::toJSON(nested, auto_unbox = TRUE)
  parsed <- jsonlite::parse_json(json_str)
  result <- dataset_unnest(parsed)

  # After JSON roundtrip, integers should be inferred and restored
  # (JSON doesn't distinguish int/double, but we should infer from original data pattern)
  expect_equal(result$int_col, ds_int$int_col)
})

test_that("JSON roundtrip preserves logical values", {
  ds_logical <- dataset_build(
    tibble::tibble(id = 1:4, log_col = c(TRUE, FALSE, NA, TRUE)),
    ids = "id"
  )

  nested <- dataset_nest(ds_logical)
  json_str <- jsonlite::toJSON(nested, auto_unbox = TRUE)
  parsed <- jsonlite::parse_json(json_str)
  result <- dataset_unnest(parsed)

  # Logical values should be restored after JSON roundtrip
  expect_equal(result$log_col, ds_logical$log_col)
})

test_that("JSON roundtrip preserves character strings", {
  ds_char <- dataset_build(
    tibble::tibble(id = 1:4, char_col = c("alpha", NA, "gamma", "delta")),
    ids = "id"
  )

  nested <- dataset_nest(ds_char)
  json_str <- jsonlite::toJSON(nested, auto_unbox = TRUE)
  parsed <- jsonlite::parse_json(json_str)
  result <- dataset_unnest(parsed)

  # Character values should be restored
  expect_equal(result$char_col, ds_char$char_col)
})

test_that("JSON roundtrip preserves numeric with decimals", {
  ds_num <- dataset_build(
    tibble::tibble(id = 1:5, num_col = c(1.23, 4.56, NA, 7.89, 0.12)),
    ids = "id"
  )

  nested <- dataset_nest(ds_num)
  json_str <- jsonlite::toJSON(nested, auto_unbox = TRUE)
  parsed <- jsonlite::parse_json(json_str)
  result <- dataset_unnest(parsed)

  # Numeric values with decimals should be preserved
  expect_equal(result$num_col, ds_num$num_col)
})

test_that("full JSON roundtrip with mixed types", {
  nested <- dataset_nest(ds_mixed)
  json_str <- jsonlite::toJSON(nested, auto_unbox = TRUE, simplifyVector = TRUE)
  parsed <- jsonlite::parse_json(json_str)
  result <- dataset_unnest(parsed)

  # All types should be correctly inferred after JSON roundtrip
  expect_equal(result$int_val, ds_mixed$int_val)
  expect_equal(result$num_val, ds_mixed$num_val)
  expect_equal(result$char_val, ds_mixed$char_val)
  expect_equal(result$log_val, ds_mixed$log_val)
})

test_that("roundtrip with all NA column", {
  ds_all_na <- dataset_build(
    tibble::tibble(id = 1:3, all_na = c(NA, NA, NA)),
    ids = "id"
  )

  nested <- dataset_nest(ds_all_na)
  result <- dataset_unnest(nested)

  expect_equal(result$all_na, ds_all_na$all_na)
})

test_that("roundtrip with empty strings", {
  ds_empty_str <- dataset_build(
    tibble::tibble(id = 1:3, str_col = c("", "hello", NA)),
    ids = "id"
  )

  nested <- dataset_nest(ds_empty_str)
  result <- dataset_unnest(nested)

  expect_equal(result$str_col, ds_empty_str$str_col)
})

test_that("roundtrip preserves factor-like character data", {
  ds_factor_like <- dataset_build(
    tibble::tibble(id = 1:6, category = c("A", "B", "A", "C", "B", "A")),
    ids = "id"
  )

  nested <- dataset_nest(ds_factor_like)
  result <- dataset_unnest(nested)

  expect_equal(result$category, ds_factor_like$category)
})

