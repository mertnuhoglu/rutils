
path_file = function(dir = ".", filename = "") {
  dir_path = sprintf("%s/data/verify", dir)
  dir.create(path_assert = dir_path, recursive = T)
	sprintf("%s/%s.tsv", dir_path, filename)
}

#' @export
path_assert = function(title = "", dir = ".", filename = "") {
  dir_path = sprintf("%s/data/verify", dir)
  dir.create(dir_path, recursive = T)
	sprintf("%s/assert_%s_%s.tsv", dir_path, title, filename)
}

#' @export
assert_empty = function(df, dir = ".", filename = "", validate = F) {
	rio::export(df, path_assert("empty", dir, filename))

	if (validate) {
    assertthat::validate_that( rutils::is_empty(df) )
	} else {
    assertthat::assert_that( rutils::is_empty(df) )
	}
	#> data/verify/assert_empty.tsv
}

#' @export
assert_rows_unique = function(df, cols, dir = ".", filename = "" ) {
	dup_rows = duplicated_rows_(df, cols)

	rio::export( dup_rows, path_assert("rows_unique", dir, filename) )
	assertthat::assert_that( rutils::is_empty(dup_rows) )
	#> ../view/verify/assert_rows_are_unique.tsv
}

#' @export
assert_inclusion = function(subset, superset, fk_subset, fk_superset = fk_subset, dir = ".", filename = "", validate = F) {
	# subset - superset == ∅  
	# subset ⊆ superset 
	no_subset__missing_instances = subset %>%
		dplyr::anti_join( superset, by = setNames(fk_superset, fk_subset) )

	rio::export(no_subset__missing_instances, path_assert("inclusion", dir, filename))
	if (validate) {
    assertthat::validate_that( rutils::is_empty(no_subset__missing_instances) )
	} else {
    assertthat::assert_that( rutils::is_empty(no_subset__missing_instances) )
	}
	#> ../view/verify/assert_inclusion.tsv
}

#' @export
assert_all_have_attribute = function( df, columns, dir = ".", filename = "", validate = F ) {
  # broken, filter_ doesn't work as expected
	instances_with_no_attribute_value = df %>%
		dplyr::filter_( is_na(stringr::str_trim( columns )) )

	rio::export( instances_with_no_attribute_value, path_assert("all_have_attribute", dir, filename) )
	if (validate) {
    assertthat::validate_that( rutils::is_empty(instances_with_no_attribute_value) )
	} else {
    assertthat::assert_that( rutils::is_empty(instances_with_no_attribute_value) )
	}
	#> ../view/verify/assert_all_have_attribute.tsv
}

#' @export
assert_non_na = function( df, columns, dir = ".", filename = "", validate = F ) {
	instances_with_no_attribute_value = df %>%
		dplyr::filter( is.na(df[[columns]]) )

	rio::export( instances_with_no_attribute_value, path_assert("non_na", dir, filename) )
	if (validate) {
    assertthat::validate_that( rutils::is_empty(instances_with_no_attribute_value) )
	} else {
    assertthat::assert_that( rutils::is_empty(instances_with_no_attribute_value) )
	}
	#> ../view/verify/assert_all_have_attribute.tsv
}

#' @export
assert_no_intersection = function(df1, df2, fk1, fk2 = fk1, dir = ".", filename = "" ) {
	# df1 ∩ df2 == ∅  
	no_intersection__common_instances = df1 %>%
		dplyr::inner_join( df2, by = setNames(fk2, fk1) )

  assert_file = rutils::path_assert("no_intersection", dir, filename)
	rio::export(no_intersection__common_instances, assert_file)
	assertthat::assert_that( rutils::is_empty(no_intersection__common_instances), msg = assert_file)
	#> ../view/verify/assert_no_intersection.tsv
}

#' @export
assert_equal_set = function(subset, superset, fk_subset, fk_superset = fk_subset, dir = ".", filename = fk_subset, validate = F) {
	assert_inclusion(subset, superset, fk_subset, fk_superset, dir = dir, filename = filename, validate = validate)
	assert_inclusion(superset, subset, fk_superset, fk_subset, dir = dir, filename = filename, validate = validate)
	#> ../view/verify/assert_inclusion.tsv
}

#' @export
assert_subset = function(subset, superset) {
  assertthat::assert_that( length( setdiff(subset, superset) ) == 0 )
}

