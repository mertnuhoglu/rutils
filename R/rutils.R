`%>%` <- magrittr::`%>%`


#' @export
is.blank <- function(x, false.triggers=FALSE){
    if(is.function(x)) return(FALSE) # Some of the tests below trigger
                                     # warnings when used on functions
	 if (is.list(x)) x = unlist(x)
    return(
        is.null(x) ||                # Actually this line is unnecessary since
        length(x) == 0 ||            # length(NULL) = 0, but I like to be clear
        all(is.na(x)) ||
        all(x=="") ||
        (false.triggers && all(!x))
    )
}

#' @export
sys_getenv = function(x = NULL, default = "../../../data/jtn") {
  if (is.blank(Sys.getenv(x))) {
    return(default)
  } else {
    Sys.getenv(x)
  }
}

#' @export
duplicated_rows_ = function(df, columns) {
	# multiple columns
	# usage: 
	# columns = c("window_id", "window_name")
	# wnd %>%
	# 	duplicated_rows_(columns)
	.dots <- lapply(columns, as.symbol)
	df %>%
		dplyr::group_by_(.dots = .dots) %>%
		dplyr::filter( n() > 1 ) %>%
		dplyr::arrange_(.dots = .dots)
}

#' @export
is_empty = function(x) {
	if (is.data.frame(x)) return(nrow(x) == 0)
	if (is.vector(x)) return(length(x) == 0)
	if (is.list(x)) return(length(x) == 0)

	stop("it should be a data.frame or vector or list")
}
#' @export
not_empty <- function(x) !(is_empty(x))

#' @export
is_na = function(x) is.na(x) | all(ifelse( class(x) == "character", x == "NA", FALSE))
#' @export
not.na <- function(x) !(is.na(x))

#' @export
count_isna = function(x) { sum(is.na(x)) }
#' @export
count_unique = function(x) { length(unique(x)) }
cu = count_unique
ci = count_isna

# grep variants
#' @export
filterm = function(x, fun) Filter(fun, x)
#' @export
triml = function(x, ch) ltrim_char(ch, x)
#' @export
trimr = function(x, ch) rtrim_char(ch, x)
#' @export
duplicatedv = function(x) x %>% duplicated %>% extractm(x)
#' @export
reverse_args = function(fun) 
	function(arg1, arg2, ...)
		fun(arg2, arg1, ...)
mag = reverse_args
m = reverse_args
#' @export
extractm = function(x, df) tidyr::extract(df, x)
#' @export
partialm = function(fun, x) purrr::partial(x, fun)
#' @export
grepm = reverse_args(grep)
#' @export
grepv = purrr::partial(grepm, value = T)
#' @export
greplm = function(x, pattern, ...) {
	grepl(pattern, x, ...)
}
#' @export
vgrep = function(x, patterns, ...) {
	x %>% 
		grepm( patterns %>% paste(collapse="|"), invert = T, ...) %>%
		unique
}
#' @export
vgrepv = purrr::partial(vgrep, value = T)
#' @export
subm = function(x, pattern, replacement, ...) {
	sub(pattern, replacement, x, ...)
}
#' @export
gsubm = function(x, pattern, replacement, ...) {
	gsub(pattern, replacement, x, ...)
}

#' @export
pre0 = function(x, y) paste0(y, x)
#' @export
"%+%" = function(...) paste0(...,sep="")

#' @export
filter_na = function(df, column) {
	df[is.na(df[[column]]), ]
}
#' @export
filter_nonna = function(df, column) {
	df[!is.na(df[[column]]), ]
}
#' @export
all_nonna = function(v) { ci(v) == 0 }
#' @export
none = function(v) { !any(v) }
#' @export
all_unique = function(v) { duplicated(v) %>% sum == 0 }

#' @export
filter_nonna = function(df, column) {
	df[!is.na(df[[column]]), ]
}

#' @export
ifempty_na = function(x) {
	if ( is_empty(x) ) {
		return(NA)
	} else {
		return(x)
	}
}

#' @export
unduplicate_rows = function(df, columns) {
	# multiple columns
	# usage: 
	# columns = c("window_id", "window_name")
	# wnd %>%
	# 	unduplicate_rows(columns)
	.dots <- lapply(columns, as.symbol)
	df %>%
		group_by_(.dots = .dots) %>%
		filter( n() == 1 )
}

#' @export
duplicated_rows_ = function(df, columns) {
	# multiple columns
	# usage: 
	# columns = c("window_id", "window_name")
	# wnd %>%
	# 	duplicated_rows_(columns)
	.dots <- lapply(columns, as.symbol)
	df %>%
		group_by_(.dots = .dots) %>%
		filter( n() > 1 ) %>%
		arrange_(.dots = .dots)
}

#' @export
duplicated_values = function(df, column) {
	v = filter_nonna(df, column)[[column]]
	v[ duplicated(v) ] %>% unique
}

#' @export
duplicated_rows = function(df, column) {
	# only for one column
	df %>%
		group_by_(column) %>%
		filter( n() > 1 )
	# if multiple columns use:
	#dup_awdf = awdf %>%
		#group_by(awindow_id, data_field_id) %>%
		#filter( n() > 1 )
}

