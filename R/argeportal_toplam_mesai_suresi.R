`%>%` <- magrittr::`%>%`

#' @export
argeportal_toplam_mesai_suresi = function(path) {
	d0 = readr::read_tsv("rapor_20190716.tsv") %>%
		dplyr::select(saat = Saat, durum = Durum)
	d1 = d0 %>%
		dplyr::mutate(rowno = (1 + dplyr::row_number()) %/% 2) %>%
		tidyr::spread(durum, saat)
		##>    rowno C      G
		##>    <dbl> <drtn> <drtn>
		##>  1     1 10:38  09:19
		##>  2     2 12:51  10:43
	d2 = d1 %>%
		dplyr::mutate(sure = C - G) 
	result = lubridate::seconds_to_period(sum(d2$sure, na.rm=T)) %>%
		lubridate::as.duration() %>%
		as.numeric("hours")
	return(result)
}
