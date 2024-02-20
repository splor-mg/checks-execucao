#' @export
check_vlr_pago_rpp <- function(detalhe, totais, stop_on_failure = FALSE, output = FALSE) {
  key <- c("ano", "mes_cod")
  
  x <- detalhe |> 
    checksplanejamento:::aggregate(
      "vlr_detalhe", 
      by = key, 
      rename = list(vlr_pago_rpp = "vlr_detalhe")
    )
  
  y <- totais |> 
    checksplanejamento:::aggregate(
      "vlr_totais", 
      by = key, 
      rename = list(vlr_pago_rpp = "vlr_totais")
    )
  
  df <- merge(x, y, by = key, all = TRUE) |> 
        checksplanejamento:::as_accounting()
  rules <- validate::validator(vlr_detalhe == vlr_totais)
  report <- validate::confront(df, rules, lin.eq.eps = 0.01)
  checksplanejamento:::check_result(df, report, stop_on_failure = stop_on_failure, output = output)
}
