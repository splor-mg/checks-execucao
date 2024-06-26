#' @export
check_vlr_cota_aprovada <- function(detalhe, totais, mes = TRUE, stop_on_failure = FALSE, output = FALSE) {
  key <- if (mes) c("ano", "mes_cod") else "ano"
  
  x <- detalhe |> 
    checksplanejamento:::aggregate(
      "vlr_detalhe", 
      by = key, 
      rename = list(vlr_cota_aprovada_liquida  = "vlr_detalhe")
    )
  
  y <- totais |> 
    checksplanejamento:::aggregate(
      "vlr_totais", 
      by = key, 
      rename = list(vlr_cota_aprovada_liquida  = "vlr_totais")
    )
  
  df <- merge(x, y, by = key, all = TRUE) |> checksplanejamento:::as_accounting()
  rules <- validate::validator(vlr_detalhe == vlr_totais)
  report <- validate::confront(df, rules, lin.eq.eps=0.01)
  checksplanejamento:::check_result(df, report, stop_on_failure = stop_on_failure, output = output)
}
