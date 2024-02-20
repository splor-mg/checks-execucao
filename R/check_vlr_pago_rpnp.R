#' @export
check_vlr_pago_rpnp <- function(detalhe, totais, stop_on_failure = FALSE, output = FALSE) {
  key <- c("ano", "mes_cod")
  
  x <- detalhe |> 
    checksplanejamento:::aggregate(
      "vlr_saldo_rpp|vlr_despesa_liquidada_rpnp|vlr_despesa_liquidada_pagar", 
      by = key) |> 
    dplyr::mutate(vlr_detalhe = vlr_saldo_rpp + vlr_despesa_liquidada_rpnp - vlr_despesa_liquidada_pagar) |> 
    dplyr::select(ano, mes_cod, vlr_detalhe)
  
  y <- totais |> 
    checksplanejamento:::aggregate(
      "vlr_saldo_rpp|vlr_despesa_liquidada_rpnp|vlr_despesa_liquidada_pagar", 
      by = key) |> 
    dplyr::mutate(vlr_totais = vlr_saldo_rpp + vlr_despesa_liquidada_rpnp - vlr_despesa_liquidada_pagar) |> 
    dplyr::select(ano, mes_cod, vlr_totais)
  
  df <- merge(x, y, by = key, all = TRUE) |> checksplanejamento:::as_accounting()
  rules <- validate::validator(vlr_detalhe == vlr_totais)
  report <- validate::confront(df, rules, lin.eq.eps=0.01)
  checksplanejamento:::check_result(df, report, stop_on_failure = stop_on_failure, output = output)
}
