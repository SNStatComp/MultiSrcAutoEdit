
## Zorg dat de volgende hulpvariabelen vooraf zijn afgeleid in de data:
# 1) a_PS_SWL_Banen_rato = (SWL.Werknemers_in_vte_Gemiddeld - PS.PERSONS110100) * PS.PERSONS111000 / PS.PERSONS110100
# 2) a_PS_SWL_Loon_rato1 = (SWL.Lonen - PS.LOONSOM110002) * PS.LOONSOM121300 / PS.LOONSOM110002
# 3) a_PS_SWL_Loon_rato2 = (SWL.Lonen - PS.LOONSOM110002) * PS.LOONSOM121200 / PS.LOONSOM110002
# 4) b_PS_SWL_Loon_overig = ifelse(SWL.Lonen >= PS.LOONSOM110002,
#                                  SWL.Lonen - PS.LOONSOM110002 + a_PS_SWL_Loon_rato1 + a_PS_SWL_Loon_rato2,
#                                  M).
# Hierbij is M een groot getal. Eventuele lege waarden die ontstaan in deze afleidingen worden vervangen door nullen.
# Deze hulpvariabelen mogen niet worden aangepast door errorlocate en krijgen daarom een zeer hoog betrouwbaarheidsgewicht.
#
# Vanwege een technische tekortkoming van de huidige versie van de uitgebreide errorlocate-code,
# moeten er extra controleregels worden gedefinieerd waarin deze hulpvariabelen voorkomen
# (anders worden ze weggelaten door errorlocate). We kunnen bijvoorbeeld de volgende regels
# toevoegen, waaraan de data sowieso voldoen:
# 1) a_PS_SWL_Banen_rato >= -M
# 2) a_PS_SWL_Loon_rato1 >= -M
# 3) a_PS_SWL_Loon_rato2 >= -M
# 4) b_PS_SWL_Loon_overig >= -M
# waarbij M weer een groot getal is.


## Definitie van de toegelaten speciale acties
SPECIALE_ACTIES <- list(
  banen = data.frame(
    varTarget = I(c('PS.PERSONS111000', 'PS.SUBTOWP100000', 'PS.PERSONS100000',
                    'PS.PERSONS110100', 'PS.PERSONS110000')),
    PS.PERSONS110100 = c(0, 0, 0, -1, -1),
    SWL.Werknemers_in_vte_Gemiddeld = c(0, 0, 0, 1, 1),
    a_PS_SWL_Banen_rato = c(1, 1, 1, 0, 0)),
  loon_via_totaal = data.frame(
    varTarget = I(c('PS.LOONSOM110002', 'PS.LOONSOM121300', 'PS.LOONSOM121200',
                    'PS.PERSLST100000', 'PS.PERSLSH100000')),
    PS.LOONSOM110002 = c(-1, 0, 0, -1, -1),
    SWL.Lonen = c(1, 0, 0, 1, 1),
    a_PS_SWL_Loon_rato1 = c(0, 1, 0, 1, 1),
    a_PS_SWL_Loon_rato2 = c(0, 0, 1, 1, 1)),
  loon_via_overig = data.frame(
    varTarget = I(c('PS.LOONSOM110002', 'PS.LOONSOM121300', 'PS.LOONSOM121200',
                    'PS.BEDRLST345900')),
    PS.LOONSOM110002 = c(-1, 0, 0, 0),
    SWL.Lonen = c(1, 0, 0, 0),
    a_PS_SWL_Loon_rato1 = c(0, 1, 0, 0),
    a_PS_SWL_Loon_rato2 = c(0, 0, 1, 0),
    b_PS_SWL_Loon_overig = c(0, 0, 0, -1)),
  indus_prod_via_diensten = data.frame(
    varTarget = I(c('PS.OMZETPS211150', 'PS.OMZETPS211100', 'PS.OMZETPS211250',
	                'PS.OMZETPS211200')),
	aux_indus_prod_via_diensten = c(1, 1, -1, -1)),
  indus_prod_via_totaal = data.frame(
    varTarget = I(c('PS.OMZETPS211150', 'PS.OMZETPS211100', 'PS.OMZETPS210000',
	                'PS.OMZETPH210000')),
	aux_indus_prod_via_totaal = c(1, 1, 1, 1))
)


## Keuze van gewichten bij deze speciale acties
WEIGHT_EO_SPECIALE_ACTIES <- c(banen = 1.5,
              loon_via_totaal = 0.9,
              loon_via_overig = 0.8,
              indus_prod_via_diensten = 0.8,
              indus_prod_via_totaal = 0.9)

# Auxiliary variables to add to the data during editing
VARS_EDIT_OPERATIONS <- c("aux_indus_prod_via_diensten", "aux_indus_prod_via_totaal", "addEdits")


## Deze acties doen het volgende:
# > banen
#   - de VTE-variabele PS.PERSONS110100 wordt gelijk aan SWL.Werknemers_in_vte_Gemiddeld
#   - bijbehorend totaal PS.PERSONS110000 wordt aangepast met hetzelfde bedrag als PS.PERSONS110100
#   - de aantallen-variabele PS.PERSONS111000 wordt naar rato mee aangepast
#     zodat de verhouding tussen PS.PERSONS110100 en PS.PERSONS111000 gelijk blijft
#   - de (sub)totalen PS.SUBTOWP100000 en PS.PERSONS100000 worden mee aangepast met PS.PERSONS111000
#     zodat geen nieuwe schending van een optelregel ontstaat
# > loon_via_totaal
#   - PS.LOONSOM110002 wordt gelijk aan SWL.Lonen
#   - PS.LOONSOM121300 wordt naar rato mee aangepast
#     zodat de verhouding tussen PS.LOONSOM121300 en PS.LOONSOM110002 gelijk blijft
#   - PS.LOONSOM121200 wordt naar rato mee aangepast
#     zodat de verhouding tussen PS.LOONSOM121200 en PS.LOONSOM110002 gelijk blijft
#   - de totaalvariabele PS.PERSLST100000 en zijn herhaalvariabele PS.PERSLSH100000
#     worden mee aangepast zodat geen nieuwe schending van een optelregel ontstaat
# > loon_via_overig
#   - PS.LOONSOM110002 wordt gelijk aan SWL.Lonen
#   - PS.LOONSOM121300 wordt naar rato mee aangepast
#     zodat de verhouding tussen PS.LOONSOM121300 en PS.LOONSOM110002 gelijk blijft
#   - PS.LOONSOM121200 wordt naar rato mee aangepast
#     zodat de verhouding tussen PS.LOONSOM121200 en PS.LOONSOM110002 gelijk blijft
#   - de overige personeelskosten PS.BEDRLST345900 worden mee aangepast (in omgekeerde richting)
#     zodat geen nieuwe schending van een optelregel ontstaat;
#     vanwege de manier waarop b_PS_SWL_Loon_overig is afgeleid, is deze actie in de praktijk
#     alleen zinvol wanneer PS.LOONSOM110002 omhoog wordt aangepast (en PS.BEDRLST345900 dus omlaag)
# > indus_prod_via_diensten
#   - hevel een (vrij te kiezen) bedrag over van PS.OMZETPS211250 naar PS.OMZETPS211150 (of vice versa)
#   - bereken deze overheveling door in de subtotalen PS.OMZETPS211200 en PS.OMZETPS211100
#     zodat de totaalvariabele PS.OMZETPS210000 hierdoor niet verandert
# > indus_prod_via_totaal
#   - verander PS.OMZETPS211150 met een (vrij te kiezen) bedrag
#   - bereken deze verandering door in het subtotaal PS.OMZETPS211100 én
#     in de totaalvariabele PS.OMZETPS210000 en zijn herhaalvariabele PS.OMZETPH210000


#' Add helper variables for edit operations
#'
#' @param data
#'
#' @return data incl. extra variables for edit operations
#' @export
#'
#' @examples
add_variables_edit_operations <- function(data) {
  log_info("Add variables edit operations to data")
  data <- data %>%
    mutate(
      a_PS_SWL_Banen_rato = (SWL.Werknemers_in_vte_Gemiddeld - PS.PERSONS110100) * PS.PERSONS111000 / PS.PERSONS110100,
      a_PS_SWL_Loon_rato1 = (SWL.Lonen - PS.LOONSOM110002) * PS.LOONSOM121300 / PS.LOONSOM110002,
      a_PS_SWL_Loon_rato2 = (SWL.Lonen - PS.LOONSOM110002) * PS.LOONSOM121200 / PS.LOONSOM110002,
      b_PS_SWL_Loon_overig = ifelse(SWL.Lonen >= PS.LOONSOM110002,
                                    SWL.Lonen - PS.LOONSOM110002 + a_PS_SWL_Loon_rato1 + a_PS_SWL_Loon_rato2,
                                    M
      )
    )
}
