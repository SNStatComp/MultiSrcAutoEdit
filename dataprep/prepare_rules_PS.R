
rm(list = ls())
gc()

library(readxl)
library(stringr)
library(dplyr)
library(validate)
library(validatetools)
library(glue)

# load constants ----
source(stringr::str_glue('./input_files/constants.R'))

PID <- Sys.getenv("USERNAME")
InputFolder <- "//Cbsp.nl/productie/projecten/EBM/305311EBN2xPOC/Werk/POC_IT/Eurostat grant EBS 2023/Automatisch gaafmaken/Input"
InputFolder <- file.path(InputFolder, glue::glue(PID))

input_excel <- file.path('input_files', stringr::str_glue('vierkanttellingen_gaafmaakregels_PS_alleen{YEAR}.xlsx'))
input_data <- file.path(InputFolder, stringr::str_glue('data_{YEAR}_sel.rds'))

output_csv_auto <- file.path('edited_input', stringr::str_glue('edited_rules_ps_{YEAR}.csv'))
output_csv_auto_linearized <- file.path('edited_input', stringr::str_glue('edited_rules_ps_{YEAR}_linearized.csv'))
output_csv_other <- file.path('edited_input', stringr::str_glue('edited_rules_ps_{YEAR}_soft.csv'))


########
# auxiliary functions

negateRule <- function(s) {
  # (assumption: PS variable names occurring in edit rules consist of less than 14 symbols)

  # replace "NOT(x >= y)" by "x < y"
  s <- stringr::str_replace_all(string = s,
                                pattern = 'NOT\\({1}([[:print:]]{0,15})>={1}([[:print:]]{0,15})\\){1}',
                                replacement = '\\1<\\2')
  s <- stringr::str_replace_all(string = s,
                                pattern = 'NOT \\({1}([[:print:]]{0,15})>={1}([[:print:]]{0,15})\\){1}',
                                replacement = '\\1<\\2')

  # replace "NOT(x <= y)" by "x > y"
  s <- stringr::str_replace_all(string = s,
                                pattern = 'NOT\\({1}([[:print:]]{0,15})<={1}([[:print:]]{0,15})\\){1}',
                                replacement = '\\1>\\2')
  s <- stringr::str_replace_all(string = s,
                                pattern = 'NOT \\({1}([[:print:]]{0,15})<={1}([[:print:]]{0,15})\\){1}',
                                replacement = '\\1>\\2')

  # replace "NOT(x <> y)" by "x == y"
  s <- stringr::str_replace_all(string = s,
                                pattern = 'NOT\\({1}([[:print:]]{0,15})<>{1}([[:print:]]{0,15})\\){1}',
                                replacement = '\\1==\\2')
  s <- stringr::str_replace_all(string = s,
                                pattern = 'NOT \\({1}([[:print:]]{0,15})<>{1}([[:print:]]{0,15})\\){1}',
                                replacement = '\\1==\\2')

  # replace "NOT(x > y)" by "x <= y"
  s <- stringr::str_replace_all(string = s,
                                pattern = 'NOT\\({1}([[:print:]]{0,15})>{1}([[:print:]]{0,15})\\){1}',
                                replacement = '\\1<=\\2')
  s <- stringr::str_replace_all(string = s,
                                pattern = 'NOT \\({1}([[:print:]]{0,15})>{1}([[:print:]]{0,15})\\){1}',
                                replacement = '\\1<=\\2')

  # replace "NOT(x < y)" by "x >= y"
  s <- stringr::str_replace_all(string = s,
                                pattern = 'NOT\\({1}([[:print:]]{0,15})<{1}([[:print:]]{0,15})\\){1}',
                                replacement = '\\1>=\\2')
  s <- stringr::str_replace_all(string = s,
                                pattern = 'NOT \\({1}([[:print:]]{0,15})<{1}([[:print:]]{0,15})\\){1}',
                                replacement = '\\1>=\\2')

  # replace "NOT(x == y)" by "x != y"
  s <- stringr::str_replace_all(string = s,
                                pattern = 'NOT\\({1}([[:print:]]{0,15})=={1}([[:print:]]{0,15})\\){1}',
                                replacement = '\\1!=\\2')
  s <- stringr::str_replace_all(string = s,
                                pattern = 'NOT \\({1}([[:print:]]{0,15})=={1}([[:print:]]{0,15})\\){1}',
                                replacement = '\\1!=\\2')

  # all other cases: replace "NOT(x)" by "!(x)"
  s <- stringr::str_replace_all(string = s,
                                pattern = 'NOT\\({1}([[:print:]]*)\\){1}',
                                replacement = '!\\(\\1\\)')
  s <- stringr::str_replace_all(string = s,
                                pattern = 'NOT \\({1}([[:print:]]*)\\){1}',
                                replacement = '!\\(\\1\\)')

  return(s)
}

compareRules <- function(df1, df2, name) {
  df <- cbind(df1 %>%
                dplyr::select(dplyr::all_of(name)) %>%
                dplyr::rename_with(function(x) 'pre'),
              df2 %>%
                dplyr::select(dplyr::all_of(name)) %>%
                dplyr::rename_with(function(x) 'post')) %>%
    dplyr::filter(pre != post)
  return(df)
}



########
### load data
data_PS <- readRDS(input_data) %>%
  dplyr::select(c(starts_with('PS.'),
                  SbiGecoordineerd, GkSbsGecoordineerd1D,
                  RechtsvormCode, WD_ID = PS.VragenlijstID)) %>%
  dplyr::rename_all(function(x) gsub(pattern = '^PS[.]', replacement = '', x))



### read rules

df_orig <- readxl::read_excel(input_excel, sheet = stringr::str_glue('PS{YEAR}_per_VL'))
df_orig <- df_orig %>%
  dplyr::filter(T090_Ind == 1 | T070_Ind == 1)

# select unique rules
# (to avoid unnecessary work when translating the rules)
df_unique0 <- df_orig %>%
  dplyr::select(dt_id, FOUT_TEKST, DT_Regel) %>%
  unique()


## remove certain rules that are unusable here
df_unique1 <- df_unique0 %>%
  dplyr::filter(!stringr::str_detect(DT_Regel, pattern = '[[:alnum:]]{13}T')) %>%
  dplyr::filter(!stringr::str_detect(DT_Regel,
                                     pattern = 'begin_boekjaar|BEGIN_BOEKJAAR|eind_boekjaar|EIND_BOEKJAAR|BTW_JAAROMZET|AACTIEF100000'))
# View(df_unique0 %>% dplyr::anti_join(df_unique1, by = 'dt_id'))


## replace variable names and replace labels of Rechtsvorm by code numbers
df_unique <- df_unique1 %>%
  dplyr::mutate(DT_Regel = stringr::str_replace_all(DT_Regel, pattern = "rechtsvorm|RECHTSVORM", "RechtsvormCode"),
                DT_Regel = stringr::str_replace_all(DT_Regel, pattern = "SBI_COOR", "SbiGecoordineerd"),
                DT_Regel = stringr::str_replace_all(DT_Regel, pattern = "GK_COORDINATIE_ABR", "GkSbsGecoordineerd1D"),
                DT_Regel = stringr::str_replace_all(DT_Regel, pattern = "'Eenmanszaak'|'EENMANSZAAK'", "'001'"),
                DT_Regel = stringr::str_replace_all(DT_Regel, pattern = "'Maatschap'", "'006'"),
                DT_Regel = stringr::str_replace_all(DT_Regel, pattern = "'Vennootschap onder firma'", "'012'"),
                DT_Regel = stringr::str_replace_all(DT_Regel, pattern = "'Commanditaire Vennootschap'", "'025'"),
                DT_Regel = stringr::str_replace_all(DT_Regel, pattern = "'BV met gewone structuur'", "'041'"),
                DT_Regel = stringr::str_replace_all(DT_Regel, pattern = "'Besloten Vennootschap'", "'043'"),
                DT_Regel = stringr::str_replace_all(DT_Regel, pattern = "'Naamloze Vennootschap'", "'057'"),
                DT_Regel = stringr::str_replace_all(DT_Regel, pattern = "'Europese naamloze vennootschap \\(SE\\)'", "'058'"),
                DT_Regel = stringr::str_replace_all(DT_Regel, pattern = "'Kerkgenootschap'", "'073'"),
                DT_Regel = stringr::str_replace_all(DT_Regel, pattern = "'Stichting'", "'074'"),
                DT_Regel = stringr::str_replace_all(DT_Regel, pattern = "'Vereniging'", "'077'"),
                DT_Regel = stringr::str_replace_all(DT_Regel,
                                                    pattern = '([[:alnum:]]{13}) = EMPTY',
                                                    replacement = 'is.na\\(\\1\\)'))
# View(unique(compareRules(df_unique1, df_unique, 'DT_Regel')))


########
### translate rules to 'validate' syntax

# 1) all uppercase letters (with a few exceptions)
df_adapt <- df_unique %>%
  dplyr::mutate(DT_Regel = toupper(DT_Regel),
                DT_Regel = stringr::str_replace_all(DT_Regel, pattern = "RECHTSVORMCODE", "RechtsvormCode"),
                DT_Regel = stringr::str_replace_all(DT_Regel, pattern = "SBIGECOORDINEERD", "SbiGecoordineerd"),
                DT_Regel = stringr::str_replace_all(DT_Regel, pattern = "GKSBSGECOORDINEERD1D", "GkSbsGecoordineerd1D"),
                DT_Regel = stringr::str_replace_all(DT_Regel, pattern = "IS.NA", "is.na"))

test1 <- compareRules(df_unique, df_adapt, 'DT_Regel')
# View(unique(test1))


# 2) replace '=' by '==' and '<>' by '!='
df_adapt2 <- df_adapt %>%
  dplyr::mutate(DT_Regel = stringr::str_replace_all(string = DT_Regel,
                                                    pattern = '([^<>]{1})=',
                                                    replacement = '\\1==')) %>%
  dplyr::mutate(DT_Regel = stringr::str_replace_all(string = DT_Regel,
                                                    pattern = '<>',
                                                    replacement = '!='))

test2 <- compareRules(df_adapt, df_adapt2, 'DT_Regel')
# View(unique(test2))
df_adapt <- df_adapt2


# 3) negate rules with "NOT()"
df_adapt2 <- df_adapt %>%
  dplyr::mutate(DT_Regel = negateRule(DT_Regel))

test3 <- compareRules(df_adapt, df_adapt2, 'DT_Regel')
# View(unique(test3))
df_adapt <- df_adapt2


# 4) remove unnecessary brackets
df_adapt2 <- df_adapt %>%
  dplyr::mutate(DT_Regel = if_else(grepl('!\\(|is.na', DT_Regel),
                                   DT_Regel,
                                   stringr::str_replace_all(string = DT_Regel,
                                                            pattern = '\\(([[:alnum:]|[:space:]|[-\\+\\*<>=!/]]*)\\)',
                                                            replacement = ifelse(grepl('[ AND ]|[ OR ]', x = '\\1'), '(\\1)', '\\1'))))

test4 <- compareRules(df_adapt, df_adapt2, 'DT_Regel')
# View(unique(test4))
df_adapt <- df_adapt2


# 5) replace "IF x THEN y ENDIF" by "if(x) y"
df_adapt2 <- df_adapt %>%
  dplyr::mutate(DT_Regel = stringr::str_replace_all(string = DT_Regel,
                                                    pattern = 'IF ([[:print:]]*) THEN ([[:print:]]*) ENDIF',
                                                    replacement = 'if(\\1) \\2'))

test5 <- compareRules(df_adapt, df_adapt2, 'DT_Regel')
# View(unique(test5))
df_adapt <- df_adapt2


# 6) replace 'x AND y' by 'x & y' and 'x OR y' by 'x | y'
df_adapt2 <- df_adapt %>%
  dplyr::mutate(DT_Regel = stringr::str_replace_all(string = DT_Regel,
                                                    pattern = ' AND ',
                                                    replacement = ' & ')) %>%
  dplyr::mutate(DT_Regel = stringr::str_replace_all(string = DT_Regel,
                                                    pattern = ' OR ',
                                                    replacement = ' | '))

test6 <- compareRules(df_adapt, df_adapt2, 'DT_Regel')
# View(unique(test6))
df_adapt <- df_adapt2

# 7) replace 'y / x <= a' by 'y <= a * x' (and similarly with >= and ==)
# (assumption: x is non-negative)
df_adapt2 <- df_adapt %>%
  dplyr::mutate(DT_Regel = stringr::str_replace_all(string = DT_Regel,
                                                    pattern = '\\(LOONSOM121300 / LOONSOM110000 >= ([[:print:]]*)\\)',
                                                    replacement = 'LOONSOM121300 / LOONSOM110000 >= \\1'),
                DT_Regel = stringr::str_replace_all(string = DT_Regel,
                                                    pattern = '\\(BEDRLST341400 / BEDRLST341000 <= ([[:print:]]*)\\)',
                                                    replacement = 'BEDRLST341400 / BEDRLST341000 <= \\1'),
                DT_Regel = stringr::str_replace_all(string = DT_Regel,
                                                    pattern = 'LOONSOM121300 / LOONSOM110000 <= ([[:print:]]*) & LOONSOM121300 / LOONSOM110000 >= ([[:print:]]*)',
                                                    replacement = 'LOONSOM121300 <= \\1 * LOONSOM110000 & LOONSOM121300 >= \\2 * LOONSOM110000'),
                DT_Regel = stringr::str_replace_all(string = DT_Regel,
                                                    pattern = '([[:print:]]*) (<=|>=|==|<|>) ([[:print:]]*) / 2',
                                                    replacement = '\\1 \\2 0.5 * \\3'),
                DT_Regel = stringr::str_replace_all(string = DT_Regel,
                                                    pattern = '([[:print:]]*)/([[:print:]]*) (<=|>=|==|<|>) ([[:print:]]*)',
                                                    replacement = '\\1 \\3 \\4 \\* \\2'))

test7 <- compareRules(df_adapt, df_adapt2, 'DT_Regel')
# View(unique(test7))
df_adapt <- df_adapt2


# 8) other changes, for now 'hard-coded'
df_adapt2 <- df_adapt %>%
  dplyr::mutate(DT_Regel = stringr::str_replace_all(string = DT_Regel,
                                                    pattern = fixed('if(OMZETPS210000 > 0) PERSONS100000 > 0 | PERSONS111000 > 0'),
                                                    replacement = 'if(OMZETPS210000 > 0) PERSONS100000 + PERSONS111000 > 0'),
                DT_Regel = stringr::str_replace_all(string = DT_Regel,
                                                    pattern = fixed('if(OMZETPS213000<=0 & OMZETPS213008<=0 & OMZETPS213000<=0) INKWRDE120000==0'),
                                                    replacement = 'if(OMZETPS213000<=0 & OMZETPS213008<=0) INKWRDE120000==0'),
                DT_Regel = stringr::str_replace_all(string = DT_Regel,
                                                    pattern = fixed('!(LOONSOM110000+INKWRDE120300 < 0)'),
                                                    replacement = 'LOONSOM110000+INKWRDE120300 >= 0'),
                DT_Regel = stringr::str_replace_all(string = DT_Regel,
                                                    pattern = fixed('BEDRLST345100+BEDRLST345200/(PERSONS110000-PERSONS110100-PERSONS113000)>10'),
                                                    replacement = 'BEDRLST345100+BEDRLST345200 > 10*(PERSONS110000-PERSONS110100-PERSONS113000)'),
                DT_Regel = stringr::str_replace_all(string = DT_Regel,
                                                    pattern = fixed('PERSONS940000/PERSONS110100>=180 & PERSONS940000/PERSONS110100<=240'),
                                                    replacement = 'PERSONS940000 >= 180 * PERSONS110100 & PERSONS940000 <= 240 * PERSONS110100'))

test8 <- compareRules(df_adapt, df_adapt2, 'DT_Regel')
# View(unique(test8))
df_adapt <- df_adapt2



########
### as a check, create validator objects based on the rules

df_validate <- df_adapt %>%
  dplyr::rename(DT_ID = dt_id,
                description = FOUT_TEKST,
                rule = DT_Regel) %>%
  dplyr::mutate(name = paste0('V', DT_ID))

probeersel <- validate::validator(.data = df_validate)
testlin <- validatetools::is_linear(probeersel)
table(testlin, useNA = 'ifany')
unique(df_validate$rule[!testlin])


########
## make final selection of rules and write results to csv file
# (original version)

# some soft edits involve variables that are not part of the present data
# so these cannot be used; remove them
cf <- validate::confront(dat = head(data_PS, 2), x = probeersel)
unusable <- which(summary(cf)$error == TRUE)
print(df_validate$rule[unusable])
df_validate_usable <- df_validate[-unusable,]

df_final_not_linearized <- df_orig %>%
  dplyr::rename(c('DT_ID' = 'dt_id', 'VL_ID' = 'vl_id')) %>%
  dplyr::select(-c(WND_OMSCHRIJVING, DT_Regel, FOUT_TEKST)) %>%
  dplyr::left_join(df_validate_usable %>%
                     dplyr::select (-description),
                   by = 'DT_ID') %>%
  dplyr::filter(!is.na(rule))

## hard rules for automatic editing
df_auto_final_not_linearized <- df_final_not_linearized %>%
  dplyr::filter(T090_Ind == 1, FOUT_SOORT == 'H')

# other rules (soft rules and hard rules not used for automatic editing)
df_other_final_not_linearized <- df_final_not_linearized %>%
  dplyr::filter(T090_Ind == 0 | FOUT_SOORT == 'Z')


write.csv2(df_auto_final_not_linearized,
           file = output_csv_auto, row.names = FALSE)
write.csv2(df_other_final_not_linearized,
           file = output_csv_other, row.names = FALSE)



########
### linearize non-linear rules where possible
# using methods described in Van der Loo (2013): "Conditionele controleregels vereenvoudigen"
# (only for hard rules for automatic editing)

## auxiliary functions (by Van der Loo and Van Hoek, 2020)

# Replace constants ---------------------------------------------------------

replace_c <- function(x, compr = ">=", data, eps = 1e-4) {
  x <- as.character(x)

  if (grepl(" c", x)){
    lr <- stringr::str_split(x, pattern = compr)
    xr <- lr[[1]][2]
    rlr <- stringr::str_split(xr, pattern = "\\*")
    var <- stringr::str_replace_all(string=rlr[[1]][1], pattern=" ", repl="")

    # case: multiple variables
    if (length(stringr::str_split(var, pattern = "\\+")[[1]])>1 ) {
      var <- stringr::str_replace_all(string=var, pattern="\\(", repl="")
      var <- stringr::str_replace_all(string=var, pattern="\\)", repl="")
      vars <- stringr::str_split(var, pattern = "\\+")[[1]]
      c <- 0
      for (i in 1:length(vars)) {
        if (vars[i] %in% colnames(data)) {
          if (sum(is.na(data[[vars[i]]])) != nrow(data)) {
            if (max(data[[vars[i]]], na.rm=TRUE) != 0) {
              c <- c + max(data[[vars[i]]], na.rm=TRUE)
            }
          }
        }
      }

      if (c == 0){
        c <- eps
      } else {
        c <- 1/c
      }

    } else { # single variable

      if (var %in% colnames(data)) {
        if (sum(is.na(data[[var]])) == nrow(data)) {
          c <- eps
        } else if (max(data[[var]], na.rm=TRUE) == 0) {
          c <- eps
        } else {
          c <- 1/max(data[[var]], na.rm=TRUE)
        }
      }
      else {
        c <- eps
      }
    }

    rule <- paste0(lr[[1]][1], compr, rlr[[1]][1], "* ", c)
  } else {
    rule <- x
  }
  return (rule)
}

replace_d <- function(x, compr = "<=", data, eps = 1e8) {
  x <- as.character(x)

  if (grepl(" d", x)){
    lr <- stringr::str_split(x, pattern = compr)
    xr <- lr[[1]][2]
    rlr <- stringr::str_split(xr, pattern = "\\*")
    var <- stringr::str_replace_all(string=lr[[1]][1], pattern=" ", repl="")

    if (var %in% colnames(data)) {
      if (sum(is.na(data[[var]])) == nrow(data)) {
        d <- eps
      } else {
        d <- max(data[[var]], na.rm=TRUE)
      }
    }
    else {
      d <- eps
    }

    rule <- paste0(lr[[1]][1], compr, rlr[[1]][1], "* ", d)
  } else {
    rule <- x
  }
  return (rule)
}

replace_eps <- function(x, compr= ">|<", eps = 1e-4) {
  x <- as.character(x)

  if (grepl(" eps", x)){
    lr <- stringr::str_split(x, pattern = compr)
    xr <- lr[[1]][2]
    var <- stringr::str_replace_all(string=lr[[1]][1], pattern=" ", repl="")

    rule <- paste0(str_sub(x, 1L, -(1 + nchar("eps"))), eps)
  } else {
    rule <- x
  }
  return (rule)
}



## select non-linear rules
df_auto_unique <- df_auto_final_not_linearized %>%
  dplyr::select(DT_ID, rule, name) %>%
  unique()
probeersel_auto <- validate::validator(.data = df_auto_unique)
testlin_auto <- validatetools::is_linear(probeersel_auto)
df_nonlin <- df_auto_unique[!testlin_auto, ]

# for now 'hard-coded': remove rules that cannot be linearized by the methods used here
# and that would cause confusion during string matching
df_nonlin <- df_nonlin %>%
  filter(!(DT_ID %in% c(2422, 835, 1817, 848, 832, 987, 2018, 2118)))


# variant 1: if (A > 0) B > 0
df_nonlin <- df_nonlin %>%
  dplyr::mutate(variant1 = stringr::str_detect(string = rule,
                                               pattern = 'if[(][[:print:]]+[[:space:]]?>[[:space:]]?0[)] [[:print:]]+[[:space:]]?>[[:space:]]?0'))

df_variant1 <- df_nonlin %>%
  dplyr::filter(variant1) %>%
  dplyr::mutate(a = stringr::str_replace_all(string = rule,
                                             pattern = 'if[(]([[:print:]]+)[[:space:]]?>[[:space:]]?0[)] ([[:print:]]+)[[:space:]]?>[[:space:]]?0',
                                                        replacement = '\\1 >= 0'),
                b = stringr::str_replace_all(string = rule,
                                             pattern = 'if[(]([[:print:]]+)[[:space:]]?>[[:space:]]?0[)] ([[:print:]]+)[[:space:]]?>[[:space:]]?0',
                                             replacement = '\\2 >= \\1 * c')) %>%
  dplyr::mutate(b = stringr::str_replace_all(string = b,
                                             pattern = '([[:print:]]+) >= ([[:print:]]+[+][[:print:]]+) [*] c',
                                             replacement = '\\1 >= (\\2) * c'))

df_variant1a <- df_variant1 %>%
  dplyr::mutate(rule = a,
                name = paste0(name, 'a')) %>%
  dplyr::select(all_of(colnames(df_nonlin)))

df_variant1b <- df_variant1 %>%
  dplyr::mutate(rule = unlist(lapply(b, replace_c, compr = '>=', data = data_PS)),
                name = paste0(name, 'b')) %>%
  dplyr::mutate(rule = stringr::str_replace_all(string = rule,
                                                pattern = '([[:print:]]+) >= [(]([[:print:]]+)[+]([[:print:]]+)[)] [*] ([[:print:]]+)',
                                                replacement = '\\1 >= \\2 * \\4 + \\3 * \\4')) %>%
  dplyr::select(all_of(colnames(df_nonlin)))
# last mutation eliminates brackets by multiplying through;
# assumption: maximum of two variables occur in the right-hand-side

df_nonlin <- df_nonlin %>%
  dplyr::filter(!variant1) %>%
  dplyr::select(-variant1)

df_linearized <- rbind(df_variant1a %>% dplyr::select(-variant1),
                       df_variant1b %>% dplyr::select(-variant1))


# variant 2: if (A > 0) B > A
df_nonlin <- df_nonlin %>%
  dplyr::mutate(variant2 = stringr::str_detect(string = rule,
                                               pattern = 'if[(][[:print:]]+[[:space:]]?>[[:space:]]?0[)] [[:print:]]+[[:space:]]?>[[:space:]]?[[:print:]]+')) %>%
  dplyr::mutate(aux1 = if_else(variant2,
                               stringr::str_replace_all(string = rule,
                                                        pattern = 'if[(]([[:print:]]+)[[:space:]]?>[[:space:]]?0[)] [[:print:]]+[[:space:]]?>[[:space:]]?([[:print:]]+)',
                                                        replacement = '\\1'),
                               NA),
                aux2 = if_else(variant2,
                               stringr::str_replace_all(string = rule,
                                                        pattern = 'if[(]([[:print:]]+)[[:space:]]?>[[:space:]]?0[)] [[:print:]]+[[:space:]]?>[[:space:]]?([[:print:]]+)',
                                                        replacement = '\\2'),
                               NA)) %>%
  dplyr::mutate(variant2 = variant2 & (stringr::str_trim(aux1) == stringr::str_trim(aux2)))

df_variant2 <- df_nonlin %>%
  dplyr::filter(variant2) %>%
  dplyr::mutate(a = paste0(aux1, ' >= 0'),
                b = stringr::str_replace_all(string = rule,
                                             pattern = 'if[(]([[:print:]]+)[[:space:]]?>[[:space:]]?0[)] ([[:print:]]+)[[:space:]]?>[[:space:]]?([[:print:]]+)',
                                             replacement = '\\2 > \\1 - eps'))

df_variant2a <- df_variant2 %>%
  dplyr::mutate(rule = a,
                name = paste0(name, 'a')) %>%
  dplyr::select(all_of(colnames(df_nonlin)))

df_variant2b <- df_variant2 %>%
  dplyr::mutate(rule = unlist(lapply(b, replace_eps, compr = '>')),
                name = paste0(name, 'b')) %>%
  dplyr::select(all_of(colnames(df_nonlin)))

df_nonlin <- df_nonlin %>%
  dplyr::filter(!variant2) %>%
  dplyr::select(-c(variant2,aux1,aux2))

df_linearized <- rbind(df_linearized,
                       df_variant2a %>% dplyr::select(-c(variant2,aux1,aux2)),
                       df_variant2b %>% dplyr::select(-c(variant2,aux1,aux2)))


# variant 3: if (A > 0) B >= A
df_nonlin <- df_nonlin %>%
  dplyr::mutate(variant3 = stringr::str_detect(string = rule,
                                               pattern = 'if[(][[:print:]]+[[:space:]]?>[[:space:]]?0[)] [[:print:]]+[[:space:]]?>=[[:space:]]?[[:print:]]+')) %>%
  dplyr::mutate(aux1 = if_else(variant3,
                               stringr::str_replace_all(string = rule,
                                                        pattern = 'if[(]([[:print:]]+)[[:space:]]?>[[:space:]]?0[)] [[:print:]]+[[:space:]]?>=[[:space:]]?([[:print:]]+)',
                                                        replacement = '\\1'),
                               NA),
                aux2 = if_else(variant3,
                               stringr::str_replace_all(string = rule,
                                                        pattern = 'if[(]([[:print:]]+)[[:space:]]?>[[:space:]]?0[)] [[:print:]]+[[:space:]]?>=[[:space:]]?([[:print:]]+)',
                                                        replacement = '\\2'),
                               NA)) %>%
  dplyr::mutate(variant3 = variant3 & (stringr::str_trim(aux1) == stringr::str_trim(aux2)))

df_variant3 <- df_nonlin %>%
  dplyr::filter(variant3) %>%
  dplyr::mutate(a = paste0(aux1, ' >= 0'),
                b = stringr::str_replace_all(string = rule,
                                             pattern = 'if[(]([[:print:]]+)[[:space:]]?>[[:space:]]?0[)] ([[:print:]]+)[[:space:]]?>=[[:space:]]?([[:print:]]+)',
                                             replacement = '\\2 >= \\1 - eps'))

df_variant3a <- df_variant3 %>%
  dplyr::mutate(rule = a,
                name = paste0(name, 'a')) %>%
  dplyr::select(all_of(colnames(df_nonlin)))

df_variant3b <- df_variant3 %>%
  dplyr::mutate(rule = unlist(lapply(b, replace_eps, compr = '>=')),
                name = paste0(name, 'b')) %>%
  dplyr::select(all_of(colnames(df_nonlin)))

df_nonlin <- df_nonlin %>%
  dplyr::filter(!variant3) %>%
  dplyr::select(-c(variant3,aux1,aux2))

df_linearized <- rbind(df_linearized,
                       df_variant3a %>% dplyr::select(-c(variant3,aux1,aux2)),
                       df_variant3b %>% dplyr::select(-c(variant3,aux1,aux2)))


# variant 4: if (A > 0) A <= B (equivalent aan variant 3)
df_nonlin <- df_nonlin %>%
  dplyr::mutate(variant4 = stringr::str_detect(string = rule,
                                               pattern = 'if[(][[:print:]]+[[:space:]]?>[[:space:]]?0[)] [[:print:]]+[[:space:]]?<=[[:space:]]?[[:print:]]+')) %>%
  dplyr::mutate(aux1 = if_else(variant4,
                               stringr::str_replace_all(string = rule,
                                                        pattern = 'if[(]([[:print:]]+)[[:space:]]?>[[:space:]]?0[)] ([[:print:]]+)[[:space:]]?<=[[:space:]]?[[:print:]]+',
                                                        replacement = '\\1'),
                               NA),
                aux2 = if_else(variant4,
                               stringr::str_replace_all(string = rule,
                                                        pattern = 'if[(]([[:print:]]+)[[:space:]]?>[[:space:]]?0[)] ([[:print:]]+)[[:space:]]?<=[[:space:]]?[[:print:]]+',
                                                        replacement = '\\2'),
                               NA)) %>%
  dplyr::mutate(variant4 = variant4 & (stringr::str_trim(aux1) == stringr::str_trim(aux2)))

df_variant4 <- df_nonlin %>%
  dplyr::filter(variant4) %>%
  dplyr::mutate(a = paste0(aux1, ' >= 0'),
                b = stringr::str_replace_all(string = rule,
                                             pattern = 'if[(]([[:print:]]+)[[:space:]]?>[[:space:]]?0[)] ([[:print:]]+)[[:space:]]?<=[[:space:]]?([[:print:]]+)',
                                             replacement = '\\3 >= \\2 - eps'))

df_variant4a <- df_variant4 %>%
  dplyr::mutate(rule = a,
                name = paste0(name, 'a')) %>%
  dplyr::select(all_of(colnames(df_nonlin)))

df_variant4b <- df_variant4 %>%
  dplyr::mutate(rule = unlist(lapply(b, replace_eps, compr = '>=')),
                name = paste0(name, 'b')) %>%
  dplyr::select(all_of(colnames(df_nonlin)))

df_nonlin <- df_nonlin %>%
  dplyr::filter(!variant4) %>%
  dplyr::select(-c(variant4,aux1,aux2))

df_linearized <- rbind(df_linearized,
                       df_variant4a %>% dplyr::select(-c(variant4,aux1,aux2)),
                       df_variant4b %>% dplyr::select(-c(variant4,aux1,aux2)))


# variant 5: if (A > 0) A < B (equivalent aan variant 2)
df_nonlin <- df_nonlin %>%
  dplyr::mutate(variant5 = stringr::str_detect(string = rule,
                                               pattern = 'if[(][[:print:]]+[[:space:]]?>[[:space:]]?0[)] [[:print:]]+[[:space:]]?<[[:space:]]?[[:print:]]+')) %>%
  dplyr::mutate(aux1 = if_else(variant5,
                               stringr::str_replace_all(string = rule,
                                                        pattern = 'if[(]([[:print:]]+)[[:space:]]?>[[:space:]]?0[)] ([[:print:]]+)[[:space:]]?<[[:space:]]?[[:print:]]+',
                                                        replacement = '\\1'),
                               NA),
                aux2 = if_else(variant5,
                               stringr::str_replace_all(string = rule,
                                                        pattern = 'if[(]([[:print:]]+)[[:space:]]?>[[:space:]]?0[)] ([[:print:]]+)[[:space:]]?<[[:space:]]?[[:print:]]+',
                                                        replacement = '\\2'),
                               NA)) %>%
  dplyr::mutate(variant5 = variant5 & (stringr::str_trim(aux1) == stringr::str_trim(aux2)))

df_variant5 <- df_nonlin %>%
  dplyr::filter(variant5) %>%
  dplyr::mutate(a = paste0(aux1, ' >= 0'),
                b = stringr::str_replace_all(string = rule,
                                             pattern = 'if[(]([[:print:]]+)[[:space:]]?>[[:space:]]?0[)] ([[:print:]]+)[[:space:]]?<[[:space:]]?([[:print:]]+)',
                                             replacement = '\\3 > \\2 - eps'))

df_variant5a <- df_variant5 %>%
  dplyr::mutate(rule = a,
                name = paste0(name, 'a')) %>%
  dplyr::select(all_of(colnames(df_nonlin)))

df_variant5b <- df_variant5 %>%
  dplyr::mutate(rule = unlist(lapply(b, replace_eps, compr = '>')),
                name = paste0(name, 'b')) %>%
  dplyr::select(all_of(colnames(df_nonlin)))

df_nonlin <- df_nonlin %>%
  dplyr::filter(!variant5) %>%
  dplyr::select(-c(variant5,aux1,aux2))

df_linearized <- rbind(df_linearized,
                       df_variant5a %>% dplyr::select(-c(variant5,aux1,aux2)),
                       df_variant5b %>% dplyr::select(-c(variant5,aux1,aux2)))


# variant 6: if (A <= 0) B == 0
df_nonlin <- df_nonlin %>%
  dplyr::mutate(variant6 = stringr::str_detect(string = rule,
                                               pattern = 'if[(][[:print:]]+[[:space:]]?<=[[:space:]]?0[)] [[:print:]]+[[:space:]]?==[[:space:]]?0'))

df_variant6 <- df_nonlin %>%
  dplyr::filter(variant6) %>%
  dplyr::mutate(a = stringr::str_replace_all(string = rule,
                                             pattern = 'if[(]([[:print:]]+)[[:space:]]?<=[[:space:]]?0[)] [[:print:]]+[[:space:]]?==[[:space:]]?0',
                                             replacement = '\\1 >= 0'),
                b = stringr::str_replace_all(string = rule,
                                             pattern = 'if[(][[:print:]]+[[:space:]]?<=[[:space:]]?0[)] ([[:print:]]+)[[:space:]]?==[[:space:]]?0',
                                             replacement = '\\1 >= 0'),
                c = stringr::str_replace_all(string = rule,
                                             pattern = 'if[(]([[:print:]]+)[[:space:]]?<=[[:space:]]?0[)] ([[:print:]]+)[[:space:]]?==[[:space:]]?0',
                                             replacement = '\\2 <= \\1 * d'))

df_variant6a <- df_variant6 %>%
  dplyr::mutate(rule = a,
                name = paste0(name, 'a')) %>%
  dplyr::select(all_of(colnames(df_nonlin)))

df_variant6b <- df_variant6 %>%
  dplyr::mutate(rule = b,
                name = paste0(name, 'b')) %>%
  dplyr::select(all_of(colnames(df_nonlin)))

df_variant6c <- df_variant6 %>%
  dplyr::mutate(rule = unlist(lapply(c, replace_d, compr = '<=', data = data_PS)),
                name = paste0(name, 'c')) %>%
  dplyr::select(all_of(colnames(df_nonlin)))

df_nonlin <- df_nonlin %>%
  dplyr::filter(!variant6) %>%
  dplyr::select(-variant6)

df_linearized <- rbind(df_linearized,
                       df_variant6a %>% dplyr::select(-variant6),
                       df_variant6b %>% dplyr::select(-variant6),
                       df_variant6c %>% dplyr::select(-variant6))


# the remaining rules in df_nonlin cannot be linearized by methods used here
# and are discarded

# check
test <- validator(.data = df_linearized)
linearized_testlin <- validatetools::is_linear(test)
table(linearized_testlin, useNA = 'ifany')


########
## make final selection of rules and write results to csv file
# (linearized version)

df_auto_final_linearized <- df_auto_final_not_linearized %>%
  dplyr::select(-c(rule, name)) %>%
  dplyr::left_join(rbind(df_auto_unique[testlin_auto, ],
                         df_linearized),
                   by = 'DT_ID',
                   relationship = 'many-to-many') %>%
  dplyr::filter(!is.na(rule))


### write results to csv

write.csv2(df_auto_final_linearized,
           file = output_csv_auto_linearized, row.names = FALSE)


