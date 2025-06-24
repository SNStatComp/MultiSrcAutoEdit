
#####
## Define scenario

# prefix to add to file names
PREFIX <- 'sc2_'

# apply deductive correction rules (TRUE or FALSE)
use_deductive_correction <- FALSE

# method(s) for computing dynamic reliability weights
# options:
# NULL             - do not compute dynamic reliability weights
# 'quality'        - adjust reliability weights based on input quality
# 'neighbour'      - adjust reliability weights based on nearest-neighbour method
# c('quality', 'neighbour') - apply both adjustment methods
set_weights_dynamic <- c('quality', 'neighbour')

# use soft interstat edit rules (TRUE or FALSE)
use_interstat_rules_soft <- TRUE



#####
## general settings

# year to be processed
YEAR <- 2022L

# relative bound for inconsistencies
LOWER_BOUND_REL <- 0.95
UPPER_BOUND_REL <- 1.05

# absolute bound for inconsistencies for financial amounts
LOWER_BOUND_ABS <- 50
UPPER_BOUND_ABS <- 50

# absolute bound for inconsistencies for counts
LOWER_BOUND_ABS_COUNT <- 2
UPPER_BOUND_ABS_COUNT <- 2

# method to take absolute bounds into account
# 'exact' or 'approx'
method_bound_abs <- 'approx'

# variables that can have negative values
neg_vars <- c('Reversals_Impairments',
              'Saldo_boekwinsten_verliezen_op_Verkopen_iMVA',
              'Saldo_Uitzonderlijke_Baten_Lasten',
              'Exploitatie_Overschot',
              'EBITDA',
              'Financieel_Resultaat_Op_Lease')

# variables that are counts, not financial amounts
count_vars <- c('Werknemers_in_vte_Gemiddeld')

# selection criterion for soft rules based on prediction intervals
# (bound on R^2)
pred_rule_minR2 <- 0.95

# weights for soft restrictions
WEIGHT_SOFT_LOW <- 0.5
WEIGHT_SOFT_HIGH <- 30


#####
## settings for imputation

# blocks of variables where stratum means should be computed based on the same units
variable_blocks <- list(
  # PS variables which overlap with WIA (admin. data):
  PS_WIA = c('Netto_Omzet_minus_accijnzen',
             'Arbeidskosten',
             'Lonen',
             'Sociale_lasten',
             'Pensioenlasten',
             'Overige_personeelskosten',
             'Inkoopwaarde_Omzet',
             'Uitbesteed_werk',
             'Inkoopwaarde_Omzet_excl_uitbesteed',
             'Marge_omzet'),
  # PS variables which do not overlap with WIA (admin. data):
  PS_noWIA = c('Omzet_Industrieel_minus_accijnzen',
               'Omzet_Handel_en_Overig',
               'Netto_Omzet_minus_accijnzen_minus_doorberekende_vrachtkosten',
               'Omzet_Zelfvervaardigd_Nederland_minus_accijnzen_minus_doorberekende_vrachtkosten',
               'Omzet_Industriele_Diensten',
               'Werknemers_in_vte_Gemiddeld',
               'Productie_SFO',
               'Subsidies',
               'Afschrijvingen_excl_die_op_operational_lease',
               'Afschrijvingen_op_operational_Lease',
               'Reversals_Impairments',
               'Saldo_boekwinsten_verliezen_op_Verkopen_iMVA',
               'Saldo_Uitzonderlijke_Baten_Lasten',
               'Exploitatie_Overschot',
               'EBITDA',
               'Financieel_Resultaat_Op_Lease'),
  # INIVA variables which overlap with WIA (admin. data):
  INIVA_WIA = c('Investeringen_materiele_vaste_activa_INIVA_WIA',
                'Investeringen_immateriele_vaste_activa_INIVA_WIA'),
  # INIVA variables which do not overlap with WIA (admin. data):
  INIVA_noWIA = c('Investeringen_materiele_vaste_activa_INIVA_SFGO',
                  'Investeringen_immateriele_vaste_activa_INIVA_SFGO',
                  'Investeringen_op_lease_INIVA_SFGO'),
  # international trade variables:
  IH = c('Import',
         'Export')
)

# auxiliary variables for imputation in Step 2a
naam_omzetvar <- 'Netto_Omzet_minus_accijnzen'
naam_wpvar <- 'Werknemers_in_vte_Gemiddeld'

# minimal number of donors needed per stratum to compute a mean
MIN_STRATUM_MEAN <- 5L

# collapsing scheme for merging strata to reach the minimal number of donors
COLLAPSE_IMPUTATION <- Jaar * SbiGecoordineerd3D * GkSbsGecoordineerd1D ~
  SbiGecoordineerd3D * GkSbsGecoordineerd1D +
  Jaar * SbiGecoordineerd2D * GkSbsGecoordineerd1D +
  SbiGecoordineerd2D * GkSbsGecoordineerd1D +
  Jaar * SbiGecoordineerd3D * GkSbsGecoordineerdSML +
  SbiGecoordineerd3D * GkSbsGecoordineerdSML +
  Jaar * SbiGecoordineerd2D * GkSbsGecoordineerdSML +
  SbiGecoordineerd2D * GkSbsGecoordineerdSML +
  Jaar * SbiGecoordineerd3D +
  SbiGecoordineerd3D +
  Jaar * SbiGecoordineerd2D +
  SbiGecoordineerd2D +
  Jaar * een +
  een


#####
## settings for PS imputation

# auxiliary variables for imputation
naam_omzetvar_PS <- 'PS.OMZETPS210000'
naam_wpvar_PS <- 'PS.PERSONS100000'

# collapsing scheme for merging strata to reach the minimal number of donors
COLLAPSE_IMPUTATION_PS <- PS.VragenlijstID * SbiGecoordineerd4D * GkSbsGecoordineerd1D ~
  PS.VragenlijstID * SbiGecoordineerd4D * GkSbsGecoordineerdSML +
  PS.VragenlijstID * SbiGecoordineerd4D +
  PS.VragenlijstID * SbiGecoordineerd3D * GkSbsGecoordineerd1D +
  PS.VragenlijstID * SbiGecoordineerd3D * GkSbsGecoordineerdSML +
  PS.VragenlijstID * SbiGecoordineerd3D +
  PS.VragenlijstID * SbiGecoordineerd2D * GkSbsGecoordineerd1D +
  PS.VragenlijstID * SbiGecoordineerd2D * GkSbsGecoordineerdSML +
  PS.VragenlijstID * SbiGecoordineerd2D +
  PS.VragenlijstID +
  SbiGecoordineerd4D +
  SbiGecoordineerd3D +
  SbiGecoordineerd2D +
  een


######
## settings for assigning dynamic reliability weights by nearest-neighbour approach

# minimal number of donors needed per stratum to assign dynamic reliability weights
MIN_STRATUM_WEIGHTS <- 20L

# collapsing scheme for merging strata to reach the minimal number of donors
COLLAPSE_DYNAMIC_WEIGHTS <- BE_ID * Jaar ~
  Jaar * SbiGecoordineerd3D * GkSbsGecoordineerd1D +
  SbiGecoordineerd3D * GkSbsGecoordineerd1D +
  Jaar * SbiGecoordineerd2D * GkSbsGecoordineerd1D +
  SbiGecoordineerd2D * GkSbsGecoordineerd1D +
  Jaar * SbiGecoordineerd3D * GkSbsGecoordineerdSML +
  SbiGecoordineerd3D * GkSbsGecoordineerdSML +
  Jaar * SbiGecoordineerd2D * GkSbsGecoordineerdSML +
  SbiGecoordineerd2D * GkSbsGecoordineerdSML +
  Jaar * SbiGecoordineerd3D +
  SbiGecoordineerd3D +
  Jaar * SbiGecoordineerd2D +
  SbiGecoordineerd2D +
  Jaar * een +
  een

# lower and upper quantile to use in measure of stratum variation
# during assignment of dynamic reliability weights
S_QUANTILE_LOWER <- 0.1
S_QUANTILE_UPPER <- 0.9

# method to adjust reliability weights
# options:
#  - 'factor': divide weights by a factor based on relative contribution to distance
#  - 'weight_function': use weight_function to compute transformed distances;
#                       in this case, also choose the value of a_weight
weight_method <- 'factor'
# a_weight <- 1


#####
## settings for error localization

# constant to use for a 'large number' in edit rules
M <- 100000

# maximum computing time per record for errorlocate
TIMEOUT <- 30

# maximum number of records per foreach-loop for parallel processing
N_MAX <- 100

# precision for evaluating inequalities and equalities (in match_restrictions)
LIN.EQ.EPS <- 0.001
LIN.INEQ.EPS <- 0.001

# maximum number of iterations for match_restrictions
MAX_IT_MR <- 1e5
