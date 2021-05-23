#' discrep
#'
#' [micah project] compute discrepancies between two coders
#' @path folder in which the two coders' method extraction sheets in .csv format are
#' @initial1 initials for first coder in quotes
#' @initial2 initials for second coder in quotes
#' @aidfrom article id from in quotes
#' @aidto article id to in quotes
#'
#' @examples
#' discrep(path = "/Users/phoebelam/Desktop/micah", initial1 = "kk", initial2 = "arp", aidfrom = "b21", aidto = "b26")
#'
#' @export
discrep <- function(path, initial1, initial2, aidfrom, aidto) {

  a <- read.csv(paste(path, "/method extraction_", initial1, " - method.csv", sep=""), na="") %>%
    tidyr::fill(article.id) %>%
    dplyr::rename_at(dplyr::vars(!link.id & !article.id & !exclude), ~paste(., "_a", sep="")) %>%
    dplyr::filter(is.na(link.id)==F)

  a <- a[which(a$article.id == aidfrom)[1]:tail(which(a$article.id == aidto), 1), ] %>%
    dplyr::filter(exclude == 0) %>%
    select(-c(exclude, article.id)) #killing these duplicated variables in set a

  b <- read.csv(paste(path, "/method extraction_", initial2, " - method.csv", sep=""), na="") %>%
    tidyr::fill(article.id) %>%
    dplyr::rename_at(dplyr::vars(!link.id & !article.id & !exclude), ~paste(., "_b", sep="")) %>%
    dplyr::filter(is.na(link.id)==F)

  b <- b[which(b$article.id == aidfrom)[1]:tail(which(b$article.id == aidto), 1), ] %>%
    dplyr::filter(exclude == 0)

  dat <- merge(a, b, by = "link.id") %>%
    dplyr::mutate_at(dplyr::vars(matches("assessment|timeframe|age|design|country|adiposity|female|white|black|latino|asian|other|psychiatric|physical|subgroup")),
                     ~as.character(.)) %>%
    dplyr::mutate_at(dplyr::vars(matches("assessment|timeframe|age|design|country|adiposity|female|white|black|latino|asian|other|psychiatric|physical|subgroup")),
                     list(~dplyr::case_when(is.na(.)==T~ "missing", TRUE~.))) %>%
    dplyr::mutate_at(dplyr::vars(matches("assessment|timeframe|design|country|adiposity|psychiatric|physical|subgroup")),
                     list(~tolower(.))) -> dat

  dat %>%
    dplyr::filter(iv.assessment_a != iv.assessment_b) %>%
    dplyr::select (link.id, iv.assessment_a, iv.assessment_b) -> iv.ass

  dat %>%
    dplyr::filter(iv.timeframe_a != iv.timeframe_b) %>%
    dplyr::select (link.id, iv.timeframe_a, iv.timeframe_b) -> iv.time

  dat %>%
    dplyr::filter(iv.mean.age_b != iv.mean.age_a) %>%
    dplyr::select (link.id, iv.mean.age_a, iv.mean.age_b) -> iv.agemean

  dat %>%
    dplyr::filter(iv.age.sd_b != iv.age.sd_a) %>%
    dplyr::select (link.id, iv.age.sd_a, iv.age.sd_b) -> iv.agesd

  dat %>%
    dplyr::filter(iv.age.range_b != iv.age.range_a) %>%
    dplyr::select (link.id, iv.age.range_a, iv.age.range_b) -> iv.agerange

  dat %>%
    dplyr::filter(dv.assessment_b != dv.assessment_a) %>%
    dplyr::select (link.id, dv.assessment_a, dv.assessment_b) -> dv.ass

  dat %>%
    dplyr::filter(dv.timeframe_b != dv.timeframe_a) %>%
    dplyr::select (link.id, dv.timeframe_a, dv.timeframe_b) -> dv.time

  dat %>%
    dplyr::filter(dv.mean.age_b != dv.mean.age_a) %>%
    dplyr::select (link.id, dv.mean.age_a, dv.mean.age_b) -> dv.agemean

  dat %>%
    dplyr::filter(dv.age.sd_b != dv.age.sd_a) %>%
    dplyr::select (link.id, dv.age.sd_a, dv.age.sd_b) -> dv.agesd

  dat %>%
    dplyr::filter(dv.age.range_b != dv.age.range_a) %>%
    dplyr::select (link.id, dv.age.range_a, dv.age.range_b) -> dv.agerange

  dat %>%
    dplyr::filter(design_b != design_a) %>%
    dplyr::select (link.id, design_a, design_b) -> design

  dat %>%
    dplyr::filter(female.._b != female.._a) %>%
    dplyr::select (link.id, female.._a, female.._b) -> female

  dat %>%
    dplyr::filter(white.._b != white.._a) %>%
    dplyr::select (link.id, white.._a, white.._b) -> white

  dat %>%
    dplyr::filter(black.._b != black.._a) %>%
    dplyr::select (link.id, black.._a, black.._b) -> black

  dat %>%
    dplyr::filter(latino.._b != latino.._a) %>%
    dplyr::select (link.id, latino.._a, latino.._b) -> latino

  dat %>%
    dplyr::filter(asian.._b != asian.._a) %>%
    dplyr::select (link.id, asian.._a, asian.._b) -> asian

  dat %>%
    dplyr::filter(other.._b != other.._a) %>%
    dplyr::select (link.id, other.._a, other.._b) -> other

  dat %>%
    dplyr::filter(psychiatric.._b != psychiatric.._a) %>%
    dplyr::select (link.id, psychiatric.._a, psychiatric.._b) -> psychiatric

  dat %>%
    dplyr::filter(physical.._b != physical.._a) %>%
    dplyr::select (link.id, physical.._a, physical.._b) -> physical

  dat %>%
    dplyr::filter(subgroup.n.a_b != subgroup.n.a_a) %>%
    dplyr::select (link.id, subgroup.n.a_a, subgroup.n.a_b) -> subgroupna

  dat %>%
    dplyr::filter(country_b != country_a ) %>%
    dplyr::select (link.id, country_a, country_b) -> country

  dat %>%
    dplyr::filter(adiposity_b != adiposity_a) %>%
    dplyr::select (link.id, adiposity_a, adiposity_b) -> adiposity

  list ("iv.assessment"= iv.ass,
        "iv.timeframe"= iv.time,
        "iv.agemean" = iv.agemean,
        "iv.agesd" = iv.agesd,
        "iv.agerange" = iv.agerange,
        "dv.assessment"= dv.ass,
        "dv.timeframe"= dv.time,
        "dv.agemean" = dv.agemean,
        "dv.agesd" = dv.agesd,
        "dv.agerange" = dv.agerange,
        "design" = design,
        "female" = female,
        "white" = white,
        "black" = black,
        "latino"= latino,
        "asian"= asian,
        "other" = other,
        "psychiatric" = psychiatric,
        "physical"= physical,
        "subgroupna" = subgroupna,
        "country" = country,
        "adiposity" = adiposity) -> summary

  openxlsx::write.xlsx(summary, file = paste(path, "/method discrepancies_", aidfrom, "-", aidto, "_", format(Sys.Date(), format="%m.%d.%y"), ".xlsx", sep=""))


}
