#' Get species pairs
#'
#' @param nspp max n species
#'
#' @return data frame of all unique species pairs
#' @export
#'
#' @importFrom dplyr filter rename
get_pairs <- function(nspp) {

  pairs <- expand.grid(1:nspp, 1:nspp) %>%
    as.data.frame() %>%
    dplyr::filter(Var1 < Var2) %>%
    dplyr::rename(sp1 = Var1,
           sp2 = Var2)

  return(pairs)

}

#' Get species pairs
#'
#' @param pair vector of 2 species ids
#' @param community_df community df
#'
#' @return list of the two species wgts
#' @export
#' @importFrom dplyr filter
get_species_pairs <- function(pair, community_df) {

  sp1 <- dplyr::filter(community_df, species == pair[1])$wgt
  sp2 <- dplyr::filter(community_df, species == pair[2])$wgt

  return(list(sp1 = sp1, sp2 = sp2))

}

#' Make scaled kde
#'
#' @param sbsd Wgt vector
#' @param min_size minimum range for kde
#' @param max_size max range for kde
#' @param npoints nb eval points, specify to powers of 2
#'
#' @return scaled probability density
#' @export
#'
make_kde <- function(sbsd, min_size, max_size, npoints) {

  raw_kde <- density(sbsd, from = min_size, to = max_size, n = npoints)$y

  scaled_kde <- raw_kde / sum(raw_kde)

  return(scaled_kde)

}

pair_overlap <- function(species_vects, min_size, max_size, npoints) {

  kdes <- lapply(species_vects, FUN = make_kde, npoints = npoints, min_size = min_size, max_size = max_size)

  kdes_df <- dplyr::bind_cols(kdes) %>%
    dplyr::mutate(index = dplyr::row_number())  %>%
    dplyr:: group_by(index) %>%
    dplyr::mutate(min_density = min(sp1, sp2)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-index)

  overlap_val <- sum(kdes_df$min_density)

  return(overlap_val)

}

#' Harmonic mean of abundances
#'
#' @param species_vects wgt vectors
#'
#' @return Harmonic mean of abundances
#' @export
#' @importFrom psych harmonic.mean
pair_harmonic_mean <- function(species_vects){
  abunds <- lapply(species_vects, FUN = length)

  hm <- psych::harmonic.mean(unlist(abunds))

  return(hm)
}

#' Community level overlap
#'
#' @param community_df full community df
#'
#' @return df of overlap values for all species pairs + harmonic mean of abundances
#' @export
community_overlap <- function(community_df) {

  all_pairings <- get_pairs(max(community_df$species))

  all_species_pairs <- apply(all_pairings, MARGIN = 1, FUN = get_species_pairs, community_df = community_df)

  all_pair_overlaps <- lapply(all_species_pairs, FUN = pair_overlap, min_size = 0, max_size = max(community_df$wgt) * 1.5, npoints = 1024)

  all_pair_hms <- lapply(all_species_pairs, FUN = pair_harmonic_mean)

  all_pairings$overlap <- unlist(all_pair_overlaps)

  all_pairings$hm <- unlist(all_pair_hms)

  return(all_pairings)
}
