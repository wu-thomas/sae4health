.onLoad <- function(libname, pkgname) {

  ############################################################################
  ### Use planar (GEOS) geometry instead of s2 spherical geometry.
  ###
  ### Some country boundary layers (e.g. Benin) contain rings that s2 considers
  ### invalid ("Loop N is not valid: Edge A crosses edge B"). Under s2, the first
  ### spatial operation inside surveyPrev::clusterInfo() (the point-in-polygon
  ### st_join) aborts, which surfaces during model fitting / data-sparsity as
  ### that geometry error or, via a half-built cluster.info, as
  ### "`x` and `y` must share the same src". Repairing geometry with
  ### st_make_valid() is not sufficient: GEOS-valid geometry is not guaranteed to
  ### be s2-valid, so s2 still rejects it.
  ###
  ### Switching sf to planar geometry makes these spatial joins use GEOS, which
  ### tolerates such geometry, and is entirely adequate for the country-scale
  ### point-in-polygon assignment and mapping this app performs. We only set it
  ### when sf is available and leave it unchanged if the user has already chosen.
  ############################################################################
  if (requireNamespace("sf", quietly = TRUE)) {
    try(suppressMessages(sf::sf_use_s2(FALSE)), silent = TRUE)
  }
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "sae4health loaded successfully.\n",
    "Run 'sae4health::run_app()' to launch the app.\n",
    "For comprehensive documentation, visit https://sae4health.stat.uw.edu/."
  )
}
