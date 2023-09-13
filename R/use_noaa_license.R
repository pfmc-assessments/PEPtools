#' Document the license for a NOAA package
#'
#' @description
#' Defines the License portion of your DESCRIPTION file and prints the NOAA
#' non-license file to the LICENSE, which is ignored in your build but
#' available for those who wish to view it.
#'
#' @param version An integer specifying which GPL license you wish to use.
#'   Currently, only two or three are allowed and it defaults to three, which
#'   is the latest version as of May 2023.
#' @param include_future A logical value. If `TRUE`, the result will be to
#'   license your package under the current and any potential future versions
#'   of the license. This is generally considered to be good practice because
#'   it means your package will automatically include "bug" fixes in licenses.
#'   That is, the resulting text will have a great than or equals to sign for
#'   when the license is updated.
#' @return
#' A logical vector indicating if file was modified.
#' @export
#' @author Kelli F. Johnson
#' @seealso
#' * [usethis::use_gpl_license()]
#' * <https://choosealicense.com>
#' * [license chapter](https://r-pkgs.org/license.html) in _R Packages_.
use_noaa_license <- function(version = 3, include_future = TRUE) {
  version <- usethis:::check_license_version(version, 2:3)

  if (usethis:::is_package()) {
    abbr <- usethis:::license_abbr("GPL", version, include_future)
    full <- glue::glue("{abbr} + file LICENSE")
    usethis:::proj_desc_field_update("License", full, overwrite = TRUE)
  }
  usethis::use_template(
    template = "LICENSE_noaa",
    package = "PEPtools",
    save_as = "LICENSE",
    ignore = TRUE
  )
}
