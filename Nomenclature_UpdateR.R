# Function to update nomenclature of lipid species
Nomenclature_update <- function(Data,
                                method_version = c("v2", "v4")) {

  #Input validation
  if (!is.data.frame(Data)) {
    stop("`Data` must be a data frame.", call. = FALSE)
  }
  if (ncol(Data) == 0) {
    stop("`Data` has no columns to rename.", call. = FALSE)
  }

  #Check required packages
  required_pkgs <- c("dplyr", "readr", "stringr")
  missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace,
                                        quietly = TRUE, FUN.VALUE = logical(1))]
  if (length(missing_pkgs) > 0) {
    stop("The following required packages are not installed: ",
         paste(missing_pkgs, collapse = ", "),
         "\nPlease install them with: install.packages(c(",
         paste0('"', missing_pkgs, '"', collapse = ", "), "))",
         call. = FALSE)
  }

  method_version <- match.arg(method_version)

  if (method_version == "v2") {
    key_url <- "https://raw.githubusercontent.com/Hszemray/Lipid_Nomenclature_Conversion/main/lipid_nomenclature_conversion_v2.csv"
    target_classes <- c("CE", "CER", "MAG", "DAG", "FFA", "LCER", "HCER",
                        "LPC", "LPE", "LPI", "LPG", "LPS",
                        "PE", "PC", "PS", "PG", "PI", "SM", "TAG", "DCER")
  } else {
    key_url <- "https://raw.githubusercontent.com/Hszemray/Lipid_Nomenclature_Conversion/main/lipid_nomenclature_conversion_v4.csv"
    target_classes <- c("CE", "CER", "MAG", "DAG", "FFA", "LCER", "HCER",
                        "LPA", "LPC", "LPE", "LPI", "LPG", "LPS",
                        "PA", "PE", "PC", "PS", "PG", "PI", "SM", "TAG",
                        "DCER", "CAR", "CL")
  }

  #Download and read the key with error handling
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)

  tryCatch(
    download.file(key_url, destfile = tmp, mode = "wb", quiet = TRUE),
    error = function(e) {
      stop("Failed to download nomenclature key from:\n  ", key_url,
           "\n  Underlying error: ", conditionMessage(e), call. = FALSE)
    }
  )

  Nomenclature_key <- tryCatch(
    readr::read_csv(tmp, show_col_types = FALSE),
    error = function(e) {
      stop("Failed to parse the nomenclature CSV.\n  Underlying error: ",
           conditionMessage(e), call. = FALSE)
    }
  )

  Nomenclature_key <- Nomenclature_key[, c("precursor_name", "updated_precursor_name")]
  
  #Build named map: old -> new
  Nomenclature_key <- Nomenclature_key |>
    dplyr::mutate(
      precursor_name         = stringr::str_trim(as.character(precursor_name)),
      updated_precursor_name = stringr::str_trim(as.character(updated_precursor_name))
    )

  nomenclature_map <- setNames(Nomenclature_key$updated_precursor_name,
                                Nomenclature_key$precursor_name)

  #Vectorised normalisation helper
  # Builds a regex prefix pattern from target classes once
  class_prefix <- paste0("^(", paste(target_classes, collapse = "|"), ")")

  normalise_cols <- function(x) {
    x <- stringr::str_trim(x)

    # CLASS.xx:yy  ->  CLASS(xx:yy)
    x <- stringr::str_replace(
      x,
      paste0(class_prefix, "\\.(\\d{1,3}:\\d{1,2}(?:;\\w+)*)$"),
      "\\1(\\2)"
    )

    # PE.P.xx:yy.xx:yy  ->  PE(P-xx:yy/xx:yy)  (plasmalogen)
    x <- stringr::str_replace(
      x,
      "^(PE|PC)\\.P\\.(\\d{1,3}:\\d{1,2}(?:;\\w+)*)\\.(\\d{1,3}:\\d{1,2}(?:;\\w+)*)$",
      "\\1(P-\\2_\\3)"
    )

    # PE.O.xx:yy.xx:yy  ->  PE(O-xx:yy/xx:yy)  (ether)
    x <- stringr::str_replace(
      x,
      "^(PE|PC)\\.O\\.(\\d{1,3}:\\d{1,2}(?:;\\w+)*)\\.(\\d{1,3}:\\d{1,2}(?:;\\w+)*)$",
      "\\1(O-\\2_\\3)"
    )

    # CLASS:xx:yy  ->  CLASS(xx:yy)
    x <- stringr::str_replace(
      x,
      paste0(class_prefix, ":(\\d{1,3}:\\d{1,2}(?:;\\w+)*)$"),
      "\\1(\\2)"
    )

    x
  }

  # Normalise and remap column names
  original_names  <- colnames(Data)
  normalised_names <- normalise_cols(original_names)

  # v4 key uses "/" for ether/plasmalogen chains; v2 uses "_"
  if (method_version == "v4") {
    # PE(O-16:0_18:1) -> PE(O-16:0/18:1)
    # PE(P-16:0_18:1) -> PE(P-16:0/18:1)
    normalised_names <- stringr::str_replace(
      normalised_names,
      "^(PE|PC)\\(([OP]-\\d{1,3}:\\d{1,2})_(\\d{1,3}:\\d{1,2})\\)$",
      "\\1(\\2/\\3)"
    )
  }

  colnames(Data) <- normalised_names

  #direct named-vector lookup
  matched   <- nomenclature_map[colnames(Data)]
  new_names <- ifelse(is.na(matched), colnames(Data), matched)

   # Diagnostics — compare normalised names to their looked-up replacements
  normalised <- colnames(Data)          # names after normalise_cols()
  in_key     <- normalised %in% names(nomenclature_map)
 
  # 1. Changed: found in key AND the new name differs from the normalised name
  changed_mask   <- in_key & (new_names != normalised)
  changed_count  <- sum(changed_mask)
  changed_names  <- paste0(normalised[changed_mask], " -> ", new_names[changed_mask])
 
  # 2. Already matching: found in key BUT old == new (already had the updated name)
  already_mask   <- in_key & (new_names == normalised)
  already_count  <- sum(already_mask)
  already_names  <- normalised[already_mask]
 
  # 3. No match: lipid-like columns that are NOT in the key at all
  lipid_pattern  <- paste0("^(", paste(target_classes, collapse = "|"), ")")
  is_lipid       <- stringr::str_detect(normalised, lipid_pattern)
  no_match_mask  <- !in_key & is_lipid
  no_match_names <- normalised[no_match_mask]
 
  # Apply new names
  colnames(Data) <- new_names
 
  # Report
  message("Columns updated: ", changed_count)
  if (changed_count > 0) {
    message("  ", paste(changed_names, collapse = "\n  "))
  }
 
  message("Columns already matching updated nomenclature: ", already_count)
  if (already_count > 0) {
    message("  ", paste(already_names, collapse = ", "))
  }
 
  if (length(no_match_names) > 0) {
    message("Lipid columns with no match in conversion key (", length(no_match_names), "): ",
            paste(no_match_names, collapse = ", "))
  }

  Data
}
