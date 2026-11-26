# Phase 1: profile, clean, and document the OULAD tables ---------------------
source("R/00_config.R")

files <- c("courses.csv", "assessments.csv", "vle.csv", "studentInfo.csv",
           "studentRegistration.csv", "studentAssessment.csv", "studentVle.csv")

raw <- lapply(files, read_oulad)
names(raw) <- sub("\\.csv$", "", files)

profiles <- data.table::rbindlist(
  Map(profile_table, raw, names(raw)), fill = TRUE
)
data.table::fwrite(profiles, file.path(PROFILE_DIR, "table_profile.csv"))

# Natural keys documented by OULAD. Duplicates in event tables are aggregated
# rather than silently dropped, preserving click totals.
clean <- raw
clean$studentVle <- clean$studentVle[
  , .(sum_click = sum(sum_click, na.rm = TRUE)),
  by = .(code_module, code_presentation, id_student, id_site, date)
]
clean$studentAssessment <- clean$studentAssessment[
  , .(date_submitted = first(date_submitted),
      is_banked = first(is_banked), score = first(score)),
  by = .(id_assessment, id_student)
]

# Normalize common missing-value representations and types.
for (nm in names(clean)) {
  for (j in names(clean[[nm]])) {
    if (is.character(clean[[nm]][[j]])) {
      clean[[nm]][[j]] <- trimws(clean[[nm]][[j]])
      clean[[nm]][[j]][clean[[nm]][[j]] %in% c("", "NA", "NULL")] <- NA_character_
    }
  }
}

saveRDS(clean, file.path(CLEAN_DIR, "oulad_clean.rds"), compress = FALSE)
clean_profiles <- data.table::rbindlist(
  Map(profile_table, clean, names(clean)), fill = TRUE
)
data.table::fwrite(clean_profiles, file.path(PROFILE_DIR, "clean_table_profile.csv"))

methodology <- c(
  "# GRASP network methodology (Phase 1)",
  "",
  paste0("- Cutoffs: Day 28 and Day 56, using `date <= cutoff`."),
  paste0("- Weekly snapshots: weeks 0 through 8; week = floor(date / 7)."),
  "- Student-resource edge weight: sum of `sum_click` for a student-resource pair.",
  "- Student-student projection: similarity between student resource profiles.",
  "- Resource-resource projection: similarity between resource student profiles.",
  paste0("- Similarity: ", SIMILARITY, "; cutoff: similarity > ", SIMILARITY_CUTOFF, "."),
  "- The temporal network is exported as a student-resource graph per snapshot.",
  "- Duplicate event rows are aggregated; missing categorical values remain NA.",
  "- Students and resources with no interactions in a snapshot are not added as edges."
)
writeLines(methodology, file.path(PROFILE_DIR, "network_methodology.md"))
message("Phase 1 complete. Profiles and cleaned data are in outputs/.")
