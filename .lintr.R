# lintr configuration for Kalium360 (used by CI and locally, e.g. in RStudio)
#
# Only "correctness"-tagged linters are enabled: they flag likely bugs,
# not style issues. Style linters (line length, spacing, assignment
# operators, ...) are deliberately not used: the scripts intentionally
# use <<- to share state between the sourced files.
#
# object_usage_linter is excluded because lintr checks each file on its
# own, while the scripts are sourced together by Main.R: functions from
# sibling files, packages attached in Main.R, globals from config.R and
# variables used inside glue_sql() strings would all be reported as
# false positives.
linters <- linters_with_tags(tags = "correctness", object_usage_linter = NULL)
