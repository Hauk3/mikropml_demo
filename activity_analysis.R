library(tidyverse)
library(lubridate)
library(readxl)

# ── 1. Load ──────────────────────────────────────────────────────────────────

# Accepts CSV (Workload_3 format) or Excel (SharePoint export format)
data_file <- "data/query_4.xlsx"

if (str_ends(data_file, "\\.xlsx|\\.xls")) {
  raw <- read_excel(data_file)
  # Normalise SharePoint column names to match CSV schema
  raw <- raw %>%
    rename_with(~ case_when(
      . == "Start Time"            ~ "start_time",
      . == "End Time"              ~ "end_time",
      . == "...or Duration (mins)" ~ "duration_mins_raw",
      . == "Title"                 ~ "title",
      . == "Task"                  ~ "task_raw",
      . == "Task Subset"           ~ "task_subset_raw",
      . == "Project"               ~ "project",
      . == "Created By"            ~ "created_by",
      . == "Biochemist"            ~ "biochemist",
      . == "Calculated Duration"   ~ "calc_duration",
      TRUE                         ~ .
    )) %>%
    mutate(across(everything(), as.character))
  sharepoint_format <- TRUE
} else {
  raw <- read_csv(
    data_file,
    col_names = c("start_time", "end_time", "duration_mins_raw",
                  "title", "task_raw", "task_subset_raw", "project",
                  "created_by", "biochemist", "calc_duration"),
    skip = 1,
    col_types = cols(.default = "c")
  )
  sharepoint_format <- FALSE
}

# ── 2. Parse times & durations ───────────────────────────────────────────────

df <- raw %>%
  mutate(
    start_dt       = if_else(sharepoint_format,
                             ymd_hms(start_time, tz = "UTC"),
                             mdy_hm(start_time)),
    end_dt         = if_else(sharepoint_format,
                             ymd_hms(end_time, tz = "UTC"),
                             mdy_hm(end_time)),
    dur_explicit   = as.numeric(duration_mins_raw),
    dur_from_times = as.numeric(difftime(end_dt, start_dt, units = "mins")),
    duration_mins  = if_else(!is.na(end_dt), dur_from_times, dur_explicit)
  )

skipped <- df %>% filter(is.na(duration_mins) | duration_mins <= 0)
message(sprintf("Skipping %d rows with no usable duration:", nrow(skipped)))
skipped %>% select(start_time, title) %>% print()

df <- df %>%
  filter(!is.na(start_dt), !is.na(duration_mins), duration_mins > 0) %>%
  mutate(end_dt_derived = start_dt + dminutes(duration_mins))

# ── 3. Wall-clock tracked time (de-overlapped per biochemist) ─────────────────

deoverlap_minutes <- function(starts, ends) {
  ord    <- order(starts)
  starts <- starts[ord]; ends <- ends[ord]
  cur_s  <- starts[1];   cur_e <- ends[1]
  total  <- 0
  for (i in seq_along(starts)[-1]) {
    s <- starts[i]; e <- ends[i]
    if (s >= cur_e) {
      total <- total + as.numeric(difftime(cur_e, cur_s, units = "mins"))
      cur_s <- s; cur_e <- e
    } else {
      cur_e <- max(cur_e, e)
    }
  }
  total + as.numeric(difftime(cur_e, cur_s, units = "mins"))
}

tracked <- df %>%
  group_by(created_by) %>%
  summarise(wall_clock_mins = deoverlap_minutes(start_dt, end_dt_derived), .groups = "drop")

message("\n── Tracked time (wall-clock, de-overlapped) ──")
tracked %>% mutate(hours = round(wall_clock_mins / 60, 1)) %>% print()

# ── 4. Parse multi-value tag arrays ──────────────────────────────────────────

# CSV format uses JSON-like ["Tag A","Tag B"]; SharePoint Excel uses "Tag1;#Tag2"
parse_tags <- function(x, sharepoint = FALSE) {
  if (sharepoint) {
    x %>%
      str_split(";#") %>%
      lapply(function(v) str_trim(v[nchar(str_trim(v)) > 0]))
  } else {
    x %>%
      str_remove_all('\\[|\\]|"') %>%
      str_split(",") %>%
      lapply(function(v) str_trim(v[nchar(str_trim(v)) > 0]))
  }
}

df <- df %>%
  mutate(
    tasks        = parse_tags(task_raw,        sharepoint = sharepoint_format),
    task_subsets = parse_tags(task_subset_raw, sharepoint = sharepoint_format)
  )

# ── 5. Summarise by task category ────────────────────────────────────────────

task_summary <- df %>%
  select(created_by, duration_mins, tasks) %>%
  unnest(tasks) %>%
  filter(tasks != "") %>%
  group_by(created_by, task_category = tasks) %>%
  summarise(raw_mins = sum(duration_mins), .groups = "drop") %>%
  left_join(tracked, by = "created_by") %>%
  mutate(hours = round(raw_mins / 60, 1),
         pct_of_tracked = round(100 * raw_mins / wall_clock_mins, 1)) %>%
  arrange(created_by, desc(raw_mins))

message("\n── Time by Task Category ──")
task_summary %>% select(created_by, task_category, hours, pct_of_tracked) %>% print(n = Inf)

# ── 6. Summarise by task subset ──────────────────────────────────────────────

subset_summary <- df %>%
  select(created_by, duration_mins, task_subsets) %>%
  unnest(task_subsets) %>%
  filter(task_subsets != "") %>%
  group_by(created_by, task_subset = task_subsets) %>%
  summarise(raw_mins = sum(duration_mins), .groups = "drop") %>%
  left_join(tracked, by = "created_by") %>%
  mutate(hours = round(raw_mins / 60, 1),
         pct_of_tracked = round(100 * raw_mins / wall_clock_mins, 1)) %>%
  arrange(created_by, desc(raw_mins))

message("\n── Time by Task Subset ──")
subset_summary %>% select(created_by, task_subset, hours, pct_of_tracked) %>% print(n = Inf)

# ── 7. Daily breakdown ───────────────────────────────────────────────────────

daily <- df %>%
  mutate(date = as_date(start_dt)) %>%
  group_by(created_by, date) %>%
  summarise(raw_sum_mins = sum(duration_mins), n_activities = n(), .groups = "drop") %>%
  arrange(created_by, date)

message("\n── Daily tracked time ──")
daily %>% mutate(hours = round(raw_sum_mins / 60, 1)) %>% print(n = Inf)

# ── 8. Average daily time with range ─────────────────────────────────────────

daily_stats <- daily %>%
  group_by(created_by) %>%
  summarise(
    days_tracked = n(),
    mean_h       = round(mean(raw_sum_mins) / 60, 2),
    median_h     = round(median(raw_sum_mins) / 60, 2),
    min_h        = round(min(raw_sum_mins) / 60, 2),
    max_h        = round(max(raw_sum_mins) / 60, 2),
    sd_h         = round(sd(raw_sum_mins) / 60, 2),
    .groups = "drop"
  )

message("\n── Average daily time with range ──")
print(daily_stats)

# ── 9. Report generation by type ─────────────────────────────────────────────

categorize_report <- function(title) {
  t <- tolower(title)
  case_when(
    str_detect(t, "phadia|autoimmune|ana cap|ttg|ige|special chem") ~
      "Autoimmune / Special Chem QC",
    str_detect(t, "qc summary|qc regional|regional qc|ih regional|update qc|quality report|quality metric|quality report coding|qc data|coding the qc|r coding of quality|report generation - r|coding six sigma|qc/ept|co-pilot|fishbone|worklog|quality planning") ~
      "QC / Quality Reports",
    str_detect(t, "tat|turnaround|meditech graph") ~
      "TAT Reports",
    str_detect(t, "potassium|critical k|critical value") ~
      "Critical Value Analysis",
    str_detect(t, "hepa|hep a|hepatitis") ~
      "HepA Stability",
    str_detect(t, "mass spec|transplant background") ~
      "Mass Spec",
    str_detect(t, "workload tracking|biochem workload") ~
      "Workload Tracking Tool",
    str_detect(t, "r/medicine|cwg meeting") ~
      "Conference / Meetings",
    str_detect(t, "data stuff|coding for stuff|coding stuff|not sure") ~
      "Ad hoc / Exploratory Coding",
    str_detect(t, "ctp|qc investigation tutorial|qc review presentation") ~
      "Teaching / Presentations",
    TRUE ~ "Other"
  )
}

df_rg <- df %>%
  select(created_by, start_dt, duration_mins, title, task_subsets) %>%
  unnest(task_subsets) %>%
  filter(task_subsets == "Data Analysis / Report Generation") %>%
  mutate(
    title        = replace_na(title, "(untitled)"),
    report_type  = categorize_report(title),
    week         = floor_date(as_date(start_dt), "week", week_start = 1)
  )

rg_by_type <- df_rg %>%
  group_by(created_by, report_type) %>%
  summarise(raw_mins = sum(duration_mins), .groups = "drop") %>%
  mutate(hours = round(raw_mins / 60, 1),
         pct   = round(100 * raw_mins / sum(raw_mins), 1)) %>%
  arrange(desc(raw_mins))

message("\n── Report generation by type (total) ──")
rg_by_type %>% select(created_by, report_type, hours, pct) %>% print(n = Inf)

rg_weekly <- df_rg %>%
  group_by(created_by, week, report_type) %>%
  summarise(hours = round(sum(duration_mins) / 60, 1), .groups = "drop")

rg_weekly_wide <- rg_weekly %>%
  pivot_wider(names_from = report_type, values_from = hours, values_fill = 0) %>%
  mutate(TOTAL = rowSums(across(where(is.numeric) & !matches("week")))) %>%
  arrange(created_by, week)

message("\n── Report generation by type per week ──")
print(rg_weekly_wide, width = Inf)

# ── 10. Sign-out averages (days when reported) ────────────────────────────────

signout_subsets <- c(
  "Sign Out - Protein Electrophoresis",
  "Sign Out - HbA1c"
)

signout_daily <- df %>%
  select(created_by, start_dt, duration_mins, task_subsets) %>%
  unnest(task_subsets) %>%
  filter(task_subsets %in% signout_subsets) %>%
  mutate(date = as_date(start_dt)) %>%
  group_by(created_by, task_subsets, date) %>%
  summarise(day_mins = sum(duration_mins), .groups = "drop")

signout_stats <- signout_daily %>%
  group_by(created_by, task_subset = task_subsets) %>%
  summarise(
    days_reported = n(),
    mean_h        = round(mean(day_mins) / 60, 2),
    median_h      = round(median(day_mins) / 60, 2),
    min_h         = round(min(day_mins) / 60, 2),
    max_h         = round(max(day_mins) / 60, 2),
    .groups = "drop"
  ) %>%
  arrange(task_subset, created_by)

message("\n── Sign-out averages (days when reported) ──")
print(signout_stats, width = Inf)

# ── 11. Save ──────────────────────────────────────────────────────────────────

write_csv(task_summary,       "data/task_summary.csv")
write_csv(subset_summary,     "data/task_subset_summary.csv")
write_csv(daily,              "data/daily_summary.csv")
write_csv(daily_stats,        "data/daily_stats.csv")
write_csv(rg_by_type,         "data/report_gen_by_type.csv")
write_csv(rg_weekly_wide,     "data/report_gen_weekly_by_type.csv")
write_csv(signout_stats,      "data/signout_stats.csv")

message("\nDone. CSVs written to data/")
