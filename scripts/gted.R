


df_gted_tep <- as_tibble(read_excel(paste0(PROJECT_DIR, "data/gted/GTED_FullDatabase_20220307.xlsx"), sheet = 1))
df_gted_rev <- as_tibble(read_excel(paste0(PROJECT_DIR, "data/gted/GTED_FullDatabase_20220307.xlsx"), sheet = 2))
df_gted_ben <- as_tibble(read_excel(paste0(PROJECT_DIR, "data/gted/GTED_FullDatabase_20220307.xlsx"), sheet = 3))
df_gted_cry <- as_tibble(read_excel(paste0(PROJECT_DIR, "data/gted/GTED_FullDatabase_20220307.xlsx"), sheet = 4))
