#Addtional_Experiment_ICWSM_250428

# ---------------------------
# Section 1: Regression b/h with cascade_factors
# ---------------------------

# 左连接，保持 complete_tree 的所有行
complete_tree <- complete_tree %>%
  left_join(cascade_factor, by = c("cascade_id" = "cluster_id"))

complete_dag <- complete_dag %>%
  left_join(cascade_factor, by = c("cascade_id" = "cluster_id"))

complete_tree <- complete_tree %>%
  filter(complete.cases(.))

complete_dag <- complete_dag %>%
  filter(complete.cases(.))


# Create categorical forwarding score variable (0–4 and 5+)
# Keep only selected modalities
complete_tree <- complete_tree %>%
  filter(modality %in% c("video", "image", "chat")) %>%
  mutate(
    modality = factor(modality),  # ensure it's a factor
    content_type = factor(content_type),
    forwarding_score_cat = factor(
      ifelse(forwarding_score == 127, "5plus", as.character(forwarding_score)),
      levels = c("0", "1", "2", "3", "4", "5plus")
    )
  )


# Make modality and content_type factors as well
complete_tree$modality <- as.factor(complete_tree$modality)
complete_tree$content_type <- as.factor(complete_tree$content_type)
# 确保是 factor，然后设置 "hateful" 为 reference level
complete_tree$content_type <- relevel(factor(complete_tree$content_type), ref = "other")

# 重新跑模型
lm_b <- lm(b ~ forwarding_score_cat + modality + content_type+avg_group_activities+avg_author_activities, data = complete_tree)
summary(lm_b)

lm_h <- lm(h ~ forwarding_score_cat + modality + content_type+avg_group_activities+avg_author_activities, data = complete_tree)
summary(lm_h)