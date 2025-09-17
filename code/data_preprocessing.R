#Data Preprocessing_250428
library(readr)
library(dplyr)

# ---------------------------
# Section 1: generate cascade_data_final
# ---------------------------
messageContents <- read_csv("D:/user/Research_1_social_learning/code/R_whatsapp_250428/data/messageContents.csv")
messageContents <- messageContents[!is.na(messageContents$body) & messageContents$body != "", ]

messageStats <- read.csv("D:/user/Research_1_social_learning/code/R_whatsapp_250428/data/messageStats.csv")
messageStats <- merge(messageStats, messageContents[, c("message_id", "body")], by = "message_id", all.x = TRUE)
# Filter out rows where modality is "chat" and body is either too short or matches a short string
short_strings <- c("ok", "ok ok", "thanks", "hello")
messageStats <- messageStats[!(messageStats$modality == "chat" & 
                                 (nchar(messageStats$body) <= 10 | 
                                    tolower(trimws(messageStats$body)) %in% short_strings)), ]

# Remove all rows that have empty or NA values in the body column
messageStats <- na.omit(messageStats)

misinfo_hateful_propaganda_IDs <- read.delim("D:/user/Research_1_social_learning/code/R_whatsapp_250428/data/misinfo_hateful_propaganda_IDs.csv", sep = "\t", header = TRUE) 

# 1. 修改 misinfo_hateful_propaganda_IDs 中的列名

misinfo_hateful_propaganda_IDs <- misinfo_hateful_propaganda_IDs %>%
  rename(message_id = messageID)

# Read the text file
normal_ID<- read.table("D:/user/Research_1_social_learning/code/R_whatsapp_250428/data/other.txt", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
# Rename column in normal_ID
colnames(normal_ID) <- "message_id"


# Step 1: mark misinfo/hateful/propaganda messages
messageStats <- messageStats %>%
  left_join(select(misinfo_hateful_propaganda_IDs, message_id, content_type), by = "message_id")

# Step 2: mark normal messages if not already labeled
messageStats <- messageStats %>%
  mutate(content_type = ifelse(is.na(content_type) & message_id %in% normal_ID$message_id, "normal", content_type)) %>%
  mutate(content_type = ifelse(is.na(content_type), "other", content_type))  # fallback for anything else

#messageStats <-  messageStats %>%
#  left_join(select(misinfo_hateful_propaganda_IDs, message_id, content_type), by = "message_id") %>%
#  mutate(content_type = ifelse(is.na(content_type), "other", content_type))  # 处理 NA 值

cluster_chat <- read.csv("D:/user/Research_1_social_learning/code/R_whatsapp_250428/data/messages_cluster.csv")
cluster_image <- read.csv("D:/user/Research_1_social_learning/code/R_whatsapp_250428/data/images_cluster.csv")
cluster_video <- read.csv("D:/user/Research_1_social_learning/code/R_whatsapp_250428/data/videos_cluster.csv")

#有时候会出现相同message_id对应不同cluster_id的情况，暂时无视
# Remove duplicate message_id in cluster_chat
cluster_chat <- cluster_chat %>%
  distinct(message_id, .keep_all = TRUE)

# Remove duplicate message_id in cluster_image
cluster_image <- cluster_image %>%
  distinct(message_id, .keep_all = TRUE)

# Remove duplicate message_id in cluster_video
cluster_video <- cluster_video %>%
  distinct(message_id, .keep_all = TRUE)

# 2. 找到每个数据集独特的 message_id
chat_unique_message_ids <- unique(cluster_chat$message_id)
image_unique_message_ids <- unique(cluster_image$message_id)
video_unique_message_ids <- unique(cluster_video$message_id)

shared_message_ids_chat_image <- intersect(chat_unique_message_ids, image_unique_message_ids)
shared_message_ids_chat_video <- intersect(chat_unique_message_ids, video_unique_message_ids)
shared_message_ids_image_video <- intersect(image_unique_message_ids, video_unique_message_ids)
shared_message_ids<- union(union(shared_message_ids_chat_image, shared_message_ids_chat_video), shared_message_ids_image_video)
cluster_chat <- cluster_chat %>%
  filter(!message_id %in% shared_message_ids)

#3.为每个类别的 cluster_id 加上偏移量
max_chat_id <- max(cluster_chat$cluster_id, na.rm = TRUE)
max_image_id<- max(cluster_image$cluster_id, na.rm = TRUE)
max_video_id <- max(cluster_video$cluster_id, na.rm = TRUE)

cluster_image <- cluster_image %>%
  mutate(cluster_id = cluster_id + max_chat_id)
cluster_video <- cluster_video %>%
  mutate(cluster_id = cluster_id + max_chat_id + max_image_id)

merged_clusters <- bind_rows(cluster_chat, cluster_image, cluster_video)


merged_data <- messageStats %>%
  left_join(merged_clusters, by = "message_id")
merged_data <- na.omit(merged_data)
#4.  构造cascade数据集统一 'group_id' 格式
cascade_data <- merged_data %>%
  mutate(group_id = ifelse(grepl("@g.us", group_id), group_id, paste0(group_id, "@g.us")))


#5. 有时候misinformation和other会同时出现请替换
cascade_data <- cascade_data %>%
  group_by(cluster_id) %>%
  mutate(
    # 查找非 "other" 的类型
    non_other_type = first(content_type[content_type != "other"], default = "other"),
    # 如果存在非 "other" 类型，则将所有 "other" 替换为该类型
    content_type = if_else(content_type == "other", non_other_type, content_type)
  ) %>%
  ungroup() %>%
  select(-non_other_type)  # 移除辅助列

#6. 根据指定列删除重复行
cascade_data_unique <- cascade_data %>%
  distinct(cluster_id, group_id, timestamp, .keep_all = TRUE)

#7. 对于每个 cluster_id 和 group_id 分组，并保留 timestamp 最小的行
cascade_data_final <- cascade_data_unique %>%
  arrange(cluster_id, group_id, timestamp) %>%  # 提前排序
  group_by(cluster_id, group_id) %>%
  slice(1) %>%  # 保留每组中最早的一行
  ungroup()

# 8. 首先，过滤掉出现次数为1的 cluster_id
cascade_data_final <- cascade_data_final %>%
  group_by(cluster_id) %>%
  filter(n() > 1) %>%  # 只保留 cluster_id 出现多次的行
  ungroup()

write.csv(cascade_data_final, "D:/user/Research_1_social_learning/code/R_whatsapp_250428/data/cascade_data_final_demogra.csv", row.names = FALSE)



# ---------------------------
# Section 2: generate cascade_factor
# ---------------------------

# Create the new dataframe
group_activity <- messageStats %>%
  group_by(group_id) %>%
  summarise(group_activities = n()) %>%
  arrange(desc(group_activities))

author_activity <- messageStats %>%
  group_by(author) %>%
  summarise(author_activities = n()) %>%
  arrange(desc(author_activities))

cascade_data_final <- cascade_data_final %>%
  left_join(author_activity, by = "author")

cascade_data_final <- cascade_data_final %>%
  left_join(group_activity, by = "group_id")

cascade_factor <- cascade_data_final %>%
  group_by(cluster_id) %>%
  filter(
    n_distinct(modality) == 1,
    n_distinct(content_type) == 1
  ) %>%
  summarise(
    modality = first(modality),
    content_type = first(content_type),
    forwarding_score = max(forwarding_score, na.rm = TRUE),
    avg_group_activities = mean(group_activities, na.rm = TRUE),
    avg_author_activities = mean(author_activities, na.rm = TRUE),
    .groups = "drop"
  )

cascade_factor <- cascade_factor %>%
  mutate(
    avg_group_activities = (avg_group_activities - min(avg_group_activities, na.rm = TRUE)) / 
      (max(avg_group_activities, na.rm = TRUE) - min(avg_group_activities, na.rm = TRUE)),
    avg_author_activities = (avg_author_activities - min(avg_author_activities, na.rm = TRUE)) / 
      (max(avg_author_activities, na.rm = TRUE) - min(avg_author_activities, na.rm = TRUE))
  )



