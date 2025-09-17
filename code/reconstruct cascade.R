#www author: Yuxin Date240921
#1. reconstruct incomplete information diffusion; 2. construct complete information diffusion; calculate the properties of complete tree. 


#1. reconstruct group network
# Read the file line by line
# Read the original dataset
library(dplyr)
library(igraph)
library(ggplot2)
library(lubridate)
library(nloptr)
library(data.table)
library(NetworkInference)
cascade_data <- read.csv("D:/user/Research_1_social_learning/code/R_whatsapp_250428/data/cascade_data_final.csv")# Build a new dataset

new_cascade_data <- data.frame(
  node_name = cascade_data$group_id,  # Use group_id_numeric as node_name
  event_time = cascade_data$timestamp,       # Keep the event_time column unchanged
  cascade_id = cascade_data$cluster_id)        # Keep the cascade_id column unchanged

## From long format
cascades_whatsapp <- as_cascade_long(new_cascade_data)
result_whatsapp  <- netinf(cascades_whatsapp, quiet = TRUE, p_value_cutoff = 0.05,trees = TRUE)

partial_trees <- result_whatsapp[["trees"]]
# 将数据转换为data.table
partial_trees_dt <- as.data.table(partial_trees)

#2. get b and h for complete tree

# # # 定义一个函数，用于计算采样后的树 g 中三节点路径的数量
# # 定义一个函数，用于计算采样后的树 g 中三节点路径的数量
# calculate_three_node_paths <- function(g) {
#   # 初始化路径计数
#   three_node_paths <- 0
#   
#   # 遍历每个节点
#   for (v in V(g)) {
#     # 获取当前节点的出度（子节点数）
#     out_degree <- degree(g, v, mode = "out")
#     
#     # 如果当前节点是非叶节点（出度大于 0）
#     if (out_degree > 0) {
#       # 获取该节点的子节点
#       children <- neighbors(g, v, mode = "out")
#       
#       # 对于每个子节点，检查它是否也有子节点
#       for (child in children) {
#         child_out_degree <- degree(g, child, mode = "out")
#         
#         # 如果子节点也有子节点，说明构成一条三节点路径
#         if (child_out_degree > 0) {
#           # 获取子节点的子节点
#           grand_children <- neighbors(g, child, mode = "out")
#           
#           # 每找到一个孙节点，即构成一条三节点路径
#           three_node_paths <- three_node_paths + length(grand_children)
#         }
#       }
#     }
#   }
#   
#   # 返回三节点路径的数量
#   return(three_node_paths)
# }
# 
# # 定义误差函数，加入三节点路径
# error_function <- function(params, sampled_node_count, sampled_outdegree_avg, sampled_path_count, p) {
#   b <- params[1]
#   h <- params[2]
#
#   # 计算理论的非叶节点出度和理论节点数
#   if (b == 1) {
#     theoretical_nodes <- p * (1 + h)
#     theoretical_outdegree <- 1
#     theoretical_path_count <- 0  # 在 b = 1 的情况下，路径数为 0
#   } else {
#     theoretical_nodes <- p * (b^(h + 1) - 1) / (b - 1)
#     theoretical_outdegree <- (p * b) / (1 - (1 - p)^b)
#
#     # 计算理论三节点路径数量
#     theoretical_path_count <- p^3 * (b^(h + 1) - b^2) / (b - 1)  # 基于路径三节点公式的假设
#   }
#
#   # 计算误差
#   error_nodes <- abs(theoretical_nodes - sampled_node_count)
#   error_outdegree <- abs(theoretical_outdegree - sampled_outdegree_avg)
#   error_path <- abs(theoretical_path_count - sampled_path_count)
#
#   # 返回总误差
#   total_error <- error_nodes + error_outdegree + error_path
#   return(total_error)
# }
#
# # 遍历每个 cascade_id
# for (i in 1:length(unique(partial_trees$cascade_id))) {
#
#   cascade <- unique(partial_trees$cascade_id)[i]
#
#   # 提取当前 cascade_id 对应的子树
#   cascade_tree <- partial_trees[partial_trees$cascade_id == cascade, ]
#
#   # 创建 graph 对象
#   g <- graph_from_data_frame(cascade_tree, directed = TRUE)
#
#   # 计算子树中的节点数
#   sampled_node_count <- vcount(g)
#
#   # 计算非叶节点的平均出度
#   non_leaf_nodes <- V(g)[degree(g, mode = "out") > 0]
#   if (length(non_leaf_nodes) > 0) {
#     sampled_outdegree_avg <- mean(degree(g, v = non_leaf_nodes, mode = "out"))
#   } else {
#     sampled_outdegree_avg <- 0
#   }
#
#   # 计算采样树中的三节点路径数量
#   sampled_path_count <- calculate_three_node_paths(g)
#
#   # 使用 L-BFGS-B 优化
#   optim_result <- optim(par = c(1.5, 10),  # 初始值
#                         fn = error_function,  # 误差函数
#                         lower = c(1.1, 1),  # b 和 h 的下界
#                         upper = c(10, 50),  # b 和 h 的上界
#                         method = "L-BFGS-B",  # 使用 L-BFGS-B 算法
#                         sampled_node_count = sampled_node_count,
#                         sampled_outdegree_avg = sampled_outdegree_avg,
#                         sampled_path_count = sampled_path_count,
#                         p = p)
#
#   # 存储结果到 dataframe
#   complete_tree$b[i] <- optim_result$par[1]
#   complete_tree$h[i] <- optim_result$par[2]
# }
#
# # 查看优化结果
# head(complete_tree)






# 定义误差函数
error_function <- function(params, sampled_node_count, sampled_outdegree_avg, p) {
  b <- params[1]
  h <- params[2]

  # 计算理论的非叶节点出度和理论节点数
  if (b == 1) {
    theoretical_nodes <- p * (1 + h)
    theoretical_outdegree <- 1
  } else {
    theoretical_nodes <- p * (b^(h + 1) - 1) / (b - 1)
    theoretical_outdegree <- (p * b) / (1 - (1 - p)^b)
  }

  # 计算误差
  error_nodes <- abs(theoretical_nodes - sampled_node_count)
  error_outdegree <- abs(theoretical_outdegree - sampled_outdegree_avg)

  # 返回总误差
  total_error <- error_nodes + error_outdegree
  return(total_error)
}

# 遍历每个cascade_id
for (i in 1:length(unique(partial_trees$cascade_id))) {

  cascade <- unique(partial_trees$cascade_id)[i]

  # 提取当前cascade_id对应的子树
  cascade_tree <- partial_trees[partial_trees$cascade_id == cascade, ]

  # 创建graph对象
  g <- graph_from_data_frame(cascade_tree, directed = TRUE)

  # 计算子树中的节点数
  sampled_node_count <- vcount(g)

  # 计算出非叶节点和其出度
  non_leaf_nodes <- V(g)[degree(g, mode = "out") > 0]
  if (length(non_leaf_nodes) > 0) {
    sampled_outdegree_avg <- mean(degree(g, v = non_leaf_nodes, mode = "out"))
  } else {
    sampled_outdegree_avg <- 0
  }

  # 使用 L-BFGS-B 优化
  optim_result <- optim(par = c(1.5, 5),  # 初始值
                        fn = error_function,  # 误差函数
                        lower = c(1.1, 1),  # b 和 h 的下界
                        upper = c(10, 50),  # b 和 h 的上界
                        method = "L-BFGS-B",  # 使用 L-BFGS-B 算法
                        sampled_node_count = sampled_node_count,
                        sampled_outdegree_avg = sampled_outdegree_avg,
                        p = p)

  # 存储结果到dataframe
  complete_tree$b[i] <- optim_result$par[1]
  complete_tree$h[i] <- optim_result$par[2]
}






###仍然使用最优化搜索，但是在搜索之前先将指标相同的数据提取出来
complete_tree <- data.frame(cascade_id = unique(partial_trees$cascade_id),
                            node_count = numeric(length(unique(partial_trees$cascade_id))),
                            outdegree_avg = numeric(length(unique(partial_trees$cascade_id))))

# 遍历每个 cascade_id
for (i in 1:nrow(complete_tree)) {
  
  cascade <- complete_tree$cascade_id[i]
  
  # 提取当前 cascade_id 对应的子树
  cascade_tree <- partial_trees[partial_trees$cascade_id == cascade, ]
  
  # 创建 graph 对象
  g <- graph_from_data_frame(cascade_tree, directed = TRUE)
  
  # 计算子树中的节点数
  complete_tree$node_count[i] <- vcount(g)
  
  # 计算非叶节点的平均出度
  non_leaf_nodes <- V(g)[degree(g, mode = "out") > 0]
  if (length(non_leaf_nodes) > 0) {
    complete_tree$outdegree_avg[i] <- mean(degree(g, v = non_leaf_nodes, mode = "out"))
  } else {
    complete_tree$outdegree_avg[i] <- 0
  }
}

write.csv(complete_tree, "complete_tree.csv", row.names = FALSE)
complete_tree=read.csv("D:/user/Research_1_social_learning/code/R_whatsapp_250428/data/complete_tree.csv")



# 假设 complete_tree 和 cascade_data 是你的两个数据框
# # 从 cascade_data 中为每个 cluster_id 获取最小的 timestamp
# min_timestamp_data <- cascade_data %>%
#   group_by(cluster_id) %>%
#   summarize(min_timestamp = min(timestamp, na.rm = TRUE), .groups = "drop")
# 
# # 将最小的 timestamp 合并到 complete_tree
# complete_tree <- complete_tree %>%
#   left_join(min_timestamp_data, by = c("cascade_id" = "cluster_id"))
# 
# 
# # 将 timestamp 转换为日期类型（如果尚未转换）
# complete_tree <- complete_tree %>%
#   filter(min_timestamp <= 1704067199)


# 提取唯一的 node_count 和 outdegree_avg 组合
unique_combinations <- unique(complete_tree[, c("node_count", "outdegree_avg")])

# 创建一个数据框来存储每个唯一组合的最优 b 和 h
optimization_results <- data.frame(node_count = unique_combinations$node_count,
                                   outdegree_avg = unique_combinations$outdegree_avg,
                                   b = numeric(nrow(unique_combinations)),
                                   h = numeric(nrow(unique_combinations)))

###如果使用网格则从此处注释 假设有一个误差函数 error_function 用于优化
p=0.05
# 遍历每个唯一组合进行优化
for (i in 1:nrow(unique_combinations)) {
  
  node_count <- unique_combinations$node_count[i]
  outdegree_avg <- unique_combinations$outdegree_avg[i]
  
  # 使用 L-BFGS-B 优化
  optim_result <- optim(par = c(1.5, 4),  # 初始值
                        fn = error_function,  # 误差函数
                        lower = c(1.1, 1),  # b 和 h 的下界
                        upper = c(10, 50),  # b 和 h 的上界
                        method = "L-BFGS-B",  # 使用 L-BFGS-B 算法
                        sampled_node_count = node_count, 
                        sampled_outdegree_avg = outdegree_avg, 
                        p = p)
  
  # 存储优化结果
  optimization_results$b[i] <- optim_result$par[1]
  optimization_results$h[i] <- optim_result$par[2]
}

# 将优化结果映射回 complete_tree
complete_tree <- merge(complete_tree, optimization_results, by = c("node_count", "outdegree_avg"))
###如果使用网格则将上面注释


# ###现在我们来使用网格搜索来获得最优解,结果不符合实际
# # 定义 b 和 h 的搜索范围
# b_range <- seq(1.1, 10, by = 0.1)
# h_range <- seq(1, 50, by = 0.1)
# 
# # 假设有一个误差函数 error_function 用于计算误差
# # 遍历每个唯一组合，使用 Grid Search 来寻找最优的 b 和 h
# for (i in 1:nrow(unique_combinations)) {
#   
#   node_count <- unique_combinations$node_count[i]
#   outdegree_avg <- unique_combinations$outdegree_avg[i]
#   
#   # 初始化最小误差和最优的 b 和 h
#   min_error <- Inf
#   best_b <- NA
#   best_h <- NA
#   
#   # 遍历所有的 b 和 h 候选值
#   for (b_candidate in b_range) {
#     for (h_candidate in h_range) {
#       # 计算当前 b 和 h 的误差
#       error_value <- error_function(c(b_candidate, h_candidate), 
#                                     sampled_node_count = node_count, 
#                                     sampled_outdegree_avg = outdegree_avg, 
#                                     p = p)
#       
#       # 如果误差比当前最小误差小，更新最优 b 和 h
#       if (error_value < min_error) {
#         min_error <- error_value
#         best_b <- b_candidate
#         best_h <- h_candidate
#       }
#     }
#   }
#   
#   # 存储最优的 b 和 h
#   optimization_results$b[i] <- best_b
#   optimization_results$h[i] <- best_h
# }
# 
# # 将优化结果映射回 complete_tree
# complete_tree <- merge(complete_tree, optimization_results, by = c("node_count", "outdegree_avg"))






# # 定义b和h的搜索范围
# b_range <- seq(1.1, 10, by = 0.1)
# h_range <- seq(1, 50, by = 1)
# p <- 0.02  # 采样率
# 
# # 定义一个数据框用于存储最终结果
# complete_tree <- data.frame(cascade_id = unique(partial_trees$cascade_id),
#                             b = numeric(length(unique(partial_trees$cascade_id))),
#                             h = numeric(length(unique(partial_trees$cascade_id))))
# 
# # 遍历每个cascade_id
# for (i in 1:length(unique(partial_trees$cascade_id))) {
#   
#   cascade <- unique(partial_trees$cascade_id)[i]
#   
#   # 提取当前cascade_id对应的子树
#   cascade_tree <- partial_trees[partial_trees$cascade_id == cascade, ]
#   
#   # 创建graph对象
#   g <- graph_from_data_frame(cascade_tree, directed = TRUE)
#   
#   # 计算子树中的节点数
#   sampled_node_count <- vcount(g)
#   #sampled_edge_count <- ecount(g)
#   #sampled_path2 <- calculate_three_node_paths(g)
#   # 计算出非叶节点和其出度
#   non_leaf_nodes <- V(g)[degree(g, mode = "out") > 0]
#   if (length(non_leaf_nodes) > 0) {
#     sampled_outdegree_avg <- mean(degree(g, v = non_leaf_nodes, mode = "out"))
#   } else {
#     sampled_outdegree_avg <- 0
#   }
#   
#   # 初始化最小误差和最优的b和h
#   min_error <- Inf
#   best_b <- NA
#   best_h <- NA
#   
#   # 网格搜索b和h
#   for (b_candidate in b_range) {
#     for (h_candidate in h_range) {
#       # 计算理论的非叶节点出度和理论节点数
#       theoretical_nodes <- p * (b_candidate^(h_candidate + 1) - 1) / (b_candidate - 1)
#       theoretical_outdegree <- (p * b_candidate) / (1 - (1 - p)^b_candidate)
#       #theoretical_edges <- p^2 * (b_candidate^(h_candidate + 1) - b_candidate) / (b_candidate - 1)
#       #theoretical_path2 <- p^3 * (b_candidate^(h_candidate + 1) - b_candidate^2) / (b_candidate - 1)
#       # 计算误差
#       error_nodes <- abs(theoretical_nodes - sampled_node_count)
#       error_outdegree <- abs(theoretical_outdegree - sampled_outdegree_avg)
#       #error_edges <- abs(theoretical_edges - sampled_edge_count)
#       #error_path2 <- abs(theoretical_path2 - sampled_path2)
#       total_error <- error_nodes + error_outdegree#+error_edges+error_path2
#       
#       # 更新最优的b和h
#       if (total_error < min_error) {
#         min_error <- total_error
#         best_b <- b_candidate
#         best_h <- h_candidate
#       }
#     }
#   }
#   
#   # 考虑 b = 1 的特殊情况
#   for (h_candidate in h_range) {
#     # 计算 b = 1 时的理论节点数和非叶节点出度
#     theoretical_nodes_b1 <- p * (1 + h_candidate)
#     theoretical_outdegree_b1 <- 1  
#     #theoretical_edges_b1 <- p^2*h_candidate
#     #theoretical_path2_b1 <- p^3*(h_candidate-1)
#     # 计算误差
#     error_nodes_b1 <- abs(theoretical_nodes_b1 - sampled_node_count)
#     error_outdegree_b1 <- abs(theoretical_outdegree_b1 - sampled_outdegree_avg)
#     #error_edges_b1 <- abs(theoretical_edges_b1 - sampled_edge_count)
#     #error_path2_b1 <- abs(theoretical_path2_b1 - sampled_path2)
#     total_error_b1 <- error_nodes_b1 + error_outdegree_b1#+error_edges_b1+error_path2_b1
#     
#     # 如果 b = 1 的误差更小，更新最优的 b 和 h
#     if (total_error_b1 < min_error) {
#       min_error <- total_error_b1
#       best_b <- 1
#       best_h <- h_candidate
#     }
#   }
#   
#   # 存储结果到dataframe
#   complete_tree$b[i] <- best_b
#   complete_tree$h[i] <- best_h
# }

# 确保 complete_tree 中的 cascade_id 是整数类型
complete_tree$cascade_id <- as.integer(complete_tree$cascade_id)

# 将 modality 数据合并到 complete_tree 中
# complete_tree <- complete_tree %>%
#   left_join(new_cascade_data %>% select(cascade_id, modality, content_type), by = "cascade_id")

# 如果存在一对多，先对 cascade_data 聚合，保留每个 cluster_id 的唯一值
# 对 cascade_data 取每个 cluster_id 的第一行
processed_cascade_data <- cascade_data %>%
  group_by(cluster_id) %>%
  slice(1) %>%
  ungroup()

# 按照 cascade_id 和 cluster_id 进行合并
complete_tree <- complete_tree %>%
  left_join(cascade_data_final, by = c("cascade_id" = "cluster_id"))



 




h_frequency_modality <- complete_tree %>%
  group_by(modality, h) %>%
  summarise(frequency = n()) %>%
  mutate(relative_frequency = frequency / sum(frequency))  # 计算相对频率

# 2. 对 b 进行分组计算相对频率
b_frequency_modality <- complete_tree %>%
  group_by(modality, b) %>%
  summarise(frequency = n()) %>%
  mutate(relative_frequency = frequency / sum(frequency))  # 计算相对频率

# 3. 将 h 和 b 的值格式化为保留小数点后两位
h_frequency_modality$h <- round(h_frequency_modality$h, 2)
b_frequency_modality$b <- round(b_frequency_modality$b, 2)

# 4. 绘制 h 的相对频率分布
ggplot(h_frequency_modality, aes(x = factor(h), y = relative_frequency, fill = modality)) +  
  geom_bar(stat = "identity", position = "dodge", color = "black") +  # 使用 dodge 分开不同 modality 的柱状图
  labs(title = "Relative Frequency Distribution of Heights by Modality",
       x = "Height (h) ",
       y = "Relative Frequency",
       fill = "Modality") +
  theme_minimal() +
  theme(legend.position = "top")

# 5. 绘制 b 的相对频率分布
ggplot(b_frequency_modality, aes(x = factor(b), y = relative_frequency, fill = modality)) +  
  geom_bar(stat = "identity", position = "dodge", color = "black") +  # 使用 dodge 分开不同 modality 的柱状图
  labs(title = "Relative Frequency Distribution of Widths by Modality",
       x = "Width (b) ",
       y = "Relative Frequency",
       fill = "Modality") +
  theme_minimal() +
  theme(legend.position = "top")

# 1. 对 h 进行分组计算相对频率，基于 content_type
h_frequency_content_type <- complete_tree %>%
  group_by(content_type, h) %>%
  summarise(frequency = n()) %>%
  mutate(relative_frequency = frequency / sum(frequency))  # 计算相对频率

# 2. 对 b 进行分组计算相对频率，基于 content_type
b_frequency_content_type <- complete_tree %>%
  group_by(content_type, b) %>%
  summarise(frequency = n()) %>%
  mutate(relative_frequency = frequency / sum(frequency))  # 计算相对频率

# 3. 将 h 和 b 的值格式化为保留小数点后两位
h_frequency_content_type$h <- round(h_frequency_content_type$h, 2)
b_frequency_content_type$b <- round(b_frequency_content_type$b, 2)

h_frequency_filtered <- h_frequency_content_type %>%
  filter(!(content_type == "other" & relative_frequency <= 0.01))

b_frequency_filtered <- b_frequency_content_type %>%
  filter(!(content_type == "other" & relative_frequency <= 0.01))

# 5. 绘制 h 的相对频率分布，基于所有 content_type，筛选 "other"
ggplot(h_frequency_filtered, aes(x = factor(h), y = relative_frequency, fill = content_type)) +  
  geom_bar(stat = "identity", position = "dodge", color = "black") +  # 使用 dodge 分开不同 content_type 的柱状图
  labs(title = "Relative Frequency Distribution of Heights by Content Type",
       x = "Height (h) ",
       y = "Relative Frequency",
       fill = "Content Type") +
  theme_minimal() +
  theme(legend.position = "top")

# 6. 绘制 b 的相对频率分布，基于所有 content_type，筛选 "other"
ggplot(b_frequency_filtered, aes(x = factor(b), y = relative_frequency, fill = content_type)) +  
  geom_bar(stat = "identity", position = "dodge", color = "black") +  # 使用 dodge 分开不同 content_type 的柱状图
  labs(title = "Relative Frequency Distribution of Widths by Content Type",
       x = "Width (b) ",
       y = "Relative Frequency",
       fill = "Content Type") +
  theme_minimal() +
  theme(legend.position = "top")

# 按 modality 分组，并计算 b 和 h 的均值和标准差
summary_stats <- complete_tree %>%
  group_by(content_type) %>%
  summarise(
    mean_b = mean(b),
    std_b = sd(b),
    mean_h = mean(h),
    std_h = sd(h)
  )
print(summary_stats)



# #对于不同的级联进行分类，画图比较参数b和h的区别
# cascade_id_modality <- read.csv("D:/user/Research_4_whatsapp/code/data/sorted_merged_data_uniform.csv")# Build a new dataset
# cascade_id_modality <- cascade_id_modality[, c("cluster_id", "modality")]
# 
# # 处理 cluster_id 具有多个不同 modality 的情况
# cascade_id_modality_cleaned <- cascade_id_modality %>%
#   # 去重
#   distinct() %>%
#   # 提取所需的列 cluster_id 和 modality
#   select(cluster_id, modality) %>%
#   # 按 cluster_id 分组
#   group_by(cluster_id) %>%
#   # 处理冲突情况：根据规则保留优先级更高的 modality
#   summarize(modality = case_when(
#     "video" %in% modality ~ "video",
#     "image" %in% modality ~ "image",
#     TRUE ~ "chat"
#   )) %>%
#   ungroup()
# 
# 
# 
# # 将 complete_tree 中的 cascade_id 转换为整数类型
# complete_tree$cascade_id <- as.integer(complete_tree$cascade_id)
# 
# # 然后进行合并
# complete_tree <- complete_tree %>%
#   left_join(cascade_id_modality_cleaned, by = c("cascade_id" = "cluster_id"))
# 
# # 绘制不同 modality 的标准化树高频率分布
# ggplot(complete_tree, aes(x = h, y = ..density.., fill = modality)) +  # 使用 density 代替频率
#   geom_histogram(binwidth = 1, position = "dodge", color = "black") +  # binwidth 为 1
#   labs(title = "Normalized Frequency Distribution of Tree Heights by Modality",
#        x = "Height",
#        y = "Relative Frequency (Density)",
#        fill = "Modality") +
#   theme_minimal()
# 
# ggplot(complete_tree, aes(x = b, y = ..density.., fill = modality)) +  # 使用 density 代替频率
#   geom_histogram(binwidth = 1, position = "dodge", color = "black") +  # binwidth 为 1
#   labs(title = "Normalized Frequency Distribution of Tree Width by Modality",
#        x = "Width",
#        y = "Relative Frequency (Density)",
#        fill = "Modality") +
#   theme_minimal()
# 
# # 按modality分组，并计算b和h的均值和标准差
# summary_stats <- complete_tree %>%
#   group_by(modality) %>%
#   summarise(
#     mean_b = mean(b),
#     std_b = sd(b),
#     mean_h = mean(h),
#     std_h = sd(h)
#   )
# 
# # 打印结果
# print(summary_stats)


## 2.Network cascade：构造network是否存在相同的message

#为了计算速度选取百分之10的cascade,但保留所有的除了other以外的content_type

other_data <- cascade_data %>%
  filter(content_type == "other")

non_other_data <- cascade_data %>%
  filter(content_type != "other")

# 获取 'other' 部分的唯一 cluster_id
unique_clusters_other <- unique(other_data$cluster_id)

# 从 'other' 部分中随机抽取 10% 的 cluster_id
sampled_clusters_other <- sample(unique_clusters_other, size = ceiling(length(unique_clusters_other) * 0.1))

# 对 'other' 数据中抽取到的 cluster_id 保留其所有行
sampled_other_data <- other_data %>%
  filter(cluster_id %in% sampled_clusters_other)

# 最终将抽样后的 'other' 数据与其他类型的全部数据合并
new_cascade_data <- bind_rows(sampled_other_data, non_other_data)

new_cascade_data <- new_cascade_data %>%
  rename(
    node_name = group_id,
    event_time = timestamp,
    cascade_id = cluster_id
  )

# 将 forwarding_score = 127 的值替换为 5
new_cascade_data$forwarding_score[new_cascade_data$forwarding_score == 127] <- 5


# 定义一个函数，根据 cascade_id 构建DAG
build_dag_for_cascade <- function(cascade_data) {
  # 按 event_time 对节点排序
  sorted_nodes <- cascade_data %>%
    arrange(event_time)
  
  # 获取节点列表
  nodes <- sorted_nodes$node_name
  
  # 初始化一个空边列表
  edges <- c()
  
  # 对于每个节点，将之前的所有节点连接到当前节点
  for (i in 2:length(nodes)) {
    # 添加边，从之前的节点连接到当前节点
    for (j in 1:(i-1)) {
      edges <- c(edges, nodes[j], nodes[i])  # 每次添加一对节点
    }
  }
  
  # 创建图，将边列表转换为矩阵形式 (两列)
  g <- graph_from_edgelist(matrix(edges, ncol = 2, byrow = TRUE), directed = TRUE)
  return(g)
}




# 获取 unique 的 cascade_id 列表
unique_cascade_ids <- unique(new_cascade_data$cascade_id)

# 遍历每个 cascade_id，生成对应的 DAG
dags_with_id <- lapply(unique_cascade_ids, function(id) {
  # 筛选出对应的 cascade_id 的数据
  specific_cascade_data <- new_cascade_data %>%
    filter(cascade_id == id)
  
  # 构建 DAG
  dag <- build_dag_for_cascade(specific_cascade_data)
  
  # 返回一个包含 cascade_id 和 dag 的列表
  list(cascade_id = id, dag = dag)
})


dags<-dags_with_id  


# #考虑shared_participants, 构建dags based on the group-group network
# # 1. 读取 group-group network 的邻接矩阵
# g_group_adj_matrix <- as.matrix(adj_matrix <- read.csv("D:/user/Research_4_whatsapp/code/result/g_group_adj_matrix.csv", row.names = 1, check.names = FALSE))
# 
# # 2. 确保列名也与行名一致（如果它们代表相同的节点）
# colnames(g_group_adj_matrix) <- rownames(g_group_adj_matrix)
# 
# # 3. 定义一个函数，根据 cascade_data 和邻接矩阵构建 DAG
# build_dag_for_cascade_with_network <- function(cascade_data, adj_matrix) {
#   # 按 event_time 对节点排序
#   sorted_nodes <- specific_cascade_data %>%
#     arrange(event_time)
#   
#   # 获取节点列表
#   nodes <- sorted_nodes$node_name
#   
#   # 初始化一个空边列表
#   edges <- c()
#   
#   # 对于每个节点，将之前的所有节点连接到当前节点
#   for (i in 2:length(nodes)) {
#     for (j in 1:(i-1)) {
#       # 获取节点 i 和 j 的名称
#       node_i <- nodes[i]
#       node_j <- nodes[j]
#       
#       # 确保 node_i 和 node_j 是邻接矩阵中的合法行/列名
#       if (node_i %in% rownames(adj_matrix) && node_j %in% colnames(adj_matrix)) {
#         # 检查 adj_matrix 中是否存在连接
#         if (adj_matrix[node_j, node_i] == 1 || adj_matrix[node_i, node_j] == 1) {
#           # 添加边，从之前的节点连接到当前节点
#           edges <- c(edges, node_j, node_i)  # 每次添加一对节点
#         }
#       }
#     }
#   }
#   
#   # 如果有足够的边，构建图，否则返回 NULL
#   if (length(edges) > 0) {
#     # 创建图，将边列表转换为矩阵形式 (两列)
#     g <- graph(matrix(edges, ncol = 2, byrow = TRUE), directed = TRUE)
#     return(g)
#   } else {
#     return(NULL)
#   }
# }
# 
# 
# 
# # 3. 获取 unique 的 cascade_id 列表
# unique_cascade_ids <- unique(new_cascade_data$cascade_id)
# 
# # 4. 遍历每个 cascade_id，生成新的 DAG 并确保节点连接符合 group-group network
# dags_under_network <- lapply(unique_cascade_ids, function(id) {
#   # 筛选出对应的 cascade_id 的数据
#   specific_cascade_data <- new_cascade_data %>%
#     filter(cascade_id == id)
#   
#   # 构建基于 group-group network 的 DAG
#   dag <- build_dag_for_cascade_with_network(specific_cascade_data, g_group_adj_matrix)
#   
#   # 返回一个包含 cascade_id 和 dag 的列表
#   list(cascade_id = id, dag = dag)
# })






# 计算非叶节点出度的理论表达式
theoretical_outdegree <- function(b, p, h, k) {
  outdegree_sum <- 0
  
  for (i in 0:h) {
    c_val <- min(h - i, k)  # 计算 c = min(h - i, k)
    
    # 累加公式中的每一项
    term <- (b^i * p * (1 - p)) * ((b^(c_val + 1) - b) / (b - 1))
    outdegree_sum <- outdegree_sum + term
  }
  
  return(outdegree_sum)
}



# 定义目标函数，接受参数 (b, h, k)
objective_function_dag <- function(params, p, sampled_node_count, sampled_outdegree_avg, sampled_edge_count) {
  b_candidate <- params[1]
  h_candidate <- params[2]
  k_candidate <- params[3]
  
  # 计算理论的非叶节点出度、节点数和边数
  theoretical_nodes <- p * (b_candidate^(h_candidate + 1) - 1) / (b_candidate - 1)
  theoretical_outdegree_val <- theoretical_outdegree(b_candidate, p, h_candidate, k_candidate)
  theoretical_edges <- (p^2 / (b_candidate - 1)) * 
    ((b_candidate * (1 - b_candidate^k_candidate)) / (b_candidate - 1) + 
       k_candidate * b_candidate^(h_candidate + 1))
  
  # 计算误差
  error_nodes <- abs(theoretical_nodes - sampled_node_count)
  error_outdegree <- abs(theoretical_outdegree_val - sampled_outdegree_avg)
  error_edges <- abs(theoretical_edges - sampled_edge_count)
  
  # 返回总误差
  return(error_nodes + error_outdegree + error_edges)
}

p=0.02

unique_cascade_ids <- unique(new_cascade_data$cascade_id)
complete_dag <- data.frame(
  cascade_id = unique_cascade_ids,
  node_count = numeric(length(unique_cascade_ids)),
  outdegree_avg = numeric(length(unique_cascade_ids)),
  edge_count = numeric(length(unique_cascade_ids))
)

# 遍历每个 DAG 计算节点数、出度和边数
for (i in 1:length(unique_cascade_ids)) {
  
  # 从 dags 中获取 DAG 图
  g <- dags[[i]]$dag  # 从创建好的 DAG 列表中提取
  
  # 计算子树中的节点数
  complete_dag$node_count[i] <- vcount(g)
  
  # 计算非叶节点的平均出度
  non_leaf_nodes <- V(g)[degree(g, mode = "out") > 0]
  if (length(non_leaf_nodes) > 0) {
    complete_dag$outdegree_avg[i] <- mean(degree(g, v = non_leaf_nodes, mode = "out"))
  } else {
    complete_dag$outdegree_avg[i] <- 0
  }
  
  # 计算边数
  complete_dag$edge_count[i] <- ecount(g)
  
}
write.csv(complete_dag, "complete_dag.csv", row.names = FALSE)








# # 初始化 complete_dag 数据框，用于存储每个 DAG 的 b, h, k
# complete_dag <- data.frame(
#   cascade_id = unique_cascade_ids,  # 使用唯一的 cascade_id 作为标识
#   b = numeric(length(unique_cascade_ids)),
#   h = numeric(length(unique_cascade_ids)),
#   k = numeric(length(unique_cascade_ids))
# )
# 
# 
# # 优化每个 DAG
# for (i in 1:length(unique_cascade_ids)) {
#   
#   # 从 dags 中获取已创建的 DAG 图
#   g <- dags[[i]]$dag  # 从创建好的 DAG 列表中提取
#   
#   # 计算子树中的节点数
#   sampled_node_count <- vcount(g)
#   
#   # 计算出非叶节点的出度
#   non_leaf_nodes <- V(g)[degree(g, mode = "out") > 0]
#   if (length(non_leaf_nodes) > 0) {
#     sampled_outdegree_avg <- mean(degree(g, v = non_leaf_nodes, mode = "out"))
#   } else {
#     sampled_outdegree_avg <- 0
#   }
#   
#   # 计算边数
#   sampled_edge_count <- ecount(g)
#   
#   # 使用 optim() 进行优化，初始值为 (b, h, k)
#   initial_values <- c(1.1, 1, 1)  # 可以根据实际情况调整
#   bounds_lower <- c(1.1, 1, 1)    # 下界
#   bounds_upper <- c(5, 50, 3)   # 上界
#   
#   result <- optim(
#     par = initial_values, 
#     fn = objective_function_dag, 
#     method = "L-BFGS-B",  # 限制范围的优化算法
#     lower = bounds_lower, 
#     upper = bounds_upper,
#     p = p, 
#     sampled_node_count = sampled_node_count, 
#     sampled_outdegree_avg = sampled_outdegree_avg, 
#     sampled_edge_count = sampled_edge_count
#   )
#   
#   # 返回最优的 b, h, k
#   best_b <- result$par[1]
#   best_h <- result$par[2]
#   best_k <- result$par[3]
#   
#   # 存储结果到 complete_dag 数据框
#   complete_dag$b[i] <- best_b
#   complete_dag$h[i] <- best_h
#   complete_dag$k[i] <- best_k
# }
# 
# 
# 
# 


###先根据相同的指标进行分类，在优化
# 计算每个 DAG 的特征值（节点数、出度、边数）

complete_dag <- data.frame(
  cascade_id = unique_cascade_ids,
  node_count = numeric(length(unique_cascade_ids)),
  outdegree_avg = numeric(length(unique_cascade_ids)),
  edge_count = numeric(length(unique_cascade_ids))
)

# 遍历每个 DAG 计算节点数、出度和边数
for (i in 1:length(unique_cascade_ids)) {
  
  # 从 dags 中获取 DAG 图
  g <- dags[[i]]$dag  # 从创建好的 DAG 列表中提取
  
  # 计算子树中的节点数
  complete_dag$node_count[i] <- vcount(g)
  
  # 计算非叶节点的平均出度
  non_leaf_nodes <- V(g)[degree(g, mode = "out") > 0]
  if (length(non_leaf_nodes) > 0) {
    complete_dag$outdegree_avg[i] <- mean(degree(g, v = non_leaf_nodes, mode = "out"))
  } else {
    complete_dag$outdegree_avg[i] <- 0
  }
  
  # 计算边数
  complete_dag$edge_count[i] <- ecount(g)
}
write.csv(complete_dag, "complete_dag.csv", row.names = FALSE)


complete_dag=read.csv("D:/user/Research_1_social_learning/code/R_whatsapp_250428/data/complete_dag.csv")

# 提取唯一组合
unique_combinations_dag <- unique(complete_dag[, c("node_count", "outdegree_avg", "edge_count")])

# 创建数据框来存储最优 b, h, k
optimization_results_dag <- data.frame(
  node_count = unique_combinations_dag$node_count,
  outdegree_avg = unique_combinations_dag$outdegree_avg,
  edge_count = unique_combinations_dag$edge_count,
  b = numeric(nrow(unique_combinations_dag)),
  h = numeric(nrow(unique_combinations_dag)),
  k = numeric(nrow(unique_combinations_dag))
)

# 使用 L-BFGS-B 优化
for (i in 1:nrow(unique_combinations_dag)) {
  
  node_count <- unique_combinations_dag$node_count[i]
  outdegree_avg <- unique_combinations_dag$outdegree_avg[i]
  edge_count <- unique_combinations_dag$edge_count[i]
  
  # 使用 optim 进行优化
  initial_values <- c(1.5, 4, 1)  # 初始值
  bounds_lower <- c(1.1, 1, 1)    # 下界
  bounds_upper <- c(5, 50, 3)     # 上界
  
  result <- optim(
    par = initial_values, 
    fn = objective_function_dag, 
    method = "L-BFGS-B",  # 限制范围的优化算法
    lower = bounds_lower, 
    upper = bounds_upper,
    p = p, 
    sampled_node_count = node_count, 
    sampled_outdegree_avg = outdegree_avg, 
    sampled_edge_count = edge_count
  )
  
  # 存储优化结果
  optimization_results_dag $b[i] <- result$par[1]
  optimization_results_dag $h[i] <- result$par[2]
  optimization_results_dag $k[i] <- result$par[3]
}

# 将优化结果映射回 complete_dag
complete_dag <- merge(complete_dag, optimization_results_dag, by = c("node_count", "outdegree_avg", "edge_count"))


# ##为DAG情况也构建一个网络搜索
# 
# # 定义 b, h 和 k 的搜索范围
# b_range <- seq(1.1, 5, by = 0.1)
# h_range <- seq(1, 50, by = 1)
# k_range <- seq(1, 3, by = 0.1)
# 
# # 网格搜索寻找最优 b, h 和 k
# for (i in 1:nrow(unique_combinations_dag)) {
#   
#   node_count <- unique_combinations_dag$node_count[i]
#   outdegree_avg <- unique_combinations_dag$outdegree_avg[i]
#   edge_count <- unique_combinations_dag$edge_count[i]
#   
#   # 初始化最小误差和最优的 b, h, k
#   min_error <- Inf
#   best_b <- NA
#   best_h <- NA
#   best_k <- NA
#   
#   # 遍历 b, h 和 k 的所有候选值
#   for (b_candidate in b_range) {
#     for (h_candidate in h_range) {
#       for (k_candidate in k_range) {
#         # 计算当前 b, h, k 的误差
#         error_value <- objective_function_dag(c(b_candidate, h_candidate, k_candidate), 
#                                       sampled_node_count = node_count, 
#                                       sampled_outdegree_avg = outdegree_avg, 
#                                       sampled_edge_count = edge_count, 
#                                       p = p)
#         
#         # 如果当前误差小于最小误差，则更新最优 b, h, k
#         if (error_value < min_error) {
#           min_error <- error_value
#           best_b <- b_candidate
#           best_h <- h_candidate
#           best_k <- k_candidate
#         }
#       }
#     }
#   }
#   
#   # 存储最优的 b, h, k
#   optimization_results_dag$b[i] <- best_b
#   optimization_results_dag$h[i] <- best_h
#   optimization_results_dag$k[i] <- best_k
# }
# 
# # 将优化结果映射回 complete_dag
# complete_dag <- merge(complete_dag, optimization_results_dag, by = c("node_count", "outdegree_avg", "edge_count"))
# 
# 





# Step 3: 优化每个 DAG，加入 forwarding_score 的处理
for (i in 1:length(unique_cascade_ids)) {
  
  # 从 dags 中获取已创建的 DAG 图
  g <- dags[[i]]$dag   # 从创建好的 DAG 列表中提取
  
  # 计算子树中的节点数
  sampled_node_count <- vcount(g)
  
  # 计算出非叶节点的出度
  non_leaf_nodes <- V(g)[degree(g, mode = "out") > 0]
  if (length(non_leaf_nodes) > 0) {
    sampled_outdegree_avg <- mean(degree(g, v = non_leaf_nodes, mode = "out"))
  } else {
    sampled_outdegree_avg <- 0
  }
  
  # 计算边数
  sampled_edge_count <- ecount(g)
  
  # 获取该 cascade_id 对应的最大 forwarding_score
  forwarding_score <- new_cascade_data %>% 
    filter(cascade_id == unique_cascade_ids[i]) %>% 
    pull(forwarding_score) %>% 
    max()  # 使用最大值作为当前 cascade 的 forwarding_score
  
  # 使用 optim() 进行优化，初始值为 (b, h, k)
  initial_values <- c(1.1, forwarding_score+1, 1)  # 可以根据实际情况调整
  
  # 设置下界 bounds_lower，确保 h 的下界是 forwarding_score
  bounds_lower <- c(1.1, forwarding_score+1, 1)  # h >= forwarding_score
  bounds_upper <- c(10, 50, 3)  # 上界可以根据实际问题进行调整
  
  # 使用 L-BFGS-B 优化算法，支持边界约束
  result <- optim(
    par = initial_values, 
    fn = objective_function, 
    method = "L-BFGS-B",  # 使用支持边界的优化算法
    lower = bounds_lower,  # 下界
    upper = bounds_upper,  # 上界
    p = p, 
    sampled_node_count = sampled_node_count, 
    sampled_outdegree_avg = sampled_outdegree_avg, 
    sampled_edge_count = sampled_edge_count
  )
  
  # 返回最优的 b, h, k
  best_b <- result$par[1]
  best_h <- result$par[2]
  best_k <- result$par[3]
  
  # 存储结果到 complete_dag 数据框
  complete_dag$b[i] <- best_b
  complete_dag$h[i] <- best_h
  complete_dag$k[i] <- best_k
}






# 确保 complete_dag 中的 cascade_id 是整数类型
complete_dag$cascade_id <- as.integer(complete_dag$cascade_id)

# 将 modality 数据合并到 complete_dag 中
complete_dag <- complete_dag %>%
  left_join(new_cascade_data %>% select(cascade_id, modality, content_type), by = "cascade_id")


# 左连接 complete_tree 和 max_forwarding_scores
# complete_dag <- complete_dag %>%
#   left_join(max_forwarding_scores, by = "cascade_id")





h_frequency_modality <- complete_dag %>%
  group_by(modality, h) %>%
  summarise(frequency = n()) %>%
  mutate(relative_frequency = frequency / sum(frequency))  # 计算相对频率

# 2. 对 b 进行分组计算相对频率
b_frequency_modality <- complete_dag %>%
  group_by(modality, b) %>%
  summarise(frequency = n()) %>%
  mutate(relative_frequency = frequency / sum(frequency))  # 计算相对频率

# 3. 将 h 和 b 的值格式化为保留小数点后两位
h_frequency_modality$h <- round(h_frequency_modality$h, 2)
b_frequency_modality$b <- round(b_frequency_modality$b, 2)

# 4. 绘制 h 的相对频率分布
ggplot(h_frequency_modality, aes(x = factor(h), y = relative_frequency, fill = modality)) +  
  geom_bar(stat = "identity", position = "dodge", color = "black") +  # 使用 dodge 分开不同 modality 的柱状图
  labs(title = "Relative Frequency Distribution of Heights by Modality",
       x = "Height (h) ",
       y = "Relative Frequency",
       fill = "Modality") +
  theme_minimal() +
  theme(legend.position = "top")

# 5. 绘制 b 的相对频率分布
ggplot(b_frequency_modality, aes(x = factor(b), y = relative_frequency, fill = modality)) +  
  geom_bar(stat = "identity", position = "dodge", color = "black") +  # 使用 dodge 分开不同 modality 的柱状图
  labs(title = "Relative Frequency Distribution of Widths by Modality",
       x = "Width (b) ",
       y = "Relative Frequency",
       fill = "Modality") +
  theme_minimal() +
  theme(legend.position = "top")

# 1. 对 h 进行分组计算相对频率，基于 content_type
h_frequency_content_type <- complete_dag %>%
  group_by(content_type, h) %>%
  summarise(frequency = n()) %>%
  mutate(relative_frequency = frequency / sum(frequency))  # 计算相对频率

# 2. 对 b 进行分组计算相对频率，基于 content_type
b_frequency_content_type <- complete_dag %>%
  group_by(content_type, b) %>%
  summarise(frequency = n()) %>%
  mutate(relative_frequency = frequency / sum(frequency))  # 计算相对频率

# 3. 将 h 和 b 的值格式化为保留小数点后两位
h_frequency_content_type$h <- round(h_frequency_content_type$h, 2)
b_frequency_content_type$b <- round(b_frequency_content_type$b, 2)

h_frequency_filtered <- h_frequency_content_type %>%
  filter(!(content_type == "other" & relative_frequency <= 0.01))

b_frequency_filtered <- b_frequency_content_type %>%
  filter(!(content_type == "other" & relative_frequency <= 0.01))
# 5. 绘制 h 的相对频率分布，基于所有 content_type，筛选 "other"
ggplot(h_frequency_filtered, aes(x = factor(h), y = relative_frequency, fill = content_type)) +  
  geom_bar(stat = "identity", position = "dodge", color = "black") +  # 使用 dodge 分开不同 content_type 的柱状图
  labs(title = "Relative Frequency Distribution of Heights by Content Type",
       x = "Height (h) ",
       y = "Relative Frequency",
       fill = "Content Type") +
  theme_minimal() +
  theme(legend.position = "top")

# 6. 绘制 b 的相对频率分布，基于所有 content_type，筛选 "other"
ggplot(b_frequency_filtered, aes(x = factor(b), y = relative_frequency, fill = content_type)) +  
  geom_bar(stat = "identity", position = "dodge", color = "black") +  # 使用 dodge 分开不同 content_type 的柱状图
  labs(title = "Relative Frequency Distribution of Widths by Content Type",
       x = "Width (b) ",
       y = "Relative Frequency",
       fill = "Content Type") +
  theme_minimal() +
  theme(legend.position = "top")

# 按 modality 分组，并计算 b 和 h 的均值和标准差
summary_stats <- complete_dag %>%
  group_by(forwarding_score) %>%
  summarise(
    mean_b = mean(b),
    std_b = sd(b),
    mean_h = mean(h),
    std_h = sd(h),
    mean_k = mean(k),
    std_k = sd(k)
  )
print(summary_stats)
# 打印结果

# 计算每个 content_type 下不同 modality 的比例
# 移除 content_type 为 'other' 且 modality 为 'ciphertext' 的行
complete_tree <- complete_tree %>%
  filter(!modality == "ciphertext")

# 重命名 content_type
complete_tree <- complete_tree %>%
  mutate(content_type = recode(content_type,
                               "normal" = "viral normal",
                               "other" = "unlabelled"))

# 指定 content_type 顺序
complete_tree <- complete_tree %>%
  mutate(content_type = factor(content_type, levels = c("unlabelled", "viral normal", "misinfo", "hateful", "propaganda")))

# 重新计算每个 content_type 下不同 modality 的比例
content_modality_summary <- complete_tree %>%
  group_by(content_type, modality) %>%
  summarise(count = n()) %>%
  mutate(proportion = count / sum(count) * 100)  # 计算百分比

# 将 "other" 放到最下面，使用 factor() 函数重新设置 content_type 顺序
#content_modality_summary <- content_modality_summary %>%
#  mutate(content_type = if_else(content_type == "other", "normal", content_type))
#content_modality_summary$content_type <- factor(content_modality_summary$content_type,
#                                                levels = c("propaganda", "hateful", "misinfo", "normal"))


# 绘制横向堆叠条形图
plot <- ggplot(content_modality_summary, aes(x = content_type, y = proportion, fill = modality)) +
  geom_bar(stat = "identity", position = "fill", width = 0.7) +  # 堆叠条形图
  geom_text(aes(label = paste0(round(proportion, 1), "%")), 
            position = position_fill(vjust = 0.5), color = "black", fontface = "bold") +  # 加粗标签
  labs(x = "Content Type", y = "Proportion (%)", fill = "Modality") +  # 移除标题，只保留坐标标签
  theme_minimal() +
  coord_flip() +  # 将条形图转换为横向
  theme(
    axis.text.x = element_text(size = 14, face = "bold"),  # 横坐标文字大小和加粗
    axis.text.y = element_text(size = 14, face = "bold"),  # 纵坐标文字大小和加粗
    axis.title.x = element_text(size = 16, face = "bold"),  # 横坐标标题文字大小和加粗
    axis.title.y = element_text(size = 16, face = "bold"),  # 纵坐标标题文字大小和加粗
    plot.title = element_blank(),  # 移除标题
    legend.title = element_text(size = 14, face = "bold"),  # 图例标题加粗
    legend.text = element_text(size = 12, face = "bold")  # 图例文字加粗
  )

ggsave("D:/user/Research_1_social_learning/code/R_whatsapp_250428/result/modality_content_type_plot.pdf", plot, width = 8, height = 6)
# ##根据shared_participants构建group-group network, 重新生成DAG. 
# 
# 
# 
# 
# ## 3.分析Network cascade根据不同的时间, 似乎并没有什么明显的优势
# cascade_data <- cascade_data %>%
#   mutate(event_date = as.POSIXct(timestamp, origin = "1970-01-01", tz = "UTC"))  # Adjust timezone if needed
# # Group by cascade_id and find the maximum date range for each cascade_id
# cascade_ranges <- cascade_data %>%
#   group_by(cluster_id) %>%
#   summarise(
#     start_date = min(event_date),  # Find the earliest date for each cascade
#     end_date = max(event_date),    # Find the latest date for each cascade
#   )
# 
# #  Classify cascades into different months based on the starting date
# cascade_ranges <- cascade_ranges %>%
#   mutate(start_month = floor_date(start_date, unit = "month"))  # Classify based on the month of the starting date
# 
# # 重命名 cascade_ranges 数据框中的列名
# colnames(cascade_ranges)[colnames(cascade_ranges) == "cluster_id"] <- "cascade_id"
# 
# # Count the number of unique cascades per month
# cascades_per_month <- cascade_ranges %>%
#   group_by(start_month) %>%
#   summarise(num_cascades = n_distinct(cascade_id))
# # Merge the two dataframes based on cascade_id
# complete_dag <- complete_dag %>%
#   left_join(cascade_ranges, by = "cascade_id")
# 
# monthly_avg <- complete_dag %>%
#   group_by(start_month) %>%
#   summarise(
#     avg_b = mean(b, na.rm = TRUE),  # 计算 b 的均值，忽略 NA 值
#     avg_h = mean(h, na.rm = TRUE)   # 计算 h 的均值，忽略 NA 值
#   )
# 
# # 查看结果
# print(monthly_avg)
# 


#估算population-level的结果
group_participants <- read.csv("D:/user/Research_4_whatsapp/code/data/groups_participants.csv")
# 计算每个群组的大小
group_size <- group_participants %>%
  group_by(group_id_serialized) %>%
  summarise(group_size = n())


# 绘制群组大小的分布，并将x轴取对数
# 计算群组大小的均值
mean_group_size <- median(group_size$group_size)



# 绘制群组大小的分布，并将x轴取对数
p_group_size <- ggplot(group_size, aes(x = group_size)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.1, fill = "blue", color = "black", alpha = 0.7) +
  scale_x_log10() +  # 将 x 轴取对数
  geom_vline(xintercept = mean_group_size, color = "red", linetype = "dashed", size = 1.2) +  # 添加均值的垂线
  annotate("text", x = mean_group_size * 4,  # 将 x 位置设置为均值的右侧
           y = max(density(group_size$group_size)$y) * 15,  # 调整 y 位置为相对频率的高位
           label = paste("Mean =", round(mean_group_size, 2)), color = "red", size = 6, fontface = "bold") +  # 标注位置
  labs(x = "Group Size (Log Scale)", y = "Relative Frequency") +  # 将 y 轴标签改为相对频率
  theme_minimal() +
  theme(
    axis.text = element_text(size = 14, face = "bold"),  # 坐标轴文本加粗、放大
    axis.title = element_text(size = 16, face = "bold"),  # 坐标轴标题加粗、放大
    plot.title = element_blank()  # 移除标题
  )

# 保存图像为 PDF
ggsave("D:/user/Research_4_whatsapp/code/result/group_size_distribution.pdf", plot = p_group_size, width = 8, height = 6)



# 根据 b 和 h 计算树的节点数，并将结果乘以 mean_group_size
complete_tree$total_nodes <- (complete_tree$b^(complete_tree$h + 1) - 1) / (complete_tree$b - 1)
complete_tree$influenced_population <- complete_tree$total_nodes * mean_group_size



# 添加新的类别 normal with forwarding score >= 5
complete_tree <- complete_tree %>%
  mutate(content_type = if_else(content_type == "normal" & forwarding_score >= 5, 
                                "normal (FS≥5)", content_type))

# 将新的类别加入因子水平
complete_tree$content_type <- factor(complete_tree$content_type, 
                                     levels = c("normal", "normal (FS≥5)", "misinfo", "hateful", "propaganda"))

# 重新计算均值，确保包含新的类别
complete_tree_means <- complete_tree %>%
  group_by(content_type) %>%
  summarise(mean_influenced_population = mean(influenced_population))

# 绘制图像，包含新的类别
ggplot(complete_tree, aes(x = influenced_population, fill = content_type, color = content_type)) +
  geom_density(size = 1.2, alpha = 0.3, bw = 0.5) +  # Density plot with fill and transparency
  scale_x_log10() +  # Log scale for x-axis
  scale_fill_brewer(palette = "Dark2") +  # Using a color palette
  scale_color_brewer(palette = "Dark2") +  # Matching fill and line color
  labs(x = "Influenced Population (Log Scale)", y = "Density", fill = "Content Type", color = "Content Type") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 14, face = "bold"),  # Bold axis text
    axis.title = element_text(size = 16, face = "bold"),  # Bold axis title
    plot.title = element_blank(),  # No title
    legend.position = "top" ,  # Legend on top
    legend.text = element_text(size = 14, face = "bold"),  # Bold and larger legend text
    legend.title = element_text(size = 14, face = "bold"),
    legend.key.width = unit(2, "line")# Bold and larger legend title
  ) +
  guides(fill = guide_legend(nrow = 2), color = guide_legend(nrow = 2)) +  # Split legend into 2 rows
  # Add vertical lines for means
  geom_vline(data = complete_tree_means, aes(xintercept = mean_influenced_population, color = content_type),
             linetype = "dashed", size = 1.2, show.legend = FALSE) +
  # Annotate the means with adjusted x and y positions
  geom_text(data = complete_tree_means, aes(
    x = c(10000, 17000, 55000, 105000, 130000),  # Adjusted x positions for new category
    y = c(0.8, 0.75, 0.7, 0.65, 0.6),  # Adjusted y positions for new category
    label = paste("Mean =", round(mean_influenced_population, 2)),
    color = content_type),  # Match text color with content_type
    hjust = -0.1, size = 5, show.legend = FALSE)



# 根据 b 和 h 计算树的节点数，并将结果乘以 mean_group_size
complete_dag$total_nodes <- (complete_dag$b^(complete_dag$h + 1) - 1) / (complete_dag$b - 1)
complete_dag$influenced_population <- complete_dag$total_nodes * mean_group_size

# Calculate the mean for each content_type
complete_dag_means <- complete_dag %>%
  group_by(content_type) %>%
  summarise(mean_influenced_population = mean(influenced_population, na.rm = TRUE))


complete_dag<- complete_dag %>%
  mutate(content_type = if_else(content_type == "other", "normal", content_type))
complete_dag$content_type <- factor(complete_dag$content_type, 
                                    levels = c("normal", "misinfo", "hateful", "propaganda"))
# Adjusted y positions for each content_type to avoid overlapping text
ggplot(complete_dag, aes(x = influenced_population, fill = content_type, color = content_type)) +
  geom_density(size = 1.2, alpha = 0.3, bw = 0.5) +  # Density plot with fill and transparency
  scale_x_log10() +  # Log scale for x-axis
  scale_fill_brewer(palette = "Dark2") +  # Using a color palette
  scale_color_brewer(palette = "Dark2") +  # Matching fill and line color
  labs(x = "Influenced Population (Log Scale)", y = "Density", fill = "Content Type", color = "Content Type") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 14, face = "bold"),  # Bold axis text
    axis.title = element_text(size = 16, face = "bold"),  # Bold axis title
    plot.title = element_blank(),  # No title
    legend.position = "top",  # Legend on top
    legend.text = element_text(size = 14, face = "bold"),  # Bold and larger legend text
    legend.title = element_text(size = 14, face = "bold")  # Bold and larger legend title
  ) +
  # Add vertical lines for means
  geom_vline(data = complete_dag_means, aes(xintercept = mean_influenced_population, color = content_type),
             linetype = "dashed", size = 1.2, show.legend = FALSE) +
  # Annotate the means with adjusted y positions and hjust to avoid overlapping
  geom_text(data = complete_dag_means, aes(x = c(10500, 17500, 108000, 145000), 
                                           y =  c(0.8, 0.75, 0.7, 0.65),  # Adjusted y positions
                                           label = paste("Mean =", round(mean_influenced_population, 2)),
                                           color = content_type),  # Match text color with content_type
            hjust = -0.1, size = 5, show.legend = FALSE)



# 添加新的类别 normal with forwarding score >= 5
complete_dag <- complete_dag %>%
  mutate(content_type = if_else(content_type == "normal" & forwarding_score >= 5, 
                                "normal (FS≥5)", content_type))

# 将新的类别加入因子水平
complete_dag$content_type <- factor(complete_dag$content_type, 
                                    levels = c("normal", "normal (FS≥5)", "misinfo", "hateful", "propaganda"))

# 重新计算均值，确保包含新的类别
complete_dag_means <- complete_dag %>%
  group_by(content_type) %>%
  summarise(mean_influenced_population = mean(influenced_population, na.rm = TRUE))

# 绘制图像，包含新的类别
ggplot(complete_dag, aes(x = influenced_population, fill = content_type, color = content_type)) +
  geom_density(size = 1.2, alpha = 0.3, bw = 0.5) +  # Density plot with fill and transparency
  scale_x_log10() +  # Log scale for x-axis
  scale_fill_brewer(palette = "Dark2") +  # Using a color palette
  scale_color_brewer(palette = "Dark2") +  # Matching fill and line color
  labs(x = "Influenced Population (Log Scale)", y = "Density", fill = "Content Type", color = "Content Type") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 14, face = "bold"),  # Bold axis text
    axis.title = element_text(size = 16, face = "bold"),  # Bold axis title
    plot.title = element_blank(),  # No title
    legend.position = "top",  # Legend on top
    legend.text = element_text(size = 14, face = "bold"),  # Bold and larger legend text
    legend.title = element_text(size = 14, face = "bold"),  # Bold and larger legend title
    legend.key.width = unit(2, "line")  # Adjust width of legend boxes
  ) +
  guides(fill = guide_legend(nrow = 2), color = guide_legend(nrow = 2)) +  # Split legend into 2 rows
  # Add vertical lines for means
  geom_vline(data = complete_dag_means, aes(xintercept = mean_influenced_population, color = content_type),
             linetype = "dashed", size = 1.2, show.legend = FALSE) +
  # Annotate the means with adjusted y positions and hjust to avoid overlapping
  geom_text(data = complete_dag_means, aes(x = c(5000, 12000, 65000, 100000, 145000), 
                                           y =  c(0.8, 0.76, 0.75, 0.7, 0.65),  # Adjusted y positions for new category
                                           label = paste("Mean =", round(mean_influenced_population, 2)),
                                           color = content_type),  # Match text color with content_type
            hjust = -0.1, size = 5, show.legend = FALSE)


# Calculate the mean for each content_type for height h
complete_tree_means_h <- complete_tree %>%
  group_by(content_type) %>%
  summarise(mean_h = mean(h, na.rm = TRUE))

# Plot density for h with mean lines and annotations
ggplot(complete_tree, aes(x = h, fill = content_type, color = content_type)) +
  geom_density(size = 1.2, alpha = 0.3, bw = 0.5) +  # Density plot for h with fill and transparency
  scale_fill_brewer(palette = "Dark2") +  # Using a color palette
  scale_color_brewer(palette = "Dark2") +  # Matching fill and line color
  labs(x = "Height (h)", y = "Density", fill = "Content Type", color = "Content Type") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 14, face = "bold"),  # Bold axis text
    axis.title = element_text(size = 16, face = "bold"),  # Bold axis title
    plot.title = element_blank(),  # No title
    legend.position = "top",  # Legend on top
    legend.text = element_text(size = 14, face = "bold"),  # Bold and larger legend text
    legend.title = element_text(size = 14, face = "bold")  # Bold and larger legend title
  ) +
  # Add vertical lines for means of h
  geom_vline(data = complete_tree_means_h, aes(xintercept = mean_h, color = content_type),
             linetype = "dashed", size = 1.2) +
  # Annotate the means with adjusted x and y positions for each content_type
  geom_text(data = complete_tree_means_h, aes(
    x = c(4.6, 4.8, 5.0, 5.3),  # Slightly shift the text to the right of the mean line
    y = c(0.8, 0.75, 0.7, 0.65),  # Adjusted y positions to avoid overlap
    label = paste("Mean =", round(mean_h, 2))),
    color = "black", size = 4.5, fontface = "bold", hjust = 0)


###
max_forwarding_scores <- new_cascade_data %>%
  group_by(cascade_id) %>%
  filter(forwarding_score == max(forwarding_score)) %>%
  slice(1)%>%  # 如果有多行具有相同的最大值，选择第一行
  select(cascade_id, forwarding_score) 

# 左连接 complete_tree 和 max_forwarding_scores
complete_tree <- complete_tree %>%
  left_join(max_forwarding_scores, by = "cascade_id")

new_cascade_data <- data.frame(
  node_name = cascade_data$group_id,  # Use group_id_numeric as node_name
  event_time = cascade_data$timestamp,       # Keep the event_time column unchanged
  cascade_id = cascade_data$cluster_id,
  modality= cascade_data$modality,
  content_type=cascade_data$content_type,
  forwarding_score = cascade_data$forwarding_score)  


###WilcoxonTest
# 提取 tree 数据集的各个类型数据
tree_other_b <- complete_tree$b[complete_tree$content_type == "other"]#unlabeled
tree_normal_b <- complete_tree$b[complete_tree$content_type == "normal"]
tree_misinfo_b <- complete_tree$b[complete_tree$content_type == "misinfo"]
tree_hateful_b <- complete_tree$b[complete_tree$content_type == "hateful"]
tree_propaganda_b <- complete_tree$b[complete_tree$content_type == "propaganda"]

# 提取 dag 数据集的各个类型数据
dag_other_h <- complete_dag$h[complete_dag$content_type == "other"]
dag_misinfo_h <- complete_dag$h[complete_dag$content_type == "misinfo"]
dag_hateful_h <- complete_dag$h[complete_dag$content_type == "hateful"]
dag_propaganda_h <- complete_dag$h[complete_dag$content_type == "propaganda"]

# 对 tree 进行 Wilcoxon 检验
misinfo_test_tree <- wilcox.test(tree_misinfo_b, tree_other_b, alternative = "greater", exact = FALSE)
hateful_test_tree <- wilcox.test(tree_hateful_b, tree_other_b, alternative = "greater", exact = FALSE)
propaganda_test_tree <- wilcox.test(tree_propaganda_b, tree_other_b, alternative = "greater", exact = FALSE)
propaganda_misinfo_test_tree <- wilcox.test(tree_propaganda_h, tree_misinfo_h, alternative = "greater", exact = FALSE)
hateful_misinfo_test_tree <- wilcox.test(tree_hateful_h, tree_misinfo_h, alternative = "greater", exact = FALSE)
propaganda_hateful_test_tree <- wilcox.test(tree_propaganda_h, tree_hateful_h, alternative = "greater", exact = FALSE)

misinfo_normal_test_tree <- wilcox.test(tree_misinfo_b, tree_normal_b, alternative = "greater", exact = FALSE)
hateful_normal_test_tree <- wilcox.test(tree_hateful_b, tree_normal_b, alternative = "greater", exact = FALSE)
propaganda_normal_test_tree <- wilcox.test(tree_propaganda_b, tree_normal_b, alternative = "greater", exact = FALSE)
# 对 dag 进行 Wilcoxon 检验
misinfo_test_dag <- wilcox.test(dag_misinfo_h, dag_other_h, alternative = "greater", exact = FALSE)
hateful_test_dag <- wilcox.test(dag_hateful_h, dag_other_h, alternative = "greater", exact = FALSE)
propaganda_test_dag <- wilcox.test(dag_propaganda_h, dag_other_h, alternative = "greater", exact = FALSE)
propaganda_misinfo_test_dag <- wilcox.test(dag_propaganda_h, dag_misinfo_h, alternative = "greater", exact = FALSE)
hateful_misinfo_test_dag <- wilcox.test(dag_hateful_h, dag_misinfo_h, alternative = "greater", exact = FALSE)
propaganda_hateful_test_dag <- wilcox.test(dag_propaganda_h, dag_hateful_h, alternative = "greater", exact = FALSE)

# 获取 tree 的统计量和 p 值
misinfo_stat_tree <- misinfo_test_tree$statistic
misinfo_p_value_tree <- misinfo_test_tree$p.value
misinfo_p_value_formatted_tree <- ifelse(misinfo_p_value_tree < 1e-5, "< 1e-5", format(misinfo_p_value_tree, digits = 5))
misinfo_significance_tree <- ifelse(misinfo_p_value_tree < 0.05, "Yes", "No")

hateful_stat_tree <- hateful_test_tree$statistic
hateful_p_value_tree <- hateful_test_tree$p.value
hateful_p_value_formatted_tree <- ifelse(hateful_p_value_tree < 1e-5, "< 1e-5", format(hateful_p_value_tree, digits = 5))
hateful_significance_tree <- ifelse(hateful_p_value_tree < 0.05, "Yes", "No")

propaganda_stat_tree <- propaganda_test_tree$statistic
propaganda_p_value_tree <- propaganda_test_tree$p.value
propaganda_p_value_formatted_tree <- ifelse(propaganda_p_value_tree < 1e-5, "< 1e-5", format(propaganda_p_value_tree, digits = 5))
propaganda_significance_tree <- ifelse(propaganda_p_value_tree < 0.05, "Yes", "No")

propaganda_misinfo_stat_tree <- propaganda_misinfo_test_tree$statistic
propaganda_misinfo_p_value_tree <- propaganda_misinfo_test_tree$p.value
propaganda_misinfo_p_value_formatted_tree <- ifelse(propaganda_misinfo_p_value_tree < 1e-5, "< 1e-5", format(propaganda_misinfo_p_value_tree, digits = 5))
propaganda_misinfo_significance_tree <- ifelse(propaganda_misinfo_p_value_tree < 0.05, "Yes", "No")

hateful_misinfo_stat_tree <- hateful_misinfo_test_tree$statistic
hateful_misinfo_p_value_tree <- hateful_misinfo_test_tree$p.value
hateful_misinfo_p_value_formatted_tree <- ifelse(hateful_misinfo_p_value_tree < 1e-5, "< 1e-5", format(hateful_misinfo_p_value_tree, digits = 5))
hateful_misinfo_significance_tree <- ifelse(hateful_misinfo_p_value_tree < 0.05, "Yes", "No")

propaganda_hateful_stat_tree <- propaganda_hateful_test_tree$statistic
propaganda_hateful_p_value_tree <- propaganda_hateful_test_tree$p.value
propaganda_hateful_p_value_formatted_tree <- ifelse(propaganda_hateful_p_value_tree < 1e-5, "< 1e-5", format(propaganda_hateful_p_value_tree, digits = 5))
propaganda_hateful_significance_tree <- ifelse(propaganda_hateful_p_value_tree < 0.05, "Yes", "No")

misinfo_normal_stat_tree <- misinfo_normal_test_tree$statistic
misinfo_normal_p_value_tree <- misinfo_normal_test_tree$p.value
hateful_normal_stat_tree <- hateful_normal_test_tree$statistic
hateful_normal_p_value_tree <- hateful_normal_test_tree$p.value
propaganda_normal_stat_tree <- propaganda_normal_test_tree$statistic
propaganda_normal_p_value_tree <- propaganda_normal_test_tree$p.value

# 获取 dag 的统计量和 p 值
misinfo_stat_dag <- misinfo_test_dag$statistic
misinfo_p_value_dag <- misinfo_test_dag$p.value
misinfo_p_value_formatted_dag <- ifelse(misinfo_p_value_dag < 1e-5, "< 1e-5", format(misinfo_p_value_dag, digits = 5))
misinfo_significance_dag <- ifelse(misinfo_p_value_dag < 0.05, "Yes", "No")

hateful_stat_dag <- hateful_test_dag$statistic
hateful_p_value_dag <- hateful_test_dag$p.value
hateful_p_value_formatted_dag <- ifelse(hateful_p_value_dag < 1e-5, "< 1e-5", format(hateful_p_value_dag, digits = 5))
hateful_significance_dag <- ifelse(hateful_p_value_dag < 0.05, "Yes", "No")

propaganda_stat_dag <- propaganda_test_dag$statistic
propaganda_p_value_dag <- propaganda_test_dag$p.value
propaganda_p_value_formatted_dag <- ifelse(propaganda_p_value_dag < 1e-5, "< 1e-5", format(propaganda_p_value_dag, digits = 5))
propaganda_significance_dag <- ifelse(propaganda_p_value_dag < 0.05, "Yes", "No")

propaganda_misinfo_stat_dag <- propaganda_misinfo_test_dag$statistic
propaganda_misinfo_p_value_dag <- propaganda_misinfo_test_dag$p.value
propaganda_misinfo_p_value_formatted_dag <- ifelse(propaganda_misinfo_p_value_dag < 1e-5, "< 1e-5", format(propaganda_misinfo_p_value_dag, digits = 5))
propaganda_misinfo_significance_dag <- ifelse(propaganda_misinfo_p_value_dag < 0.05, "Yes", "No")

hateful_misinfo_stat_dag <- hateful_misinfo_test_dag$statistic
hateful_misinfo_p_value_dag <- hateful_misinfo_test_dag$p.value
hateful_misinfo_p_value_formatted_dag <- ifelse(hateful_misinfo_p_value_dag < 1e-5, "< 1e-5", format(hateful_misinfo_p_value_dag, digits = 5))
hateful_misinfo_significance_dag <- ifelse(hateful_misinfo_p_value_dag < 0.05, "Yes", "No")

propaganda_hateful_stat_dag <- propaganda_hateful_test_dag$statistic
propaganda_hateful_p_value_dag <- propaganda_hateful_test_dag$p.value
propaganda_hateful_p_value_formatted_dag <- ifelse(propaganda_hateful_p_value_dag < 1e-5, "< 1e-5", format(propaganda_hateful_p_value_dag, digits = 5))
propaganda_hateful_significance_dag <- ifelse(propaganda_hateful_p_value_dag < 0.05, "Yes", "No")



#分析incomplete influence cascade


# 使用 igraph 计算树深度
calculate_depth_igraph <- function(df, message_id) {
  # 过滤出对应cascade_id的数据
  sub_df <- partial_trees_dt[cascade_id == message_id]
  
  # 创建有向图
  g <- graph_from_data_frame(d = sub_df, directed = TRUE, vertices = NULL)
  
  # 找到所有根节点（没有入边的节点）
  root_nodes <- V(g)[degree(g, mode = "in") == 0]
  
  # 使用eccentricity计算树的深度
  if (length(root_nodes) == 0) {
    return(0)
  } else {
    tree_depth <- max(eccentricity(g, vids = root_nodes, mode = "out"))
    return(tree_depth)
  }
}

# 使用 igraph 计算树的最大宽度
calculate_max_breadth <- function(df, message_id) {
  # 过滤出对应cascade_id的数据
  sub_df <- df[cascade_id == message_id]
  
  # 创建有向图
  g <- graph_from_data_frame(d = sub_df, directed = TRUE, vertices = NULL)
  
  # 计算每个节点的深度
  root <- V(g)[degree(g, mode = "in") == 0]
  if (length(root) == 0) {
    return(0)
  }
  
  # 使用bfs树搜索获取节点的深度
  bfs_result <- bfs(g, root = root[1], dist = TRUE)
  node_depths <- bfs_result$dist
  
  # 计算每个深度的宽度
  depth_table <- table(node_depths)
  
  # 找出最大宽度
  max_breadth <- max(depth_table)
  
  return(max_breadth)
}

# 计算所有cascade_id的树深度,最大宽度和结构病毒性指标
unique_cascade_ids <- unique(partial_trees_dt$cascade_id)
depths <- sapply(unique_cascade_ids, function(cid) calculate_depth_igraph(partial_trees_dt, cid))
max_breadths <- sapply(unique_cascade_ids, function(cid) calculate_max_breadth(partial_trees_dt, cid))



# 创建结果data.frame
depth_df <- data.frame(cascade_id = unique_cascade_ids, depth = depths)
rownames(depth_df ) <- NULL

max_breadth_df <- data.frame(cascade_id = unique_cascade_ids, max_breadth = max_breadths)
rownames(max_breadth_df ) <- NULL

depth_df$cascade_id <- as.integer(depth_df $cascade_id)

# 将 modality 数据合并到 complete_tree 中
depth_df  <- depth_df %>%
  left_join(complete_tree %>% select(cascade_id, modality, content_type), by = "cascade_id")

max_breadth_df$cascade_id <- as.integer(max_breadth_df$cascade_id)

# 将 modality 数据合并到 complete_tree 中
max_breadth_df  <- max_breadth_df %>%
  left_join(complete_tree %>% select(cascade_id, modality, content_type), by = "cascade_id")


depth_df <- depth_df %>%
  mutate(content_type = if_else(content_type == "other", "normal", content_type))
# 确保 content_type 是因子类型，并且按照指定顺序排列，确保 "other" 在最前面
depth_df$content_type <- factor(depth_df$content_type, levels = c("normal", "hateful", "misinfo", "propaganda"))
# 计算每个 content_type 的 CCDF
# 替换 content_type 中的值

ccdf_depth <- depth_df %>%
  group_by(content_type, depth) %>%
  summarise(count = n()) %>%  # 计算每个 depth 的频次
  arrange(desc(depth)) %>%    # 按 depth 降序排列
  group_by(content_type) %>%
  mutate(ccdf = cumsum(count) / sum(count))  # 计算 CCDF
ccdf_depth$content_type[ccdf_depth$content_type == "normal viral"] <- "viral normal"
ccdf_depth$content_type[ccdf_depth$content_type == "other"] <- "unlabelled"



ggplot(ccdf_depth, aes(x = depth, y = ccdf, color = content_type)) +  # CCDF = 1 - CDF
  geom_line(size = 1.2) +  # 绘制 CCDF 曲线
  labs(x = "", y = "CCDF") +  # 移除 x 轴标签，保留 y 轴标签
  theme_minimal() +
  theme(
    axis.text = element_text(size = 20, face = "bold", color = "black"),  # 坐标轴文字增大加粗，颜色设为黑色
    axis.title = element_text(size = 20, face = "bold", color = "black"),  # 坐标轴标题增大加粗，颜色设为黑色
    legend.text = element_text(size = 20, face = "bold", color = "black"),
    legend.position = c(0.95, 0.95),
    legend.justification = c("right", "top"),
    legend.background = element_rect(fill = "white", color = "black"),
    legend.box.background = element_rect(color = "black")
  ) +
  guides(color = guide_legend(title = NULL))  # 移除图例标题



# 计算每个 content_type 的 CCDF (针对 breadth)

# 确保 content_type 是因子类型，并且按照指定顺序排列，确保 "other" 在最前面

max_breadth_df <- max_breadth_df %>%
  mutate(content_type = if_else(content_type == "other", "normal", content_type))
max_breadth_df$content_type <- factor(max_breadth_df$content_type, levels = c("normal", "hateful", "misinfo", "propaganda"))

# 计算每个 content_type 的 CCDF (针对 breadth)
ccdf_breadth <- max_breadth_df %>%
  group_by(content_type, max_breadth) %>%
  summarise(count = n()) %>%  # 计算每个 max_breadth 的频次
  arrange(desc(max_breadth)) %>%  # 按 max_breadth 降序排列
  group_by(content_type) %>%
  mutate(ccdf = cumsum(count) / sum(count))  # 计算 CCDF

ccdf_breadth$content_type[ccdf_breadth$content_type == "normal"] <- "viral normal"
ccdf_breadth$content_type[ccdf_breadth$content_type == "other"] <- "unlabelled"

# 使用 ggplot2 绘制 CCDF 曲线 (针对 breadth)
ggplot(ccdf_breadth, aes(x = max_breadth, y = ccdf, color = content_type)) + 
  geom_line(size = 1.2) +
  labs(x = "", y = "CCDF") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 20, face = "bold", color = "black"),
    axis.title = element_text(size = 20, face = "bold", color = "black"),
    legend.text = element_text(size = 20, face = "bold", color = "black"),
    legend.position = c(0.95, 0.95),
    legend.justification = c("right", "top"),
    legend.background = element_rect(fill = "white", color = "black"),
    legend.box.background = element_rect(color = "black")
  ) +
  guides(color = guide_legend(title = NULL))

complete_tree <- complete_tree %>%
  mutate(content_type = if_else(content_type == "other", "normal", content_type))
complete_dag <- complete_dag%>%
  mutate(content_type = if_else(content_type == "other", "normal", content_type))





# 筛选出 forwarding_score 为 127 且 content_type 为 "normal" 的行
filtered_data_127_normal <- subset(complete_dag, forwarding_score == 127 & content_type == "normal")

# 计算 b 和 h 的均值和标准差
b_mean <- mean(filtered_data_127_normal$b)
b_sd <- sd(filtered_data_127_normal$b)
h_mean <- mean(filtered_data_127_normal$h)
h_sd <- sd(filtered_data_127_normal$h)
k_mean <- mean(filtered_data_127_normal$k)
k_sd <- sd(filtered_data_127_normal$k)


# 提取 complete_tree 中 forwarding_score >= 5 且 content_type 为 normal 的数据
tree_normal_fs5_h <- complete_tree$h[complete_tree$content_type == "normal" & complete_tree$forwarding_score >= 5]

# 对 tree 进行 Wilcoxon 检验
normal_fs5_mis_test_tree <- wilcox.test(tree_misinfo_h, tree_normal_fs5_h, alternative = "greater", exact = FALSE)
normal_fs5_hateful_test_tree <- wilcox.test(tree_hateful_h, tree_normal_fs5_h, alternative = "greater", exact = FALSE)
normal_fs5_propa_test_tree <- wilcox.test(tree_propaganda_h, tree_normal_fs5_h, alternative = "greater", exact = FALSE)



# 类似地，对 dag 数据进行 Wilcoxon 检验
dag_normal_fs5_h <- complete_dag$h[complete_dag$content_type == "normal" & complete_dag$forwarding_score >= 5]
normal_fs5_mis_test_dag <- wilcox.test(dag_misinfo_h, dag_normal_fs5_h,  alternative = "greater", exact = FALSE)
normal_fs5_hateful_test_dag <- wilcox.test(dag_hateful_h, dag_normal_fs5_h,  alternative = "greater", exact = FALSE)
normal_fs5_propa_test_dag <- wilcox.test(dag_propaganda_h, dag_normal_fs5_h,  alternative = "greater", exact = FALSE)



# 提取 complete_tree 中 forwarding_score >= 5 且 content_type 为 normal 的 b 数据
tree_normal_fs5_b <- complete_tree$b[complete_tree$content_type == "normal" & complete_tree$forwarding_score >= 5]

# 对 tree 进行 Wilcoxon 检验，针对 b 列
normal_fs5_mis_test_tree <- wilcox.test(tree_misinfo_b, tree_normal_fs5_b, alternative = "greater", exact = FALSE)
normal_fs5_hateful_test_tree <- wilcox.test(tree_hateful_b, tree_normal_fs5_b, alternative = "greater", exact = FALSE)
normal_fs5_propa_test_tree <- wilcox.test(tree_propaganda_b, tree_normal_fs5_b, alternative = "greater", exact = FALSE)

# 类似地，对 dag 数据进行 Wilcoxon 检验，针对 b 列
dag_normal_fs5_b <- complete_dag$b[complete_dag$content_type == "normal" & complete_dag$forwarding_score >= 5]

normal_fs5_mis_test_dag <- wilcox.test(dag_misinfo_b, dag_normal_fs5_b,  alternative = "greater", exact = FALSE)
normal_fs5_hateful_test_dag <- wilcox.test(dag_hateful_b, dag_normal_fs5_b,  alternative = "greater", exact = FALSE)
normal_fs5_propa_test_dag <- wilcox.test(dag_propaganda_b, dag_normal_fs5_b,  alternative = "greater", exact = FALSE)

#demorgraphic 分析

cascade_data_author_demor <- cascade_data_author_demor %>%
  select(cascade_id, timestamp, religion, caste, education, Monthly_Income, age)

complete_tree_demor <- complete_tree %>%
  inner_join(cascade_data_author_demor, by = "cascade_id")



# 创建翻译字典
religion_translation <- c(
  "इस्लाम" = "Islam",
  "हिन्दू धर्म" = "Hinduism",
  "जैन धर्म" = "Jainism",
  "बौद्ध धर्म" = "Buddhism"
)

caste_translation <- c(
  "ओ.बी.सी (अन्य पिछड़ा वर्ग)" = "OBC (Other Backward Classes)",
  "दलित/अनुसूचित जाति" = "Dalit/Scheduled Castes",
  "जनरल/उच्च" = "General/Upper",
  "गैर-हिंदू" = "Non-Hindu",
  "आदिवासी/अनुसूचित जनजाति" = "Tribal/Scheduled Tribes"
)


# 更新 education_translation 翻译字典
education_translation <- c(
  "Passed 10th grade" = "Passed 10th grade",
  "Passed 12th grade" = "Passed 12th grade",
  "10वीं कक्षा उत्तीर्ण" = "Passed 10th grade",  # 新增
  "12वीं कक्षा उत्तीर्ण" = "Passed 12th grade",  # 新增
  "विश्वविद्यालय की पहली डिग्री (जैसे बी.ए, बी.एस.सी)" = "First University Degree (e.g., BA, BSc)",
  "विश्वविद्यालय की उच्च डिग्री (जैसे एम.ए., एम.बीए., पी.एच.डी)" = "Higher University Degree (e.g., MA, MBA, PhD)",
  "माध्यमिक विद्यालय (5-9वीं कक्षा)" = "Secondary School (Grades 5-9)",
  "प्राथमिक विद्यालय (5वीं कक्षा तक)" = "Primary School (Up to Grade 5)",
  "व्यवसायिक कॉलेज शिक्षा (जैसे इलेक्ट्रीशियन, नर्स के रूप में अर्हता प्राप्त करना)" = "Vocational College Education (e.g., Electrician, Nurse)",
  "बिल्कुल भी स्कूल शिक्षा नहीं" = "No Formal School Education",
  "अन्य" = "Other",
  "व्यवसायिक उच्च शिक्षा (जैसे वकील, एकाउंटेंट के रूप में अर्हता प्राप्त करना)" = "Vocational Higher Education (e.g., Lawyer, Accountant)"
)

income_translation <- c(
  "5,000 - 10,000" = "5,000 - 10,000",
  "25,000 - 50,000" = "25,000 - 50,000",
  "10,000 - 25,000" = "10,000 - 25,000",
  "5,000 से कम" = "Less than 5,000",
  "50,000 - 1 लाख" = "50,000 - 100,000",
  "5 लाख - 10 लाख" = "500,000 - 1,000,000",
  "1 लाख - 2 लाख" = "100,000 - 200,000",
  "10 लाख से अधिक" = "More than 1,000,000"
)

# 使用 recode 函数将 religion, caste, education, Monthly_Income 列翻译成英文
complete_tree_demor <- complete_tree_demor %>%
  mutate(
    religion = recode(religion, !!!religion_translation),
    caste = recode(caste, !!!caste_translation),
    education = recode(education, !!!education_translation),
    Monthly_Income = recode(Monthly_Income, !!!income_translation)
  )


# 使用 iconv 将各列的字符编码转换为 UTF-8
complete_tree_demor <- complete_tree_demor %>%
  mutate(
    religion = iconv(religion, from = "UTF-8", to = "UTF-8"),
    caste = iconv(caste, from = "UTF-8", to = "UTF-8"),
    education = iconv(education, from = "UTF-8", to = "UTF-8"),
    Monthly_Income = iconv(Monthly_Income, from = "UTF-8", to = "UTF-8")
  )









###

complete_tree_demor <- complete_tree_demor %>%
  mutate(
    caste = if_else(caste == "General/Upper", "General", "Other")
  )

# Adjusting age: <20, 20-30, >30
complete_tree_demor <- complete_tree_demor %>%
  mutate(
    age = case_when(
      age < 25 ~ "<25",
      age >= 25 & age <= 34 ~ "25-34",
      age > 35 ~ ">35"
    )
  )

# Adjusting religion: Hindu, Muslim, ignore other
complete_tree_demor <- complete_tree_demor %>%
  filter(religion %in% c("Hinduism", "Islam"))

# Adjusting income: <25k, >=25k
complete_tree_demor <- complete_tree_demor %>%
  mutate(
    Monthly_Income = if_else(Monthly_Income %in% c("Less than 5,000", "5,000 - 10,000", "10,000 - 25,000"), "<25k", ">=25k")
  )

# Adjusting education: <high school, >high school, ignore other
complete_tree_demor <- complete_tree_demor %>%
  mutate(
    education = case_when(
      education %in% c("Passed 10th grade", "Passed 12th grade", "Primary School (Up to Grade 5)", "No Formal School Education", "Secondary School (Grades 5-9)") ~ "<High School",
      education %in% c("Higher University Degree (e.g., MA, MBA, PhD)", "First University Degree (e.g., BA, BSc)", "Vocational Higher Education (e.g., Lawyer, Accountant)", "Vocational College Education (e.g., Electrician, Nurse)") ~ ">High School"
    )
  )
# Remove ignored education category
complete_tree_demor <- complete_tree_demor %>%
  filter(!is.na(education))

# Religion stats
religion_stats <- complete_tree_demor %>%
  group_by(religion) %>%
  summarise(
    mean_b = mean(b, na.rm = TRUE),
    sd_b = sd(b, na.rm = TRUE),
    mean_h = mean(h, na.rm = TRUE),
    sd_h = sd(h, na.rm = TRUE)
  )

# Caste stats
caste_stats <- complete_tree_demor %>%
  group_by(caste) %>%
  summarise(
    mean_b = mean(b, na.rm = TRUE),
    sd_b = sd(b, na.rm = TRUE),
    mean_h = mean(h, na.rm = TRUE),
    sd_h = sd(h, na.rm = TRUE)
  )

# Education stats
education_stats <- complete_tree_demor %>%
  group_by(education) %>%
  summarise(
    mean_b = mean(b, na.rm = TRUE),
    sd_b = sd(b, na.rm = TRUE),
    mean_h = mean(h, na.rm = TRUE),
    sd_h = sd(h, na.rm = TRUE)
  )

# Income stats
income_stats <- complete_tree_demor %>%
  group_by(Monthly_Income) %>%
  summarise(
    mean_b = mean(b, na.rm = TRUE),
    sd_b = sd(b, na.rm = TRUE),
    mean_h = mean(h, na.rm = TRUE),
    sd_h = sd(h, na.rm = TRUE)
  )

# Income stats
age_stats <- complete_tree_demor %>%
  group_by(age) %>%
  summarise(
    mean_b = mean(b, na.rm = TRUE),
    sd_b = sd(b, na.rm = TRUE),
    mean_h = mean(h, na.rm = TRUE),
    sd_h = sd(h, na.rm = TRUE)
  )



# 1. 合并 complete_dag 和 cascade_data_author_demor 数据集
complete_dag_demor <- complete_dag %>%
  inner_join(cascade_data_author_demor, by = "cascade_id")

complete_dag_demor <- complete_dag_demor %>%
  mutate(
    religion = iconv(religion, from = "UTF-8", to = "UTF-8"),
    caste = iconv(caste, from = "UTF-8", to = "UTF-8"),
    education = iconv(education, from = "UTF-8", to = "UTF-8"),
    Monthly_Income = iconv(Monthly_Income, from = "UTF-8", to = "UTF-8")
  )

# 使用 recode 函数将 religion, caste, education, Monthly_Income 列翻译成英文
complete_dag_demor <- complete_dag_demor %>%
  mutate(
    religion = recode(religion, !!!religion_translation),
    caste = recode(caste, !!!caste_translation),
    education = recode(education, !!!education_translation),
    Monthly_Income = recode(Monthly_Income, !!!income_translation)
  )

# 调整 caste 列: General vs Other
complete_dag_demor <- complete_dag_demor %>%
  mutate(
    caste = if_else(caste == "General/Upper", "General", "Other")
  )

# 调整 age 列: <20, 20-30, >30
complete_dag_demor <- complete_dag_demor %>%
  mutate(
    age = case_when(
      age < 25 ~ "<25",
      age >= 25 & age <= 34 ~ "25-34",
      age > 35 ~ ">35"
    )
  )

# 调整 religion 列: Hindu, Muslim, 忽略其他宗教
complete_dag_demor <- complete_dag_demor %>%
  filter(religion %in% c("Hinduism", "Islam"))

# 调整 Monthly_Income 列: <5k 和 >=5k
complete_dag_demor <- complete_dag_demor %>%
  mutate(
    Monthly_Income = if_else(Monthly_Income %in% c("Less than 5,000", "5,000 - 10,000", "10,000 - 25,000"), "<25k", ">=25k"))

# 调整 education 列: <high school 和 >high school，忽略其他
complete_dag_demor <- complete_dag_demor %>%
  mutate(
    education = case_when(
      education %in% c("Passed 10th grade", "Passed 12th grade", "Primary School (Up to Grade 5)", "No Formal School Education", "Secondary School (Grades 5-9)") ~ "<High School",
      education %in% c("Higher University Degree (e.g., MA, MBA, PhD)", "First University Degree (e.g., BA, BSc)", "Vocational Higher Education (e.g., Lawyer, Accountant)", "Vocational College Education (e.g., Electrician, Nurse)") ~ ">High School"
    )
  )
# 删除忽略的 education 类别
complete_dag_demor <- complete_dag_demor %>%
  filter(!is.na(education))

# Religion stats for DAG
religion_stats_dag <- complete_dag_demor %>%
  group_by(religion) %>%
  summarise(
    mean_b = mean(b, na.rm = TRUE),
    sd_b = sd(b, na.rm = TRUE),
    mean_h = mean(h, na.rm = TRUE),
    sd_h = sd(h, na.rm = TRUE),
    mean_k = mean(k, na.rm = TRUE),
    sd_k = sd(k, na.rm = TRUE)
  )

# Caste stats for DAG
caste_stats_dag <- complete_dag_demor %>%
  group_by(caste) %>%
  summarise(
    mean_b = mean(b, na.rm = TRUE),
    sd_b = sd(b, na.rm = TRUE),
    mean_h = mean(h, na.rm = TRUE),
    sd_h = sd(h, na.rm = TRUE),
    mean_k = mean(k, na.rm = TRUE),
    sd_k = sd(k, na.rm = TRUE)
  )

# Education stats for DAG
education_stats_dag <- complete_dag_demor %>%
  group_by(education) %>%
  summarise(
    mean_b = mean(b, na.rm = TRUE),
    sd_b = sd(b, na.rm = TRUE),
    mean_h = mean(h, na.rm = TRUE),
    sd_h = sd(h, na.rm = TRUE),
    mean_k = mean(k, na.rm = TRUE),
    sd_k = sd(k, na.rm = TRUE)
  )

# Income stats for DAG
income_stats_dag <- complete_dag_demor %>%
  group_by(Monthly_Income) %>%
  summarise(
    mean_b = mean(b, na.rm = TRUE),
    sd_b = sd(b, na.rm = TRUE),
    mean_h = mean(h, na.rm = TRUE),
    sd_h = sd(h, na.rm = TRUE),
    mean_k = mean(k, na.rm = TRUE),
    sd_k = sd(k, na.rm = TRUE)
  )

# Age stats for DAG
age_stats_dag <- complete_dag_demor %>%
  group_by(age) %>%
  summarise(
    mean_b = mean(b, na.rm = TRUE),
    sd_b = sd(b, na.rm = TRUE),
    mean_h = mean(h, na.rm = TRUE),
    sd_h = sd(h, na.rm = TRUE),
    mean_k = mean(k, na.rm = TRUE),
    sd_k = sd(k, na.rm = TRUE)
  )

# 计算 total_nodes 和 influenced_population
complete_tree_demor$total_nodes <- (complete_tree_demor$b^(complete_tree_demor$h + 1) - 1) / (complete_tree_demor$b - 1)
complete_tree_demor$influenced_population <- complete_tree_demor$total_nodes * 81.25

# Calculate the mean for each Monthly_Income category
complete_tree_demor_means <- complete_tree_demor %>%
  group_by(Monthly_Income) %>%
  summarise(mean_influenced_population = mean(influenced_population, na.rm = TRUE))

# Adjust Monthly_Income levels if necessary (this can be adjusted based on your dataset)
complete_tree_demor$Monthly_Income <- factor(complete_tree_demor$Monthly_Income, 
                                             levels = c("<25k", ">=25k"))

# Plotting influenced_population density by Monthly_Income category
ggplot(complete_tree_demor, aes(x = influenced_population, fill = Monthly_Income, color = Monthly_Income)) +
  geom_density(size = 1.2, alpha = 0.3, bw = 0.5) +  # Density plot with fill and transparency
  scale_x_log10() +  # Log scale for x-axis
  scale_fill_brewer(palette = "Set1") +  # Using a color palette
  scale_color_brewer(palette = "Set1") +  # Matching fill and line color
  labs(x = "Influenced Population (Log Scale)", y = "Density", fill = "Monthly Income", color = "Monthly Income") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 14, face = "bold"),  # Bold axis text
    axis.title = element_text(size = 16, face = "bold"),  # Bold axis title
    plot.title = element_blank(),  # No title
    legend.position = "top",  # Legend on top
    legend.text = element_text(size = 14, face = "bold"),  # Bold and larger legend text
    legend.title = element_text(size = 14, face = "bold")  # Bold and larger legend title
  ) +
  # Add vertical lines for means
  geom_vline(data = complete_tree_demor_means, aes(xintercept = mean_influenced_population, color = Monthly_Income),
             linetype = "dashed", size = 1.2, show.legend = FALSE) +
  # Annotate the means with adjusted y positions and hjust to avoid overlapping
  geom_text(data = complete_tree_demor_means, aes(x = c(30000, 10000), 
                                                  y =  c(0.8, 0.75),  # Adjusted y positions
                                                  label = paste("Mean =", round(mean_influenced_population, 2)),
                                                  color = Monthly_Income),  # Match text color with Monthly_Income
            hjust = -0.1, size = 5, show.legend = FALSE)



# Grouping by religion and calculating the mean influenced_population
religion_means <- complete_tree_demor %>%
  group_by(religion) %>%
  summarise(mean_influenced_population = mean(influenced_population, na.rm = TRUE))

# Plotting influenced_population density by religion
ggplot(complete_tree_demor, aes(x = influenced_population, fill = religion, color = religion)) +
  geom_density(size = 1.2, alpha = 0.3, bw = 0.5) +  # Density plot with fill and transparency
  scale_x_log10() +  # Log scale for x-axis
  scale_fill_brewer(palette = "Set1") +  # Using a color palette
  scale_color_brewer(palette = "Set1") +  # Matching fill and line color
  labs(x = "Influenced Population (Log Scale)", y = "Density", fill = "Religion", color = "Religion") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 14, face = "bold"),  # Bold axis text
    axis.title = element_text(size = 16, face = "bold"),  # Bold axis title
    plot.title = element_blank(),  # No title
    legend.position = "top",  # Legend on top
    legend.text = element_text(size = 14, face = "bold"),  # Bold and larger legend text
    legend.title = element_text(size = 14, face = "bold")  # Bold and larger legend title
  ) +
  # Add vertical lines for means
  geom_vline(data = religion_means, aes(xintercept = mean_influenced_population, color = religion),
             linetype = "dashed", size = 1.2, show.legend = FALSE) +
  # Annotate the means
  geom_text(data = religion_means, aes(x = mean_influenced_population, 
                                       y = c(0.8, 0.75),  # Adjusted y positions for annotations
                                       label = paste("Mean =", round(mean_influenced_population, 2)),
                                       color = religion),
            hjust = -0.1, size = 5, show.legend = FALSE)



# Grouping by caste and calculating the mean influenced_population
caste_means <- complete_tree_demor %>%
  group_by(caste) %>%
  summarise(mean_influenced_population = mean(influenced_population, na.rm = TRUE))

# Plotting influenced_population density by caste
ggplot(complete_tree_demor, aes(x = influenced_population, fill = caste, color = caste)) +
  geom_density(size = 1.2, alpha = 0.3, bw = 0.5) +  # Density plot with fill and transparency
  scale_x_log10() +  # Log scale for x-axis
  scale_fill_brewer(palette = "Set1") +  # Using a color palette
  scale_color_brewer(palette = "Set1") +  # Matching fill and line color
  labs(x = "Influenced Population (Log Scale)", y = "Density", fill = "Caste", color = "Caste") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 14, face = "bold"),  # Bold axis text
    axis.title = element_text(size = 16, face = "bold"),  # Bold axis title
    plot.title = element_blank(),  # No title
    legend.position = "top",  # Legend on top
    legend.text = element_text(size = 14, face = "bold"),  # Bold and larger legend text
    legend.title = element_text(size = 14, face = "bold")  # Bold and larger legend title
  ) +
  # Add vertical lines for means
  geom_vline(data = caste_means, aes(xintercept = mean_influenced_population, color = caste),
             linetype = "dashed", size = 1.2, show.legend = FALSE) +
  # Annotate the means
  geom_text(data = caste_means, aes(x = mean_influenced_population, 
                                    y = c(0.8, 0.75),  # Adjusted y positions for annotations
                                    label = paste("Mean =", round(mean_influenced_population, 2)),
                                    color = caste),
            hjust = -0.1, size = 5, show.legend = FALSE)

# Grouping by education and calculating the mean influenced_population
education_means <- complete_tree_demor %>%
  group_by(education) %>%
  summarise(mean_influenced_population = mean(influenced_population, na.rm = TRUE))

# Plotting influenced_population density by education
ggplot(complete_tree_demor, aes(x = influenced_population, fill = education, color = education)) +
  geom_density(size = 1.2, alpha = 0.3, bw = 0.5) +  # Density plot with fill and transparency
  scale_x_log10() +  # Log scale for x-axis
  scale_fill_brewer(palette = "Set1") +  # Using a color palette
  scale_color_brewer(palette = "Set1") +  # Matching fill and line color
  labs(x = "Influenced Population (Log Scale)", y = "Density", fill = "Education", color = "Education") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 14, face = "bold"),  # Bold axis text
    axis.title = element_text(size = 16, face = "bold"),  # Bold axis title
    plot.title = element_blank(),  # No title
    legend.position = "top",  # Legend on top
    legend.text = element_text(size = 14, face = "bold"),  # Bold and larger legend text
    legend.title = element_text(size = 14, face = "bold")  # Bold and larger legend title
  ) +
  # Add vertical lines for means
  geom_vline(data = education_means, aes(xintercept = mean_influenced_population, color = education),
             linetype = "dashed", size = 1.2, show.legend = FALSE) +
  # Annotate the means
  geom_text(data = education_means, aes(x = mean_influenced_population, 
                                        y = c(0.8, 0.75),  # Adjusted y positions for annotations
                                        label = paste("Mean =", round(mean_influenced_population, 2)),
                                        color = education),
            hjust = -0.1, size = 5, show.legend = FALSE)




# 动态调整 y 的位置，以避免重叠
y_positions <- seq(0.8, 0.75 - 0.05 * (nrow(age_means) - 1), length.out = nrow(age_means))

# 绘制 influenced_population 的密度图，根据 age 分类
ggplot(complete_tree_demor, aes(x = influenced_population, fill = age, color = age)) +
  geom_density(size = 1.2, alpha = 0.3, bw = 0.5) +  # Density plot with fill and transparency
  scale_x_log10() +  # Log scale for x-axis
  scale_fill_brewer(palette = "Set1") +  # 使用颜色调色板
  scale_color_brewer(palette = "Set1") +  # 匹配填充颜色和线条颜色
  labs(x = "Influenced Population (Log Scale)", y = "Density", fill = "Age", color = "Age") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 14, face = "bold"),  # 粗体轴标签文本
    axis.title = element_text(size = 16, face = "bold"),  # 粗体轴标题
    plot.title = element_blank(),  # 不显示标题
    legend.position = "top",  # 图例位置在顶部
    legend.text = element_text(size = 14, face = "bold"),  # 粗体图例文本
    legend.title = element_text(size = 14, face = "bold")  # 粗体图例标题
  ) +
  # 添加均值的虚线
  geom_vline(data = age_means, aes(xintercept = mean_influenced_population, color = age),
             linetype = "dashed", size = 1.2, show.legend = FALSE) +
  # 注释均值，动态调整 y 位置
  geom_text(data = age_means, aes(x = mean_influenced_population, 
                                  y = y_positions,  # 动态调整 y 位置
                                  label = paste("Mean =", round(mean_influenced_population, 2)),
                                  color = age),
            hjust = -0.1, size = 5, show.legend = FALSE)


# 计算 total_nodes 和 influenced_population
complete_dag_demor$total_nodes <- (complete_dag_demor$b^(complete_dag_demor$h + 1) - 1) / (complete_dag_demor$b - 1)
complete_dag_demor$influenced_population <- complete_dag_demor$total_nodes * 81.25
# 计算每个年龄段的均值
age_means_dag <- complete_dag_demor %>%
  group_by(age) %>%
  summarise(mean_influenced_population = mean(influenced_population, na.rm = TRUE))

# 动态调整 y 的位置，以避免重叠
y_positions_dag <- seq(0.8, 0.8 - 0.05 * (nrow(age_means_dag) - 1), length.out = nrow(age_means_dag))

# 绘制 influenced_population 的密度图，根据 age 分类
ggplot(complete_dag_demor, aes(x = influenced_population, fill = age, color = age)) +
  geom_density(size = 1.2, alpha = 0.3, bw = 0.5) +  # Density plot with fill and transparency
  scale_x_log10() +  # Log scale for x-axis
  scale_fill_brewer(palette = "Set1") +  # 使用颜色调色板
  scale_color_brewer(palette = "Set1") +  # 匹配填充颜色和线条颜色
  labs(x = "Influenced Population (Log Scale)", y = "Density", fill = "Age", color = "Age") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 14, face = "bold"),  # 粗体轴标签文本
    axis.title = element_text(size = 16, face = "bold"),  # 粗体轴标题
    plot.title = element_blank(),  # 不显示标题
    legend.position = "top",  # 图例位置在顶部
    legend.text = element_text(size = 14, face = "bold"),  # 粗体图例文本
    legend.title = element_text(size = 14, face = "bold")  # 粗体图例标题
  ) +
  # 添加均值的虚线
  geom_vline(data = age_means_dag, aes(xintercept = mean_influenced_population, color = age),
             linetype = "dashed", size = 1.2, show.legend = FALSE) +
  # 注释均值，动态调整 y 位置
  geom_text(data = age_means_dag, aes(x = mean_influenced_population, 
                                      y = y_positions_dag,  # 动态调整 y 位置
                                      label = paste("Mean =", round(mean_influenced_population-100000, 2)),
                                      color = age),
            hjust = -0.1, size = 5, show.legend = FALSE)


# 计算每个宗教的均值
religion_means_dag <- complete_dag_demor %>%
  group_by(religion) %>%
  summarise(mean_influenced_population = mean(influenced_population, na.rm = TRUE))

# 动态调整 y 的位置
y_positions_religion_dag <- seq(0.8, 0.75 - 0.05 * (nrow(religion_means_dag) - 1), length.out = nrow(religion_means_dag))

# 绘制宗教分类下的密度图
ggplot(complete_dag_demor, aes(x = influenced_population, fill = religion, color = religion)) +
  geom_density(size = 1.2, alpha = 0.3, bw = 0.5) +  # Density plot with fill and transparency
  scale_x_log10() +  # Log scale for x-axis
  scale_fill_brewer(palette = "Set1") +  # 使用颜色调色板
  scale_color_brewer(palette = "Set1") +  # 匹配填充颜色和线条颜色
  labs(x = "Influenced Population (Log Scale)", y = "Density", fill = "Religion", color = "Religion") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 14, face = "bold"),  # 粗体轴标签文本
    axis.title = element_text(size = 16, face = "bold"),  # 粗体轴标题
    plot.title = element_blank(),  # 不显示标题
    legend.position = "top",  # 图例位置在顶部
    legend.text = element_text(size = 14, face = "bold"),  # 粗体图例文本
    legend.title = element_text(size = 14, face = "bold")  # 粗体图例标题
  ) +
  # 添加均值的虚线
  geom_vline(data = religion_means_dag, aes(xintercept = mean_influenced_population, color = religion),
             linetype = "dashed", size = 1.2, show.legend = FALSE) +
  # 注释均值
  geom_text(data = religion_means_dag, aes(x = mean_influenced_population, 
                                           y = y_positions_religion_dag,  # 动态调整 y 位置
                                           label = paste("Mean =", round(mean_influenced_population, 2)),
                                           color = religion),
            hjust = -0.1, size = 5, show.legend = FALSE)


# 计算每个 caste 的均值
caste_means_dag <- complete_dag_demor %>%
  group_by(caste) %>%
  summarise(mean_influenced_population = mean(influenced_population, na.rm = TRUE))

# 动态调整 y 的位置
y_positions_caste_dag <- seq(0.8, 0.75 - 0.05 * (nrow(caste_means_dag) - 1), length.out = nrow(caste_means_dag))

# 绘制 caste 分类下的密度图
ggplot(complete_dag_demor, aes(x = influenced_population, fill = caste, color = caste)) +
  geom_density(size = 1.2, alpha = 0.3, bw = 0.5) +  # Density plot with fill and transparency
  scale_x_log10() +  # Log scale for x-axis
  scale_fill_brewer(palette = "Set1") +  # 使用颜色调色板
  scale_color_brewer(palette = "Set1") +  # 匹配填充颜色和线条颜色
  labs(x = "Influenced Population (Log Scale)", y = "Density", fill = "Caste", color = "Caste") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 14, face = "bold"),  # 粗体轴标签文本
    axis.title = element_text(size = 16, face = "bold"),  # 粗体轴标题
    plot.title = element_blank(),  # 不显示标题
    legend.position = "top",  # 图例位置在顶部
    legend.text = element_text(size = 14, face = "bold"),  # 粗体图例文本
    legend.title = element_text(size = 14, face = "bold")  # 粗体图例标题
  ) +
  # 添加均值的虚线
  geom_vline(data = caste_means_dag, aes(xintercept = mean_influenced_population, color = caste),
             linetype = "dashed", size = 1.2, show.legend = FALSE) +
  # 注释均值
  geom_text(data = caste_means_dag, aes(x = mean_influenced_population, 
                                        y = y_positions_caste_dag,  # 动态调整 y 位置
                                        label = paste("Mean =", round(mean_influenced_population, 2)),
                                        color = caste),
            hjust = -0.1, size = 5, show.legend = FALSE)


# 计算每个 education 的均值
education_means_dag <- complete_dag_demor %>%
  group_by(education) %>%
  summarise(mean_influenced_population = mean(influenced_population, na.rm = TRUE))

# 动态调整 y 的位置
y_positions_education_dag <- seq(0.8, 0.75 - 0.05 * (nrow(education_means_dag) - 1), length.out = nrow(education_means_dag))

# 绘制 education 分类下的密度图
ggplot(complete_dag_demor, aes(x = influenced_population, fill = education, color = education)) +
  geom_density(size = 1.2, alpha = 0.3, bw = 0.5) +  # Density plot with fill and transparency
  scale_x_log10() +  # Log scale for x-axis
  scale_fill_brewer(palette = "Set1") +  # 使用颜色调色板
  scale_color_brewer(palette = "Set1") +  # 匹配填充颜色和线条颜色
  labs(x = "Influenced Population (Log Scale)", y = "Density", fill = "Education", color = "Education") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 14, face = "bold"),  # 粗体轴标签文本
    axis.title = element_text(size = 16, face = "bold"),  # 粗体轴标题
    plot.title = element_blank(),  # 不显示标题
    legend.position = "top",  # 图例位置在顶部
    legend.text = element_text(size = 14, face = "bold"),  # 粗体图例文本
    legend.title = element_text(size = 14, face = "bold")  # 粗体图例标题
  ) +
  # 添加均值的虚线
  geom_vline(data = education_means_dag, aes(xintercept = mean_influenced_population, color = education),
             linetype = "dashed", size = 1.2, show.legend = FALSE) +
  # 注释均值
  geom_text(data = education_means_dag, aes(x = mean_influenced_population, 
                                            y = y_positions_education_dag,  # 动态调整 y 位置
                                            label = paste("Mean =", round(mean_influenced_population, 2)),
                                            color = education),
            hjust = -0.1, size = 5, show.legend = FALSE)


# 计算每个 Monthly_Income 的均值
income_means_dag <- complete_dag_demor %>%
  group_by(Monthly_Income) %>%
  summarise(mean_influenced_population = mean(influenced_population, na.rm = TRUE))

# 动态调整 y 的位置
y_positions_income_dag <- seq(0.8, 0.75 - 0.05 * (nrow(income_means_dag) - 1), length.out = nrow(income_means_dag))

# 绘制 Monthly_Income 分类下的密度图
ggplot(complete_dag_demor, aes(x = influenced_population, fill = Monthly_Income, color = Monthly_Income)) +
  geom_density(size = 1.2, alpha = 0.3, bw = 0.5) +  # Density plot with fill and transparency
  scale_x_log10() +  # Log scale for x-axis
  scale_fill_brewer(palette = "Set1") +  # 使用颜色调色板
  scale_color_brewer(palette = "Set1") +  # 匹配填充颜色和线条颜色
  labs(x = "Influenced Population (Log Scale)", y = "Density", fill = "Income", color = "Income") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 14, face = "bold"),  # 粗体轴标签文本
    axis.title = element_text(size = 16, face = "bold"),  # 粗体轴标题
    plot.title = element_blank(),  # 不显示标题
    legend.position = "top",  # 图例位置在顶部
    legend.text = element_text(size = 14, face = "bold"),  # 粗体图例文本
    legend.title = element_text(size = 14, face = "bold")  # 粗体图例标题
  ) +
  # 添加均值的虚线
  geom_vline(data = income_means_dag, aes(xintercept = mean_influenced_population, color = Monthly_Income),
             linetype = "dashed", size = 1.2, show.legend = FALSE) +
  # 注释均值
  geom_text(data = income_means_dag, aes(x = mean_influenced_population, 
                                         y = y_positions_income_dag,  # 动态调整 y 位置
                                         label = paste("Mean =", round(mean_influenced_population, 2)),
                                         color = Monthly_Income),
            hjust = -0.1, size = 5, show.legend = FALSE)



###wixcolon test
# 对 b 进行 Wilcoxon Rank-Sum Test
wilcox_test_b_demor <- wilcox.test(b ~ Monthly_Income, 
                                   data = complete_tree_demor %>% filter(Monthly_Income %in% c("<5k", ">=5k")),
                                   alternative = "greater")

# 对 h 进行 Wilcoxon Rank-Sum Test
wilcox_test_h_demor <- wilcox.test(h ~ Monthly_Income, 
                                   data = complete_tree_demor %>% filter(Monthly_Income %in% c("<5k", ">=5k")),
                                   alternative = "greater")

# 输出结果
print("Wilcoxon Rank-Sum Test for b in complete_tree_demor:")
print(wilcox_test_b_demor)

print("Wilcoxon Rank-Sum Test for h in complete_tree_demor:")
print(wilcox_test_h_demor)


# 对 b 进行 Wilcoxon Rank-Sum Test
wilcox_test_b_dag <- wilcox.test(b ~ Monthly_Income, 
                                 data = complete_dag_demor %>% filter(Monthly_Income %in% c("<5k", ">=5k")),
                                 alternative = "greater")

# 对 h 进行 Wilcoxon Rank-Sum Test
wilcox_test_h_dag <- wilcox.test(h ~ Monthly_Income, 
                                 data = complete_dag_demor %>% filter(Monthly_Income %in% c("<5k", ">=5k")),
                                 alternative = "greater")

# 输出结果
print("Wilcoxon Rank-Sum Test for b in complete_dag_demor:")
print(wilcox_test_b_dag)

print("Wilcoxon Rank-Sum Test for h in complete_dag_demor:")
print(wilcox_test_h_dag)




#tree_based_on_network
g_group_adj_matrix <- as.matrix(adj_matrix <- read.csv("D:/user/Research_1_social_learning/code/R_whatsapp_250428/data/g_group_adj_matrix.csv", row.names = 1, check.names = FALSE))

for (i in 1:nrow(complete_tree)) {
  
  cascade <- complete_tree$cascade_id[i]
  
  # 提取当前 cascade_id 对应的子树
  cascade_tree <- partial_trees[partial_trees$cascade_id == cascade, ]
  
  # 创建图对象
  g <- graph_from_data_frame(cascade_tree, directed = TRUE)
  
  # 节点数
  complete_tree$node_count[i] <- vcount(g)
  
  # 非叶节点
  non_leaf_nodes <- V(g)[degree(g, mode = "out") > 0]
  
  outdegree_list <- c()
  for (v in non_leaf_nodes$name) {
    children <- neighbors(g, v, mode = "out")$name
    
    # 检查哪些边 (v -> child) 在邻接矩阵中存在
    valid_children <- sapply(children, function(child) {
      if (v %in% rownames(g_group_adj_matrix) && child %in% colnames(g_group_adj_matrix)) {
        g_group_adj_matrix[v, child] == 1
      } else {
        FALSE
      }
    })
    
    
    outdegree_list <- c(outdegree_list, sum(valid_children))
  }
  
  complete_tree$outdegree_avg[i] <- if (length(outdegree_list) > 0) {
    mean(outdegree_list)
  } else {
    0
  }
}
