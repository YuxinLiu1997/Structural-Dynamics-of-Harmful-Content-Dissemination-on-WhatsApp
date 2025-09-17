# 🧠 WhatsApp Cascade Analysis

This project analyzes the spread of different types of messages (e.g., misinformation, propaganda, hateful, and normal content) on WhatsApp by reconstructing **message cascades**, estimating **structural parameters**, and performing **regression analysis** on their dissemination patterns.

We integrate message metadata, clustering labels, and content-type annotations to uncover how different message types propagate across group chats.

---

## 📁 Repository Structure

| File | Description |
|------|-------------|
| `data_preprocessing_250428.R` | Cleans and merges message metadata, clusters messages by content, assigns content types (misinformation, hateful, propaganda, normal), and generates the final dataset for analysis. |
| `reconstruct_cascade.R` | Constructs cascades from the cleaned dataset, estimates cascade structure parameters (`b`, `h`, `k`), computes CCDFs, and performs **Wilcoxon rank-sum tests** to compare structural differences across message types. |
| `regression_for_breadth_and_depth.R` | Runs regression analysis to model the relationship between message-level covariates (e.g., group activity, author activity, modality) and the **breadth** and **depth** of message spread. |

---

## 🧪 Dependencies

### R Packages

Please install the following packages before running the scripts:

```r
install.packages(c("dplyr", "readr", "ggplot2", "stringr", "tidyr"))
