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

Make sure the following R packages are installed:

```r
install.packages(c("dplyr", "readr", "ggplot2", "stringr", "tidyr"))
```

> If you extend clustering or embedding steps, additional packages like `hdbscan`, `text`, or `topicmodels` may be needed.

---

## 🚀 How to Reproduce

### Step 1: Data Preprocessing

Run `data_preprocessing_250428.R` to prepare the message dataset for cascade analysis.  
This includes cleaning, clustering, and labeling messages by content type and modality.

### Step 2: Cascade Reconstruction & Structural Analysis

Run `reconstruct_cascade.R` to:
- Build cascade trees using `cluster_id`, `group_id`, and `timestamp`
- Estimate structural parameters:
  - **Breadth (b)**: number of branches per cascade level
  - **Height (h)**: maximum depth of the cascade
  - **Degree (k)**: total number of nodes
- Compute and visualize the complementary cumulative distribution functions (CCDFs)
- Perform Wilcoxon rank-sum tests to compare structure across message types

---

## 📄 Reference

If you use this repository, please cite our paper:

**Structural Dynamics of Harmful Content Dissemination on WhatsApp**  
arXiv: [[2505.18099](https://arxiv.org/abs/2505.18099)
](https://arxiv.org/abs/2505.18099)

### Step 3: Regression Analysis

Run `regression_for_breadth_and_depth.R` to analyze how message-level factors (e.g., content type, modality, author/group activity) influence the cascade's breadth and depth through regression models.
