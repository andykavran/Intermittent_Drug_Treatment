# Figure 4 PCA plot and heatmap
# Also generates supplmental tables 1 -

## Environment 
library(tidyverse)
library(DESeq2)
library(BiocParallel)
library(magrittr)
library(cowplot)
library(RColorBrewer)
library(pheatmap)
source("code/Helper_Functions/theme_int_dosing.R")

## Ingest HTSeq files
directory = "/Users/Andy/Google Drive/research/intermittent_dosing/Gene_Set_Analysis/Data"
files = grep("htseq", list.files(directory), value=TRUE)
sampleCondition = factor(c(rep(1,3), rep(2,3), rep(3,3), rep(4,3), rep(5,3),
                           rep(6,3), rep(7,3), rep(8,3), rep(9,2), rep(10,3)))
sampleName = factor(c(rep("Vector", 3), rep("V600E", 3), rep("V600E_20hr_drug", 3),
                      rep("Week1_ON", 3), rep("Week2_Cont", 3), rep("Week2_Int_OFF", 3), 
                      rep("Week3_Cont", 3), rep("Week3_Int_On", 3), rep("Week4_Cont", 2), 
                      rep("Week4_Int_OFF", 3)) )

sampleTable = data.frame(sampleName = files,
                         fileName = files,
                         condition = sampleCondition
)
data = DESeqDataSetFromHTSeqCount(sampleTable = sampleTable,
                                  directory = directory,
                                  design = ~ condition)
filtered_data = data[ rowSums(counts(data)) > 60, ]

# Load in a file to map ensembl names to gene symbols.
names_file = read_tsv("/Users/Andy/Google Drive/research/intermittent_dosing/Gene_Set_Analysis/ensembl_gene_name_map.txt", quote = "")

mapped_names = match(rownames(filtered_data), names_file$Ensembl.gene.ID)
gene_names = as_tibble(names_file[mapped_names,])
gene_names %<>% dplyr::rename(ensembl = "Ensembl.gene.ID")

# Supplmental Table 1
unnormalized_counts <- counts(data, normalized = FALSE)
all_names_match <- match(rownames(unnormalized_counts), names_file$Ensembl.gene.ID)
all_gene_names <- as_tibble(names_file[all_names_match,])
table_s1 <-bind_cols(all_gene_names, unnormalized_counts%>%as_tibble)
write_csv(table_s1, "Supplemental_Table_1.csv")

#Variance stabilizaing transformation
vst_our_data <- filtered_data %>% vst(blind = TRUE) %>% assay %>% as_tibble()
variances <- vst_our_data %>% as.matrix %>% rowVars()
vst_our_data %<>% add_column(gene = gene_names$symbol, 
                             ensembl = gene_names$ensembl,
                             variance = variances,
                             .before = 1)
# Supplmental Table 2
table_2_data <- vst_our_data %>% add_column(full_name = gene_names$name, .after = "ensembl")
write_csv(table_2_data, "Supplemental_Table_2.csv")

# Now Run PCA on subset of genes setting scale and center to be TRUE
subset_vst_our_data <- vst_our_data %>% slice_max(variances, n = 6000)
top_6000_gene_names <- subset_vst_our_data[,1:2]
pca_output_all <- subset_vst_our_data[,4:32] %>% t %>% prcomp(center = TRUE, scale = TRUE)
summary(pca_output_all)

## PLOT PCA PLOT
pca_plot_data <- tibble(pc1 = pca_output_all$x[,1],  
                        pc2 = -pca_output_all$x[,2],
                        sample_name = sampleName)

ggplot(pca_plot_data, aes(x = pc1, y = pc2, fill = sample_name, shape = sample_name, label = sample_name))+
  geom_point(size = 2)+
  scale_shape_manual(values = c(rep(23, 3), rep(24, 2), 21, 24, 21, 24, 21))+
  scale_fill_manual(values = c( "#161616","#4E4187", "#888888", "#fddbc7", "#f4a582", "#92c5de", "#d6604d", "#4393c3", "#b2182b", "#2166ac"))+
  labs(x = "PC1\n38.6% Variance Explained",
       y = "PC2\n28.9% Variance Explained")+
  theme_int_dosing(legend = "none", base_size = 12, aspect = 1)
ggsave("results/fig4_b_pca_plot.pdf", width = 3, height =3, dpi = 600)

## IDENTIFY PC1 AND PC2 ASSOCIATED GENES
pca_gene_loadings <- tibble(gene = subset_vst_our_data$gene,
                            ensembl = subset_vst_our_data$ensembl,
                            pc1_loading = pca_output_all$rotation[,1],
                            pc2_loading = -pca_output_all$rotation[,2])

n_genes = 400

top_pc1_genes_high <- pca_gene_loadings %>% dplyr::slice_max(n=n_genes, order_by = pc1_loading)
top_pc1_genes_low <-pca_gene_loadings %>% dplyr::slice_min(n=n_genes,  order_by = pc1_loading)
top_pc1_genes <- bind_rows(top_pc1_genes_low, top_pc1_genes_high)


top_pc2_genes_high <- pca_gene_loadings %>% dplyr::slice_max(n=n_genes, order_by = pc2_loading)
top_pc2_genes_low <-pca_gene_loadings %>% dplyr::slice_min(n=n_genes,  order_by = pc2_loading)
top_pc2_genes <- bind_rows(top_pc2_genes_low, top_pc2_genes_high)


# check if any genes overlap between the PCs
# depending on the cut-off above there may be no genes belonging to both PCs
pc1_only_genes <- setdiff(top_pc1_genes$ensembl, top_pc2_genes$ensembl)
pc2_only_genes <- setdiff(top_pc2_genes$ensembl, top_pc1_genes$ensembl)
pc_both = intersect(top_pc2_genes$ensembl, top_pc1_genes$ensembl)
pc_both %>% length()

# do some filtering of the full data set to get only PC genes
pc1_gene_data <- vst_our_data %>% filter(ensembl %in% pc1_only_genes) %>%
  add_column(category = "PC1", .before = 3)
pc1_gene_data <- left_join(pc1_gene_data, pca_gene_loadings)
pc1_gene_data <- relocate(pc1_gene_data, pc1_loading, pc2_loading, .after = variance)

pc2_gene_data <- vst_our_data %>% filter(ensembl %in% pc2_only_genes) %>%
  add_column(category = "PC2", .before = 3)
pc2_gene_data <- left_join(pc2_gene_data, pca_gene_loadings)
pc2_gene_data <- relocate(pc2_gene_data, pc1_loading, pc2_loading, .after = variance)

# assemble the genes into the appropriate format for plotting heatmaps
pc1_gene_data_order<- pc1_gene_data %>% select(c(6:34)+1) %>%t %>%
  scale(center = TRUE, scale = TRUE) %>% t %>% dist %>% hclust(method = "ward.D2")

pc2_gene_data_order<- pc2_gene_data %>% select(c(6:34)+1) %>%t %>% 
  scale(center = TRUE, scale = TRUE) %>% t %>% dist %>% hclust(method = "ward.D2")

plot_pc_genes <- bind_rows(pc1_gene_data[rev(pc1_gene_data_order$order),],
                           pc2_gene_data[rev(pc2_gene_data_order$order),])

plot_pc_genes2<- plot_pc_genes %>% select(c(6:14, 15:20, 24:26, 30:31, 21:23, 27:29, 32:34)+1) %>% as.data.frame
rownames(plot_pc_genes2) <- plot_pc_genes$ensembl


#define an appropriate color map palette for the heatmaps
rdbu <- rev(brewer.pal(11, "RdBu"))
color_map <- colorRampPalette(rdbu)
rdbu_color_map <- color_map(101)
columns_order <- c(1:15, 19:21,25:26, 16:18, 22:24, 27:29)

## PLOT HEATMAP
plot_pc_genes2 %>% pheatmap(labels_col = sampleName[columns_order],
                            cluster_rows = FALSE, 
                            cluster_cols = FALSE, 
                            scale = "row",
                            show_rownames = FALSE,
                            show_colnames = FALSE,
                            border_color = NA,
                            gaps_row = (n_genes*2), 
                            gaps_col = c(9, 20),
                            filename = "results/fig4_c_PC1_PC2_genes_heatmap.pdf",
                            width = 3.28,
                            height = 2.2,
                            color = rdbu_color_map)