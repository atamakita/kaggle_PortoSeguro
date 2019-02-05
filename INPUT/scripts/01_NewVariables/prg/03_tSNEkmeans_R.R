# k-meansを使ってラベルを付与
# ps_calc_XXを対象とする

library(magrittr)
library(ggplot2)
library(FNN)
library(cluster)

setwd("/Users/shunsukeakita/Park/PortoSeguro/INPUT/scripts/01_NewVariables/prg/")

# ---------------------------------------------------------------- 読み込み
datapath <- "/Users/shunsukeakita/Park/PortoSeguro/INPUT/in/data_org/"

# train
data.train <- paste0(datapath, "train.csv") %>% 
  readr::read_csv() %>% 
  data.frame

# test
data.test <- paste0(datapath, "test.csv") %>% 
  readr::read_csv() %>% 
  data.frame


coltypes <- paste0(datapath, "coltypes.csv") %>% 
  readr::read_csv() %>% 
  data.frame

# ---------------------------------------------------------------- k-means
# ps_calc_XXシリーズ
# varnms <- formatC(1:14, flag = "0", width = 2) %>% paste0("ps_calc_", .)
# 
nms_fillna <- c("ps_reg_03", "ps_car_12", "ps_car_14", "ps_reg_02")
varnms <- c("ps_ind_01", "ps_ind_03", "ps_ind_15", "ps_car_13", nms_fillna)

df.median <- data.train %>% 
  dplyr::select(!!varnms) %>%
  purrrlyr::dmap(function(x) dplyr::na_if(x, -1)) %>% 
  purrrlyr::dmap(function(x) median(x, na.rm = T)) %>% 
  data.frame

# -1を中央値で埋める
data <- data.train %>% 
  dplyr::select(-target) %>% 
  rbind(., data.test) %>% 
  dplyr::select(!!varnms)
for (nm in varnms) {
  index <- (data[,nm] == -1)
  data[index,nm] <- df.median[1,nm]
}

# res <- cluster::clusGap(x = data, FUNcluster = kmeans, K.max = 1, B = 1)

res <- kmeans(data, centers = 5)

temp <- data.train["target"] %>% 
  dplyr::mutate(cluster = res$cluster[1:nrow(data.train)])

temp %>% 
  dplyr::group_by(cluster) %>% 
  dplyr::summarise(PD = sum(target) / length(target),
                   sd = sqrt(sum(target) / length(target) * (1 - sum(target) / length(target)) / sum(target)),
                   N = length(target))

temp %>% 
  purrrlyr::dmap(as.factor) %>% 
  ggplot(., aes(x = cluster, colour = target, fill = target)) %>% 
  add(geom_bar(position = "fill")) %>% 
  print

# 出力
df.train <- data.train %>% 
  dplyr::select(id) %>% 
  dplyr::mutate(kmeans_5 = res$cluster[1:nrow(.)])
write.table(df.train, file = "../out/kmeanstrain.csv", sep = ",", na = "", row.names = F)

df.test <- data.test %>% 
  dplyr::select(id) %>% 
  dplyr::mutate(kmeans_5 = res$cluster[(nrow(data.train)+1):length(res$cluster)])
write.table(df.test, file = "../out/kmeanstest.csv", sep = ",", na = "", row.names = F)


# ---------------------------------------------------------------- t-SNE

data_sampled <- dplyr::filter(data.train, target == 1) %>% 
  dplyr::sample_n(2500)
data_sampled <- dplyr::filter(data.train, target == 0) %>% 
  dplyr::sample_n(2500) %>% 
  rbind(., data_sampled)

res <- data_sampled %>% 
  dplyr::select(!!varnms) %>% 
  as.matrix %>% 
  Rtsne::Rtsne(X = ., check_duplicates = FALSE, verbose=TRUE)

temp <- res$Y %>% 
  data.frame %>% 
  dplyr::mutate(target = data_sampled$target)

temp %>% 
  purrrlyr::dmap_at("target", as.factor) %>% 
  ggplot(., aes(x = X1, y = X2, colour = target)) %>% 
  add(geom_point()) %>% 
  print
sum(data)



