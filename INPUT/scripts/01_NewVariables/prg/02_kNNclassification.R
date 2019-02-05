# k-NNを使ってラベルを付与

library(magrittr)
library(ggplot2)
library(FNN)

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

# ---------------------------------------------------------------- k-NN

# sample
if (F) {
  data <- iris %>% dplyr::sample_n(150)
  res <- FNN::knn(train = data[1:100, -5], 
                  test = data[101:150, -5], 
                  cl = data[1:100, 5], 
                  k=1,prob=T,algorithm="kd_tree")
  
  sum(data[101:150,5] == res[1:50]) / 50
}

# k-NN対象の変数
usecols <- c("ps_reg_02", "ps_reg_03", 
             "ps_car_12", "ps_car_13", "ps_car_14", 
             "ps_ind_06_bin", "ps_ind_07_bin")

# 取りだす
id.train <- data.train["id"]
id.test <- data.test["id"]
cl <- data.train$target
data.train %<>% dplyr::select(!!usecols)
data.test %<>% dplyr::select(!!usecols)

### -1はNAなので中央値で置換
df.median <- data.train %>% purrrlyr::dmap(function(x) median(x, na.rm=T)) %>% data.frame
df.mean <- data.train %>% purrrlyr::dmap(function(x) mean(x, na.rm=T)) %>% data.frame
df.sd <- data.train %>% purrrlyr::dmap(function(x) sd(x, na.rm=T)) %>% data.frame

data.train %<>% purrrlyr::dmap(function(x) dplyr::na_if(x, -1))
data.test %<>% purrrlyr::dmap(function(x) dplyr::na_if(x, -1))

for (nm in names(data.train)) {
  # insample
  index <- is.na(data.train[,nm])
  data.train[index, nm] <- df.median[1,nm]
  
  # outsample
  index <- is.na(data.test[,nm])
  data.test[index, nm] <- df.median[1,nm]
}

### 正規化する
for (nm in names(data.train)) {
  # insample
  data.train[, nm] %<>% add(-1 * df.mean[1,nm]) %>% divide_by(df.sd[1,nm])
  
  # outsample
  data.test[, nm] %<>% add(-df.mean[1,nm]) %>% divide_by(df.sd[1,nm])
}

### k-NN
data.train %<>% data.frame
data.test %<>% data.frame

res_3 <- FNN::knn(train = data.train, 
                test = rbind(data.train, data.test), 
                cl = cl, 
                k=3,
                prob=T,
                algorithm="kd_tree")

res_5 <- FNN::knn(train = data.train, 
                  test = rbind(data.train, data.test), 
                  cl = cl, 
                  k=5,
                  prob=T,
                  algorithm="kd_tree")

res_9 <- FNN::knn(train = data.train, 
                  test = rbind(data.train, data.test), 
                  cl = cl, 
                  k=9,
                  prob=T,
                  algorithm="kd_tree")

# 各kNNの結果
p.kNN_3 <- attributes(res_3)$prob
p.kNN_5 <- attributes(res_5)$prob
p.kNN_9 <- attributes(res_9)$prob

# kNNの結果df
index <- c(1:nrow(data.train))
df.train <- id.train %>% 
  dplyr::mutate(kNN_3_prob = p.kNN_3[index], 
                kNN_5_prob = p.kNN_5[index],
                kNN_9_prob = p.kNN_9[index])

index <- c((nrow(data.train)+1):length(p.kNN))
df.test <- id.test %>% 
  dplyr::mutate(kNN_3_prob = p.kNN_3[index], 
                kNN_5_prob = p.kNN_5[index],
                kNN_9_prob = p.kNN_9[index])

# ---------------------------------------------------------------- 出力
write.table(df.train, file = "../out/kNNtrain.csv", sep = ",", na = "", row.names = F)  
write.table(df.test, file = "../out/kNNtest.csv", sep = ",", na = "", row.names = F)  





