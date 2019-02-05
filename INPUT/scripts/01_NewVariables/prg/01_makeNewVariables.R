# 説明変数を作成する
library(magrittr)
library(ggplot2)

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


# ---------------------------------------------------------------- 説明変数作成

df.train <- data.train %>% dplyr::select(id)
df.test <- data.test %>% dplyr::select(id)


# sum_ind_bin
# indかつbinが名前に入っている変数の和
varnms <- coltypes %>% 
  dplyr::filter(category == "ind" & type == "bin") %$% nm

make_sum_ind_bin <- function(df, f.data, varnms) {
  df$sum_ind_bin <- f.data %>% 
    dplyr::select(!!varnms) %>% 
    rowSums
  return(df)
}
df.train %<>% make_sum_ind_bin(., f.data = data.train, varnms = varnms)
df.test %<>% make_sum_ind_bin(., f.data = data.test, varnms = varnms)


# count_NA
# NA(-1)が入力された列の数
make_count_NA <- function(df, f.data) {
  df$count_NA <- f.data %>% 
    purrrlyr::dmap(function(x) dplyr::if_else((x == -1), 1, 0)) %>% 
    rowSums
  return(df)
}
df.train %<>% make_count_NA(., f.data = data.train)
df.test %<>% make_count_NA(., f.data = data.test)


# ps_ind_03_rev
# 0を-1に置換
make_ps_ind_03_rev <- function(df, f.data) {
  temp <- f.data$ps_ind_03
  index <- (temp == 0)
  temp[index] <- -1

  df$ps_ind_03_rev <- temp 

  return(df)
}
df.train %<>% make_ps_ind_03_rev(., f.data = data.train)
df.test %<>% make_ps_ind_03_rev(., f.data = data.test)

# ps_ind_03_NA
# ps_ind_03が0ならば1
make_ps_ind_03_NA <- function(df, f.data) {
  df$ps_ind_03_NA <- f.data %$% 
    ifelse(test = (ps_ind_03 == 0), yes = 1, no = 0)
  return(df)
}
df.train %<>% make_ps_ind_03_NA(., f.data = data.train)
df.test %<>% make_ps_ind_03_NA(., f.data = data.test)

# ps_ind_05_rev
# 0, -1, それ以外に置換
make_ps_ind_05_rev <- function(df, f.data) {
  temp <- f.data %$% 
    dplyr::case_when(
      ps_ind_05_cat == 0  ~ "n_0", 
      ps_ind_05_cat == -1 ~ "n_NA", 
                 TRUE ~ "n_other"
    )
  
  df$ps_ind_05_rev <- temp 
  
  return(df)
}
df.train %<>% make_ps_ind_05_rev(., f.data = data.train)
df.test %<>% make_ps_ind_05_rev(., f.data = data.test)


# ps_reg_02_rev
# 0を-1に置換
make_ps_reg_02_rev <- function(df, f.data) {
  temp <- f.data$ps_reg_02
  index <- (temp == 0)
  temp[index] <- -1
  
  df$ps_reg_02_rev <- temp 

  return(df)
}
df.train %<>% make_ps_reg_02_rev(., f.data = data.train)
df.test %<>% make_ps_reg_02_rev(., f.data = data.test)

# ps_reg_02_NA
# ps_reg_02が0ならば1
make_ps_reg_02_NA <- function(df, f.data) {
  df$ps_reg_02_NA <- f.data %$% 
    ifelse((ps_reg_02 == 0), yes = 1, no = 0)
  return(df)
}
df.train %<>% make_ps_reg_02_NA(., f.data = data.train)
df.test %<>% make_ps_reg_02_NA(., f.data = data.test)


# ps_reg_03_NA
make_ps_reg_03_NA <- function(df, f.data) {
  df$ps_reg_03_NA <- f.data %$% 
    dplyr::if_else((ps_reg_03 < -0.99), 1, 0)
  return(df)
}
df.train %<>% make_ps_reg_03_NA(., f.data = data.train)
df.test %<>% make_ps_reg_03_NA(., f.data = data.test)


# ps_car_03_rev
# ps_car_03の-1を新たなカテゴリ(2)とする
make_ps_car_03_rev <- function(df, f.data) {
  temp <- f.data$ps_car_03_cat
  index <- (temp == -1)
  temp[index] <- 2
  
  df$ps_car_03_rev <- temp
  return(df)
}
df.train %<>% make_ps_car_03_rev(., f.data = data.train)
df.test %<>% make_ps_car_03_rev(., f.data = data.test)


# ps_car_04_not0
# ps_car_04が0でなければ1
make_ps_car_04_not0 <- function(df, f.data) {
  df$ps_car_04_not0 <- f.data %$% 
    dplyr::if_else((ps_car_04_cat != 0), 1, 0)
  return(df)
}
df.train %<>% make_ps_car_04_not0(., f.data = data.train)
df.test %<>% make_ps_car_04_not0(., f.data = data.test)


# ps_car_05_rev
# ps_car_05の-1を新たなカテゴリ(2)とする
make_ps_car_05_rev <- function(df, f.data) {
  temp <- f.data$ps_car_05_cat
  index <- (temp == -1)
  temp[index] <- 2
  
  df$ps_car_05_rev <- temp
  return(df)
}
df.train %<>% make_ps_car_05_rev(., f.data = data.train)
df.test %<>% make_ps_car_05_rev(., f.data = data.test)


# ps_car_06_PD
# カテゴリごとのPDに変換する
df.PD <- data.train %>% 
  dplyr::group_by(ps_car_06_cat) %>% 
  dplyr::summarise(PD = sum(target) / length(target), 
                   sd = sqrt(sum(target) / length(target) * (1.0 - sum(target) / length(target)) / length(target)), 
                   N  = length(target)) %>% 
  dplyr::arrange(PD) %>% 
  data.frame

make_ps_car_06_PD <- function(df, f.data, f.PD) {
  df$ps_car_06_PD <- f.data %$%
    factor(x = ps_car_06_cat, levels = f.PD$ps_car_06_cat, labels = f.PD$PD) %>% 
    as.character %>% 
    as.numeric
  return(df)
}
df.train %<>% make_ps_car_06_PD(., data.train, df.PD)
df.test %<>% make_ps_car_06_PD(., data.test, df.PD)


# ps_car_11_PD
# カテゴリごとのPDに変換する
df.PD <- data.train %>% 
  dplyr::group_by(ps_car_11_cat) %>% 
  dplyr::summarise(PD = sum(target) / length(target), 
                   sd = sqrt(sum(target) / length(target) * (1.0 - sum(target) / length(target)) / length(target)), 
                   N  = length(target)) %>% 
  dplyr::arrange(PD) %>% 
  data.frame

make_ps_car_11_PD <- function(df, f.data, f.PD) {
  df$ps_car_11_PD <- f.data %$%
    factor(x = ps_car_11_cat, levels = f.PD$ps_car_11_cat, labels = f.PD$PD) %>% 
    as.character %>% 
    as.numeric
  return(df)
}
df.train %<>% make_ps_car_11_PD(., data.train, df.PD)
df.test %<>% make_ps_car_11_PD(., data.test, df.PD)


# ps_car_07_rev
# ps_car_07の-1を新たなカテゴリ(2)とする
make_ps_car_07_rev <- function(df, f.data) {
  temp <- f.data$ps_car_07_cat
  index <- (temp == -1)
  temp[index] <- 2
  
  df$ps_car_07_rev <- temp
  return(df)
}
df.train %<>% make_ps_car_07_rev(., f.data = data.train)
df.test %<>% make_ps_car_07_rev(., f.data = data.test)


# ps_car_12_NA
# -1が一つだけなので凍結
# make_ps_car_12_NA <- function(df, f.data) {
#   df$ps_car_12_NA <- f.data %$% 
#     dplyr::if_else((ps_car_12 < -0.99), 1, 0)
#   return(df)
# }
# df.train %<>% make_ps_car_12_NA(., f.data = data.train)
# df.test %<>% make_ps_car_12_NA(., f.data = data.test)

# ps_car_14_NA
make_ps_car_14_NA <- function(df, f.data) {
  df$ps_car_14_NA <- f.data %$% 
    dplyr::if_else((ps_car_14 < -0.99), 1, 0)
  return(df)
}
df.train %<>% make_ps_car_14_NA(., f.data = data.train)
df.test %<>% make_ps_car_14_NA(., f.data = data.test)

# kNN_X_prob
kNN.train <- readr::read_csv("../out/kNNtrain.csv")
kNN.test <- readr::read_csv("../out/kNNtest.csv")
df.train %<>% dplyr::left_join(., kNN.train, by = c("id"))
df.test %<>% dplyr::left_join(., kNN.test, by = c("id"))

# k-meansのクラスタ
kmeans.train <- readr::read_csv("../out/kmeanstrain.csv")
kmeans.test <- readr::read_csv("../out/kmeanstest.csv")
df.train %<>% dplyr::left_join(., kmeans.train, by = c("id"))
df.test %<>% dplyr::left_join(., kmeans.test, by = c("id"))

if (F) {
  # car_13_X_reg_03
  temp <- data.train %>% 
    dplyr::mutate(car_13_X_reg_03 = ps_car_13 * ps_reg_03) %>% 
    dplyr::select(target, car_13_X_reg_03)
  
  temp$target %<>% as.factor
  ggplot(temp, aes(x=car_13_X_reg_03, y=..density.., fill=target)) %>% 
    add(geom_histogram(alpha = 0.5, position = "identity", bins = 100)) %>% 
    add(xlim(-3, 4)) %>% 
    print
}


# ---------------------------------------------------------------- 出力
outpath <- "../out/newtrain.csv"
write.table(df.train, file = outpath, sep = ",", na = "", row.names = F)

outpath <- "../out/newtest.csv"
write.table(df.test, file = outpath, sep = ",", na = "", row.names = F)




