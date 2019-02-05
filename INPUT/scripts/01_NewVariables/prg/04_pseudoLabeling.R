# PDの高い先を追加のサンプルとする
data <- "../../../in/submitted/171127/submission_dataframe01.csv" %>% 
  readr::read_csv()

backup <- data
data <- backup

data %<>% dplyr::mutate(pred_mean = (pred_00 + pred_01 + pred_02 + pred_03 + pred_04 + 
                                      pred_05 + pred_06 + pred_07 + pred_08 + pred_09) / 10) %>% 
  dplyr::arrange(pred_mean)

data %<>% data.frame
ID.asdef <- data %>% 
  dplyr::filter(pred_mean > 0.14 | pred_mean < 0.01) %>%
  dplyr::mutate(target = dplyr::if_else((pred_mean < 0.03), 0, 1)) %>% 
  dplyr::arrange(id) %>% 
  dplyr::select(id, target, pred_mean)

write.table(ID.asdef, file = "../out/pseudolabel.csv", sep = ",", na = "", row.names = F)
