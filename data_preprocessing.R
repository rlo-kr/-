library(data.table)
library(tidyverse)
library(readxl)
library(magrittr)
library(rgdal)

#setwd("C:/Users/seses/Desktop/Shiny/App")

#### 지도 데이터 ---------------------------------------------------------------------
## 지도 데이터와 결합 
seoul_map <- fread("./data/seoul_map.csv")

p <- ggplot() + 
  geom_polygon(data = seoul_map, aes(x = long, y = lat, group = group),
               fill = "grey", color = 'lightgrey', alpha = 0.1) +
  theme(panel.background = element_rect(fill='white', color='white'),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        strip.background = element_rect(fill="white", color="darkgrey"),
        legend.title=element_text(size=10),
        strip.text = element_text(face="bold")) +
  labs(fill = " ")

#### 행정동 코드데이터 ---------------------------------------------------------------------
## 필요한 열만 추출 
dong_code <- readxl::read_excel("./data/dong_code.xlsx", sheet = 1, col_names = T)

dong_code %<>% select(-c(행자부행정동코드)) %>%
  filter(시도명 == "서울") %>%
  drop_na() %>%
  select(-c(시도명))

#### 지하철 데이터 -------------------------------------------------------------------
subway <- fread("./data/subway.csv")
gyeong <- fread("./data/gyeong.csv")
chun <- fread("./data/chun.csv")
airport <- fread("./data/airport.csv")
bundang <- fread("./data/bundang.csv")
ui <- fread("./data/ui.csv")

gyeong %<>% select(역명, 선명, gu, dong, x, y) %>% 
  rename(전철역명 = 역명, 호선 = 선명)

chun %<>% select(역명, 선명, gu, dong, 경도, 위도) %>% 
  rename(전철역명 = 역명, 호선 = 선명, x = 경도, y = 위도)

airport %<>% select(역명, 선명, gu, dong, 경도, 위도) %>% 
  rename(전철역명 = 역명, 호선 = 선명, x = 경도, y = 위도)

bundang %<>% select(역명, 선명, gu, dong, 경도, 위도) %>% 
  rename(전철역명 = 역명, 호선 = 선명, x = 경도, y = 위도)

ui %<>% select(역명, 선명, gu, dong, x, y) %>% 
  rename(전철역명 = 역명, 호선 = 선명)

subway %<>% rbind(gyeong) %>% 
  rbind(chun) %>% rbind(airport) %>% 
  rbind(bundang) %>% rbind(ui)

rm(airport, bundang, chun, gyeong, ui)

subway$호선 <- ifelse(subway$호선 == "경의중앙" | subway$호선 == "경춘", paste0(subway$호선, "선"),
                    ifelse(subway$호선 == "공항", "공항철도", subway$호선))

subway %<>% filter(gu != "하남시", gu != "고양시 덕양구", gu != "부천시",
                  gu != "성남시 수정구", gu != "의정부시", gu != "광명시", 
                  gu != "부평구", gu != "성남시 중원구")

color_list <- data.table(호선 =  subway$호선 %>% unique(), 
                           color = c("#0052A4", "#009D3E", "#996CAC","#747F00", "#EF7C1C",
                                     "#EA545D","#BDB092","#00A5DE", "#CD7C2F",
                                     "#77C4A3","#0C8E72", "#0065B3", "#F5A200", "#B7C452"))

subway_sum <- subway %>% group_by(gu, dong) %>% 
  summarise(역 = n())

mapping <- dong_code %>% 
  left_join(subway_sum, by = c("시군구명" = "gu", "행정동명" = "dong")) %>%
  rename(행정동코드 = 통계청행정동코드) 

mapping[is.na(mapping$역), "역"] <- 0 
mapping$행정동코드 %<>% as.numeric() 
subway_map <- mapping %>% left_join(seoul_map, by = c("행정동코드" = "id"))

#### 데이터 불러오기 -------------------------------------------------------------------

final <- fread("./data/final.csv")
candi <- fread("./data/cluster_include.csv")
candi %<>% select(-c(cluster))
prior <- fread("./data/cluster_include.csv")
prior %<>% filter(cluster == 1)
youthhouse <- fread("./data/youthhouse.csv")

save.image(file = "./data_preprocessing.RData")
