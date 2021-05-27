library(shiny)
library(ggplot2)
library(tidyverse)
library(cluster)
library(stats)
library(factoextra)

#setwd("C:/Users/seses/Desktop/대학/2021-1/대용량자료관리및시각화/Project/data/Shiny/")

load("./data_preprocessing.RData")

# Definne UI ---
ui <- fluidPage(
  titlePanel("서울시 역세권 청년주택 우선입지 선정"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("노선별 지하철역 위치"),
      
      selectInput("subway_station",
                  label = "지하철호선 선택",
                  choices = c("01호선", "02호선", 
                              "03호선", "04호선",
                              "05호선", "06호선",
                              "07호선", "08호선",
                              "09호선", "경의중앙선",
                              "경춘선", "공항철도","신분당선","우이신설"),
                  selected = "01호선"),
      
      checkboxInput("subway_fill",
                    label = "행정동별 분포", value = FALSE), 
      img(src = "p06020100_03.gif", height = 200, width = 200)
    ),
    mainPanel(
      h3(strong("노선별 역의 위치 및 행정동별 역의 분포")),
      p("노선별로 서울 내에 위치한 역의 위치를 알 수 있습니다.
        또한 체크박스 클릭 시 행정동 별 역의 분포를 알 수 있으며, 이 때 색이 진할수록 많은 역이 존재하는 것입니다."),
      plotOutput("subway_map"))
  ),
  
  hr(),
  
  sidebarLayout(
    sidebarPanel(
      helpText("변수별 지도 시각화"),
      
      radioButtons("variables",
                   label = "변수 선택:",
                   choices = c("청년인구" = "청년인구",
                               "사업체" = "사업체수",
                               "종사자" = "종사자수",
                               "신혼부부" = "혼인",
                               "대학" = "대학 수",
                               "교통밀접도" = "교통밀접도",
                               "부동산 가격" = "완전월세액"))
    ),
    mainPanel(
      h3(strong("각 변수에 대한 행정동별 분포")),
      p("변수를 선택하면 지도 시각화를 통해 행정동별 분포를 알 수 있습니다."),
      plotOutput("variable_map"))
  ),
  
  hr(),
  
  sidebarLayout(
    sidebarPanel(
      helpText("클러스터링 시각화"),
      
      radioButtons("clustering", 
                   label = "클러스터링 기법 선택:",
                   choices = c("k-means" = 'kmeans',
                               "k-medoids" = "pam")),
      
      sliderInput("num_cluster",
                  label = "클러스터 개수 선택",
                  min = 2, max = 10, value = 3
        
      )
    ),
    
    mainPanel(
      h3(strong("클러스터링 기법별 시각화 및 테이블")),
      p("두 가지의 클러스터링 기법에 대해 클러스터 개수별 결과를 시각화하여 보여주고, 
        각 클러스터의 통계값을 테이블로 나타냅니다. 
        클러스터링 기법이 'k-means'인 경우, table에서는 각 클러스터의 평균값을 보여주고
        'k-medoids'인 경우에는 각 클러스터의 중간값을 보여줍니다."),
      
      tabsetPanel(position = "below", 
                  tabPanel("Plot", plotOutput("cluster_plot")),
                  tabPanel("Table", tableOutput("cluster_table"))
      )
    )
  ),
  
  hr(),
  
  sidebarLayout(
    sidebarPanel(
      helpText("필요점수 상위 행정동 시각화"),
      
      sliderInput("prior_site",
                  label = "행정동 개수 선택",
                  min = 1, max = 10, value = 3),
      
      checkboxInput("prior_subway",
                    label = "역 표시", value = FALSE),
      
      checkboxInput("current",
                    label = "현재 역세권 청년주택 표시", value = FALSE)
    ),
    mainPanel(
      h3(strong("필요점수 기준 상위 행정동")),
      p("본 보고서에서 최종 모델이었던 k가 3인 k-means 클러스터링을 사용 했을 때, 타겟 클러스터 내에서 필요점수를 기준으로 상위 행정동을 시각화하여 보여줍니다."),
      p("체크박스를 클릭하면, 행정동 뿐만 아니라 행정동 내에 위치한 역의 위치도 함께 볼 수 있습니다. 또한 현재 존재하는 역세권 청년주택의 위치와 비교하기 쉽도록 함께 시각화할 수 있습니다."),
      
      plotOutput("final_place")
    )
  )
)

# Define server logic ---
server <- function(input, output) {
  stationInput <- reactive({
    subway %>% filter(호선 == input$subway_station)
  })
  
  colorInput <- reactive({
    #color_list %>% filter(호선 == input$subway_station) %>% select(color)
    color_list[color_list$호선 == input$subway_station,]$color
  })
  
  fillInput <- reactive({
    if(input$subway_fill){
      return(p + 
               geom_polygon(data = subway_map, aes(x = long, y = lat, group = group, fill = 역), color = 'lightgrey') +
               scale_fill_gradient(low = "white", high = "grey50") +
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
               labs(fill = " "))
    }else{
      return(p)
    }
  })
  
  variableInput <- reactive({
    input$variables
  })
  
  clusters <- reactive({
    if (input$clustering == "kmeans"){
      return(kmeans(candi[,c(9,11)], input$num_cluster))
    } else {
      return(pam(candi[,c(9,11)], input$num_cluster))
    }
  })
  
  priorInput <- reactive({
    prior[order(prior$필요도지수, decreasing = T),] %>% head(input$prior_site) %>% 
      left_join(seoul_map, by = c("행정동코드" = "id"))
  })
  
  topsubwayInput <- reactive({
    if(input$prior_subway){
    top_subway <- subway[subway$dong %in% priorInput()$행정동,] %>% unique()
    return(
      geom_point(data = top_subway, aes(x = x, y = y), color = "black", alpha = 0.55, size = 3)
    )
    } else return(geom_point())
  })
  
  currentInput <- reactive({
    if (input$current){
      return(
        geom_point(data = youthhouse, aes(x = long, y = lat), color = "#965A9D", alpha = 0.55, size = 3))
    } else return(geom_point())
  })
  
  
  ## Output
  
  output$subway_map <- renderPlot({
    fillInput() + geom_point(data = stationInput(), aes(x, y), size = 2.5, colour = colorInput())
  })
  
  output$variable_map <- renderPlot({
    
    mapping <- final %>% select(행정구, 행정동, 행정동코드, variableInput()) %>%
      left_join(seoul_map, by = c("행정동코드" = "id"))
    
    ggplot(data = mapping, aes(x = long, y = lat, group = group, fill = get(variableInput())), color = "lightgrey") +
      geom_polygon() +
      scale_fill_gradient(low = "white", high = "#965A9D") +
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
  })
  
  output$cluster_plot <- renderPlot({
    fviz_cluster(clusters(),data=candi[,c(9,11)],geom="point",stand= F)
  })
  
  output$cluster_table <- renderTable({
    if (input$clustering == "kmeans"){
      cluster_df <- data.frame(클러스터 = c(1:input$num_cluster),clusters()$centers)
      cluster_df
    } else{
      cluster_df <- data.frame(클러스터 = c(1:input$num_cluster), clusters()$medoids)
      cluster_df
    }
  })
  
  output$final_place <- renderPlot({
    ggplot()+
      geom_polygon(data = seoul_map, aes(x = long, y = lat, group = group), fill = "white", color = "lightgrey") +
      geom_polygon(data = priorInput(), aes(x = long, y = lat, group = group), fill = "#14B9B7", color = "lightgrey", alpha = 0.8) +
      topsubwayInput() + currentInput() +
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
  })
}

# Run the app ---
shinyApp(ui = ui, server = server)
