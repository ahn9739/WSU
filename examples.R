# 필요한 패키지 load
library(readr)      # read_file(), read_csv(), ...
library(stringr)    # str_replace_all(), str_squish()
library(dplyr)      # %>%, select(), as_tibble(), mutate(), count(), filter(), arrange(), ...   
library(tidyverse)  # package for data science  ( https://www.tidyverse.org/ )
library(tidytext)   # unnest_tokens(), cast_dtm()   

library(tm)
library(data.table)

library(rvest)      #웹 스크래핑(Web Scraping)을 위한 패키지
library(httr)

library(wordcloud)  # wordcloud()

library(pdftools)   # pdf 화일을 읽어오기 위한 패키지 
library(N2H4)       # 네이버 뉴스 기사에 대한 댓글, 공감수, 반감수 등 수집

library(KoNLP)      # useSejongDic(), extractNoun(), useNIADic()  
library(RcppMeCab)  # pos()


################################################################################
####### Example 1 : 대통령 취임사의 명사 빈도 비교

# (step 1) 원시 데이터 read 
raw_moon = read_file("19_jiMoon(2017).txt")
raw_yoon = read_file("20_syYoon(2022).txt")

# (step 2) 원시 데이터를 사전처리하고 티블구조 data set으로 만듬 
make_DataSet <- function(raw_data, pname) {
  data <- raw_data %>%
    str_replace_all("[^가-힣 & ^.]", " ") %>%  # 한글, . 만 남기기
    str_replace_all("[\r\t\n]", "") %>%        # \r\t\n 제거
    str_squish() %>%                           # 연속된 공백 제거
    as_tibble() %>%                            # tibble로 변환
    mutate(president=pname)
  
  return (data)
}

moon = make_DataSet(raw_moon, "moon")
yoon = make_DataSet(raw_yoon, "yoon")

# (step 3) 데이터 합치고 명사 단위 토큰화, 빈도수 계산
to_Tokenize = function(data1, data2) {
  bindingData <- bind_rows(data1, data2) %>%
    select(president, value)
  
  word_space = pos(sentence = bindingData$value, format = "data.frame") # 문장의 형태소 분리
  
  word_space = word_space %>% 
    filter(pos == "NNG") %>%               # 명사만 추출
    select(doc_id, token)
  
  colnames(word_space) <-c('president', 'word')
  word_space$president = ifelse(word_space$president==1, 'moon', 'yoon')
  
  
  # 빈도수로 정렬, 두 글자 이상만 남기기
  word_space <- word_space %>%
    count(president, word, sort = T) %>%  # 대통령별 단어별 빈도
    filter(str_count(word) > 1)           # 두 글자 이상 추출
  
  return (word_space)
}

noun_space = to_Tokenize(moon, yoon)

# (step 4) 출현 빈도가 높은 10개 명사만 추출
get_TopFreqWord = function(word_space) {
  top10 <- word_space %>%
    group_by(president) %>%             # word_space를 president별로 분리
    slice_max(n, n=10, with_ties = F)   # 빈도가 높은 상위 10개 단어 선택(빈도 동점 단어 제외)
  
  return (top10)
}

noun_top10 = get_TopFreqWord(noun_space)

# (step 5) 단어 빈도 막대그래프 표현
make_BarChart = function(top10) {
  ggplot(top10, aes(x = reorder_within(word, n, president),
                    y = n,
                    fill = president)) +
    geom_col() +
    coord_flip() +
    facet_wrap(~ president, scales = "free_y") +
    scale_x_reordered() +                               # 항목 이름 제거 
    labs(x = NULL) +                                    # x축 이름 삭제
    theme(text = element_text(family = "mono"))         # 폰트
}

make_BarChart(noun_top10)  

# (step 6) 워드 클라우드 : 출현 단어 워드 클라우드  
make_WordCloud = function(noun_space) {
  
  n_p = 2
  p = c("moon", "yoon")
  
  pal <- brewer.pal(4, "Dark2")
  
  for (i in 1:n_p) {
    freq = noun_space %>% 
      filter(president==p[i])
    
    layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
    par(mar=rep(0, 4))
    plot.new()
    text(x=0.5, y=0.5, cex=2, p[i])
    
    wordcloud(freq$word, freq=freq$n, scale=c(10, 0.1), rot.per=0.0, min.freq=1, 
              random.order=FALSE, col=pal)
  }  
}

make_WordCloud(noun_space) 


################################################################################
####### Example 2 : 뉴스 기사에 대한 댓글 정보 수집

url = "https://n.news.naver.com/mnews/article/001/0013299250?sid=100"

# (step 1) 뉴스 기사 정보 읽기
cont = N2H4::getContent(url)
cont$title
cont$body


# (step 2) 뉴스 기사에 대한 댓글 정보 읽기
comm <- N2H4::getAllComment(url)
df = data.frame(comm$contents, comm$sympathyCount, comm$antipathyCount)

#빈줄 있을 때 제거하기 위한 code
df = df %>% 
  filter(nchar(df$comm.contents)!=0)

# (step 3) 화일로 저장
write.csv(df, "naver_comm.csv", row.names=FALSE)
#write.csv(df, "naver_comm.csv", row.names=FALSE, fileEncoding="cp949")

# (step 4) 화일 내용 확인
ds = read_csv("naver_comm.csv") # tibble 구조로 read
ds



################################################################################
####### Example 3 : 학교 홈페이지 게시물 제목 읽어 data set 생성 

code_url <- "https://www.woosuk.ac.kr/main/?menu=183&"

# (step 1) 게시물 제목 읽기
url <- paste0(code_url, "pno=", 1)   #page
html <- read_html(url)

title <- html %>%
  html_nodes("td.left") %>%
  html_nodes("a") %>%
  html_text() 
title  

# (step 2) 사전처리
title <- gsub("[\r\n\t]","", title) #r태그, t태그 제거

#빈줄 있을 때 제거하기 위한 code
title2 = c()
for(i in 1:length(title)){
  if(nchar(title[i]) > 1) {
    title2 = append(title2, title[i])
  }
}
title2

# (step 3) data set 생성 및 저장
df = data.frame(title2)  # 데이터 프레임 생성
df = df %>% 
  mutate(id = row_number()) %>%
  select (id, title2)

setnames(df, "title2", "post_title") # 변수 이름 변경
write.csv(df, "woosuk_arbeit_post.csv", row.names=FALSE)
#write.csv(df, "woosuk_arbeit_post.csv", row.names=FALSE, fileEncoding="cp949")

# (step 4) 화일 내용 확인
ds = read_csv("woosuk_arbeit_post.csv") # tibble 구조로 read
ds



################################################################################
####### Example 4 : 학교 홈페이지 게시물 제목 읽어 data set 생성 

url <- "https://encykorea.aks.ac.kr/Article/E0033776"
html <- read_html(url)

# (step 1) raw_data 읽기
raw_cont <- html %>%
  html_nodes("div.section-body") %>%
  html_nodes("p") %>%
  html_text() 
raw_cont  

cont <- c()
for (i in 1:length(raw_cont)) {
  cont <- paste(cont, raw_cont[i], collapse=" ")
  cont <- gsub("[\r\t\n]", "", cont)
}

# (step 2) 사전처리(생략) 및 티블 구조로 변환 
cont = as_tibble(cont) 

# (step 3) KoNLP 패키지 extractNoun() 함수를 이용한 명사 추출(토큰화)
word_noun <- extractNoun(cont)
word_noun <- as_tibble(word_noun)
colnames(word_noun) = "noun"  
  
  # 자주 사용된 단어, 두 글자 이상만 남기기
noun_space <- word_noun %>%
  count(noun, sort = T) %>%
  filter(str_count(noun) > 1)

# (step 4) 출현 빈도가 높은 명사 추출
top20 <- noun_space %>%
    head(20)               # 빈도가 높은 상위 20개 단어
  
# (step 5) 출현빈도 높은 명사 시각화
# 막대그래프
ggplot(top20, aes(x = reorder(noun, n), y = n)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = n), hjust = -0.3) +            # 막대 밖 빈도 표시
  labs(title = "심리학 설명 글의 명사 빈도",           # 그래프 제목
       x = NULL, y = NULL) +                           # 축 이름 삭제
  theme(title = element_text(size = 12))               # 제목 크기


# 워드 클라우드
pal <- brewer.pal(6, "Dark2")
windowsFonts(malgun=windowsFont("맑은 고딕"))
  
set.seed(1000)
wordcloud(words=noun_space$noun, freq=noun_space$n, colors=pal, min.freq=1, random.order=F, family="mono")

