##############ライブラリ###############
library(tidyverse)
library(quanteda)
library(topicmodels)
library(modelsummary)
library(ldatuning)
library(gt)
"%not.in%" <- Negate("%in%")

##############データ読み込み###############
speech <- read_csv("data/HR_speech_all.csv") # 議事録(下院)を読み込み

##議員の情報
mem_info_HR <- read_csv("data/HR_basics/giin_info.csv") %>%
  mutate("female" = (gender=="女性"),
         "age" = (as.Date("2020-01-01") - .$誕生日)/365) %>%
  rename("name" = `氏名`, "seniority" = `当選回数`)
mem_info_HC <- read_csv("data/mem_info_HC.csv")

##トピックモデル
fit.topic <- read_rds("data/HR_topicmodel_11.rds") # 上院トピック(k=11)
fit.topic <- read_rds("data/HC_topicmodel_16.rds") # 下院トピック(k=16)

## utas
utas2019 <- read_csv("data/utas/2019UTASP20191109.csv", 
                     locale = locale(encoding="shift-jis"))

##############データ整形###############


#####議員情報のスクレイピング(参議院)#####
page_master <- read_html("https://www.sangiin.go.jp/japanese/joho1/kousei/giin/204/giin.htm")
links <- page_master %>%
  html_nodes("a.Graylink") %>%
  html_attr("href") %>%
  str_subset("profile") %>%
  str_remove("..")

mem_base <- page_master %>%
  html_table() %>%
  .[[2]] %>%
  .[,-c(1,7)] %>%
  mutate("議員氏名" = str_remove_all(.$議員氏名, " |　")) %>%
  rename("name" = `議員氏名`)

df<-NULL
for(i in 1:length(links)){
  page <- read_html(paste0("https://www.sangiin.go.jp/japanese/joho1/kousei/giin", links[i]))
  name <- page %>%
    html_nodes("h1.profile-name") %>%
    html_text() %>%
    str_remove_all("（.+）| |　")
  
  int <- page %>% 
    html_nodes("dl.profile-detail") %>% 
    .[2] %>% 
    html_text() %>% 
    str_extract("\\d") %>%
    as.integer()
  dsc <- page %>%
    html_nodes("p.profile2") %>%
    html_text()
  
  df <- bind_rows(df, tibble("name" = name,
                             "seniority" = int,
                             "discription" = dsc))
  Sys.sleep(3)
  cat("done", i, "\n")
}
f <- read_csv("data/female_politicianlist.csv")
df <- df %>%
  mutate("female" = (df$name %in% f$name))
df <- inner_join(df, mem_base, by = "name")
write_csv(df, "data/mem_info_HC.csv")

##############テキスト解析###############
#####前処理#####
capacity <- speech$text %>%
  str_replace_all("\\s+.+|\n", "") %>% # 冒頭の名前部分の取り出し
  str_replace( "^.+?(参事|政府特別補佐人|内閣官房|会計検査院|最高裁判所長官代理者|主査|議員|副?大臣|副?議長|委員|参考人|分科員|公述人|君(（.+）)?$)", "\\1") %>% # 冒頭の○から，名前部分までを消去
  str_replace("（.+）", "")
capacity <- str_replace(capacity, "^○.+", "Other") # マイナーな役職名は一括して"Other"に
speech <- speech %>% 
  mutate("capacity" = capacity) # capacity欄を作成
head.call <- speech$text %>%
  str_replace_all("\\s+.+|\n", "") %>%
  unique()
speech$text <- speech$text %>%
  str_remove_all(paste(head.call %>% 
                         .[1:length(.)/2], 
                       collapse = "|")) %>% 
  str_remove_all(paste(head.call %>% 
                         .[length(.)/2:length(.)], 
                       collapse = "|"))

#####発言量#####
hr_speech_analysis <- speech %>%
  filter(capacity %in% c("議員", "委員", "君", "委員以外の議員", "委員（鈴木俊一君)"))
hr_speech_analysis <- map_df(unique(hr_speech_analysis$name), ~ {
  vec <- hr_speech_analysis %>% # 議員ごとにまとめる
    filter(name == .x, session >= 199) %>%
    .$text
  txt <- paste(vec, collapse = " ")
  df <- tibble("name" = .x,
               "text" = txt)
  return(df)
})
speech_length <- tibble("name" = hr_speech_analysis$name,
              "chr_count" = map(hr_speech_analysis$text, nchar) %>%
                unlist()) %>%
  inner_join(., speech %>%
               filter(capacity %in% c("議員", "委員", "君",
                                      "委員以外の議員", "委員（鈴木俊一君)"),
                      session >= 195) %>%
               group_by(name, session, issue) %>%
               group_by(name) %>%
               summarise("count" = n()), 
             by = "name") %>%
  mutate("speech_length" = chr_count / count,
         "speech_time" = chr_count / 360)

table.analysis <- inner_join(mem_info %>%
                               select(name, male, `会派`, seniority) %>%
                               mutate("LDP" = (`会派` == "自民"),
                                      "CDP" = (`会派` == "立民"),
                                      "JRP" = (`会派` == "維新"),
                                      "Koumei" = (`会派` == "公明"),
                                      "JCP" = (`会派` == "共産"),
                                      "DPP" = (`会派` == "国民"),
                                      "Others" = (`会派` == "無")) %>%
                               select(-`会派`), 
                             speech_length %>%
                               select(name, chr_count),
                             by = "name")

fit <- lm(chr_count ~ ., data = table.analysis %>% select(-name))



#####発言内容#####
#####トピックモデル用に整形#####
speech_bymeeting  <- speech %>%
  filter(capacity %in% c("議員", "委員", "君", "委員以外の議員", "委員（鈴木俊一君)")) %>%
  filter(session >= 199) %>%
  group_by(name, session, meeting, issue) %>%
  summarise("txt" = list(text))
speech_analysis <- speech_bymeeting %>% # 会議ごとにまとめる
  dplyr::select(name, session, meeting, issue) %>%
  bind_cols(., tibble("text" = speech_bymeeting$txt %>%
                        map( ~ {
                          vec <- unlist(.x)
                          return(paste(vec, collapse = ""))
                        }) %>%
                        unlist()))
corp <- corpus(speech_analysis)
corp <- corpus_subset(corp, name != "")
toks <- tokens(corp)
toks <- tokens_select(toks, "^[０-９ぁ-んァ-ヶー一-龠]+$", valuetype = "regex", padding = TRUE)
min_count <- 10

# 漢字
kanji_col <- tokens_select(toks, "^[一-龠]+$", valuetype = "regex", padding = TRUE) %>% 
  textstat_collocations(min_count = min_count)
toks <- tokens_compound(toks, kanji_col[kanji_col$z > 3,], concatenator = "")

# カタカナ
kana_col <- tokens_select(toks, "^[ァ-ヶー]+$", valuetype = "regex", padding = TRUE) %>% 
  textstat_collocations(min_count = min_count)
toks <- tokens_compound(toks, kana_col[kana_col$z > 3,], concatenator = "")

# 漢字，カタカナおよび数字
any_col <- tokens_select(toks, "^[０-９ァ-ヶー一-龠]+$", 
                         valuetype = "regex", padding = TRUE) %>% 
  textstat_collocations(min_count = min_count)
toks <- tokens_compound(toks, any_col[any_col$z > 3,], concatenator = "")
speech_dfm <- dfm(toks, remove = "") %>% 
  dfm_remove("^[ぁ-ん]+$", valuetype = "regex", min_nchar = 2) %>% 
  dfm_trim(min_termfreq = 0.50, 
           termfreq_type = "quantile", max_termfreq = 0.99)
dfm <- dfm(toks, remove = "")
dfm.smp <- dfm_sample(speech_dfm, 
                      size = 0.8*nrow(speech_analysis)) # 教師データ
dfm.test <- dfm_subset(speech_dfm, 
                       docnames(dfm) %not.in% docnames(dfm.smp)) # テストデータ
dtm.smp <- convert(dfm.smp, to = "topicmodels")
dtm.test <- convert(dfm.test, to = "topicmodels")
dtm <- convert(speech_dfm, to = "topicmodels")

#####トピック数の推定#####
lda.list <- list()
prpl <- NULL
max.topics <- 100 # トピック数の最大値を設定

result <- FindTopicsNumber(
  dtm.smp,
  topics = seq(from = 2, max.topics, by = 10),
  metrics = c("Griffiths2004","CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 2L,
  verbose = TRUE
)
FindTopicsNumber_plot(result)
topic_auto <- function(k){
  lda.dat <- LDA(dtm.smp, k = k)
  outocome <- tibble("topics" = k,
                     "perplexity" = perplexity(lda.dat, dtm.test))
  return(outocome)
}
prpl.list <- map_df(c(17:27), topic_auto)

#####トピック内容の検討#####
fit.topic <- LDA(dtm, k = 11)
#write_rds(fit.topic, "data/HR_topicmodel_11.rds")

##割り当てられたトピックを分析
terms(fit.topic, 5)
df.list <- list()
speech_topic <- speech_analysis %>%
  ungroup() %>%
  mutate("topic_num" = topics(fit.topic)) %>%
  group_by(name) %>%
  summarise("topics" = list(topic_num))
df.list <- list()
for(i in 1:11){
  dat <- map_df(speech_topic$topics, ~ {
    vec <- sum(unlist(.x) %in% i)
    df <- tibble(!!paste0("topic", i) := vec)
  })
  df.list <- c(df.list, list(dat))
}
df.all <- do.call("bind_cols", df.list)
df.all <- df.all %>% 
  mutate("sum" = apply(., 1, sum)) %>%
  mutate(topic1 = topic1 / sum,
         topic2 = topic2 / sum,
         topic3 = topic3 / sum,
         topic4 = topic4 / sum,
         topic5 = topic5 / sum,
         topic6 = topic6 / sum,
         topic7 = topic7 / sum,
         topic8 = topic8 / sum,
         topic9 = topic9 / sum,
         topic10 = topic10 / sum,
         topic11 = topic11 / sum,
         topic12 = topic12 / sum,
         topic13 = topic13 / sum,
         topic14 = topic14 / sum,
         topic15 = topic15 / sum,
         topic16 = topic16 / sum)

speech_topic <- bind_cols(speech_topic %>% dplyr::select(-topics), 
                          df.all %>% dplyr::select(-sum))

##記述統計
stargazer::stargazer(as.data.frame(dat), type = "text")
car::scatterplotMatrix(dat %>% dplyr::select(-name, -topic11), 
                       span = 0.5, cex.labels = 1.1, cex.axis=.85)

##解析
pairs(df.all %>% dplyr::select(-sum), panel = panel.smooth)
fit.list <- list()
mem_info <- mem_info_HR # 下院か上院かを選択
for(i in 1:11){
  dat <- inner_join(speech_topic %>% #,
                      dplyr::select(name, !!paste0("topic", i)), 
                    mem_info %>%
                      dplyr::select(name, female, seniority, `会派`) %>%
                      mutate("LDP" = (`会派` == "自民"),
                             "CDP" = (`会派` == "立民"),
                             "JRP" = (`会派` == "維新"),
                             "Koumei" = (`会派` == "公明"),
                             "JCP" = (`会派` == "共産"),
                             "DPP" = (`会派` == "国民"),
                             "Others" = (`会派` == "無")) %>%
                      dplyr::select(-`会派`) , by = "name")
  
  fit_topic.lm <- lm(paste0("topic", i, "~."), data = dat %>% dplyr::select(-name))
  fit.list <- c(fit.list, list(fit_topic.lm))
}
names(fit.list) <- paste("topic", 1:11, sep = " ")
var_nam <- c("femaleTRUE" = "女性", "seniority" = "当選回数", "LDPTRUE" = "自民",
             "CDPTRUE" = "立民", "JRPTRUE" = "維新", "KoumeiTRUE" = "公明",
             "JCPTRUE" = "共産", "DPPTRUE" = "国民")

modelsummary(fit.list, "衆議院_重回帰表.png",
         coef_map = var_nam, 
         estimate = "{estimate}({p.value})", gof_omit = "R2|AIC|BIC", 
         statistic = NULL,
         title = "衆議院議員のトピック言及度",
         notes = list("1. 「女性」変数は発言者が女性議員の場合1、そうでない場合0を取るダミー変数",
                      "2. 各政党名は所属している場合に1、そうでない場合0を取るダミー変数",
                      "3. 括弧内の数字はp値")) %>% 
  gtsave("衆議院_重回帰表.pdf")

fit.topic %>% 
  terms(5) %>% 
  as.tibble() %>% 
  gt() %>% 
  tab_header(title = "衆議院議員 発言トピック") %>% 
  tab_style(style=cell_text(align="left"), locations = cells_title("title"))

pairs(df.all %>% dplyr::select(-sum), panel = panel.smooth)