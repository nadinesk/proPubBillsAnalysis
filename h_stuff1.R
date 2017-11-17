api_key <- Sys.getenv('api_key')

h_enact_115 <- getBills(api_key, "115", "house","enacted",1,8000 )
s_enact_115 <- getBills(api_key, "115", "senate","enacted",1,8000 )

h_enact_114 <- getBills(api_key,"114", "house","enacted",1,8000 )
s_enact_114 <- getBills(api_key,"114", "senate","enacted",1,8000 )

h_enact_113 <- getBills(api_key,"113", "house","enacted",1,500 )
s_enact_113 <- getBills(api_key,"113", "senate","enacted",1,500 )

h_enact_115$dup <- duplicated(h_enact_115$bill_id)
s_enact_115$dup <- duplicated(s_enact_115$bill_id)

hs_enact_115 <- rbind(h_enact_115, s_enact_115) %>%
  filter(dup == FALSE) %>%
  filter(!(str_detect(latest_major_action, "^Presented")))

h_enact_114$dup <- duplicated(h_enact_114$bill_id)
s_enact_114$dup <- duplicated(s_enact_114$bill_id)

hs_enact_114 <- rbind(h_enact_114, s_enact_114) %>%
  filter(dup == FALSE)

hs_enact_114_1 <- hs_enact_114 %>%
  filter((str_detect(latest_major_action, "^Became")) | (str_detect(latest_major_action, "^By")) | (str_detect(latest_major_action, "^Referred")))

h_enact_113$dup <- duplicated(h_enact_113$bill_id)
s_enact_113$dup <- duplicated(s_enact_113$bill_id)

hs_enact_113 <- rbind(h_enact_113, s_enact_113) %>%
  filter(dup == FALSE)

hs_115_party <- as.data.frame(table(hs_enact_115$sponsor_party, hs_enact_115$bill_type)) %>%
  mutate(total_p = sum(Freq)) %>%
  mutate(congress = '115') 
hs_114_party <- as.data.frame(table(hs_enact_114_1$sponsor_party, hs_enact_114_1$bill_type)) %>%
  mutate(total_p = sum(Freq)) %>%
  mutate(congress = '114')
hs_113_party <- as.data.frame(table(hs_enact_113$sponsor_party, hs_enact_113$bill_type)) %>%
  mutate(total_p = sum(Freq)) %>%
  mutate(congress = '113')


hs_113_to_115 <- rbind(hs_115_party, hs_114_party, hs_113_party) %>%
  mutate(chamber = ifelse(str_detect(Var2, "^h"),"H","S"))  %>%
  select(-Var2) %>%
  group_by(Var1, congress, chamber,total_p) %>%
  summarise_all(funs(sum)) %>%
  arrange(congress)

hs_113_to_115

hs_113_to_115_1 <- hs_113_to_115 %>%
  group_by(congress, chamber) %>%
  summarise(chamber_total_p = sum(Freq))

hs_113_to_115_1

hs_113_to_115_2 <- hs_113_to_115 %>%
  left_join(hs_113_to_115_1, by=c("congress", "chamber")) %>%
  mutate(party_chamber_percent = round((Freq/chamber_total_p),2)) %>%
  mutate(party_congress_percent = round((Freq/total_p),2)) %>%
  data.frame()

hs_113_to_115_2

hs_113_to_115_3 <- hs_113_to_115_2 %>%
  select(Var1, congress, chamber, party_chamber_percent) %>%
  melt(id.var=c("Var1", "congress","chamber"))

hs_113_to_115_3

hs_113_to_115_3_h <- hs_113_to_115_3 %>%
  filter(chamber == 'H')

hs_113_to_115_3_s <- hs_113_to_115_3 %>%
  filter(chamber == 'S')

house_113_to_115_graph <- ggplot(hs_113_to_115_3_h, aes(x=congress, y=value, group=Var1, fill=Var1)) +   
  geom_bar(stat="identity", position="dodge", size=0.25, width=0.8) +
  scale_fill_manual(values=c("blue","red", "purple"))


senate_113_to_115_graph <- ggplot(hs_113_to_115_3_s, aes(x=congress, y=value, group=Var1, fill=Var1)) +   
  geom_bar(stat="identity", position="dodge", size=0.25, width=0.8) +
  scale_fill_manual(values=c("blue","red", "purple"))

senate_113_to_115_graph

hs_113_to_115_4 <- hs_113_to_115_2 %>%
  select(Var1, congress, party_congress_percent) %>%
  melt(id.var=c("Var1", "congress"))

house_senate_113_to_115_graph <- ggplot(hs_113_to_115_4, aes(x=congress, y=value, group=Var1, fill=Var1)) +   
  geom_bar(stat="identity", position="dodge", size=0.25, width=0.8) +
  scale_fill_manual(values=c("blue","red", "purple"))






###

h_intro_112 <- getBills(api_key,"112","house", "introduced", 1,12000)
s_intro_112 <- getBills(api_key,"112","senate", "introduced", 1,12000)

t01 <- as.data.frame(as.character(h_intro_112$title))

t01$`as.character(h_intro_112$title)` <- gsub('Act', '', t01$`as.character(h_intro_112$title)`)
t01$`as.character(h_intro_112$title)` <- gsub('2011', '', t01$`as.character(h_intro_112$title)`)
t01$`as.character(h_intro_112$title)` <- gsub('2012', '', t01$`as.character(h_intro_112$title)`)
t01$`as.character(h_intro_112$title)` <- gsub('United States', 'U.S.', t01$`as.character(h_intro_112$title)`)
t01$`as.character(h_intro_112$title)` <- gsub('amend', '', t01$`as.character(h_intro_112$title)`)

t02 <- paste(unlist(t01), collapse =" ")

t <- Corpus(VectorSource(t02))
            

b1 <- as.data.frame(as.character(t3))

t1 <- tm_map(t, PlainTextDocument)
t2 <- tm_map(t1, removePunctuation)
t3 <- tm_map(t2, removeWords, stopwords('english'))
t4 <- tm_map(t3, stemDocument)

wordcloud(t3, max.words = 10, random.order = FALSE)

str(t3)

str(t1)



h_intro_111 <- getBills(api_key,"111","house", "introduced", 1,12000)
s_intro_111 <- getBills(api_key,"111","senate", "introduced", 1,12000)

t01 <- as.data.frame(as.character(h_intro_111$title))

t01$`as.character(h_intro_111$title)` <- gsub('Act', '', t01$`as.character(h_intro_111$title)`)
t01$`as.character(h_intro_111$title)` <- gsub('2010', '', t01$`as.character(h_intro_111$title)`)
t01$`as.character(h_intro_111$title)` <- gsub('2009', '', t01$`as.character(h_intro_111$title)`)
t01$`as.character(h_intro_111$title)` <- gsub('United States', 'U.S.', t01$`as.character(h_intro_111$title)`)
t01$`as.character(h_intro_111$title)` <- gsub('amend', '', t01$`as.character(h_intro_111$title)`)


t02 <- paste(unlist(t01), collapse =" ")

t <- Corpus(VectorSource(t02))


b1 <- as.data.frame(as.character(t3))

t1 <- tm_map(t, PlainTextDocument)
t2 <- tm_map(t1, removePunctuation)
t3 <- tm_map(t2, removeWords, stopwords('english'))
t4 <- tm_map(t3, stemDocument)

wordcloud(t3, max.words = 10, random.order = FALSE)

str(t3)

str(t1)

h_intro_110 <- getBills(api_key,"110","house", "introduced", 1,12000)
s_intro_110 <- getBills(api_key,"110","senate", "introduced", 1,12000)

h_intro_109 <- getBills(api_key,"109","house", "introduced", 1,12000)
s_intro_109 <- getBills(api_key,"109","senate", "introduced", 1,12000)

# h_intro_108 <- getBills(api_key,"108","house", "introduced", 1,12000)
# s_intro_108 <- getBills(api_key,"108","senate", "introduced", 1,12000)
# 
# h_intro_107 <- getBills(api_key,"107","house", "introduced", 1,12000)
# s_intro_107 <- getBills(api_key,"107","senate", "introduced", 1,12000)
# 
# h_intro_106 <- getBills(api_key,"106","house", "introduced", 1,12000)
# s_intro_106 <- getBills(api_key,"106","senate", "introduced", 1,12000)
# 

#t2 <- getBills(api_key,"112","house", "introduced", 8200,10000)

# library(httr)
# t3 <- GET("https://api.propublica.org/congress/v1/108/bills/hr4837.json",
#           add_headers(`X-API-Key` = api_key))
# 
# 
# 
# ft_pr <- content(t3, 'parsed')
# ft_res1 <- ft_pr$results
# ft_res2 <- ft_res1[[1]]
# ft_res3 <- ft_res2$title
# 
# str(ft_res3)
# 
# 
# ft_res2$short_title
