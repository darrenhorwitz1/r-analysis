library("tidyverse")
library("quantmod")
library("plotly")
library("lubridate")

securities <- c('^GSPC','ZAR=X','^J200.JO')

#getting the symbols from yahoo
getSymbols(securities,src='yahoo')

#extracting the dates from the data structure
sp500_dates <- index(GSPC)
dr_dates <- index(`ZAR=X`)
t40_dates <- index(J200.JO)

#transforming to a tibble to tidy the data using tidyverse packages
sp_500 <- as_tibble(GSPC)
dollar_rand <- as_tibble(`ZAR=X`)
jse_40 <- as_tibble(J200.JO)

#adding dates to indices and removing irrelevant columns
sp_500 <- 
  sp_500 %>%
  mutate("date" = sp500_dates)%>%
  select(date,"price"=GSPC.Adjusted)
dollar_rand <- 
  dollar_rand %>% 
  mutate("date" = dr_dates)%>% 
  select(date,"price"=`ZAR=X.Adjusted`)
jse_40 <- 
  jse_40 %>% 
  mutate("date" = t40_dates) %>% 
  select(date,"price"=J200.JO.Adjusted)

# joining each index on date which they all share
joined_df <- 
  inner_join(dollar_rand,
             sp_500 , 
             by = "date",
             suffix=c(".zar",".sp500"))
joined_df <- 
  inner_join(x=joined_df,y=jse_40,by = "date")

#basing all relevant indices to 1
indices_based_at_1 <- 
  joined_df %>% 
  mutate(
    sp500_rand_price = price.zar * price.sp500,
    zar_sp500_base_price = sp500_rand_price /sp500_rand_price[[1]] ,
    t40_base_price = price/joined_df$price[[1]],
    zar_base_price = price.zar/joined_df$price.zar[[1]]
     ) 

#nice graph data that can be faceted on base
out <- 
  indices_based_at_1 %>%
  pivot_longer(
    cols=c(zar_sp500_base_price,t40_base_price,zar_base_price),
    names_to = "base",
    values_to = "index" ,names_repair = "unique")

## peaks and trough dates in zar

# 2008-10-22 zar peak
# 2011-05-03 zar trough
# 2011-11-25 zar peak
# 2012-03-02 zar trough
# 2014-01-30 zar peak
# 2014-05-27 zar trough
# 2016-01-12 zar peak
# 2017-03-27 zar trough
# 2017-11-14 zar peak
# 2018-02-27 zar trough
# 2018-09-06 zar peak
# 2019-02-01 zar trough
# 2019-08-20 zar peak
# 2020-01-02 zar trough
# 2020-04-06 zar peak
# 2021-02-16 zar trough

dates <- c("2008-10-22",
           "2011-05-03",
           "2011-11-25",
           "2012-03-02",
           "2014-01-30",
           "2014-05-27",
           "2016-01-12",
           "2017-03-27",
           "2017-11-14",
           "2018-02-27",
           "2018-09-06",
           "2019-02-01",
           "2019-08-20",
           "2020-01-02",
           "2020-04-06",
           "2021-02-16")

#converting to date vector
dates <- as_date(dates)

#getting the peaks and troughs in the data
peaks_and_troughs <- 
  out %>% 
  select(date,base,index) %>% 
  filter(as_date(date)%in% dates)

#setting up the plot
plot <- 
  ggplot()+
  geom_line(data=out,aes(date,index,color = base))+
  theme(legend.position = "bottom")

# ggplotly(plot)
  

# analysis
roi <- 
  indices_based_at_1 %>% 
  filter(as_date(date)%in% dates) %>% 
  mutate(
    sp500_roi = (zar_sp500_base_price/lag(zar_sp500_base_price)-1)*100,
    t40_roi = (t40_base_price/lag(t40_base_price)-1 )*100,
    zar_roi = (zar_base_price/lag(zar_base_price)-1 )*100,
    winner =  if_else(sp500_roi > t40_roi,"USA","RSA") ,
    zar = if_else(price.zar>lag(price.zar),"Weakened","Strengthened"),
    duration = difftime(date , lag(date),"days") 
  ) 



# ROI Table
p_t_tibble <- 
  roi %>% 
  select(sp500_roi:duration)

# perfectmarket timing variables
p_mkt_timing <- 1
p_div <- 1
p_mkt_timing_list <- c()
p_div_list <- c()
#looping to performing calculations for perfect market timing
for (i in seq_along(p_t_tibble$winner)) {
  
  winner <- p_t_tibble$winner[i]
  if(identical(winner,"RSA")){
    p_mkt_timing <-  p_mkt_timing*(1+(p_t_tibble$t40_roi[i]/100))
  }else if(identical(winner,"USA")){
    p_mkt_timing <-  p_mkt_timing*(1+(p_t_tibble$sp500_roi[i]/100))
  }
  
  if(is.na(winner)){
    p_mkt_timing_list[i] <-  p_mkt_timing
    p_div_list[i] <- p_div
  }else{
    p_div <- p_div*(1 + 0.5*(p_t_tibble$sp500_roi[i]/100+p_t_tibble$t40_roi[i]/100))
    p_div_list[i] <- p_div 
    p_mkt_timing_list[i] <-  p_mkt_timing
  }
  
}

perfect_tibble <- 
  tibble(date = dates, diversified=p_div_list,perfect =p_mkt_timing_list) %>% 
  pivot_longer(cols = c(perfect,diversified), names_to = "type",values_to = "index")

#PERFECT CHART
perfect_plot <- 
  plot +
  geom_line(data=perfect_tibble,mapping = aes(date,index,color= type))+
  labs(y="Index Value (based at 1)", 
       x = "Date", 
       title="The inclusion of a portfolio that has perfect market timing and another that is diversified")
ggplotly(perfect_plot)  
#CHART COMPARISONS
comparisons <- 
  plot+
  labs(y="Index Value (based at 1)", 
       x = "Date", 
       title="Comparisons of indices from 2008-current")
ggplotly(comparisons)

#ZAR PLOT
zar_dates <- 
  joined_df %>% 
  filter(date %in% dates)
zar_plot <- 
  ggplot() +
  geom_line(data = joined_df,mapping = aes(date,price.zar))+
  geom_point(data = zar_dates,aes(date,price.zar,color="red",size=1))+
  theme(legend.title = element_blank(),
        legend.position = "none") +
  labs(y="Dollar/Rand", x = "Date", title="Peaks and Troughs in Dollar/Rand from 2008 onwards")

ggplotly(zar_plot)

# ROI summary
roi_summary <- 
  roi %>% 
  group_by(winner) %>%
  summarise(
    count=n(),
    ave_duration=mean(duration,na.rm = T),
    ave_zar_change = mean(zar_roi,na.rm=T),
    ave_sp500_roi = mean(sp500_roi,na.rm = T),
    ave_t40_roi = mean(t40_roi,na.rm = T) 
    ) %>% 
  filter(!is.na(winner))
gridExtra::grid.table(roi_summary)

# ROI Table over the 16 periods
roi %>%
  select(-(price.zar:zar_base_price)) %>% 
  gridExtra::grid.table()

#last plot that is pretty insightful
okay <- roi %>% 
  select(sp500_roi,t40_roi,zar,duration) %>% 
  filter(!is.na(sp500_roi)) %>% 
  pivot_longer(
    cols=c(sp500_roi,t40_roi), 
    names_to = "index",
    values_to ="period_roi" ) 

okay <- okay%>% 
  mutate(index = if_else( index== "sp500_roi","SP500 (in ZAR)","JSE Top 40" ))
                        
                            

okay_plot <- 
  ggplot(okay) +
  geom_point(aes(duration,period_roi,color = index),size=5)+
  geom_hline(yintercept = c(23,-5)) +
  facet_grid(~zar)+
  labs(
    x="duration in days",
    y="return on investment (in %)",
    title = "Return on Investments for respective indices when the Rand had stregthened or weakened"
    )

ggplotly(okay_plot)

  








