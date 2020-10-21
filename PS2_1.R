# Set working dir
setwd("C:/users/99707/Desktop/Assignment/R_Assignment02")
# Load the libraries
library(tidyr)
library(dplyr)
library(ggplot2)

#1.1
Keeling_Data <- read.table("signif.txt",sep="\t",header=T,quote="")   #quote=""的作用是禁用引号
Sig_Eqs      <- as_tibble(Keeling_Data)
# Check the class
class(Sig_Eqs)
# Check the variables
head(Sig_Eqs)

#1.2
Sig_Eqs                                            %>%  
  select(COUNTRY,TOTAL_DEATHS)                     %>%  
  filter(TOTAL_DEATHS!="NA")                       %>%  #剔除死亡人数未知的地震
  group_by(COUNTRY)                                %>%
  summarise(COUNTRY_TOTAL_DEATHS=sum(TOTAL_DEATHS))%>%  #求地震死亡人数之和
  arrange(desc(COUNTRY_TOTAL_DEATHS))              %>%  #反向排序
  head(n=10L)                                           #输出前10行

#1.3
#筛选出地震等级大于6的年份，并且累加地震次数
YEAR_times <- Sig_Eqs                            %>%    
   filter(EQ_PRIMARY > 6.0)                      %>%    #地震等级大于6，计数为1，否则，为零
   mutate(times = ifelse(EQ_PRIMARY>6.0,1,0))    %>%
   select(YEAR,EQ_PRIMARY,times)                 %>%
   group_by(YEAR)                                %>%
   summarise(total_times = sum(times))           %>%
   select(YEAR,total_times)
YEAR_times                                       %>%
   ggplot(aes(x=YEAR, y=total_times)) + 
   geom_point()

#1.4
Sig_Eqs[is.na(Sig_Eqs)] <- 0                            #将NA值全赋值为0

total_times_country <- Sig_Eqs                   %>%    #得到每个国家地震总数，并按照地震总数大小排序
  select (YEAR, MONTH, DAY, COUNTRY, DEATHS, EQ_PRIMARY) %>%
  group_by(COUNTRY)                              %>%
  mutate(times = 1)                              %>%
  summarise(total_earthquake = sum(times))       %>%
  arrange(desc(total_earthquake))
#输出结果
total_times_country

maxEq_country       <- Sig_Eqs                   %>%    #得到每个国家最大地震等级
  select( COUNTRY, EQ_PRIMARY)                   %>%
  group_by(COUNTRY)                              %>%
  summarize(MAX_Eq = max(EQ_PRIMARY)) 
#输出结果
maxEq_country 

all_data            <- Sig_Eqs                   %>%    #整理所需数据存在一个变量中
  select(COUNTRY,EQ_PRIMARY,YEAR,MONTH,DAY)

CountEq_LargestEq   <-  function(n){
  for(i in 1:nrow(total_times_country)){                #nrow(total_times_country)为国家总数
    #首先输出地震总数，即问题（1）
    if (total_times_country[i,1] == n ){
      T_earthquake <- total_times_country[i,2]          #如果国家能对应上，则输出对应的地震总数
      print(paste0(n,'的地震总数为：',T_earthquake))
    }
    #然后在下一行输出有史以来最大地震的日期，即问题（2）
    if(maxEq_country[i,1] == n ){
      for(j in 1:nrow(all_data)){
        if(all_data[j,1] == n && 
           all_data[j,2] == maxEq_country[i,2])
        {
          print(paste0('最大地震震级：',maxEq_country[i,2],',' ,'最大地震发生日期：',
                       all_data[j,3],'年',all_data[j,4],'月',
                       all_data[j,5],'日'))
        }
      }
    }
  }
}
 
 #测试函数功能
 CountEq_LargestEq("CHINA")
 #按照地震总数降序输出每个国家的地震总数和最大地震发生的日期
 for(i in 1:nrow(total_times_country)){       
   test    <- total_times_country[i,1]
   print(CountEq_LargestEq(test))
 }

 
 