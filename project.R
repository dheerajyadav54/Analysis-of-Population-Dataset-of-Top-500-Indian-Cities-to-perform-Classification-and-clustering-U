indiancities=read.csv("F:\\abc\\academic\\VIT\\Winter Semester\\Data Mining and Business Intelligence\\cities_r2.csv",header=TRUE,sep=",")
y<-c(indiancities$state_code)
x1<-c(indiancities$population_total) 
x2<-c(indiancities$population_male) 
x3<-c(indiancities$population_female) 
set.seed(100)

set.seed(100)
all_data<-data.frame(y,x1,x2,x3)
positions <- sample(nrow(all_data),size=floor((nrow(all_data)/4)*3)) 
training<- all_data[positions,]
testing<- all_data[-positions,]
plot(positions)

library(dplyr)
library(ggplot2)
library(ggthemes) 
library(reshape2) 
library(gridExtra)

df<-read.csv("F:\\abc\\academic\\VIT\\Winter Semester\\Data Mining and Business Intelligence\\cities_r2.csv")

#no of cities per state

df %>% 
  group_by(state_name) %>%
  summarise(total_cities = n_distinct(name_of_city)) %>% 
  arrange(desc(total_cities)) %>%
  ggplot(aes(reorder(x = state_name, -total_cities), y = total_cities))+ 
  geom_bar(aes(fill = state_name), stat = "identity")+
  labs(list(title = "Number of Cities Per State", x = "States", y = "Total Cities"))+ 
  theme_solarized(light=FALSE)+
  scale_colour_solarized("red")+ 
  theme(text = element_text(size=12),
 axis.text.x = element_text(angle=90, hjust=1), legend.position = "none")


#no of district per state

df %>% 
  group_by(state_name) %>%
  summarise(total_districts = n_distinct(dist_code)) %>%
  arrange(desc(total_districts)) %>%
  ggplot(aes(reorder(x = state_name, -total_districts), y = total_districts))+
  geom_bar(aes(fill = state_name), stat = "identity")+
  labs(list(title = "Number of Districts Per State", x = "States", y = "Total Districts"))+
  theme_solarized(light=FALSE)+ 
  scale_colour_solarized("blue")+ 
  theme(text = element_text(size=12),
  axis.text.x = element_text(angle=90, hjust=1), legend.position = "none")
  
#Toata population per state

df %>%
  group_by(state_name) %>%
  summarise(total = sum(population_total)) %>% 
  arrange(desc(total)) %>%
  ggplot(aes(reorder(x = state_name, -total), y = total))+ 
  geom_bar(aes(fill = state_name), stat = "identity")+
  labs(list(title = "Total Population Per State", x = "States", y = "Total Population"))+
  theme_solarized(light=FALSE)+
  scale_colour_solarized("blue")+
  theme(text = element_text(size=12),
  axis.text.x = element_text(angle=90, hjust=1), legend.position = "none")

#Toatal male population per state

P1<-df %>%
  group_by(state_name) %>%
  summarise(total = sum(population_male)) %>%
  arrange(desc(total)) %>%
  ggplot(aes(reorder(x = state_name, -total), y = total))+ 
  geom_bar(aes(fill = state_name), stat = "identity")+
  labs(list(title = "Total Male Population Per State", x = "States", y = "Total Male Population"))+
  theme_solarized(light=FALSE)+
  scale_colour_solarized("blue")+
  theme(text = element_text(size=9),
  axis.text.x = element_text(angle=90, hjust=1), legend.position = "none")
P1


#Toatal female population per state

P2<-df %>%
  group_by(state_name) %>%
  summarise(total = sum(population_female)) %>% 
  arrange(desc(total)) %>%
  ggplot(aes(reorder(x = state_name, -total), y = total))+ 
  geom_bar(aes(fill = state_name), stat = "identity")+
  labs(list(title = "Total Female Population Per State", x = "States", y = "Total Female Population"))+
  theme_solarized(light=FALSE)+ 
  scale_colour_solarized("blue")+ 
  theme(text = element_text(size=9),
  axis.text.x = element_text(angle=90, hjust=1), legend.position = "none")

P2

grid.arrange(P1, P2, ncol =2)

#total population per city

df %>% 
  group_by(name_of_city) %>%
  summarise(total = sum(population_total)) %>%
  arrange(desc(total)) %>% top_n(n= 10, wt = total) %>%
  ggplot(aes(reorder(x = name_of_city, -total), y = total))+ 
  geom_bar(aes(fill = name_of_city), stat = "identity")+
  labs(list(title = "Total Population Per City", x = "City", y = "Total Population"))+ 
  theme_solarized(light=FALSE)+
  scale_colour_solarized("blue")+
  theme(text = element_text(size=12),
  axis.text.x = element_text(angle=90, hjust=1))

#Toatl number of literates(Male Vs Female)

df_literate<-df %>% 
  group_by(state_name) %>%
  summarise(female = sum(literates_female), male = sum(literates_male))

  mdata<-melt(df_literate, id = "state_name") 
  ggplot(aes(reorder(x = state_name, -value), y = value, group = variable), data = mdata)+
  geom_line(aes(color = variable))+
  labs(list(title = "Total Number of literates(Male Vs Female)", x = "State", y = "Total Number of Literates"))+
  theme_solarized(light=FALSE)+ 
  scale_colour_solarized("blue")+ 
  theme(text = element_text(size=12),
  axis.text.x = element_text(angle=90, hjust=1)) 
 
  #Literacy Rate Comparison(male vs female)
   
  ggplot(aes(x = name_of_city, group = 1 ), data = df)+
    geom_line(aes(y = effective_literacy_rate_female, colour = "Female")) +
    geom_line(aes(y = effective_literacy_rate_male, colour = "Male")) +
    labs(list(title = "Literacy Rate Comparison(Male Vs Female)", x = "Cities", y = "Literacy Rate"))+
    theme_solarized(light=FALSE)+ 
    scale_colour_solarized("blue")+ 
    theme(text = element_text(size=9),
    axis.text.x = element_text(angle=90, hjust=1)) 
  
#Sex ratio Vs child sex ratio
  
ggplot(aes(x = name_of_city, group = 1 ), data = df)+ 
  geom_line(aes(y = sex_ratio, colour = "sex_ratio")) + 
  geom_line(aes(y = child_sex_ratio, colour = "child_sex_ratio")) +
  labs(list(title = "Sex Ratio Vs Child Sex Ratio", x = "Cities", y = "Ratio"))+ 
  theme_solarized(light=FALSE)+
  scale_colour_solarized("blue")+ 
  theme(text = element_text(size=9),
  axis.text.x = element_text(angle=90, hjust=1))
  
#cluster perform

library(cluster)
library(dendextend)
library(factoextra)
# Load the data set
data("USArrests")

# Remove any missing value (i.e, NA values for not available)
# That might be present in the data
df <- na.omit(USArrests)

# View the firt 6 rows of the data
head(df, n = 6)

desc_stats <- data.frame(
  Min = apply(df, 2, min), # minimum
  Med = apply(df, 2, median), # median
  Mean = apply(df, 2, mean), # mean
  SD = apply(df, 2, sd), # Standard deviation
  Max = apply(df, 2, max) # Maximum
)
desc_stats <- round(desc_stats, 1)
head(desc_stats)

df <- scale(df)
head(df)



# Dissimilarity matrix
d <- dist(df, method = "euclidean")

# Hierarchical clustering using Ward's method
res.hc <- hclust(d, method = "average" )

# Plot the obtained dendrogram
plot(res.hc, cex = 0.6, hang = -1)

