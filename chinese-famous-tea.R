library(tidyverse)
library(ggthemes)


dat <- read.csv('~/china-famous-teas.csv', header = FALSE)
tea.type <- read.csv('~/china-famous-tea-type.csv')
tea.type %>% mutate(tea.color=recode(type,
                                     oolong='cyan4',
                                     flower='darkorchid',
                                     yellow='yellow4',
                                     white='grey50',
                                     green='darkgreen')
) -> tea.type

dat %>% rename(year=V1, source=V2) %>%
  gather(tea.order, tea, starts_with('V')) %>%
  mutate(tea.order=as.numeric(str_extract(tea.order, '[0-9]+'))-2,
         tea=str_trim(tea, side='both'),
         source=factor(str_c(year,source)),
         )  %>% left_join(tea.type, by='tea') %>%
  as_tibble() %>% group_by(tea) %>%
  mutate(n=n()) %>% arrange(desc(n)) %>%
  group_by() %>%
  mutate(tea=as_factor(tea), tea.color=str_c(tea.color)) -> china.tea

china.tea %>% select(tea, n, tea.color) %>% unique() -> tea.leaderbaord

nrow(tea.leaderbaord) -> nteas
x.angles <- c(seq(90-360/nteas/2, -90, length.out = nteas/2),
              seq(90-360/nteas, -90, length.out = nteas/2))

china.tea %>% ggplot(aes(fill=source, x=tea)) + geom_bar() +
  theme_tufte() +
  theme(text=element_text(size=16,  family="SimHei"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        axis.text.x = element_text(angle = x.angles,
                                   colour = tea.leaderbaord$tea.color
                                   )
        ) +
  coord_polar() +
  labs(title='中国“十大”名茶',
       caption='Copyright, Hormet Yiltiz, 2019\n
       数据来自https://baike.baidu.com/item/中国十大名茶/176919'
       ) -> pp


china.tea %>% ggplot(aes(y=tea, x=source, label=tea.order)) +
  geom_point(size=5,shape=0) +
  geom_text()+
  theme_tufte() +
  theme(text=element_text(size=16,  family="SimHei"),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1),
        axis.text.y = element_text(colour = tea.leaderbaord$tea.color),
        plot.title = element_text(hjust = 0),
        ) +
  labs(title='中国“十大”名茶',
       caption='版权所有(c) 2010 Hormet Yiltiz'
  ) -> p
g <- ggplotGrob(p)
g$layout$l[g$layout$name == "title"] <- 1
grid::grid.draw(g)


cairo_pdf(filename='china-famous-teas.pdf', width=14, height = 10, family='SimHei',
    pointsize = 12)
print(pp)
dev.off()

cairo_pdf(filename='china-famous-teas-grid.pdf', width=3, height = 8, family='SimHei',
          pointsize = 12)
grid::grid.draw(g)
dev.off()