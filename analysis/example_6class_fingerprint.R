
library(tidyverse)
library(readxl)
# 创建数据：
#emotion gambling language motor relational social wm
task_name = c('emotion','gambling','language','motor','relational','social','wm')
for (task_num in 1:6){
  read_path = paste("D:/vtk_picture/r_picature/6class_hcp_dk_",task_name[task_num],".xlsx", sep = "")
  data <-read_excel(read_path)
  data$subgroup <- c(rep(1, 20),rep(2, 20),rep(3, 12),rep(4,12),rep(5, 14),rep(6, 14),
                     rep(7, 12),rep(8, 12),rep(9, 10),rep(10, 10),rep(11, 19))
  data$lrs <- c(rep(1,10),rep(2,10),rep(1,10),rep(2,10),
                rep(1,6),rep(2,6),rep(1,6),rep(2,6),
                rep(1,7),rep(2,7),rep(1,7),rep(2,7),
                rep(1,6),rep(2,6),rep(1,6),rep(2,6),
                rep(1,5),rep(2,5),rep(1,5),rep(2,5),
                rep(3,19))
  # data$id <- c(rank(-subset(data,data$group=="1")$value),
  #              131+rank(-subset(data,data$group=="2")$value),
  #              40+rank(-subset(data,data$group=="3")$value),
  #              88+rank(-subset(data,data$group=="4")$value),
  #              68+rank(-subset(data,data$group=="5")$value),
  #              112+rank(-subset(data,data$group=="6")$value))
  data$id <- c(1:40,132:155,41:68,89:112,69:88,113:131)
  for (region_num in 1:136){
    if (data$subgroup[region_num]%%2==1 & data$lrs[region_num]%%2==1){
      data$RegionsName[region_num] = paste(data$RegionsName[region_num],'.l', sep = "")
    }
    if (data$subgroup[region_num]%%2==1 & data$lrs[region_num]%%2==0){
      data$RegionsName[region_num] = paste(data$RegionsName[region_num],'.r', sep = "")
    }
    if (data$subgroup[region_num]%%2==0 & data$lrs[region_num]%%2==1){
      data$RegionsName[region_num] = paste(data$RegionsName[region_num],'.l', sep = "")
    }
    if (data$subgroup[region_num]%%2==0 & data$lrs[region_num]%%2==0){
      data$RegionsName[region_num] = paste(data$RegionsName[region_num],'.r', sep = "")
    }
  }


  data_gs <- data.frame(
    label = c('sulci','gyri','sulci','gyri','sulci','gyri','sulci','gyri','sulci','gyri'),
    x = c(9,29,46,60,73,83,93,105,136,148),
    y = c(rep(0.4,10))
  )
  data_lobe <- data.frame(
    label = c('Frontal','Temporal','Occipital','Parietal','Subcortical','Limbic'),
    x = c(18,50,75,96,117,140),
    y = c(rep(0.7,6))
  )

  frontal_color <- colorRampPalette(c("#a7cbe3", "#1d435d"))(1001)
  f_value = subset(data,data$group=="1")$value

  limbic_color <- colorRampPalette(c("#f6d8bc", "#e1812c"))(1001)
  l_value = subset(data,data$group=="2")$value


  temporal_color <- colorRampPalette(c("#b3d1b3", "#3a923a"))(1001)
  t_value = subset(data,data$group=="3")$value


  paritel_color <- colorRampPalette(c("#e2d1d1", "#c03d3e"))(1001)
  p_value = subset(data,data$group=="4")$value


  occipital_color <- colorRampPalette(c("#d9cce3", "#5c4273"))(1001)
  o_value = subset(data,data$group=="5")$value


  sub_color <- colorRampPalette(c("#CFB9B5", "#63443F"))(1001)
  s_value = subset(data,data$group=="6")$value


  color_all = c(frontal_color[1+floor((f_value-min(f_value))/(max(f_value)-min(f_value))*1000)],
                limbic_color[1+floor((l_value-min(l_value))/(max(l_value)-min(l_value))*1000)],
                temporal_color[1+floor((t_value-min(t_value))/(max(t_value)-min(t_value))*1000)],
                paritel_color[1+floor((p_value-min(p_value))/(max(p_value)-min(p_value))*1000)],
                occipital_color[1+floor((o_value-min(o_value))/(max(o_value)-min(o_value))*1000)],
                sub_color[1+floor((s_value-min(s_value))/(max(s_value)-min(s_value))*1000)])


  # 设置一些“空栏”添加到每个组的末尾
  empty_bar <- 4
  to_add <- data.frame( matrix(NA, empty_bar*nlevels(data$group), ncol(data)) )
  colnames(to_add) <- colnames(data)
  to_add$group <- rep(levels(data$group), each=empty_bar)
  data <- rbind(data, to_add)
  data <- data %>% arrange(group)
  # data$id <- seq(1, nrow(data))

  # 获取每个标签的名称和y位置
  label_data <- data[,-grep("...1",colnames(data))]
  number_of_bar <- nrow(label_data)
  angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
  label_data$hjust <-  c( rep(0, 155)) #ifelse( angle < -90, 1, 0)
  label_data$angle <- ifelse(angle < -90, angle+360, angle)

  # 绘图
  ggplot(data, aes(x=as.factor(id), y=-value)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
    geom_bar(stat="identity", alpha=1,fill=color_all)+
    geom_errorbar(aes(ymin = -min95, ymax = -max95),width = 0.5)+

    ylim(-2,0.8) +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      plot.margin = unit(rep(-1,4), "cm")
    ) +
    coord_polar()  +
    # geom_text(data=label_data, aes(x=id, y= 0.02, label=RegionsName, hjust=hjust), color=c(rep("#3274A1", 40),rep('#E1812C', 24),rep('#3A923A', 28),
    #                                                                                        rep('#C03D3E', 24),rep('#9372B2', 20),rep('#845B53', 19)), family="sans",alpha=1, size=1.7, angle= label_data$angle, inherit.aes = FALSE ) +
    # geom_text(data = data_lobe, aes(x = x,y = y,label = label,hjust = 0),color=c('#3274A1','#3A923A','#9372B2','#C03D3E','#845B53','#E1812C'
    # ), family="sans",angle = -360/155*data_lobe$x-8,alpha=1, size=6)

    geom_text(data=label_data, aes(x=id, y= 0.02, label=RegionsName, hjust=hjust), color=c(rep(frontal_color[800], 20),
                                                                                           rep(frontal_color[300], 20),
                                                                                           rep(limbic_color[800], 12),
                                                                                           rep(limbic_color[300],12),
                                                                                           rep(temporal_color[800], 14),
                                                                                           rep(temporal_color[300], 14),
                                                                                           rep(paritel_color[800], 12),
                                                                                           rep(paritel_color[300], 12),
                                                                                           rep(occipital_color[800], 10),
                                                                                           rep(occipital_color[300], 10),
                                                                                           rep('#5D4037', 19)), family="sans",alpha=1, size=1.7, angle= label_data$angle, inherit.aes = FALSE ) +
    geom_text(data = data_gs, aes(x = x,y = y,label = label,hjust = 0),color=c(frontal_color[800],
                                                                               frontal_color[300],
                                                                               temporal_color[800],
                                                                               temporal_color[300],
                                                                               occipital_color[800],
                                                                               occipital_color[300],
                                                                               paritel_color[800],
                                                                               paritel_color[300],
                                                                               limbic_color[800],
                                                                               limbic_color[300]
    ), family="sans",angle = -360/155*data_gs$x-5,alpha=1, size=4)+
    geom_text(data = data_lobe, aes(x = x,y = y,label = label,hjust = 0),color=c('#3274A1','#3A923A','#9372B2','#C03D3E','#845B53','#E1812C'
    ), family="sans",angle = -360/155*data_lobe$x-10,alpha=1, size=6)

  ggsave(paste("D:/draw_pic/新版/rpic/other_class/6class_dk_",task_name[task_num],".png"), height = 6, width = 6)
}



















