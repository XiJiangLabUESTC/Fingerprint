#code library https://rdrr.io/cran/circlize/man/circos.text.html
#document https://jokergoo.github.io/circlize_book/book/graphics.html#text
# rm(list = ls())

library(circlize)
library(readxl)
#emotion gambling language motor relational social wm
task_name = c('EMOTION','GAMBLING','LANGUAGE','MOTOR','RELATIONAL','SOCIAL','WM')
for (task_num in 1:7){
  png(file = paste("D:/draw_pic/conn/hcp_",task_name[task_num],".png"), width = 6, height = 6, units = "in", res = 300)
  test_excel_read <-read_excel("D:/vtk_picture/r_picature/regions.xlsx")
  read_path = paste("D:/draw_pic/conn/hcp_",task_name[task_num],".xlsx", sep = "")
  links_read <-read_excel(read_path)
  #dev.off()
  circos.clear()
  #2.4
  circos.par("track.height" = 0.1,start.degree=245,gap.degree = 4)
  #2.3
  circos.initialize(test_excel_read$CaValue, x = test_excel_read$XValue)
  
  
  #circos.trackPlotRegion(test_excel_read$CaValue, ylim = c(0, 1),bg.border = NA)
  
  #
  
  d1 = c("OCC","PAR","TEM","LIM","FRO")
  d2 = rev(d1)
  d4 = c(1+33/38*5,1+33/38*15,1+33/38*23,1+33/38*28,1+33/38*34)
  d3 = c(1+33/38*3,1+33/38*10,1+33/38*16,1+33/38*23,1+33/38*32)
  d5 = ('SUB')
  d6 = (0.5+19/2)
  index_ <<- 1
  circos.trackPlotRegion(test_excel_read$CaValue, ylim = c(0, 1),bg.border = 'NA',
                         panel.fun = function(x, y){
                           xlim = CELL_META$xlim
                           ylim = CELL_META$ylim
                           if(index_%%2==1 && index_<=4){
                             name_list = d2
                             name_index = d4
                             index_list = c(1,2,3,4,5)
                             for(i in index_list){
                               
                               circos.text(name_index[i],-1, labels = name_list[i],niceFacing = FALSE, 
                                           facing = "inside",cex=0.7,adj=c(0.5,0.5)
                                           ,family  = 'serif',font=2)
                               #adj文字对齐，0左对齐，0.5居中，(水平，垂直)
                               #family 样式 font 格式 2加粗
                             }
                           }
                           if(index_%%2==0 && index_<=4){
                             name_list = d1
                             name_index = d3
                             index_list = c(1,2,3,4,5)
                             for(i in index_list){
                               
                               circos.text(name_index[i], -1, labels = name_list[i],niceFacing = FALSE, 
                                           facing = "inside",cex=0.7,adj=c(0.5,0.5)
                                           ,family  = 'serif',font=2)
                               #adj文字对齐，0左对齐，0.5居中，(水平，垂直)
                               #family 样式 font 格式 2加粗
                             }
                           }
                           if(index_==5){
                             name_list = d5
                             name_index = d6
                             index_list = c(1)
                             for(i in index_list){
                               
                               circos.text(name_index[i], -1, labels = name_list[i],niceFacing = FALSE, 
                                           facing = "inside",cex=0.7,adj=c(0.5,0.5)
                                           ,family  = 'serif',font=2)
                               #adj文字对齐，0左对齐，0.5居中，(水平，垂直)
                               #family 样式 font 格式 2加粗
                             }
                           }
                           print(index_)
                           index_<<-index_+1
                         })
  
  
  
  d1=seq(1,38,by=1)
  d2=seq(101,138,by=1)
  d3 = seq(201,219,by=1)
  name = test_excel_read$RegionsName
  name_index= test_excel_read$BigNameIndex
  index_ <<- 1
  circos.trackPlotRegion(test_excel_read$CaValue, ylim = c(0, 1),bg.border = NA,
                         panel.fun = function(x, y){
                           xlim = CELL_META$xlim
                           ylim = CELL_META$ylim
                           if(index_%%2==1 && index_<=4){
                             for(i in d1){
                               if (is.na(which(name_index==i)[1])) {
                                 next
                               }
                               name_ = name[which(name_index==i)[1]]
                               circos.text(1+i*33/38-33/38/2, -1, labels = name_,niceFacing = FALSE, 
                                           facing = "clockwise",cex=0.35,adj=c(0,0.5))
                             }
                           }
                           if(index_%%2==0 && index_<=4){
                             for(i in d2){
                               if (is.na(which(name_index==i)[1])) {
                                 next
                               }
                               name_ = name[which(name_index==i)[1]]
                               i = i-100
                               circos.text(1+i*33/38-33/38/2, -1, labels = name_,niceFacing = FALSE, 
                                           facing = "clockwise",cex=0.35,adj=c(0,0.5))
                               #adj文字对齐，0左对齐，0.5居中，(水平，垂直)
                             }
                           }
                           if(index_==5){
                             for(i in d3){
                               if (is.na(which(name_index==i)[1])) {
                                 next
                               }
                               name_ = name[which(name_index==i)[1]]
                               i = i-200
                               circos.text(1+i*33/38-33/38/2, -1, labels = name_,niceFacing = FALSE, 
                                           facing = "clockwise",cex=0.35,adj=c(0,0.5))
                               #adj文字对齐，0左对齐，0.5居中，(水平，垂直)
                             }
                           }
                           index_ <<- 1+index_ 
                         })
  
  
  
  add_rect_1_34 = function(colr) {
    blocklen=33/38
    breaks = seq(1, blocklen*10+1, by = blocklen)
    y_b=rep(-0.45,10)
    y_t=rep(0.5,10)
    n_breaks = length(breaks)
    circos.rect(breaks[-n_breaks],y_b,breaks[-1],y_t, col = colr, border = "#f2f4f6")
    
    circos.rect(blocklen*10+1,0,blocklen*11+1,1, col = "white",border = "white")
    
    breaks = seq(blocklen*11+1, blocklen*17+1, by = blocklen)
    y_b=rep(-0.45,6)
    y_t=rep(0.5,6)
    n_breaks = length(breaks)
    circos.rect(breaks[-n_breaks],y_b,breaks[-1],y_t, col = colr, border = "#f2f4f6")
    
    circos.rect(blocklen*17+1,0,blocklen*18+1,1, col = "white",border = "white")
    
    breaks = seq(blocklen*18+1, blocklen*25+1, by = blocklen)
    y_b=rep(-0.45,7)
    y_t=rep(0.5,7)
    n_breaks = length(breaks)
    circos.rect(breaks[-n_breaks],y_b,breaks[-1],y_t, col = colr, border = "#f2f4f6")
    
    circos.rect(blocklen*25+1,0,blocklen*26+1,1, col = "white",border = "white")
    
    breaks = seq(blocklen*26+1, blocklen*32+1, by = blocklen)
    y_b=rep(-0.45,6)
    y_t=rep(0.5,6)
    n_breaks = length(breaks)
    circos.rect(breaks[-n_breaks],y_b,breaks[-1],y_t, col = colr, border = "#f2f4f6")
    
    circos.rect(blocklen*32+1,0,blocklen*33+1,1, col = "white",border = "white")
    
    breaks = seq(blocklen*33+1, blocklen*38+1, by = blocklen)
    y_b=rep(-0.45,5)
    y_t=rep(0.5,5)
    n_breaks = length(breaks)
    circos.rect(breaks[-n_breaks],y_b,breaks[-1],y_t, col = colr, border = "#f2f4f6")
  }
  
  add_rect_34_1 = function(colr) {
    blocklen=33/38
    breaks = seq(1, blocklen*5+1, by = blocklen)
    y_b=rep(-0.45,5)
    y_t=rep(0.5,5)
    n_breaks = length(breaks)
    circos.rect(breaks[-n_breaks],y_b,breaks[-1],y_t, col = colr, border = "#f2f4f6")
    
    circos.rect(blocklen*5+1,0,blocklen*6+1,1, col = "white",border = "white")
    
    breaks = seq(blocklen*6+1, blocklen*12+1, by = blocklen)
    y_b=rep(-0.45,6)
    y_t=rep(0.5,6)
    n_breaks = length(breaks)
    circos.rect(breaks[-n_breaks],y_b,breaks[-1],y_t, col = colr, border = "#f2f4f6")
    
    circos.rect(blocklen*12+1,0,blocklen*13+1,1, col = "white",border = "white")
    
    breaks = seq(blocklen*13+1, blocklen*20+1, by = blocklen)
    y_b=rep(-0.45,7)
    y_t=rep(0.5,7)
    n_breaks = length(breaks)
    circos.rect(breaks[-n_breaks],y_b,breaks[-1],y_t, col = colr, border = "#f2f4f6")
    
    circos.rect(blocklen*20+1,0,blocklen*21+1,1, col = "white",border = "white")
    
    breaks = seq(blocklen*21+1, blocklen*27+1, by = blocklen)
    y_b=rep(-0.45,6)
    y_t=rep(0.5,6)
    n_breaks = length(breaks)
    circos.rect(breaks[-n_breaks],y_b,breaks[-1],y_t, col = colr, border = "#f2f4f6")
    
    circos.rect(blocklen*27+1,0,blocklen*28+1,1, col = "white",border = "white")
    
    breaks = seq(blocklen*28+1, blocklen*38+1, by = blocklen)
    y_b=rep(-0.45,10)
    y_t=rep(0.5,10)
    n_breaks = length(breaks)
    circos.rect(breaks[-n_breaks],y_b,breaks[-1],y_t, col = colr, border = "#f2f4f6")
  }
  
  add_rect_19 = function(colr) {
    blocklen=33/38
    breaks = seq(1, blocklen*19+1, by = blocklen)
    y_b=rep(-0.45,19)
    y_t=rep(0.5,19)
    n_breaks = length(breaks)
    circos.rect(breaks[-n_breaks],y_b,breaks[-1],y_t, col = colr, border = "#f2f4f6")
  }
  
  
  color = c("#FF6969","#3AB0FF","#3AB0FF","#FF6969","#F6BA6F")
  label_ = c("Sulci L","Gyri L","Gyri R","Sulci R",'Sub')
  i=1
  circos.track(test_excel_read$CaValue, ylim = c(0, 1),bg.border = NA,
               panel.fun = function(x, y) {
                 circos.text(CELL_META$xcenter, 
                             CELL_META$cell.ylim[2] + mm_y(14), 
                             label_[i],col=color[i],cex=1.5,family  = 'serif',font=2)
                 xlim = CELL_META$xlim
                 ylim = CELL_META$ylim
                 if(i%%2==1 && i<=4){
                   add_rect_1_34(color[i])
                 }else if(i%%2==0 && i<=4){
                   add_rect_34_1(color[i])
                 }else{
                   add_rect_19(color[i])
                 }
                 print(i)
                 i <<- i+1
               })
  
  
  col=c("#F27970", "#BB9727","#54B345",'#05B9E2','#8983BF','#C76DA2'  )
  for (i in seq_along(links_read$`0`)) {
    l <- links_read$`0`[i]
    r <- links_read$`1`[i]
    WEW <- 1
    `CC` <- links_read$`2`[i]
    l_caval <- test_excel_read$CaValue[which(test_excel_read$Value==l)]
    r_caval <- test_excel_read$CaValue[which(test_excel_read$Value==r)]
    l <- test_excel_read$BigNameIndex[which(test_excel_read$Value==l)]
    r <- test_excel_read$BigNameIndex[which(test_excel_read$Value==r)]
    if(l_caval==2|l_caval==4){
      l=l-100
    }
    if(r_caval==2|r_caval==4){
      r=r-100
    }
    if(l_caval==5){
      l=l-200
    }
    if(r_caval==5){
      r=r-200
    }
    l=33/38*l+1-33/38/2
    r=33/38*r+1-33/38/2
    circos.link(l_caval, c(l-0.1*WEW,l+0.1*WEW), r_caval, c(r-0.1*WEW,r+0.1*WEW),col=col[CC])
  }
  
  dev.off()
  
}






