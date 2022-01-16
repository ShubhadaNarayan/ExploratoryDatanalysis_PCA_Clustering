library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(ggplot2)
library(DT)
library("readxl")
library(shinyjs)
library(sodium)
library(shinyWidgets)
library(magrittr)
library(maps)
library(tidyr)
library(shinythemes)
library(bslib)
library(factoextra)
library(ggfortify)
library(shinylogs)
library(grid)
library(reshape2)
library(RColorBrewer)
library(data.table) 
library("ggpubr")



specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))

df<- read.csv("data-1.csv")
df11<- read_excel("Final_df_WisconsinCancer.xlsx")
df11<- as.data.frame(df11)
df11<- df11[,c(2,3,4)]
colnames(df11)<- c("Diagnosis","Features",'Value')
df_34<- df %>% mutate_at(colnames(df)[3:length(colnames(df))], ~(scale(.) %>% as.vector))
d<- df_34[,3:ncol(df_34)-1]

d11<- d[,-1]
k2 <- kmeans(d11, centers = 2, nstart = 25)
print(k2)

df1<- read.csv("df_Reordered.csv")
s<-colnames(df1[,c(2:31)])
print(s)
df2<- read.csv("Final_df.csv")
df3<- read.csv("CompnentsVar.csv")
df4<- read.csv("loadings.csv")
df4<-df4[,c(2:ncol(df4))]
colnames(df4)<- s

res.pca<- prcomp(df1[,c(2:31)],scale= TRUE)
print(res.pca)
imp_pca<- as.data.frame(summary(res.pca)$importance )
pca.vars<- res.pca$x

ui<- fluidPage(tags$head(HTML("
<script>
  var _paq = window._paq = window._paq || [];
  /* tracker methods like 'setCustomDimension' should be called before 'trackPageView' */
  _paq.push(['trackPageView']);
  _paq.push(['enableLinkTracking']);
  (function() {
    var u='https://center.matomo.cloud/';
    _paq.push(['setTrackerUrl', u+'matomo.php']);
    _paq.push(['setSiteId', '1']);
    var d=document, g=d.createElement('script'), s=d.getElementsByTagName('script')[0];
    g.async=true; g.src='//cdn.matomo.cloud/center.matomo.cloud/matomo.js'; s.parentNode.insertBefore(g,s);
  })();
</script>
")),
               tags$script(HTML(
                 "$(document).on('shiny:inputchanged', function(event) {
                 if (event.name === 'Dropdown1' || event.name === 'Dropdown2'|| event.name === 'Dropdown_1' || event.name === 'Dropdown_2'||event.name === 'Dropdown3'|| event.name === 'dropdown_3') {
                 _paq.push(['trackEvent', 'input', 
                 'updates', event.name, event.value]);
                 }
                 });"
               )),
               tags$style(HTML(".class_1{color:#000000;border: 1px solid #000000 ;background-color: transparent;}")),
               tags$style(HTML(".class_1:hover{color:#000000;border: 1px solid #000000 ;background-color: transparent;}")),
               tags$style(HTML("#Dropdown_exd2+ div>.selectize-input{border: 1px solid #000000;}
                            ")),
               tags$style(HTML("#Dropdown1+ div>.selectize-input{border: 1px solid #000000;}
                            ")),
               tags$style(HTML("#Dropdown2+ div>.selectize-input{border: 1px solid #000000;}
                            ")),
               tags$style(HTML("#Dropdown_1+ div>.selectize-input{border: 1px solid #000000;}
                            ")),
               tags$style(HTML("#Dropdown_2+ div>.selectize-input{border: 1px solid #000000;}
                            ")),
               tags$script(HTML("$('#Dropdown_exd44').picker({limit: 2});")),
               navbarPage(theme=shinytheme("united"),
                          title=shiny::span('R SHINY DASHBOARD',style = "font-size: 25px;font-family: 'Caladea'"),
                          #uiOutput(outputId = "text_11"),
                          #uiOutput(outputId = "text_22"),
                          #uiOutput(outputId = "text_33"),
                          tags$h5(tags$strong("*Exploratory Data Analysis tab"),"can be used to perform Exploratory Data Analysis on the data"),
                          tags$h5(tags$strong("*Clustering tab"),"can be used to view Clustering Analysis on the data"),
                          tags$h5(tags$strong("*Principal Components Analysis tab"),"can be used to perform Principal Component Analysis on the data"),
                          mainPanel(
                            tabsetPanel(
                              tabPanel(
                                "EXPLORATORY DATA ANALYSIS",
                                fluidRow(column(6,style="display: inline-block;padding: 10px;position:relative;right:5px;",tags$h3(tags$strong("BoxPlot"))),
                                         column(6,style="display: inline-block;padding: 10px;position:relative;left:300px;",tags$h3(tags$strong("Malign Vs Benign Histogram",tags$br(),"Plot for different features")))
                                ),
                                fluidRow(column(6,style="display: inline-block;padding: 10px;;position:relative;right:5px;",uiOutput("dropDown_ex1")),
                                         column(6,style="display: inline-block;padding: 10px;;position:relative;left:300px;",uiOutput("dropDown_ex2"))),
                                #splitLayout(style = "border: 1px solid silver:;position:relative;left:-60px;", cellWidths = c(750,750),
                                   #         uiOutput("dropDown_ex1"),
                                      #      uiOutput("dropDown_ex2")
                               #),
                                splitLayout(style = "border: 1px solid silver:;position:relative;left:-40px;", cellWidths = c(700,650),
                                            plotlyOutput("Plot_ex1",width = "650px",height = 700),
                                            plotlyOutput("Plot_ex2",width = "650px",height = 700)
                                )
                              ),
                              tabPanel(
                                "CLUSTERING",
                                fluidRow(column(6,style="display: inline-block;padding: 10px;position:relative;right:5px;",tags$h3(tags$strong("HeatMap"))),
                                         column(6,style="display: inline-block;padding: 10px;position:relative;left:300px;",tags$h3(tags$strong("K Means Clustering")))
                                         ),
                                fluidRow(column(6,style="display: inline-block;padding: 10px;position:relative;right:5px;",uiOutput("dropDown_ex3")),
                                         column(6,style="display: inline-block;padding: 10px;position:relative;left:300px;",uiOutput("dropDown_ex4"))),
                                splitLayout(style = "border: 1px solid silver:;position:relative;left:-40px;", cellWidths = c(700,650),
                                            plotlyOutput("Plot_ex3",width = "650px",height = 700),
                                            plotlyOutput("Plot_ex4",width = "650px",height = 700)
                                )
                                #plotlyOutput("Plot_ex3",width = "1000px",height = 700),
                                
                              ),
                              tabPanel(
                                "PRINCIPAL COMPONENT ANALYSIS",
                                tags$br(),
                                tags$br(),
                                fluidRow(column(4,style="display: inline-block;padding: 10px;position:relative;right:5px;",uiOutput("dropDown_1")),
                                         column(4,style="display: inline-block;padding: 10px;position:relative;left:440px;",uiOutput("dropDown_11")))
                                ,fluidRow(column(4,style="display: inline-block;padding: 10px;position:relative;right:5px;",uiOutput("dropDown_2")),
                                         column(4,style="display: inline-block;padding: 10px;position:relative;left:440px;",uiOutput("dropDown_22"))
                                         ),
                                fluidRow(column(6,style="display: inline-block;padding: 10px;position:relative;left:750px;",uiOutput("dropDown_3"))
                                         ),
                                splitLayout(style = "border: 1px solid silver:;position:relative;left:-40px;", cellWidths = c(700,700),
                                            uiOutput("loadingPlot_1"),
                                            uiOutput("ScorePlot_1")
                                            #plotlyOutput("Plot_ex3",width = "650px",height = 700),
                                            #plotlyOutput("Plot_ex4",width = "650px",height = 700)
                                ),
                                tags$br(),
                                tags$br(),
                                fluidRow(column(6,style="display: inline-block;padding: 10px;position:relative;right:5px;",uiOutput("dropdown_PCA"))
                                ),
                                  column(8,uiOutput("Table"),
                                         tags$br(),
                                         uiOutput("Download_tab")
                                  )
                                )
                            )
                          )
               )
)



server<- function(input,output,session){
  
  #track_usage(
   # storage_mode = store_json(path = "/Users/shubhada/Downloads/Logs1/")
  #)
  
  output$dropDown_ex1<- renderUI({
    a<- colnames(df)
    a<- a[4:length(a)-1]
    pickerInput("Dropdown_exd","Select a Feature :",choices = a, multiple = TRUE,selected = a[1:3],options=list(style="class_1"))
  })
  
  output$dropDown_ex2<- renderUI({
    a<- colnames(df)
    a<- a[4:length(a)-1]
    selectInput("Dropdown_exd2","Select a Feature :",choices = a,selected = a[1])
  })
  
  
  #observe(print(input$Dropdown_exd))
  
  output$Plot_ex1<- renderPlotly({
    req(input$Dropdown_exd)
    a<- colnames(df)
    a<- a[4:length(a)-1]
    df<- df[,3:length(df)-1]
    df_2<- df11[df11['Features']==input$Dropdown_exd,]
    #df<- df %>% mutate_at(colnames(df)[3:length(colnames(df))], ~(scale(.) %>% as.vector))
    #df_12 = data.frame(matrix(vector(), nrow(df), 30, dimnames=list(c(), a)),stringsAsFactors=F)
    #for (i in seq_along(input$Dropdown_exd)){
     # a1<- df[colnames(df)[i]==input$Dropdown_exd[i],][i]
      #df_12[,i]=a1
     # print(df_12)
      #fig<- plot_ly(y= as.vector(unlist(df[input$Dropdown_exd[i]])), type="box")
    #}
    ggplot(df_2, aes(x=Features, y=Value,color=Features)) + 
      geom_boxplot(notch=TRUE) +
      theme(axis.text.x=element_text(angle=60,hjust=1))
    #fig<- fig %>% add_trace(y= as.vector(unlist(df[input$Dropdown_exd[2]])))
    #fig<- fig %>% add_trace(y= as.vector(unlist(df[input$Dropdown_exd[length(input$Dropdown_exd)]])))
  })
  
  output$Plot_ex2<- renderPlotly({
    df_2<- df11[df11['Features']==input$Dropdown_exd2,]
    ggplot(df_2, aes(x=Value, color=Diagnosis)) + geom_histogram(fill="white", alpha=0.5, position="identity") +
      theme(legend.title = element_text(size = 10), legend.text = element_text(size = 10))
  })
  
  output$dropDown_ex3<- renderUI({
    a<- colnames(df)
    a<- a[4:length(a)-1]
    pickerInput("Dropdown_exd34","Select a Feature :",choices = a, multiple = TRUE,selected = a[1:15],options=list(style="class_1"))
    #selectInput("Dropdown_exd34","Select a value :",choices = c("HeatMap","Clustering"),selected = a[1])
  })
  
  aa<- reactive({
    req(input$Dropdown_exd34)
    var=length(input$Dropdown_exd34)
    print(var)
    dat <- as.data.frame(matrix(ncol=0, nrow=569))
    for (i in ncol(dat)){
      print(i)
      dat[,input$Dropdown_exd34]<- d[,input$Dropdown_exd34]
      }
    m <- as.matrix(dat)
    rownames(m)<- df$diagnosis
    s1<- m[rownames(m)=="M",]
    s2<- m[rownames(m)=="B",]
    s3<- rbind(s1,s2)
    s3
  })
  
  output$Plot_ex3<- renderPlotly({
    #df<- aa()
    print(input$Dropdown_exd34)
    s<- melt(d,id.vars=c("diagnosis"))
    s1<- s[s['diagnosis']=='M',]
    s2<- s[s['diagnosis']=='B',]
    s3<- rbind(s1,s2)
    s3<- s3[s3['variable']==input$Dropdown_exd34,]
    print(s3)
    s3<- s3 %>% group_by(variable,diagnosis) %>% summarise(value=mean(value))
    s3<- as.data.frame(s3)
    print(s3)
    #s4$variable<- input$Dropdown_exd34
    colnames(s3)<- c("variable","diagnosis","value")
    print(s3)
    #df<- t(df)
    #print(head(df))
    #coul <- colorRampPalette(brewer.pal(8, "PiYG"))(25)
    #heatmap(df, scale='column',xlab="diagnosis", ylab="variable",  col= colorRampPalette(brewer.pal(8, "Blues"))(25))
    heatmap.plot <- ggplot(data = s3, aes(x = diagnosis, y = variable)) +
      geom_tile(aes(fill = value)) +
      scale_colour_gradient() +
      theme(axis.text.y = element_text(size = 6))
  })
  
  output$dropDown_ex4<- renderUI({
    a<- colnames(df)
    a<- a[4:length(a)-1]
    pickerInput("Dropdown_exd44","Select a Feature :",choices = a, multiple = TRUE,selected = a[1:2],options=list(style="class_1",`max-options`=2))
  })
  
  observe(print(input$Dropdown_exd44[1]))
  observe(print(input$Dropdown_exd44[2]))
  
  output$Plot_ex4<- renderPlotly({
    req(length(input$Dropdown_exd44)==2)
    print(input$Dropdown_exd44[1])
    #d11<- as.matrix(d11)
    #rownames(d11)<- d$diagnosis
    #print(head(d11))
    d11$diagnosis<- df$diagnosis
    d11$cluster<- factor(k2$cluster)
    #p<- d11 %>%
     # as_tibble() %>%
     # mutate(cluster = k2$cluster,
         #    state = row.names(d11)) %>%
     # ggplot(aes(x=d11[,input$Dropdown_exd44[1]],y=d11[,input$Dropdown_exd44[2]],color = factor(cluster),label = state,text=paste(input$Dropdown_exd44[1], ": ",specify_decimal(d11[,input$Dropdown_exd44[1]],3),"<br>",input$Dropdown_exd44[2], ": ",specify_decimal(d11[,input$Dropdown_exd44[2]],3),"<br> Color: ",factor(cluster),"<br> Label: ",state))) + xlab(input$Dropdown_exd44[1]) + ylab(input$Dropdown_exd44[2])+
      #geom_text()
    #ggplotly(p,tooltip = c("text"))
    ggscatter(
      d11, x =input$Dropdown_exd44[1], y = input$Dropdown_exd44[2], 
      color = "cluster", palette = "npg", ellipse = TRUE, ellipse.type = "convex",
      shape = "diagnosis", size = 1.5,  legend = "right", ggtheme = theme_bw(),
      xlab = input$Dropdown_exd44[1],
      ylab = input$Dropdown_exd44[2]
    ) + stat_mean(aes(color = cluster), size = 4)
  })
  
  output$dropDown_1<- renderUI({
    df2<-df2 %>% 
      select(-c(X,diagnosis)) %>% 
      as.data.frame()
    
    selectInput("Dropdown1","Select X Axis :",choices = colnames(df2))
  })
  
  output$dropDown_2<- renderUI({
    df2<-df2 %>% 
      select(-c(X,diagnosis)) %>% 
      as.data.frame()
    df2[input$Dropdown1]<- NULL
    
    selectInput("Dropdown2","Select Y Axis :",choices = colnames(df2))
  })
  
  output$dropDown_11<- renderUI({
    df2<-df2 %>% 
      select(-c(X,diagnosis)) %>% 
      as.data.frame()
    
    selectInput("Dropdown_1","Select X Axis :",choices = colnames(df2))
  })
  
  output$dropDown_22<- renderUI({
    df2<-df2 %>% 
      select(-c(X,diagnosis)) %>% 
      as.data.frame()
    df2[input$Dropdown1]<- NULL
    
    selectInput("Dropdown_2","Select Y Axis :",choices = colnames(df2))
  })
  
  output$dropDown_3<- renderUI({
    df1<-df1 %>% 
      select(-X) %>% 
      as.data.frame()
    
    pickerInput("Dropdown3","Select a Column :",choices = colnames(df1), multiple = TRUE,selected = colnames(df1)[1:5],options=list(style="class_1"))
  })
  
  
  output$loadingPlot_1<- renderUI({
    plotlyOutput("loadingPlot",width = "650px",height = 700)
  })
  
  
  output$loadingPlot<- renderPlotly({
    #print(input$Dropdown1)
    #print(input$Dropdown2)
    theme<-theme(panel.background = element_blank(),panel.border=element_rect(fill=NA),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),strip.background=element_blank(),axis.text.x=element_text(colour="black"),axis.text.y=element_text(colour="black"),axis.ticks=element_line(colour="black"),plot.margin=unit(c(1,1,1,1),"line"),plot.title = element_text(color = "black",size=15,face="bold"))
    rownames(df3)<- c("PC1","PC2","PC3","PC4","PC5","PC6","PC7","PC8","PC9")
    a<- df3[input$Dropdown1,][2][,1]*100
    b<- df3[input$Dropdown2,][2][,1]*100
    #a<- df3[1,][2][,1]*100
    #b<-df3[2,][2][,1]*100
    df_2<- df2[,c(input$Dropdown1,input$Dropdown2,'diagnosis')]
    a1<- paste0(input$Dropdown1,"(",paste(as.character(round(a)), "%", ")", sep=""))
    b1<- paste0(input$Dropdown2,"(",paste(as.character(round(b)), "%", ")", sep=""))
    p<-ggplot(df_2,aes(x=df_2[,1],y=df_2[,2],color=diagnosis,text=paste(input$Dropdown1, ": ",specify_decimal(df_2[,1],3),"<br>",input$Dropdown2, ": ",specify_decimal(df_2[,2],3),"<br> Diagnosis: ",diagnosis))) +
      geom_point() +theme + xlab(a1) + ylab(b1)+ ggtitle(" Score Plot")
    p
    ggplotly(p, tooltip = c("text"))
  })
  
  output$ScorePlot_1<- renderUI({
    plotlyOutput("score_plt",width = "650px",height = 700)
  })
  
  #output$score_plt<- renderPlot({
    #vars.p<- ggplot() +
    #  geom_path(data = circ,aes(x,y), lty = 2, color = "grey", alpha = 0.7) +
    #  geom_hline(yintercept = 0, lty = 2, color = "grey", alpha = 0.9) +
    #  geom_vline(xintercept = 0, lty = 2, color = "grey", alpha = 0.9) +
    #  geom_segment(data = res.pca$rotation, aes(x = 0, xend = input$Dropdown_1, y = 0, yend = input$Dropdown_2),
      #             arrow = arrow(length = unit(0.025, "npc"), type = "open"), 
      #             lwd = 1) + 
     # geom_text(data = res.pca$rotation[input$Dropdown3,], 
         #       aes(x = input$Dropdown_1*1.15, y =  input$Dropdown_2*1.15, 
           #         label=c(input$Dropdown3)),
           #     check_overlap = F, size = 3) +
     # xlab("PC 1") + 
     # ylab("PC2") +
     # coord_equal() +
      #theme_minimal() +
     # theme(panel.grid = element_blank(), 
        #    panel.border = element_rect(fill= "transparent"))
    #vars.p
 # })
  
  output$score_plt<- renderPlotly({
    theme<-theme(panel.background = element_blank(),panel.border=element_rect(fill=NA),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),strip.background=element_blank(),axis.text.x=element_text(colour="black"),axis.text.y=element_text(colour="black"),axis.ticks=element_line(colour="black"),plot.margin=unit(c(1,1,1,1),"line"),plot.title = element_text(color = "black",size=15,face="bold"))
    b<- as.data.frame(res.pca$rotation)
    b<-as.data.frame(dplyr::as_tibble(b, rownames = "Features"))
    print(input$Dropdown3)
    df_12 = data.frame(matrix(vector(), 0, 31, dimnames=list(c(), c("Features","PC1","PC2","PC3","PC4","PC5","PC6","PC7","PC8","PC9","PC10","PC11","PC12","PC13","PC14","PC15","PC16","PC17","PC18","PC19","PC20","PC21","PC22","PC23","PC24","PC25","PC26","PC27","PC28","PC29","PC30"))),stringsAsFactors=F)
    for (i in seq_along(input$Dropdown3)){
      a1<- b[b['Features']==input$Dropdown3[i],]
      df_12[i,]=a1
    }
    print(df_12)
    print(input$Dropdown_1)
    print(input$Dropdown_2)
    a<- df_12[,c("Features",input$Dropdown_1,input$Dropdown_2)]
    print(a)
    p<-ggplot(a, aes(x=a[,2],y=a[,3],color=Features,text=paste(input$Dropdown_1, ": ",specify_decimal(a[,2],3),"<br>",input$Dropdown_2, ": ",specify_decimal(a[,3],3),"<br> Feature: ",Features))) + geom_point()+
      modelr::geom_ref_line(h = 0) +
      modelr::geom_ref_line(v = 0) +
      geom_text(aes(label = Features), size = 3)+ theme +
      xlab(input$Dropdown_1) + 
      ylab(input$Dropdown_2) + 
      ggtitle("Loading Plot")
    ggplotly(p, tooltip = c("text"))
  })
  
  
  output$dropdown_PCA<- renderUI({
    selectInput("dropdown_3","Select a Dataset :",choices = c("Original Dataset","Variables_Coordinates","Variables_Contribution_towards_PCs"))
  })
  
  output$Table<- renderUI({
    DT::dataTableOutput("myTable_click", width="1000px")
  })
  
  output$myTable_click<- DT::renderDataTable({
    if (input$dropdown_3=="Original Dataset"){
      df1$X<- NULL
      return(DT:: datatable(df1, escape = FALSE,options = list(paging = FALSE,autoWidth = TRUE,scrollX=TRUE,scrollY="270px")))
    }
    else if(input$dropdown_3=="Variables_Coordinates"){
      res.var <- get_pca_var(res.pca)
      s<- as.data.frame(res.var$coord)
      s$X<- NULL
      DT:: datatable(s,escape = FALSE,options = list(paging = FALSE,autoWidth = TRUE,scrollX=TRUE,scrollY="270px"))
    }
    else if(input$dropdown_3=="Variables_Contribution_towards_PCs"){
      res.var <- get_pca_var(res.pca)
      y<- as.data.frame(res.var$contrib)
      y$X<- NULL
      DT:: datatable(y, escape = FALSE,options = list(paging = FALSE,autoWidth = TRUE,scrollX=TRUE,scrollY="270px"))
    }
  })
  
  output$Download_tab<- renderUI({
    downloadButton("downloadData","Download",class="class_1")
  })
  
  output$downloadData<- downloadHandler(
    filename=function(){
      if(input$dropdown_3=="Original Dataset"){
        paste("OriginalDataset","_",Sys.Date(),".csv", sep = "")
      }
      else if(input$dropdown_3=="Variables_Coordinates"){
        paste("Variables_Coordinates","_",Sys.Date(),".csv", sep = "")
      }
      else{
        paste("Variables_Contribution_towards_PCs","_",Sys.Date(),".csv", sep = "")
      }
    },
    content=function(file){
      if (input$dropdown_3=="Original Dataset"){
        df1$X<- NULL
        write.csv(df1, file)
      }
      else if (input$dropdown_3=="Variables_Coordinates") {
        res.var <- get_pca_var(res.pca)
        s<- as.data.frame(res.var$coord)
        s$X<- NULL
        write.csv(s, file)
      }
      else{
        res.var <- get_pca_var(res.pca)
        y<- as.data.frame(res.var$contrib)
        y$X<- NULL
        write.csv(s, file)
      }
    }
  )
  

}

shinyApp(ui,server)

