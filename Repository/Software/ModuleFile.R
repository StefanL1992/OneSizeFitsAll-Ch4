#All modules for Incentives study
library(shiny)
library(shinyjs)


setupNextUI<-function(id){
  ns<-NS(id)
  actionButton(ns("nextb"),"Next")
}

setupNext<-function(input,output,session){
  return(input)
}

PieBisectionUI<-function(id,instr,part){
  ns=NS(id)
  
  
  tabPanel(id=ns('pie'),part,
           tabsetPanel(
             tabPanel("Instructions",
                      column(7,tags$h5(instr))), 
             
             tabPanel("Start Task",
                      fluidRow(id=ns("Startrow"),actionButton(ns("Start"),"Start Task")),
                      fluidRow(hidden(textOutput(ns("nextt"))), 
                               hidden(fluidRow(id=ns("R1"),
                                               (column(4,wellPanel(id=ns('A1'),style = "background-color: #ffffff;text-align:center",  plotOutput(ns("piea")),uiOutput(ns("BA")), actionButton(ns("A"),"Choose A")))),
                                               (column(4,wellPanel(id=ns('A2'),style = "background-color: #ffffff;text-align:center", plotOutput(ns("pieb")), uiOutput(ns("BB")),actionButton(ns("B"),"Choose B")))),
                                               (column(8,wellPanel(id=ns("Ind"),style="background-color: #ffffff;text-align:center",actionButton(ns("IN"),"I am indifferent")))))),
                               hidden(fluidRow(id=ns('p'),column(4,wellPanel(actionButton(ns('N'),'Next choice'))))),
                               
                               hidden(fluidRow(id=ns("test1"),column(5,offset=2,wellPanel(style = "background-color: #ffffff",actionButton(ns("C"),"Show slider"),hidden(uiOutput(ns("slider"))))))))))
  )
}

#Server input

PieBisection<-function(input,output,session,yT1,p1=100,r1a=100,r1b=100,p2=100,r2a=100,r2b=100,ssc=0,ssn=5,pbw=0,pbwc=c(10,30,50,70,90),sg=10,nbis=5,nextb,bound=0,buttonID){
  
  observeEvent(input$Start,{
    yT1$pbword=sample(pbwc)
    if(pbw==0){
      if(p1!=0){yT1$ev=((r2a*(p2/100))+(r2b*((100-p2)/100))-(r1a*(p1/100)))/((100-p1)/100)}
      else{yT1$ev=r2a*(p2/100)+(r2b*((100-p2)/100))}
      yT1$r2b=r2b 
    }
    else{if(p1!=0){yT1$ev=((r2a*(yT1$pbword[yT1$conf+1]/100))+(r2b*((100-yT1$pbword[yT1$conf+1])/100))-(r1a*(p1/100)))/((100-p1)/100)}
      else{yT1$ev=r2a*(yT1$pbword[yT1$conf+1]/100)+(r2b*((100-yT1$pbword[yT1$conf+1])/100))}
      yT1$r2b=r2b }
    
    show("R1");hide("Startrow")
  })
  
  
  
  
  #Panel 1    
  observeEvent(input$A,{
    hide('A1');hide('A2');show('p');hide("Ind")
    if(yT1$clicks<1){yT1$l=yT1$ev}
    yT1$clicks<-yT1$clicks+1
    if(yT1$r2b<=0&r2a==sg){yT1$x[1]=r2a}
    else{yT1$x[1]<-yT1$r2b}
    yT1$x[2]<-yT1$ev
    yT1$l<-yT1$l-(0.5*abs(yT1$l-yT1$x[(yT1$clicks)]))
    yT1$x[2+yT1$clicks]<-yT1$l
    
    #after x clicks
    if(yT1$clicks>=nbis){show("test1");hide('p')
    
    #fix bound
    if(bound==1){  
      if(is.sorted(yT1$x[2:(2+yT1$clicks)])&yT1$x[3]>0){yT1$ri=10}else if(is.sorted(-(yT1$x[2:(2+yT1$clicks)]))&yT1$x[3]<0){yT1$le=10}
      }
     }
  })
  
  observeEvent(input$B,{
    hide('A1');hide('A2');show('p');hide("Ind")
    if(yT1$clicks<1){yT1$l=yT1$ev}
    yT1$clicks<-yT1$clicks+1
    if(yT1$r2b<=0&r2a==sg){yT1$x[1]=yT1$r2b}
    else{yT1$x[1]<-r2a}
    yT1$x[2]<-yT1$ev
    yT1$l<-yT1$l+(0.5*abs(yT1$l-yT1$x[(yT1$clicks)]))
    yT1$x[2+yT1$clicks]<-yT1$l
    if(yT1$clicks>=nbis){show("test1");hide('p')
      #fix bound
      if(bound==1){  
        if(is.sorted(yT1$x[2:(2+yT1$clicks)])&yT1$x[3]>0){yT1$ri=10}else if(is.sorted(-(yT1$x[2:(2+yT1$clicks)]))&yT1$x[3]<0){yT1$le=10}
      }
    }
  })
  
  observeEvent(input$N,{
    show('A1');show('A2');hide('p');show("BB");show("BA");show("Ind")
  })
  
  observeEvent(input$IN,{
    if(yT1$clicks<1){yT1$lc=yT1$ev}
    else{yT1$lc<-yT1$l}
    show('A1');show('A2')
    show("slider");show("test1")
    show("S"); hide("A");hide("B");hide("C");hide("Ind")
  })
  
  
  output$piea <- renderPlot({
    
    if(yT1$clicks<1){vala=yT1$ev}
    else{vala=yT1$l}
    
    ##colour coding
    if(r1a>0){c1="darkseagreen1"}else if(r1a==0){c1="cornsilk"}else{c1="darksalmon"}
    if(vala>0){c2="aquamarine"}else if(vala==0){c2="ivory"}else{c2="lightsalmon"}
    par(lwd=2)
    pie(c((100-p1),p1),clockwise=T, radius=1,col=c(c2,c1),labels=c(""))
    title(main="Choice A", cex.main=3)
    
    if (p1!=0){
      text(x=-0.5,y=0.4,r1a, cex=2,font=2)
      text(x=0.5,y=0.4,round(vala,2),cex=2,font=2)}
    
    else{text(x=0,y=-0.15,round(vala,2), cex=2, font=2)}
  })
  
  output$pieb <- renderPlot({
    par(lwd=2)
    ##colour coding
    if(r2a>0){c3="darkolivegreen1"}else if(r2a==0){c3="cornsilk"}else{c3="tan1"}
    if(yT1$r2b>0){c4="mediumaquamarine"}else if(yT1$r2b==0){c4="ivory"}else{c4="rosybrown1"}
    
    if(pbw==0){pie(c((100-p2),p2),clockwise=T,lwd=4, radius=1,col=c(c4,c3),labels=c(""))
      if (p2!=100){
        text(x=-0.5,y=0.4,r2a, cex=2, font=2)
        text(x=0.5,y=0.4,yT1$r2b,cex=2, font=2)
      }
      else{text(x=0,y=-0.15,r2a, cex=2, font=2)}}
    else{pie(c((100-yT1$pbword[yT1$conf+1]),yT1$pbword[yT1$conf+1]),lwd=2,clockwise=T, radius=1,col=c(c4,c3),labels=c(yT1$r2b,r2a),cex=2)
    }
    title(main="Choice B", cex.main=3)
    
    
  })
  
  
  output$BA<- renderUI({
    if(yT1$clicks<1){vala=yT1$ev}
    else{vala=yT1$l}
    if (p1!=0){
      str1<-sprintf("%s Euro with %s%% chance",r1a,p1)
      str2<-sprintf("%s Euro with %s%% chance",round(vala,2),(100-p1))  
      tags$h5(HTML(paste(str1, str2, sep = '<br/>')))}
    else{
      str1<-sprintf("%s Euro with %s%% chance",round(vala,2),(100))   
      tags$h5(HTML(paste(str1,"", sep = '<br/><br/>')))}
    
  })
  
  output$BB<- renderUI({
    if(pbw==0){
      if(p2!=100){
        str1<-sprintf("%s Euro with %s%% chance",round(r2a,2),p2)
        str2<-sprintf("%s Euro with %s%% chance",round(yT1$r2b,2),(100-p2))  
        tags$h5(HTML(paste(str1, str2, sep = '<br/>')))}
      else{
        str1<-sprintf("%s Euro with %s%% chance",round(r2a,2),(100))   
        tags$h5(HTML(paste(str1,"", sep = '<br/><br/>')))}}
    else{
      str1<-sprintf("%s Euro with %s%% chance",round(r2a,2),yT1$pbword[yT1$conf+1])
      str2<-sprintf("%s Euro with %s%% chance",round(yT1$r2b,2),(100-yT1$pbword[yT1$conf+1]))  
      tags$h5(HTML(paste(str1, str2, sep = '<br/>')))}
    
  })
  
  
  
  output$slider<-renderUI({
    ns<-session$ns
    list(sliderInput(inputId = ns("num"),label = "Please select your indifference value", value =yT1$lc, min = round(((yT1$le)*yT1$lc-1),0), max =round(((yT1$ri)*yT1$lc+1),0), round=0, ticks=TRUE,step=0.1*(yT1$ri)*(yT1$le))
         ,actionButton(ns("S"),"Confirm"))})
  
  
  
  observeEvent(input$C, ignoreInit=TRUE, {
    yT1$lc<-round(yT1$l,2)
    show('A1');show('A2')
    toggle("slider")
    show("S"); hide("A");hide("B");hide("C")
  })
  
  observeEvent(input$num, ignoreInit = TRUE, {
    yT1$clicks=1
    yT1$l<-input$num
  })  
  
  #If we confirm the slider
  observeEvent(input$S, ignoreInit = TRUE, {
    ns<-session$ns
    yT1$l<-isolate(input$num)
    yT1$clicks<-NULL;
    yT1$clicks<-0
    yT1$x<-NULL;
    yT1$x<-c()
    yT1$conf<-yT1$conf+1
    yT1$ss[yT1$conf]=isolate(input$num)
    
    if(ssc==1){
      hide("S");hide("A2");hide("A1");hide("slider");hide("BB");hide("BA");show("p");show("A");show("B");show("C");hide("test1")
      yT1$r2b=yT1$l
      yT1$ev=((r2a*(p2/100))+(yT1$r2b*((100-p2)/100))-(r1a*(p1/100)))/((100-p1)/100)
      
      if (yT1$conf>=ssn){
        hide("slider");hide("S");hide("piea");hide("pieb");toggle("next");hide("BA");hide("BB");hide("C");show("test1");hide("p")
        insertUI(selector = sprintf("#%s",ns('test1')),
                 ui = setupNextUI(buttonID))}
    }
    else if(pbw==1){
      hide("S");hide("A2");hide("A1");hide("slider");hide("BB");hide("BA");show("p");show("A");show("B");show("C");hide("test1")
      if(p1!=0){yT1$ev=((r2a*(yT1$pbword[yT1$conf+1]/100))+(r2b*((100-yT1$pbword[yT1$conf+1])/100))-(r1a*(p1/100)))/((100-p1)/100)}
      else{yT1$ev=r2a*(yT1$pbword[yT1$conf+1]/100)+(r2b*((100-yT1$pbword[yT1$conf+1])/100))}
      
      if (yT1$conf>=length(yT1$pbword)){
        hide("slider");hide("S");hide("piea");hide("pieb");toggle("next");hide("BA");hide("BB");hide("C");show("test1");hide("p")
        insertUI(selector = sprintf("#%s",ns('test1')),
                 ui = setupNextUI(buttonID))
      }
    }
    else{
      toggle("slider");hide("S");hide("piea");hide("pieb");toggle("next");hide("BA");hide("BB")
      
      insertUI(selector = sprintf("#%s",ns('test1')),
               ui = setupNextUI(buttonID))}
    
  }) 
  output$nextt<-renderText({"Continue with the next part"})
  
  observeEvent(input$nextb,{
    print(yT1$ss)})
  
  return(session)
  
}

TimeBisectionUI<-function(id,instr,part){
  ns=NS(id)
  
  
  tabPanel(id=ns('betadelta'),part,
           tabsetPanel(
             tabPanel("Instructions",
                      column(7,tags$h5(instr))), 
             
             tabPanel("Start Task",
                      fluidRow(),
                      fluidRow(hidden(textOutput(ns("nextt"))), 
                              fluidRow(id=ns("R1"),
                                        column(4,wellPanel(id=ns('A1'),style = "background-color: #ffffff;text-align:center",  plotOutput(ns("bara")),uiOutput(ns("BA")), actionButton(ns("A"),"Choose A"))),
                                        column(4,wellPanel(id=ns('A2'),style = "background-color: #ffffff;text-align:center", plotOutput(ns("barb")), uiOutput(ns("BB")),actionButton(ns("B"),"Choose B"))),
                                        column(8,wellPanel(id=ns("Ind"),style="background-color: #ffffff;text-align:center",actionButton(ns("IN"),"I am indifferent")))),
                               hidden(fluidRow(id=ns('p'),column(4,wellPanel(actionButton(ns('N'),'Next choice'))))),
                              
                               
                               hidden(fluidRow(id=ns("test1"),column(5,offset=2,wellPanel(style = "background-color: #ffffff",actionButton(ns("C"),"Show slider"),hidden(uiOutput(ns("slider")))))))))
           ))
}

#Server input

TimeBisection<-function(input,output,session,yT1,t1=0,r1=100,t2=10,r2=100,nbis=5,nextb,buttonID,bound=0){
 
  #Panel 1    
  observeEvent(input$A,{
    hide('A1');hide('A2');show('p');hide("Ind")
    if(yT1$clicks<1){yT1$l=r1}
    yT1$clicks<-yT1$clicks+1
    yT1$x[1]<-0
    yT1$x[2]<-r1
    yT1$l<-yT1$l-(0.5*abs(yT1$l-yT1$x[(yT1$clicks)]))
    yT1$x[2+yT1$clicks]<-yT1$l
    if(yT1$clicks>=nbis){
      #fix bound
      if(bound==1){  
        if(is.sorted(yT1$x[2:(2+yT1$clicks)])&yT1$x[3]>0){yT1$ri=10}else if(is.sorted(-(yT1$x[2:(2+yT1$clicks)]))&yT1$x[3]<0){yT1$le=10}
      }
      show("test1");hide('p')}
  })
  
  observeEvent(input$B,{
    hide('A1');hide('A2');show('p');hide("Ind")
    if(yT1$clicks<1){yT1$l=r1}
    yT1$clicks<-yT1$clicks+1
    yT1$x[1]<-0
    yT1$x[2]<-r1
    yT1$l<-yT1$l+(0.5*abs(yT1$l-yT1$x[(yT1$clicks)]))
    yT1$x[2+yT1$clicks]<-yT1$l
    if(yT1$clicks>=nbis){
      #fix bound
      if(bound==1){  
        if(is.sorted(yT1$x[2:(2+yT1$clicks)])&yT1$x[3]>0){yT1$ri=10}else if(is.sorted(-(yT1$x[2:(2+yT1$clicks)]))&yT1$x[3]<0){yT1$le=10}
      }
      show("test1");hide('p')}
  })
  
  observeEvent(input$N,{
    show('A1');show('A2');hide('p');show("Ind")
  })
  
  observeEvent(input$IN,{
    if(yT1$clicks<1){yT1$lc=r1}
    else{yT1$lc<-yT1$l}
    show('A1');show('A2')
    show("slider");show("test1")
    show("S"); hide("A");hide("B");hide("C");hide("Ind") 
  })
  
  output$bara <- renderPlot({
    barplot(t1,width=2,horiz=T,col=c("red"),xlim=c(-2,12),ylim=c(0,10),xlab='Weeks',density=10,axes=F)
    axis(1,at=c(0,2,4,6,8,10))
    text(x=5,y=5,"Choice A", cex=2)
    arrows(x0=t1,y0=2.5,x1=t1,y1=0,length=0.25,code=2,lwd=3)
    if(yT1$clicks==0){val=r1}else{val=yT1$l}
    text(x=t1,y=3.5,sprintf('%s\u20ac',round(val,2)), cex=2)
  })
  
  output$barb <- renderPlot({
    barplot(t2,width=2,horiz=T,col=c("green"),xlim=c(-2,12+2),ylim=c(0,10),xlab='Weeks',density=10,axes=F)
    axis(1,at=c(0,2,4,6,8,10))
    text(x=5,y=5,"Choice B", cex=2)
    arrows(x0=t2,y0=2.5,x1=t2,y1=0,length=0.25,code=2,lwd=3)
    text(x=t2,y=3.5,sprintf('%s\u20ac',r2), cex=2)
  })
  
  output$BA<- renderUI({
    
    if(yT1$clicks==0){val=r1}else{val=yT1$l}
      if(t1==0){str1<-sprintf("%s Euro today",round(val,2))}
      else{str1<-sprintf("%s Euro after %s weeks",round(val,2),t1)}
    
    tags$h5(HTML(paste(str1,"", sep = '<br/>')))
    
  })
  
  output$BB<- renderUI({
    str1<-sprintf("%s Euro after %s weeks",r2,t2)  
    tags$h5(HTML(paste(str1,"", sep = '<br/>')))
  })
  
  output$slider<-renderUI({
    ns<-session$ns
    list(sliderInput(inputId = ns("num"),label = "Please select your indifference value", value =yT1$lc, min = round((yT1$le)*yT1$lc-1,2), max =round((yT1$ri)*yT1$lc+1,2), round=0, ticks=TRUE,step=0.1*(yT1$ri)*(yT1$le))
         ,actionButton(ns("S"),"Confirm"))})
  
  
  
  observeEvent(input$C, ignoreInit=TRUE, {
    
    yT1$lc<-round(yT1$l,2)
    show('A1');show('A2')
    toggle("slider")
    print(input$slider)
    show("S"); hide("A");hide("B");hide("C")
  })
  
  observeEvent(input$num, ignoreInit = TRUE, {
    yT1$clicks=1
    yT1$l<-input$num
  })  
  
  #If we confirm the slider
  observeEvent(input$S, ignoreInit = TRUE, {
    ns<-session$ns
    yT1$l<-isolate(input$num)
    yT1$clicks<-NULL;
    yT1$clicks<-0
    yT1$x<-NULL;
    yT1$x<-c()
    yT1$conf<-yT1$conf+1
    toggle("slider");hide("S");hide("bara");hide("barb");toggle("next");hide("BA");hide("BB")
    
    insertUI(selector = sprintf("#%s",ns('test1')),
             ui = setupNextUI(buttonID))
    
  }) 
  output$nextt<-renderText({"Continue with the next part"})
  
  observeEvent(input$nextb,{
    show('nextt')})
  
  return(session)
  
}

