#Instructions NP-method
p.instr="In the rest of the experiment you will be offered several choice scenarios in which you compare two monetary lotteries (Choice A and Choice B), with differing probabilities assigned to the different rewards that you would obtain. In some cases the lotteries will involve negative values, which means that you  would lose some amount of your own money, for example due to receiving a fine. Also the pay-outs (both gains and losses) may occur at different points in time. Your task involves making choices between the two lotteries, and finally indicating for what monetary value you would be indifferent with a slider.\n\nThere are no right or wrong choices, we are just interested in your preferences. By indifferent we mean that you would like or dislike both lotteries to the same extent, and it would not matter to you which of the two you receive. \n\nTo get used to the visualization and choices you will start with a practice question. Press <Start Task> to continue."
cgl.instr="For this part, you are offered the choice between a 50/50 gamble in which you either lose or gain a certain amount of money (Choice A), or receving 0 Euros for sure, i.e. staying at your current wealth level. \n\nPress <Start Task> to continue."
ceg.instr="Now, you are offered a choice between a sure gain (Choice A), or a 50/50 gamble in which you either gain a larger sum, or receive 0 Euros.\n\nPress <Start Task> to continue."
cel.instr="For this next part, you have the choice between a sure loss (Choice A), or a 50/50 gamble where you either lose nothing or lose a larger amount of money. \n\nPress <Start Task> to continue."
ofl.instr="Next, you face two lotteries which are both risky. You have the choice between a 50/50 gamble (Choice A) which yields a gain or a loss, and a 50/50 gamble (Choice B) that either yields a loss or 0 Euros.\n\nPress <Start Task> to continue."
ssg.instr="For this part, you will face a series of consecutive choices (a total of 4). In each of these series, you choose between two 50/50 gambles that can yield both gains and losses. Choice A will yield a higher possible gain, but you also risk a larger loss, compared to Choice B. \n\nPress <Start Task> to continue."
ofg.instr="In this part, you are offered two lotteries with 50% chance. Choice A will involve both a chance of a loss and a gain, while choice B involves either a gain or receiving 0 Euro with 50% chance. \n\nPress <Start Task> to continue."
ssl.instr="Now, you will face a series of consecutive choices (a total of 4). In each of these series, you choose between two 50/50 gambles that can yield both gains and losses. Choice A will yield a larger possible loss, but you also have a chance of a higher gain, compared to Choice B. \n\nPress <Start Task> to continue."
pbwg.instr="In this series of 5 consecutive lottery choices, the chances will not always be 50/50. You will face a choice between either a sure gain (Choice A), and a gamble (Choice B). If you choose the gamble, you either gain a larger amount than in Choice A with a varying chance, or receive 0 Euros.\n\nPress <Start Task> to continue."
pbwl.instr="For this next series of 5 consecutive lottery choices, the chances will not always be 50/50. You will face a choice between either a sure loss (Choice A), and a gamble (Choice B). If you choose the gamble, you either lose a larger amount than in Choice A with a varying chance, or receive 0 Euros.\n\nPress <Start Task> to continue."
bdg1.instr="You are  faced with decisions about gains at different points of time. For this choice task you choose between a gain right now (after 0 weeks) and a gain in 5 weeks.\n\nPress <Start Task> to continue."
bdg2.instr="For this next choice task you choose between a gain after 5 weeks and a gain in 10 weeks.\n\nPress <Start Task> to continue."
bdl1.instr="Next, you are  faced with decisions about losses at different points of time. For this choice task you choose between a loss right now (after 0 weeks) and a loss in 5 weeks.\n\nPress <Start Task> to continue."
bdl2.instr="For this next choice task you choose between a loss after 5 weeks and a loss in 10 weeks.\n\nPress <Start Task> to continue."

#SETUP - Next buttons
nextbp <- callModule(setupNext,"practice")
nextb1 <- callModule(setupNext,"one")
nextb2 <- callModule(setupNext,"two")
nextb3 <- callModule(setupNext,"three")
nextb4 <- callModule(setupNext,"four")
nextb5 <- callModule(setupNext,"five")
nextb6 <- callModule(setupNext,"six")
nextb7 <- callModule(setupNext,"seven")
nextb8 <- callModule(setupNext,"eight")
nextb9 <- callModule(setupNext,"nine")
nextb10<- callModule(setupNext,"ten")
nextb11<- callModule(setupNext,"eleven")
nextb12<- callModule(setupNext,"twelve")
nextb13<- callModule(setupNext,"thirteen")



#Set-up Reactive values place-holders
yTP<-reactiveValues(l=0,x=c(),clicks=0,ri=1,le=1,sl=0,lc=0,conf=0,ss=0,r2b=0,ev=0,pbword=0);
yT1<-reactiveValues(l=0,x=c(),clicks=0,ri=1,le=1,sl=0,lc=0,conf=0,ss=0,r2b=0,ev=0,pbword=0); #loss 
yT2<-reactiveValues(l=0,x=c(),clicks=0,ri=1,le=1,sl=0,lc=0,conf=0,ss=0,r2b=0,ev=0,pbword=0); #CE - gain
yT3<-reactiveValues(l=0,x=c(),clicks=0,ri=1,le=1,sl=0,lc=0,conf=0,ss=0,r2b=0,ev=0,pbword=0); #CE - loss
yT4<-reactiveValues(l=0,x=c(),clicks=0,ri=1,le=1,sl=0,lc=0,conf=0,ss=0,r2b=0,ev=0,pbword=0); #SS - offset loss
yT5<-reactiveValues(l=0,x=c(),clicks=0,ri=1,le=1,sl=0,lc=0,conf=0,ss=0,r2b=0,ev=0,pbword=0); #SS - standard sequence
yT6<-reactiveValues(l=0,x=c(),clicks=0,ri=1,le=1,sl=0,lc=0,conf=0,ss=0,r2b=0,ev=0,pbword=0); #SS - offset loss
yT7<-reactiveValues(l=0,x=c(),clicks=0,ri=1,le=1,sl=0,lc=0,conf=0,ss=0,r2b=0,ev=0,pbword=0);
yT8<-reactiveValues(l=0,x=c(),clicks=0,ri=1,le=1,sl=0,lc=0,conf=0,ss=0,r2b=0,ev=0,pbword=0);
yT9<-reactiveValues(l=0,x=c(),clicks=0,ri=1,le=1,sl=0,lc=0,conf=0,ss=0,r2b=0,ev=0,pbword=0);
yT10<-reactiveValues(l=0,x=c(),clicks=0,ri=1,le=1,sl=0,lc=0,conf=0,ss=0,r2b=0,ev=0,pbword=0);
yT11<-reactiveValues(l=0,x=c(),clicks=0,ri=1,le=1,sl=0,lc=0,conf=0,ss=0,r2b=0,ev=0,pbword=0);
yT12<-reactiveValues(l=0,x=c(),clicks=0,ri=1,le=1,sl=0,lc=0,conf=0,ss=0,r2b=0,ev=0,pbword=0);
yT13<-reactiveValues(l=0,x=c(),clicks=0,ri=1,le=1,sl=0,lc=0,conf=0,ss=0,r2b=0,ev=0,pbword=0);



#Call modules - Sever logics
callModule(PieBisection,"practice",yT1=yTP,r1a=0,r1b=999,p1=0,r2a=100,r2b=25,p2=50,ssc=0,ssn=ssn,pbw=0,nbis=nbis,sg=sg,nextb=nextbp,bound=0,buttonID="practice") #Practice
callModule(PieBisection,"one",yT1=yT1,r1a=G,r1b=999,p1=50,r2a=0,r2b=0,p2=100,ssc=0,ssn=ssn,pbw=0,nbis=nbis,sg=sg,nextb=nextb1,bound=1,buttonID="one") #Loss/gain coupling
callModule(PieBisection,"two",yT1=yT2,r1a=0,r1b=999,p1=0,r2a=G,r2b=0,p2=50,ssc=0,ssn=ssn,pbw=0,nbis=nbis,sg=sg,nextb=nextb2,bound=0,buttonID="two") #CE-gains
callModule(PieBisection,"three",yT1=yT3,r1a=0,r1b=999,p1=0,r2a=0,r2b=yT1$l,p2=50,ssc=0,ssn=ssn,pbw=0,nbis=nbis,sg=sg,nextb=nextb3,bound=0,buttonID="three") #CE-losses
callModule(PieBisection,"four",yT1=yT4,r1a=yT2$l,r1b=999,p1=50,r2a=sl,r2b=0,p2=50,ssc=0,ssn=ssn,pbw=0,nbis=nbis,sg=sg,nextb=nextb4,bound=1,buttonID="four") #offset loss
callModule(PieBisection,"five",yT1=yT5,r1a=yT4$l,r1b=999,p1=50,r2a=sl,r2b=yT2$l,p2=50,ssc=1,ssn=ssn,pbw=0,nbis=nbis,sg=sg,nextb=nextb5,bound=1,buttonID="five") #SS gains
callModule(PieBisection,"six",yT1=yT6,r1a=yT3$l,r1b=999,p1=50,r2a=sg,r2b=0,p2=50,ssc=0,ssn=ssn,pbw=0,nbis=nbis,sg=sg,nextb=nextb6,bound=1,buttonID="six") #offset gain
callModule(PieBisection,"seven",yT1=yT7,r1a=yT6$l,r1b=999,p1=50,r2a=sg,r2b=yT3$l,p2=50,ssc=1,ssn=ssn,pbw=0,nbis=nbis,sg=sg,nextb=nextb7,bound=1,buttonID="seven") #SS losses
callModule(PieBisection,"eight",yT1=yT8,r1a=0,r1b=999,p1=0,r2a=max(yT5$ss),r2b=0,p2=50,ssc=0,ssn=ssn,pbw=1,nbis=nbis,sg=sg,nextb=nextb8,bound=0,buttonID="eight") #PBW - gains
callModule(PieBisection,"nine",yT1=yT9,r1a=0,r1b=999,p1=0,r2a=0,r2b=min(yT7$ss),p2=50,ssc=0,ssn=ssn,pbw=1,nbis=nbis,sg=sg,nextb=nextb9,bound=0,buttonID="nine") #PBW - losses
callModule(TimeBisection,"ten",yT1=yT10,t1=0,r1=0.5*max(yT5$ss),t2=5,r2=0.5*max(yT5$ss),nbis=nbis,nextb=nextb10,bound=1,buttonID="ten") #Betadelta - G1
callModule(TimeBisection,"eleven",yT1=yT11,t1=5,r1=0.5*max(yT5$ss),t2=10,r2=0.5*max(yT5$ss),nbis=nbis,nextb=nextb11,bound=1,buttonID="eleven") #Betadelta - G2
callModule(TimeBisection,"twelve",yT1=yT12,t1=0,r1=0.5*min(yT7$ss),t2=5,r2=0.5*min(yT7$ss),nbis=nbis,nextb=nextb12,bound=1,buttonID="twelve") #Betadelta - L1
callModule(TimeBisection,"thirteen",yT1=yT13,t1=5,r1=0.5*min(yT7$ss),t2=10,r2=0.5*min(yT7$ss),nbis=nbis,nextb=nextb13,bound=1,buttonID="thirteen") #Betadelta - L2



observeEvent(input$Go,{
  ##LOG SHAPER DATA
  shapedat$final=tab$y[,2]
  shapedat$p=input$lottery
  shapedat$mode=input$dincr
  print(shapedat$final)
  shapedat$df=cbind(c(subj$nr,shapedat$prec,shapedat$week,shapedat$mode,shapedat$p,shapedat$final))  
    insertTab('main',tab=PieBisectionUI("practice",p.instr,'Practice'),target='Incentives for the gym',position='after')
  removeTab('main','Incentives for the gym')
  
})

observeEvent(nextbp$nextb,{
  insertTab('main',tab=PieBisectionUI("one",cgl.instr,'Part1'),target='Practice',position='after')
  removeTab('main','Practice')
  
})

observeEvent(nextb1$nextb,{
  insertTab('main',tab=PieBisectionUI("two",ceg.instr,'Part2'),target='Part1',position='after')
  removeTab('main','Part1')
})

observeEvent(nextb2$nextb,{
  insertTab('main',tab=PieBisectionUI("three",cel.instr,'Part3'),target='Part2',position='after')
  removeTab('main','Part2')
})

observeEvent(nextb3$nextb,{
  insertTab('main',tab=PieBisectionUI("four",ofl.instr,'Part4'),target='Part3',position='after')
  removeTab('main','Part3')
})

observeEvent(nextb4$nextb,{
  insertTab('main',tab=PieBisectionUI("five",ssg.instr,'Part5'),target='Part4',position='after')
  removeTab('main','Part4')
})

observeEvent(nextb5$nextb,{
  insertTab('main',tab=PieBisectionUI("six",ofg.instr,'Part6'),target='Part5',position='after')
  removeTab('main','Part5')
})

observeEvent(nextb6$nextb,{
  insertTab('main',tab=PieBisectionUI("seven",ssl.instr,'Part7'),target='Part6',position='after')
  removeTab('main','Part6')
})

observeEvent(nextb7$nextb,{
  insertTab('main',tab=PieBisectionUI("eight",pbwg.instr,'Part8'),target='Part7',position='after')
  removeTab('main','Part7')
})

observeEvent(nextb8$nextb,{
  insertTab('main',tab=PieBisectionUI("nine",pbwl.instr,'Part9'),target='Part8',position='after')
  removeTab('main','Part8')
})

observeEvent(nextb9$nextb,{
  insertTab('main',tab=TimeBisectionUI("ten",bdg1.instr,'Part10'),target='Part9',position='after')
  removeTab('main','Part9')
})

observeEvent(nextb10$nextb,{
  insertTab('main',tab=TimeBisectionUI("eleven",bdg2.instr,'Part11'),target='Part10',position='after')
  removeTab('main','Part10')
})


observeEvent(nextb11$nextb,{
  insertTab('main',tab=TimeBisectionUI("twelve",bdl1.instr,'Part12'),target='Part11',position='after')
  removeTab('main','Part11')
})


observeEvent(nextb12$nextb,{
  insertTab('main',tab=TimeBisectionUI("thirteen",bdl2.instr,'Part13'),target='Part12',position='after')
  removeTab('main','Part12')
})



observeEvent(nextb13$nextb,{
  
  ######################################################
  # Data logging
  ######################################################
  
  r1<-reactiveValues(ssl=c(),ssg=c(),pbg=c(0,0,0,0,0),pbl=c(0,0,0,0,0),dwl=c(0,0,0,0,0),dwg=c(0,0,0,0,0),ucg=c(),ucl=c(),lambda=2,alphag=0,alphal=0,aucg=0,aucl=0,gammag=0,gammal=0,bdg=c(),bdl=c())
  
  #utility
  r1$ssl<-c(yT3$ss,yT7$ss)
  r1$ssg<-c(yT2$ss,yT5$ss)
  r1$ucl<-c(0,(1:length(r1$ssl)/length(r1$ssl)))*-1
  r1$ucg<-c(0,(1:length(r1$ssg)/length(r1$ssg)))

  
  #Beta-delta
  r1$bdl<-c(yT12$l,yT13$l)
  r1$bdg<-c(yT10$l,yT11$l)
  
  #########################
  # Probability weighting
  #########################
  
  #Reorder decision weights - losses
  ol=order(yT9$pbword)
  for(i in 1:5){r1$pbl[i]=yT9$ss[ol[i]]}
  
  #Reorder decision weights - gains
  og=order(yT8$pbword)
  for(i in 1:5){r1$pbg[i]=yT8$ss[og[i]]}
  
  
  for (j in 1:5){
    #Losses - Non-parametric
    py<-approx(c(0,r1$ssl), r1$ucl,r1$pbl[j])
    pyy<-py$y
    r1$dwl[j]<-pyy/-1
   
    #Gains - Non-Parametric
    py<-approx(c(0,r1$ssg), r1$ucg,r1$pbg[j])
    pyy<-py$y
    r1$dwg[j]<-pyy/1
  }
  print(r1$pbl)
  print(r1$dwl)
  #Losses - Parametric 
  p<-c(0,0.10,0.30,0.50,0.70,0.90,1)
  yl<-c(0,rev(r1$dwl),1)
  model3<-nlsLM(yl~p^g/(p^g + (1-p)^g)^(1/g), start=list(g=1.5))
  r1$gammal<-model3$m$getPars() 
  
  #Gains - Parametric
  p<-c(0,0.10,0.30,0.50,0.70,0.90,1)
  yg<-c(0,(r1$dwg),1)
  model2<-nlsLM(yg~p^g/(p^g + (1-p)^g)^(1/g), start=list(g=1.5))
  r1$gammag<-model2$m$getPars()
  
  #############################
  # Utility curvature
  #############################
  
  #Non-parametric - area under the curve - gains
  seqg=c(0,r1$ssg)/max(r1$ssg)
  n<-length(seqg)-1
  nn<-(0:n)
  nnn<-nn/n
  r1$aucg=trapz(seqg,nnn)
  
  #parametric - gain
  model3<-nlsLM(nnn~(seqg)^a, start=list(a=1))
  r1$alphag<-model3$m$getPars()
  
  ##Non-parametric - area under the curve - losses
  seql<-c(0,r1$ssl)/-min(r1$ssl)
  n<-length(seql)-1
  nn<-(0:n)
  nnn<-nn/-n
  r1$aucl=trapz(seql,nnn)
  
  #parametric - losses
  model4<-nlsLM(-nnn~(-seql)^a, start=list(a=1))
  r1$alphal<-model4$m$getPars()
  
  ###################################
  #  Loss aversion
  ###################################
  r1$lambda=(r1$ssg[1]/-r1$ssl[1])  
  
  
  ##################################
  # Beta-delta
  ##################################
  
  #Discount rate & Beta - gains
  uxy=approx(c(0,r1$ssg), r1$ucg, r1$bdg[1]) #0-5 r1
  ux=uxy$y
  uyy=approx(c(0,r1$ssg), r1$ucg, r1$bdg[2]) #5-10 r1
  uy=uyy$y
  uzy<-approx(c(0,r1$ssg), r1$ucg, 0.5*max(r1$ssg)) ##outcome r2
  uz<-uzy$y
  
  d5g=(uy/uz)^(1/5)
  rg=(1/d5g)-1
  btg=ux/(d5g*uz)
  
  #Discount rate & Beta - losses
  uxy=approx(c(0,r1$ssl), r1$ucl, r1$bdl[1]) #0-5 r1
  ux=uxy$y
  uyy=approx(c(0,r1$ssl), r1$ucl, r1$bdl[2]) #5-10 r1
  uy=uyy$y
  uzy<-approx(c(0,r1$ssl), r1$ucl, 0.5*min(r1$ssl)) ##outcome r2
  uz<-uzy$y
  
  d5l=(uy/uz)^(1/5)
  rl=(1/d5l)-1
  btl=ux/(d5l*uz)
  
  
  #############################
  # Data logging
  #############################
  lap=r1$lambda
  aucg=r1$aucg
  aucl=r1$aucl
  ucg=r1$alphag
  ucl=r1$alphal
  gg=r1$gammag
  gl=r1$gammal
  stab=t(shapedat$df)
  colnames(stab)=c("Subjnr","Prec","Weekly","Mode","P","W1","W2","W3","W4","W5","W6","W7","W8","W9","W10")
  ftab=cbind(c(lap,aucg,aucl,ucg,ucl,gg,gl,r1$dwg,rev(r1$dwl),c(rev(r1$ssl),0,r1$ssg),r1$pbg,rev(r1$pbl),r1$bdg,r1$bdl,d5g,rg,btg,d5l,rl,btl))
  ttab=t(ftab)
  print(ttab)
  namesft=c("LAKW","AUC.G","AUC.L","Alpha.G","Alpha.L","Gamma.G","Gamma.L","DWG10","DWG30","DWG50","DWG70","DWG90","DWL10","DWL30","DWL50","DWL70","DWL90",
            "SSL4","SSL3","SSL2","SSL1","RP","SSG1","SSG2","SSG3","SSG4","PBSG10","PBSG30","PBSG50","PBSG70","PBSG90","PBSL10","PBSL30","PBSL50","PBSL70","PBSL90","BDG05","BDG510","BDL05","BDL510",
            "D5G","RG","BetaG","D5L","RL","BetaL")
  finaltab=cbind(stab,ttab)
  finaldat$d=finaltab
  insertTab('main',tabPanel("Final part",actionButton("final","Continue to final task")),target='Part13',position='after')
  removeTab('main','Part13')
})