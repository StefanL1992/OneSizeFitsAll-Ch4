
################################################################################################################################################################
# Set-up
################################################################################################################################################################
#setwd("\\\\campus.eur.nl\\users\\home\\61869sli\\Documents\\Files Shaper") #Change depending on PC \\campus.eur.nl\users\home\61869sli\Documents\Files Shaper
#setwd("C://Users//stefa//Desktop//Files Shaper")
library(shiny);library(shinyjs);library(shinythemes);library(xtable);library(pracma);require(minpack.lm);require(Hmisc)
################################################################################################################################################################
# Load modules 
################################################################################################################################################################
eval(parse("ModuleFile.R"))

################################################################################################################################################################
# First screen
################################################################################################################################################################

ui <- navbarPage(id="main","Health Behavior and Choices", theme=shinytheme("paper"), 
                 tabPanel(id="Set-up","Set-up",useShinyjs(),
                          fluidRow(column(6,offset=0,(tags$h5('Experimenter set-up')))),
                          fluidRow(column(6,offset=0,(numericInput("subjn", "Subject number", 0, min = 1, max = 1000)))),
                          fluidRow(column(6,offset=0,(actionButton('subjc','Confirm'))))
                 ))

################################################################################################################################################################
# Server
################################################################################################################################################################
server <- function(input, output,session) {                          

  ################################################################################################################################################################
  # Instruction shaper
  ################################################################################################################################################################
instr.hyp="Please imagine the following situation: you have set yourself the goal of losing weight, so you decided to get a gym membership. Now, your employer wants to help you to lose weight. This may decrease your chances of taking up sick leave and increase your overall wellbeing. As such, your employer has offered to pay you a financial reward if you use the gym at least three times each weekfor a 10 week period. Your employer is quite flexible, and besides the expected pay-out has no preference in how your financial reward is structured. Obviously, you yourself know best what kind of pay-out structure would motivate you to go to the gym and reach your goal of losing weight. Therefore, we ask you to indicate how you would like your pay-out(s) to be structured." 
amount=100; weeks=10;
  #########################################################
  # App opening
  #########################################################
  subj=reactiveValues(nr=0)
  
  observeEvent(input$subjc,ignoreInit = T,{
    subj$nr=input$subjn
      insertTab(inputId='main',
              tabPanel(id="shaper","Incentives for the gym",useShinyjs(),
                       fluidRow(column(7,offset=0,(tags$h4('Instructions')))),
                       fluidRow(column(7,offset=0,(tags$h5(instr.hyp)))),
                       sidebarPanel(
                         tags$b(tags$h4("What incentive motivates you?")),
                         tags$h5("Pre-commitment"),
                         tags$h6("You can decide to pre-commit, by paying 100\u20ac and your employer will add 100\u20ac. If you attain your weekly goals, you will get this total amount of 200\u20ac, but you will lose (a part of) your committed 100\u20ac if you don't attain it."), 
                         selectInput('prec', 'Do you want to pre-commit?', c("","No","Yes, I will pay for entry")),
                         tags$h5("Pay-out frequency"),
                         tags$h6("For each week that you attain your goal you will be rewarded. For example, if you attain your goal 8 out of 10 weeks, you will receive 80% of the reward. You can choose to receive all of your pay-out at the end of the 10 week period, or to receive parts of this sum in weekly parts for each week you attain your weekly goal. Obviously, not attaining your goal will mean you do not receive any pay-out that week."),
                         selectInput('weekly', 'How often should your pay-outs be?', c("","One pay-out", "Weekly pay-outs")),
                         actionButton("precc","Confirm"),#change amount to reactive
                         hidden(tags$h5(id="head3","Pay-out structure")),
                         hidden(tags$h6(id="text3","If you decide on weekly pay-outs, pay-out amounts can be fixed for each week, starting low and increasing or the other way around. The slider below lets you select different structures (see right).")),
                         hidden(sliderInput('dincr','What should your pay-off structure be?',3,min=1,max=5)),
                         hidden(tags$h5(id="head4","Chance of winning")),
                         hidden(tags$h6(id="text4","Instead of receiving a sure amount, you may also receive your pay-out in the form of a lottery. Picking a lottery will increase your possible reward, but also increase the risk of not receiving any reward. The slider below lets you select different lottery structures (see right).")),
                         hidden(sliderInput('lottery', 'Probability of winning', 100, min = 1, max = 100))
                       ),
                       mainPanel(
                         hidden(tags$h4(id="h1",tags$b("Incentive programme"))),
                         hidden(tags$h5(id="h2","The following week by week incentive structure will be used")),
                         hidden(tags$h5(id="placeholder2")),
                           hidden(tableOutput('table1')),
                         hidden(tags$h5(id='s1',tags$b("Once you have have designed your preferred incentive structure, please press Submit"))),
                         hidden(actionButton("Go","Submit"))
                       )), target="Set-up",position='after', select = T)
    
    removeTab("main","Set-up")  
    
  })
  
  
  
  #########################################################
  # Shaper
  #########################################################
  
  sh<-reactiveValues(amount=amount,amount2=amount,weeks=weeks,com=0,weekly=0,struc=3,lottery=1, count=0);
  tab<-reactiveValues(tabinc=0,y=0)
  x=matrix(c(rep(0,weeks-1),amount),weeks,1)
  rownames=paste(rep("Week",weeks),1:weeks)
  y=cbind(rownames,x,c(rep(100,weeks)))
  colnames(y)=c(" ","Reward (Euro)","Chance of winning (%)")
  tab$y=y
  show('h1')
  show('h2')
  
  
  #Subject Panel
  observeEvent(input$precc,{
    if(input$prec==""|input$weekly==""){}
    else{
    show('table1')
    show("head4");show("text4");show("lottery");hide('precc')
    
    #Precommit
    if (input$prec=="Yes, I will pay for entry"){
      sh$count=sh$count+1
      str2=paste("You commit", sh$amount,"\u20ac of your own money")
      shapedat$prec=2
      if(sh$count<2){
        insertUI(
          selector = '#placeholder2',
          ## wrap element in a div with id for ease of removal
          ui = tags$h5(str2))}
      
      show('placeholder2')
      sh$amount2=sh$amount*2
      ty=tab$y
      
      ty[,2]=as.numeric(ty[,2])*2
      tab$tabinc=xtable(ty)
    }
    else{hide('placeholder2')
      tab$tabinc=xtable(tab$y)
      shapedat$prec=1} 
    
    
    #Weekly
    if(input$weekly=='Weekly pay-outs'){
      shapedat$week=2
      show('head3');show('text3');show('dincr')
      ty=tab$y
      if (input$prec=="Yes, I will pay for entry"){ty[,2]=sh$amount2/sh$weeks}
      if (input$prec=="No"){ty[,2]=sh$amount/sh$weeks}
      tab$y=ty
      tab$tabinc=xtable(ty)
    }
    else{hide('head3');hide('text3');hide('dincr')
      shapedat$week=1
      ty=tab$y
      if (input$prec=="Yes, I will pay for entry"){ty[,2]=matrix(c(rep(0,sh$weeks-1),sh$amount2),sh$weeks,1)}
      if (input$prec=="No"){ty[,2]=matrix(c(rep(0,sh$weeks-1),sh$amount),sh$weeks,1)}
      tab$y=ty
      tab$tabinc=xtable(ty)
    }
    
    updateSliderInput(session,inputId='dincr',value=3)
    updateSliderInput(session,inputId='lottery',value=100)
    show('Go');show('s1')
    }
    
  })
  
  observeEvent(input$dincr,ignoreInit = T,{
    
    w=sh$weeks
    t=1:w
    sw=1/w
    ty=tab$y
    sw2=2/w
    def=c()
    def[1]=0
    
    if(input$dincr==1){
      for(i in 1:w-1){def[i+1]=(2/(w-1)/w)*i}}
    if(input$dincr==2){for(i in 1:w-1){
      def[0]=0.5/w
      def[i+1]=(0.5/w)+(1/(w-1)/w)*i}}
    if(input$dincr==3){def=rep(sw,w)}
    if(input$dincr==4){for(i in 1:w-1){
      def[0]=0.5/w
      def[i+1]=(0.5/w)+(1/(w-1)/w)*i}
      def=rev(def)}
    if(input$dincr==5){for(i in 1:w-1){def[i+1]=(2/(w-1)/w)*i}
      def=rev(def)} 
    
    if (input$prec=="Yes, I will pay for entry"){
      ty[,2]=round(sh$amount2*def,0)}
    else{ty[,2]=round(sh$amount*def,0)}
    tab$y=ty
    tab$tabinc=xtable(ty)
    updateSliderInput(session,inputId='lottery',value=100)
    
    
  })
  
  
  observeEvent(input$weekly,ignoreInit = T,{
    show('precc'); hide('submit');hide('s1')
  })
  
  observeEvent(input$prec,ignoreInit = T,{
    show('precc');hide('submit');hide('s1')
  })
  
  
  observeEvent(input$lottery,ignoreInit = T,{
    fact=(input$lottery/100)
    ty=tab$y
    ty23=cbind(as.numeric(ty[,2]),as.numeric(ty[,3]))
    ty23[,1]=ty23[,1]/fact
    ty23[,2]=ty23[,2]*fact
    ty[,2:3]=round(ty23,0)
    tab$tabinc=xtable(ty)
    
    
  })
  
  output$table1=renderTable(tab$tabinc)
  
  shapedat=reactiveValues(prec=c(),week=c(),mode=c(),p=c(),final=c(),df=c())
 
  
  #############################################################################
  # Np method & Beta-delta
  #############################################################################
  
  
  #starting values
  G=50; #first starting gain
  sg=10; #small offset gain
  sl=-10; #small offset loss
  ssn=3 #number of elicitation in standard sequence
  nbis=5 #number of choices in bisection procedure
  
  finaldat=reactiveValues(d=c())
  #Conditional branching
  conditionnp=sample(c(1,2),1)
  if(conditionnp==1)
  {eval(parse("NPMethodOrd1.R"))}
  else if(conditionnp==2)
  {eval(parse("NPMethodOrd2.R"))}


  #############################################################################
  # Demographics
  #############################################################################
  
  observeEvent(input$final,{
    insertTab('main',tab=
         tabPanel(id="dem","Incentives for the gym",useShinyjs(),
              fluidRow(column(6,offset=0,tags$h4("Final questions"))),
              fluidRow(column(6,offset=0,tags$h5("Please answer these final demographic questions."))),
              fluidRow(column(6,offset=0,selectInput("age","What is your age (in years)?",c(15:85)))),
              fluidRow(column(6,offset=0,selectInput("gender","What is your gender?",c("Male","Female","Other")))),
              fluidRow(column(6,offset=0,selectInput("weight","What is your weight (in kilograms)? If you are unsure, please report your best estimate.",c(30:150)))),
              fluidRow(column(6,offset=0,selectInput("height","What is your height (in centimeters)? If you are unsure, please report your best estimate.",c(100:220)))),
              fluidRow(column(6,offset=0,sliderInput("cig","How many cigarettes do you smoke daily, on average (rounded upwards)?",value=0,min=0,max=50))),
              fluidRow(column(6,offset=0,sliderInput("alc","How many alcoholic beverages do you drink weekly, on average (rounded upwards)?",value=0,min=0,max=70))),
              fluidRow(column(6,offset=0,sliderInput("exercise","How many days of the week do you engage in physical exercise (i.e. running, playing sports, fitness)",value=0,min=0, max=7))),
              fluidRow(column(6,offset=0,actionButton("demfin","Submit")))),target='Final part',position='after')
    removeTab('main','Final part')
       })

  ####################################################################################
  # Data-logging
  ####################################################################################
   
    observeEvent(input$demfin,{
      dem<-reactiveValues(d=c())
      dem$d=cbind(input$age,input$gender,input$weight,input$height,input$cig,input$alc,input$exercise)
      demdf=dem$d
      colnames(demdf)=c("Age","Gender","Weight","Height","Cigarettes","Alcohol","Exercise")
    #  print(demdf);print(finaldat$d)
     dataset=c(finaldat$d,demdf)
     dataset2=t(dataset)
     colnames(dataset2)=c("Subjnr","Prec","Weekly","Mode","P","W1","W2","W3","W4","W5","W6","W7","W8","W9","W10","LAKW","AUC.G","AUC.L","Alpha.G","Alpha.L","Gamma.G","Gamma.L","DWG10","DWG30","DWG50","DWG70","DWG90","DWL10","DWL30","DWL50","DWL70","DWL90",
                          "SSL4","SSL3","SSL2","SSL1","RP","SSG1","SSG2","SSG3","SSG4","PBSG10","PBSG30","PBSG50","PBSG70","PBSG90","PBSL10","PBSL30","PBSL50","PBSL70","PBSL90","BDG05","BDG510","BDL05","BDL510","D5G","RG","BetaG","D5L","RL","BetaL","Age","Gender","Weight","Height","Cigarettes","Alcohol","Exercise")
   
    
      # Create a unique file name
     fileName <- sprintf("subject%s.csv", subj$nr)
     #Write the data to a local file
     filePath <- file.path(getwd(), fileName)
     write.csv(dataset2, filePath, row.names = F, quote = F)
     
     stopApp()
     file.edit("Done.R")
       })
  
  
}

shinyApp(ui = ui, server = server)