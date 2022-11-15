library(shiny)
library(gridExtra)
library(patchwork)
library(tidyverse)
library(rsconnect)
library(Information)

stepsx <- c("Step 1: Look for possible interaction candidates", "Step 2: Examine interactions", "Step 3: Run Regression")

files <- c('https://raw.githubusercontent.com/ericonsi/CUNY_621/main/Blogs/teengamb.csv', 'https://raw.githubusercontent.com/ericonsi/CUNY_621/main/Assignment%203/crime-training-data_modified.csv', 'https://raw.githubusercontent.com/ericonsi/CUNY_621/main/Baseball/moneyball-training-data.csv', 'https://raw.githubusercontent.com/ericonsi/CUNY_621/main/Assignment%204/insurance_training_data.csv', 'https://raw.githubusercontent.com/ericonsi/CUNY_621/main/Assignment%205/dataset.csv')

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    
    headerPanel('Exploring Interactions'),
    sidebarPanel(
        textInput('file', label="Dataset Location:"),
        uiOutput("targetSelection"),
        uiOutput("interSelection"),
        actionButton("refreshButton", "Refresh File"),
        actionButton("refreshAnalysis", "Refresh Analysis"),
    ),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput('step', 'Step:', stepsx),
            textInput("cutoff", label = "Cutoff:", value = ""),
            uiOutput("varsSelection")
        ),
        
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot"),
            verbatimTextOutput("textPlot")
        )
    )
)

#Display functions
EHExplore_Interactions_Scatterplots <- function(df, y, interaction) {
    
    library(ggsci)
    
    df <- select_if(df, is.numeric)
    
    v <- as.vector(df[,interaction])
    
    xtext1 = as.data.frame(aggregate(data.frame(count = v), list(value = v), length))
    df[interaction][df[interaction] == "0"] <- paste0("0 (n=", xtext1$count[1], ")")
    df[interaction][df[interaction] == "1"] <- paste0("1 (n=", xtext1$count[2], ")")
    
    
    df[,interaction] <- as.factor(df[,interaction])
    
    plot_list <- list()
    
    for(i in 1:ncol(df)) {     
        
        p <- eval(substitute(ggplot(df, aes_string(df[ , i], y, color=interaction)) +
                                 geom_point(alpha=.1) +
                                 geom_smooth(method = "lm") +
                                 xlab(colnames(df)[i]) +
                                 theme(title = element_text(size=9), axis.title.x = element_text(size = 9), axis.title.y = element_text(size = 9), axis.text.x = element_text(size = 8), panel.grid.major.x = element_line(color="gray"), panel.grid.minor.x=element_blank(), panel.grid.minor.y=element_blank(), panel.grid.major.y=element_line(color="gray"), panel.background = element_rect(fill = "slategray1", color="darkslategray")) +
                                 scale_color_d3()+
                                 scale_fill_d3()+
                                 ggtitle(colnames(df)[i]), list(i=i)))
        plot_list[[i]] <- p 
        
    }
    return(plot_list)
}


EHSummarize_SingleColumn_Boxplots <- function(df, font_size=7)
{  
    df <- select_if(df, is.numeric)
    
    
    plot_list2 <- list()
    
    for(i in 1:ncol(df)) {     
        
        qp <- toString(head(sort(round(df[,i],2)),5))
        qz <- toString(tail(sort(round(df[,i],2)),5))
        qk <- str_c("L:   ", qp, "\\\n", "H:   ", qz)
        
        qk <- gsub('\\\\','', qk)
        
        p <- eval(substitute(ggplot(df, aes(df[,i])) +
                                 coord_flip() +  
                                 xlab(colnames(df)[i])  +
                                 ylab(qk) +
                                 theme(axis.title.x = element_text(size = font_size), axis.title.y = element_text(size = 9), axis.text.x = element_blank(), axis.ticks.x = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor.x=element_blank(), panel.grid.minor.y=element_blank(), panel.grid.major.y=element_line(color="gray"), panel.background = element_rect(fill = "slategray2", color="darkslategray")) +
                                 geom_boxplot(), list(i=i)))
        
        plot_list2[[i]] <- p 
        
        
    }
    return (plot_list2)
}

EHSummarize_SingleColumn_Histograms <- function(df, font_size = 7, hist_nbins = 20)
{
    
    df <- select_if(df, is.numeric)
    
    plot_list2 <- list()
    
    for(i in 1:ncol(df)) {     
        
        qp <- toString(head(sort(round(df[,i],2)),5))
        qz <- toString(tail(sort(round(df[,i],2)),5))
        qk <- str_c("L:   ", qp, "\\\n", "H:   ", qz)
        
        qk <- gsub('\\\\','', qk)
        
        p <- eval(substitute(ggplot(df, aes(df[,i])) +
                                 ylab(colnames(df)[i])  +
                                 xlab(qk) +
                                 theme(axis.title.x = element_text(size = font_size), axis.title.y = element_text(size = 9), axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.text.x = element_text(size=8),  panel.grid.major.x = element_blank(), panel.grid.minor.x=element_blank(), panel.grid.minor.y=element_blank(), panel.grid.major.y=element_blank(), panel.background = element_rect(fill = "slategray2", color="darkslategray"))  + 
                                 geom_histogram(bins=hist_nbins, fill="white", aes(y = stat(density))) +
                                 geom_density(col = "red"), list(i=i)))
        plot_list2[[i]] <- p 
        
    }
    return (plot_list2)
}

EHSummarize_StandardPlots <-function(data, y, return_list = FALSE, h_nbins = 20, print=TRUE, type="scatter")
{  
    
    list1 <- EHSummarize_SingleColumn_Boxplots(data)
    list2 <- EHSummarize_SingleColumn_Histograms(data, hist_nbins =  h_nbins)
    
    if(type=="scatter"){
        list3 <- EHExplore_TwoContinuousColumns_Scatterplots(data, y)
    } else if (type=="box"){
        list3 <- EHExplore_OneContinuousAndOneCategoricalColumn_Boxplots(data, y)
    }
    
    zz2 <- list()
    
    
    for(i in 1:length(list1)) {
        zz2[i*3-2] <- list1[i]
        zz2[i*3-1] <- list2[i]
        zz2[i*3] <- list3[i]
    }
    
    if (print) {
        lenZ <- length(zz2)
        quotient <- lenZ %/% 9
        gap <- lenZ - quotient*9
        gaprows <- gap/3
        
        if (lenZ>=9) {
            for(i in 1:quotient) { 
                
                start <- (i-1)*9 + 1
                finish <- start + 8
                
                grid.arrange(grobs=zz2[c(start:finish)], ncol=3)
                
            }
        }
        
        if (gaprows>0) {
            
            start <- quotient*9 + 1
            finish <- start + gaprows*3 - 1
            
            grid.arrange(grobs=zz2[c(start:finish)], ncol=3, nrow=gaprows)
        }  
    }
    
    if (return_list) {
        return (zz2)
    }
    
}

EHExplore_TwoContinuousColumns_Scatterplots <- function(df, y, flip=FALSE)
{
    plot_list <- list()
    
    df <- select_if(df, is.numeric)
    
    for(i in 1:ncol(df)) {
        
        ct <- cor.test(df[,i], df[,y])
        
        xText <- str_c("Correlation: ", round(ct$estimate,2), "   p value: ", round(ct$p.value,2))
        
        x1 = df[[i]]
        y1 =y
        
        if(flip)
        {
            x1=y
            y1=df[[i]]
        }
        
        p <- ggplot(df, aes_string(x1, y1)) +
            geom_point(fill="navy", color="white") +
            geom_smooth(method = "loess", color="red", fill="lightcoral") +
            ylab(y) +
            xlab(xText) +
            theme(title = element_text(size=9), axis.title.x = element_text(size = 8), axis.title.y = element_text(size = 9), axis.text.x = element_text(size = 8), axis.ticks.x = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor.x=element_blank(), panel.grid.minor.y=element_blank(), panel.grid.major.y=element_line(color="gray"), panel.background = element_rect(fill = "slategray2", color="darkslategray")) +
            ggtitle(colnames(df)[i])
        
        p <- eval(substitute(p, list(i=i)))
        plot_list[[i]] <- p 
        
    }
    return(plot_list)
}


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    observeEvent(input$refreshButton, {
        
        df <- read.csv(input$file)
        yy <- colnames(df)
        ylen <- as.integer(length(yy))-1
        
        output$targetSelection <- renderUI({
            selectInput("target", "Target Column:", choices = yy)
        })
        
        output$interSelection <- renderUI({
            selectInput("inter", "Interaction Term:", choices = yy)
        })
        
        output$varsSelection <- renderUI({
            sliderInput("vars",
                        "Number of variables:",
                        min = 1,
                        max = ylen,
                        value = c(1,7))
        })
        
    })
    
    
    output$distPlot <- renderPlot({
        
        refresh <- TRUE
        
        observeEvent(input$refreshAnalysis, {
            
            refresh <- TRUE
            
        })
        
        df <- read.csv(input$file)
        
        
        v1 <- df[,input$target]
        v2 <- as.numeric(df[,input$inter])
        
        
        library(patchwork)
        
        nmin <- as.numeric(input$vars[1])
        nmax <- as.numeric(input$vars[2])
        
        
        
        df <- dplyr::select(df,nmin:nmax)
        
        
        if(input$cutoff != "") {
            df$NewTerm = as.integer(ifelse(v2>as.numeric(input$cutoff),1,0))
        }
        
        df$target_column <- v1
        
        if(refresh){
            
            if(input$step == "Step 1: Look for possible interaction candidates") {
                wrap_plots(EHSummarize_StandardPlots(df, "target_column", return_list=TRUE), ncol=6)
            }
            else{
                wrap_plots(EHExplore_Interactions_Scatterplots(df, "target_column", "NewTerm"))
            }
        }
        else{
            ggplot(df,aes(target_column,target_column))+geom_blank()
        }
    })
    
    output$textPlot <- renderPrint({
        
        if(input$step == "Step 3: Run Regression") {
            
            df <- read.csv(input$file)
            target <- df[,input$target]
            interaction_term <- as.numeric(df[,input$inter])
            test_variable <- df[,input$vars[1]]
            
            if(is.binary(target)) {
            m <- glm(target ~ interaction_term*test_variable, data = df, family = "binomial")
            } else {
            m <- lm(target ~ interaction_term*test_variable, df)
        }
            
            print (paste("Target: ", input$target))
            print (paste("Interaction Term: ", input$inter))
            print (paste("Test Term: ", colnames(df[input$vars[1]])))
            x <- summary(m)
        }
        else {
            x <- ""
        }
        return(x)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
