library(tidyverse)

# Implementierung eines vier gewinnt spieles
# "R" bzw 1 wird für rot stehen und "G" bzw -1 für gelb. 

start_matrix <- matrix(rep("E",7*6),nrow = 6)

ziehen <- function(mat,farbe,spalte){
    if(mat[1,spalte] != "E"){
        return(list(mat,"Unerlaubter Zug"))
    } else {
        n_empty <- sum(mat[,spalte] == "E")
        mat[n_empty,spalte] <- farbe
        return(list(mat,"Erlaubter Zug"))
    }
}

unterteile_in_minoren <- function(mat, dimension = 4){
    # nur fuer 4 3 und 2 dimensionen
    dim_mat <- dim(mat)
    list4 <- list()
    for(i in 1:(dim_mat[1]+1-dimension)){
        for (j in 1:(dim_mat[2]+1-dimension)) {
            list4 <- append(list4,list(mat[c(i:(i+(dimension-1))),c(j:(j+(dimension-1)))]))
        }
    }
    return(list4) 
}

analyse_minor <- function(minor,spieler){
    mat_rot <- minor == "R"
    mat_gelb <- minor == "G"
    mat_leer <- minor == "E"
    if(any(rowSums(mat_rot) == 4) | any(rowSums(t(mat_rot)) == 4) | sum(diag(mat_rot)) == 4 | sum(diag((mat_rot))) == 4){
        return(Inf)
    } else if(any(rowSums(mat_gelb) == 4) | any(rowSums(t(mat_gelb)) == 4) | sum(diag(mat_gelb)) == 4 | sum(diag((mat_gelb))) == 4){
        return(-Inf)
    } else {
        rot_gefaehrlich <- sum(rowSums(mat_rot) == 3 & rowSums(mat_leer) == 1) +
            sum(rowSums(t(mat_rot)) == 3 & rowSums(t(mat_leer)) == 1) +
            as.integer(sum(diag(mat_rot)) == 3 & sum(diag(mat_leer)) == 1) +
            as.integer(sum(diag(t(mat_rot))) == 3 & sum(diag(t(mat_leer))) == 1)
        gelb_gefaehrlich <- sum(rowSums(mat_gelb) == 3 & rowSums(mat_leer) == 1) +
            sum(rowSums(t(mat_gelb)) == 3 & rowSums(t(mat_leer)) == 1) +
            as.integer(sum(diag(mat_gelb)) == 3 & sum(diag(mat_leer)) == 1) +
            as.integer(sum(diag(t(mat_gelb))) == 3 & sum(diag(t(mat_leer))) == 1)
        # minor3_ergeb <- sapply(unterteile_in_minoren(minor,3), function(minor3){
        #     mat_rot3 <- minor3 == "R"
        #     mat_gelb3 <- minor3 == "G"
        #     rot3 <- sum(rowSums(mat_rot3) == 3) + sum(rowSums(t(mat_rot3)) == 3) +
        #         as.integer(sum(diag(mat_rot3)) == 3) + as.integer(sum(diag(t(mat_rot3))) == 3)
        #     gelb3 <- sum(rowSums(mat_gelb3) == 3) + sum(rowSums(t(mat_gelb3)) == 3) +
        #         as.integer(sum(diag(mat_gelb3)) == 3) + as.integer(sum(diag(t(mat_gelb3))) == 3)
        #     return(rot3 - gelb3)
        # },simplify = TRUE)
        # minor2_ergeb <- sapply(unterteile_in_minoren(minor,2), function(minor2){
        #     mat_rot2 <- minor2 == "R"
        #     mat_gelb2 <- minor2 == "G"
        #     rot2 <- sum(rowSums(mat_rot2) == 2) + sum(rowSums(t(mat_rot2)) == 2) +
        #         as.integer(sum(diag(mat_rot2)) == 2) + as.integer(sum(diag(t(mat_rot2))) == 2)
        #     gelb2 <- sum(rowSums(mat_gelb2) == 2) + sum(rowSums(t(mat_gelb2)) == 2) +
        #         as.integer(sum(diag(mat_gelb2)) == 2) + as.integer(sum(diag(t(mat_gelb2))) == 2)
        #     return(rot2 - gelb2)
        # },simplify = TRUE)
        # freiraum_aussen <- ifelse(sum(minor3_ergeb) != 0,sum(c(minor[1,],minor[2,1],minor[3,1],minor[2,4],minor[3,4]) == "E"),0)
        if(spieler){
            return(-100 * gelb_gefaehrlich + 10 * rot_gefaehrlich)
        } else {
            return(-100 * rot_gefaehrlich + 10 * gelb_gefaehrlich)
        }
    }
}

spielende <- function(mat,spieler){
    minoren_ergebnisse <- sapply(unterteile_in_minoren(mat,4) , function(minor){
        analyse_minor(minor,spieler)
    },simplify = TRUE)
    minoren_ergebnis <- sum(minoren_ergebnisse)
    if(minoren_ergebnis == Inf){
        return("R")
    } else if(minoren_ergebnis == -Inf){
        return("G")
    } else if(all(mat != "E")){
        return("Unentschieden") 
    } else {
        return(minoren_ergebnis)
    }
}

moegliche_zuege <- function(mat){
    return(c(1:7)[mat[1,] == "E"])
}

minimax <- function(mat, spieler, tiefe, alpha, beta){
    spielende_temp <- spielende(mat,spieler)
    if(spielende_temp %in% c("R","G","Unentschieden") | tiefe == 0){
        return(list(best_val = spielende_temp,
                    best_move = "",
                    alpha,
                    beta))
    }
    best_move <- ""
    if(spieler){
        best_val <- -Inf
        symbol <- "R"
    } else {
        best_val <- Inf
        symbol <- "G"
    }
    for(zug in moegliche_zuege(mat)){
        new_mat <- ziehen(mat,symbol,zug)[[1]]
        hypo_val <- minimax(new_mat,!spieler,tiefe-1,alpha,beta)$best_val
        if(is.character(hypo_val) & hypo_val == "Unentschieden"){
            hypo_val <- 0
        }
        if(is.character(hypo_val) & hypo_val == "R"){
            hypo_val <- Inf
        }
        if(is.character(hypo_val) & hypo_val == "G"){
            hypo_val <- -Inf
        }
        
        if(spieler & hypo_val > best_val){
            best_val <- hypo_val
            best_move <- zug
            alpha <- max(alpha, best_val)
        } 
        if(!spieler & hypo_val < best_val){
            best_val <- hypo_val
            best_move <- zug
            beta <- min(beta, best_val)
        }
        if(alpha > beta){
            break
        }
    }
    return(list(best_val = best_val,
                best_move = best_move,
                alpha,
                beta))
}

plot_grid <- function(mat){
    colnames(mat) <- as.character(1:7)
    mat %>%
        as_tibble() %>%
        mutate(n_row = 6:1) %>%
        pivot_longer(as.character(1:7)) %>%
        mutate(n_col = as.integer(name)) %>%
        ggplot(aes(x = n_col,y = n_row,fill = value)) +
            geom_point(shape = 21,size = 25) +
            scale_fill_manual(values = c(E = "white",R = "red",G = "yellow"),
                              guide = NULL)+
            labs(x = "",y = "")+
            theme_bw()+
            xlim(0.5,7.5) + ylim(0.5,6.5) +
            theme(axis.ticks = element_blank(),
                  axis.text = element_blank(),
                  panel.grid.minor = element_line(colour = "black",size = 2),
                  panel.grid.major = element_line(colour = "black",size = 2),
                  panel.background = element_rect(fill = "slategray3"))
}






library(shiny)
library(shinydashboard)
library(shinydashboardPlus)

# Define UI for application that draws a histogram
ui <- dashboardPage(
    dashboardHeader(title = "Vier gewinnt",
                    dropdownMenu(
                        type = "notifications",
                        icon = icon("link"),
                        headerText = "Links",
                        notificationItem("Github repo", icon = icon("github"),
                                         href = "https://github.com/EmanuelSommer/")
                    )
    ),
    dashboardSidebar(
        sliderInput("tiefe",label = "Einstellung der Schwierigkeit:",min = 1, max = 4,value = 2),
        tags$br()
    ),
    dashboardBody(
        fluidRow(
            column(width = 6,
                   infoBoxOutput("status",width = NULL)
                   ),
            column(width = 6,
                   actionButton("reset",label = "Neues Spiel",icon = icon("redo"))
            ),
            column(width = 12,
                   box(width = NULL,
                       plotOutput("spielfeld",click = "plot_click")))
        )
    ),
    skin = "black"

)

# Define server logic required to draw a histogram
server <- function(input, output) {
    values <- reactiveValues(mat = start_matrix,text = "Weiter",over = FALSE)
    observeEvent(input$reset,{
        values$mat <- start_matrix
        values$text <- "Weiter"
        values$over <- FALSE
    })
    # clicken aufs spielfeld:
    observeEvent(input$plot_click,{
        x_click <- round(input$plot_click$x)
        if(!values$over & x_click %in% moegliche_zuege(values$mat)){
            values$mat <- ziehen(values$mat,"R",x_click)[[1]]
            if(is.character(spielende(values$mat,TRUE))){
                values$over <- TRUE
                values$text <- spielende(values$mat,TRUE)
            } else {
                minimax_res <- minimax(values$mat,FALSE,input$tiefe,-2000,2000)
                if(!is.numeric(minimax_res$best_move)){
                    moeglich_temp <- moegliche_zuege(values$mat)
                    if(length(moeglich_temp) == 0){
                        values$over <- TRUE
                        values$text <- spielende(values$mat,TRUE)
                    } else {
                        next_move <- sample(moeglich_temp,1)
                    }
                } else {
                    next_move <- minimax_res$best_move
                }
                values$mat <- ziehen(values$mat,"G",next_move)[[1]]
                if(is.character(spielende(values$mat,TRUE))){
                    values$over <- TRUE
                    values$text <- spielende(values$mat,TRUE)
                }
            }
        }
    })
    output$status <- renderInfoBox({
        if(values$text == "Weiter"){
            infotext <- "Spiel läuft"
            infoicon <- icon("gamepad")
            infocolor <- "blue"
        } else if(values$text == "R"){
            infotext <- "Du gewinnst"
            infoicon <- icon("trophy")
            infocolor <- "lime"
        } else if(values$text == "G"){
            infotext <- "Du verlierst"
            infoicon <- icon("skull")
            infocolor <- "fuchsia"
        } else if(values$text == "Unentschieden"){
            infotext <- "Unentschieden"
            infoicon <- icon("meh")
            infocolor <- "orange"
        }
        infoBox(title = "Spielstatus",
                width = 6,
                color = infocolor,
                value = infotext,
                icon = infoicon)
    })
    output$spielfeld <- renderPlot({
        plot_grid(values$mat)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
