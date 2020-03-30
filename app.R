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

analyse_minor <- function(minor){
    mat_rot <- minor == "R"
    mat_gelb <- minor == "G"
    if(any(rowSums(mat_rot) == 4) | any(rowSums(t(mat_rot)) == 4) | sum(diag(mat_rot)) == 4 | sum(diag((mat_rot))) == 4){
        return(Inf)
    } else if(any(rowSums(mat_gelb) == 4) | any(rowSums(t(mat_gelb)) == 4) | sum(diag(mat_gelb)) == 4 | sum(diag((mat_gelb))) == 4){
        return(-Inf)
    } else {
        minor3_ergeb <- sapply(unterteile_in_minoren(minor,3), function(minor3){
            mat_rot3 <- minor3 == "R"
            mat_gelb3 <- minor3 == "G"
            rot3 <- sum(rowSums(mat_rot3) == 3) + sum(rowSums(t(mat_rot3)) == 3) +
                as.integer(sum(diag(mat_rot3)) == 3) + as.integer(sum(diag(t(mat_rot3))) == 3)
            gelb3 <- sum(rowSums(mat_gelb3) == 3) + sum(rowSums(t(mat_gelb3)) == 3) +
                as.integer(sum(diag(mat_gelb3)) == 3) + as.integer(sum(diag(t(mat_gelb3))) == 3)
            return(rot3 - gelb3)
        },simplify = TRUE)
        minor2_ergeb <- sapply(unterteile_in_minoren(minor,2), function(minor2){
            mat_rot2 <- minor2 == "R"
            mat_gelb2 <- minor2 == "G"
            rot2 <- sum(rowSums(mat_rot2) == 2) + sum(rowSums(t(mat_rot2)) == 2) +
                as.integer(sum(diag(mat_rot2)) == 2) + as.integer(sum(diag(t(mat_rot2))) == 2)
            gelb2 <- sum(rowSums(mat_gelb2) == 2) + sum(rowSums(t(mat_gelb2)) == 2) +
                as.integer(sum(diag(mat_gelb2)) == 2) + as.integer(sum(diag(t(mat_gelb2))) == 2)
            return(rot2 - gelb2)
        },simplify = TRUE)
        freiraum_aussen <- ifelse(sum(minor3_ergeb) != 0,sum(c(minor[1,],minor[2,1],minor[3,1],minor[2,4],minor[3,4]) == "E"),0)
        return(100 * sum(minor3_ergeb) + 2 * freiraum_aussen * sign(sum(minor3_ergeb)) + sum(minor2_ergeb)) 
    }
}

spielende <- function(mat){
    minoren_ergebnisse <- sapply(unterteile_in_minoren(mat,4) , function(minor){
        analyse_minor(minor)
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

minimax <- function(mat, maximierer, tiefe, alpha, beta){
    spielende_temp <- spielende(mat)
    if(spielende_temp %in% c("R","G","Unentschieden") | tiefe == 0){
        return(list(best_val = spielende_temp,
                    best_move = "",
                    alpha,
                    beta))
    }
    best_move <- ""
    if(maximierer){
        best_val <- -Inf
        symbol <- "R"
    } else {
        best_val <- Inf
        symbol <- "G"
    }
    for(zug in moegliche_zuege(mat)){
        new_mat <- ziehen(mat,symbol,zug)[[1]]
        hypo_val <- minimax(new_mat,!maximierer,tiefe-1,alpha,beta)$best_val
        if(maximierer & hypo_val > best_val){
            best_val <- hypo_val
            best_move <- zug
            alpha <- max(alpha, best_val)
        } 
        if(!maximierer & hypo_val < best_val){
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

# alpha ca -2000 und beta 2000? maybe trial and error
# "R">2 problem






library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
    "Hallöle"

)

# Define server logic required to draw a histogram
server <- function(input, output) {

}

# Run the application 
shinyApp(ui = ui, server = server)
