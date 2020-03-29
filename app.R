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
        return(0) # use unterteile in minoren
        #100 3 in a row 1 mal 2 in a row + space around streaks
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
        return("Unetschieden") 
    } else {
        return(minoren_ergebnis)
    }
}

moegliche_zuege <- function(mat){
    return(c(1:7)[mat[1,] == "E"])
}












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
