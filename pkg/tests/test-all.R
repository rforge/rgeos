library(testthat)
library(rgeos)

#current version of testthat doesn't play nice with R.app GUI
#monkeypatch colourise to fix it

colouriseFix <- function(text, fg = "black", bg = NULL) {
    term <- Sys.getenv()["TERM"]
    colour_terms <- c("xterm-color", "screen")

    if (!any(term %in% colour_terms, na.rm = TRUE)) {
        return(text)
    }

    if ("R_GUI_APP_VERSION" %in% names(Sys.getenv())) {
        return(text)
    }

    col_escape <- function(col) {
        paste("\033[", col, "m", sep = "")
    }

    col <- .fg_colours[tolower(fg)]
    if (!is.null(bg)) {
        col <- paste(col, .bg_colours[tolower(bg)], sep = ";")
    }

    init <- col_escape(col)
    reset <- col_escape("0")
    paste(init, text, reset, sep = "")
}
assign("colourise", colouriseFix, envir = as.environment("package:testthat"))

SummaryByContextReporter <- Reporter$clone()
SummaryByContextReporter$do({
    labels <- c(1:9, letters, LETTERS)
  
    self$start_context <- function(desc) {
        charrep <- function(char, times) {
            sapply(times, function(i) paste(rep.int(char, i), collapse = ""))
        }
        
        line = charrep("-",getOption("width"))
        cat(line, "\n")
        cat(desc, ": \n", sep = "")
        cat(line, "\n")
        
        self$failures <- list()
        self$n <- 0
    }
  
    self$end_context <- function() {
        cat("\n")
    
        charrep <- function(char, times) {
            sapply(times, function(i) paste(rep.int(char, i), collapse = ""))
        }
    
        if (self$n != 0) {
        
            label <- labels[seq_len(self$n)]
            type <- ifelse(sapply(self$failures, "[[", "error"), "Error", "Failure")
            tests <- sapply(self$failures, "[[", "test")
            header <- paste(label, ". ", type, ": ", tests, " ", sep = "")
            

            message <- sapply(self$failures, "[[", "message")

            cat("\n")
            cat(paste(  colourise(header, "red"), "\n", 
                        message, "\n", sep = "", collapse = "\n") )      
            stop("", call. = FALSE)
        }
        cat("\n")
    }
  
    self$start_reporter <- function() {
        self$failures <- list()
        self$n <- 0
    }
  
    self$add_result <- function(result) {
        if (result$passed) {
            cat(colourise(".", fg = "light green"))
        } else {
            self$n <- self$n + 1
      
            if (self$n > length(labels)) {
                self$n <- length(labels)
                cat(colourise("F", fg = "red"))
            } else {
                result$test <- self$test
                self$failures[[self$n]] <- result
                cat(colourise(labels[self$n], fg = "red"))
            } 
        }
    }
  
    self$end_reporter <- function() { self$end_context() }  
})


#test_dir(system.file("tests", package = "rgeos"), SummaryByContextReporter)
test_dir("/Users/rundel/Desktop/Summer of Code/rgeos/pkg/inst/tests", SummaryByContextReporter)
