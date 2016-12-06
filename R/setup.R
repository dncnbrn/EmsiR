EpistemeLogin <- function(username, password) {
    Sys.setenv(EMSIUN = username, EMSIPW = password)
}

EpistemeSettings <- function() {
    c(`episteme-username` = Sys.getenv("EMSIUN"), `episteme-secret1` = Sys.getenv("EMSIPW"), `episteme-pub` = "512", 
        `content-type` = "application/json")
}


