    start.time <- Sys.time()
    dyn.load("CompleteCaseImputation.so")
    end.time <- Sys.time()
    time.taken <- end.time - start.time