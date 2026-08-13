library(plumber)
port <- as.numeric(Sys.getenv("PORT", unset = 8080))
pr("api.R") %>% pr_run(host = "0.0.0.0", port = port)
