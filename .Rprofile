source("renv/activate.R")
# options
options(
    # activate RStudio Addins on command pallet
    vsc.rstudioapi = TRUE,
    # interactive plots with {httpgd}
    vsc.use_httpgd = TRUE,
    # radian highlight scheme (choose what suits you)
    radian.color_scheme = "native",
    # code completion triggers
    languageserver.server_capabilities = list(
        signatureHelpProvider = list(triggerCharacters = list("(", ",", "$")),
        completionProvider = list(
            resolveProvider = TRUE, triggerCharacters = list(".", ":", "$")
        )
    )
)
