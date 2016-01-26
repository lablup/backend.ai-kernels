#! /usr/bin/env Rscript
library(rzmq)
library(rjson)

# -- snippet from: http://stackoverflow.com/questions/32257970/ --
require_namespace = function (package) {
    ns = .Internal(getRegisteredNamespace(package))
    if (is.null(ns))
        ns = tryCatch(loadNamespace(package), error = identity)
    ns
}

exhibit_package_namespace = function (namespace, name, parent, export_list) {
    structure(list2env(sapply(export_list, getExportedValue, ns = namespace,
                              simplify = FALSE),
                       parent = parent.env(parent)),
              name = paste('package', name, sep = ':'),
              path = getNamespaceInfo(namespace, 'path'))
}

library_local = function (package, parent = parent.frame()) {
    pkg_ns = require_namespace(package)
    if (inherits(pkg_ns, 'error'))
        stop('Unable to load package ', sQuote(package), '\n',
             'Failed with error: ', sQuote(conditionMessage(pkg_ns)))
    export_list = getNamespaceExports(pkg_ns)
    pkg_env = exhibit_package_namespace(pkg_ns, package, parent, export_list)
    parent.env(parent) = pkg_env
}
# -- end snippet --

catchAll <- function(expr) {
    myWarnings <- NULL
    myError <- NULL
    warningHandler <- function(w) {
        myWarnings <<- c(myWarnings, w$message)
        invokeRestart("muffleWarning")
    }
    errorHandler <- function(e) {
        myError <<- list (
            "simpleError",
            e$message,
            F,
            "" # traceback() func has side-effect on stdout. just ignore it. :(
        )
        NULL
    }
    tryCatch(withCallingHandlers(expr, warning = warningHandler), error = errorHandler)
    list(warnings = myWarnings, error = myError)
}

ctx = init.context()
sock = init.socket(ctx, "ZMQ_REP")
bind.socket(sock, "tcp://*:2001")
user_env <- new.env(parent = baseenv())
# Replace library() function with that works on a new base environment.
assign("library", library_local, user_env)
while (1) {
    msg = list(receive.string(sock))
    while (get.rcvmore(sock)) {
        msg = append(msg, list(receive.string(sock)))
    }
    # We ignore cell ID (msg[[1]])
    stdout_buf <- textConnection('o', open = "w", local = F, encoding = "UTF-8")
    sink(stdout_buf, type = "output")
    ret <- catchAll(eval(parse(text = msg[[2]]), envir = user_env))
    flush(stdout_buf)
    sink(type = "output")
    if (is.null(ret$error))
        # Here, R's ifelse() func has side-effect: list() becomes list(NULL) -_-
        exceptions = list()
    else
        exceptions = list(ret$error)
    rep <- list( stdout = paste(textConnectionValue(stdout_buf), collapse='\n'),
                 stderr = paste(ret$warnings, collapse='\n'),
                 exceptions = exceptions )
    close(stdout_buf)
    json <- toJSON(rep)
    send.raw.string(sock, json)
}
