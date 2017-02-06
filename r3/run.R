#! /usr/bin/env Rscript
library(rzmq)

# -- snippet from: http://stackoverflow.com/questions/32257970/ --
require_namespace <- function(package) {
    ns <- .Internal(getRegisteredNamespace(package))
    if (is.null(ns))
        ns <- tryCatch(loadNamespace(package), error = identity)
    ns
}

exhibit_package_namespace <- function(namespace, name, parent, export_list) {
    structure(list2env(sapply(export_list, getExportedValue, ns = namespace,
                              simplify = FALSE),
                       parent = parent.env(parent)),
              name = paste('package', name, sep = ':'),
              path = getNamespaceInfo(namespace, 'path'))
}

library_local <- function(package, parent = parent.frame()) {
    pkg_ns <- require_namespace(package)
    if (inherits(pkg_ns, 'error'))
        stop('Unable to load package ', sQuote(package), '\n',
             'Failed with error: ', sQuote(conditionMessage(pkg_ns)))
    export_list <- getNamespaceExports(pkg_ns)
    pkg_env <- exhibit_package_namespace(pkg_ns, package, parent, export_list)
    parent.env(parent) <- pkg_env
}
# -- end snippet --

catchAll <- function(expr) {
    myWarnings <- NULL
    myErrors <- NULL
    warningHandler <- function(w) {
        myWarnings <<- c(myWarnings, w$message)
        invokeRestart("muffleWarning")
    }
    errorHandler <- function(e) {
        myErrors <<- c(myErrors, e)
    }
    tryCatch(withCallingHandlers(expr, warning = warningHandler), error = errorHandler)
    list(warnings = myWarnings, errors = myErrors)
}

ctx <- init.context()
input_sock <- init.socket(ctx, "ZMQ_PULL")
output_sock <- init.socket(ctx, "ZMQ_PUSH")
bind.socket(input_sock, "tcp://*:2000")
bind.socket(output_sock, "tcp://*:2001")
user_env <- new.env(parent = baseenv())
# Replace library() function with that works on a new base environment.
assign("library", library_local, user_env)
while (1) {
    msg <- receive.multipart(input_sock)
    # We ignore cell ID (msg[[1]])
    stdout_buf <- textConnection('o', open = "w", local = F, encoding = "UTF-8")
    stderr_buf <- textConnection('o', open = "w", local = F, encoding = "UTF-8")
    sink(stdout_buf, type = "output")
    sink(stderr_buf, type = "message")
    ret <- catchAll(eval(parse(text = rawToChar(msg[[2]])), envir = user_env))
    cat('\n')
    sink(type = "output")
    sink(type = "message")
    send.multipart(output_sock, list(charToRaw("stdout"), charToRaw(paste(textConnectionValue(stdout_buf), collapse='\n'))))
    send.multipart(output_sock, list(charToRaw("stderr"), charToRaw(paste(textConnectionValue(stderr_buf), collapse='\n'))))
    if (!is.null(ret$warnings)) {
        send.multipart(output_sock, list(charToRaw('stderr'), charToRaw(paste(ret$warnings, collapse='\n'))))
    }
    if (!is.null(ret$errors)) {
        send.multipart(output_sock, list(charToRaw('stderr'), charToRaw(paste(ret$errors, collapse='\n'))))
    }
    close(stdout_buf)
    close(stderr_buf)
    send.multipart(output_sock, list(charToRaw("finished"), charToRaw("")))
}

# vim: sts=4 sw=4 et
