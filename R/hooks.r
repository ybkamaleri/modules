mod_hook_registry_prototype = list(
    on_load = list(),
    on_use = list(),
    on_attach = list()
)

#' Register and call hooks for module events
#'
#' \emph{Any code} can register hooks for \emph{any module}, not just itself.
#' There is therefore an arbitrary number of hooks per event per module.
#' @keywords internal
mod_hooks = list2env(mod_hook_registry_prototype, parent = emptyenv())

#' @export
register_hook = function (hook_name, mod_name, fun) {
    stopifnot(
        length(hook_name) == 1L,
        hook_name %in% ls(mod_hooks),
        is.character(mod_name),
        length(mod_name) == 1L,
        is.function(fun)
    )

    mod_hooks[[hook_name]][[mod_name]][['']] = fun
    # TODO: Register hook in calling module’s registry, to delete it if it gets
    # unloaded.
    caller = mod_topenv(parent.frame())

    if (is_namespace(caller)) {
        old_hooks = get_namespace_info(caller, 'hooks') %||% mod_hook_registry_prototype
        old_hooks[[hook_name]][[mod_name]][['']] = fun
        set_namespace_info(caller, 'hooks', old_hooks)
    }
}

deregister_hook = function (calling_info, hook_name, fun) {
    for (mod_name in names(mod_hooks[[hook_name]])) {
        hooks = mod_hooks[[hook_name]][[mod_name]]
        if (fun %in% hooks) {
            mod_hooks[[hook_name]][[mod_name]] = setdiff(mod_hooks[[hook_name]], fun)
        }
    }
}

execute_hooks = function (info, mod_ns, hook_name) {
    hooks = mod_hooks[[hook_name]][[info$source_path]]
    for (hook in hooks) hook(spec_name(info$spec))
}
