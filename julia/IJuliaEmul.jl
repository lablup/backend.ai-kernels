"""
An emulated IJulia module for Sorna.
"""
module IJulia

const preexecute_hooks = Function[]
const postexecute_hooks = Function[]
const posterror_hooks = Function[]

push_preexecute_hook(f::Function) = push!(preexecute_hooks, f)
pop_preexecute_hook(f::Function) = splice!(preexecute_hooks, findfirst(preexecute_hooks, f))

push_postexecute_hook(f::Function) = push!(postexecute_hooks, f)
pop_postexecute_hook(f::Function) = splice!(postexecute_hooks, findfirst(postexecute_hooks, f))

push_posterror_hook(f::Function) = push!(posterror_hooks, f)
pop_posterror_hook(f::Function) = splice!(posterror_hooks, findfirst(posterror_hooks, f))

const inited = true

end # module IJulia
