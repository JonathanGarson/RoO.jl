using Documenter
using RoO

makedocs(
    sitename = "RoO",
    format = Documenter.HTML(),
    modules = [RoO]
)

# Documenter can also automatically deploy documentation to gh-pages.
# See "Hosting Documentation" and deploydocs() in the Documenter manual
# for more information.
deploydocs(
    repo = "github.com/JonathanGarson/RoO.jl"
)
