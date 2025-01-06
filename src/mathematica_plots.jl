# ================================
# Script Julia : Traduction des fonctions Mathematica
# ================================
include("files_path.jl")
using Roots
using Plots

# ================================
# Paramètres globaux
# ================================
const ζ = 0.25    # Paramètre zeta (modifiable)
const κ = 1.0    # Paramètre kappa (modifiable)
const τ = 1.1

# ================================
# Fonctions de base
# ================================
function c(λ, δ)
    λ^(ζ + 1) + δ * (1 - λ)^(ζ + 1)
end

function λc(λ, δ)
    1 / (1 + δ * ((1 - λ) / λ)^(ζ + 1))
end

function λcinv(λc_val, δ)
    1 / (1 + ((1 / λc_val - 1) / δ)^(1 / (ζ + 1)))
end

function λmin(δ)
    1 / (1 + δ^(-1 / ζ))
end

function cmin(δ)
    (1 + δ^(-1 / ζ))^(-ζ)
end

function c1(λ, δ)
    c(λ, δ) / cmin(δ)
end

function c1full(λ, δ)
    λ > λmin(δ) ? c1(λ, δ) : 1
end

# ================================
# Fonctions de politique
# ================================

function λpol(λc0, δ, τ)
    if c1full(λcinv(λc0, δ), δ) > τ
        λmin(δ)
    else
        max(λcinv(λc0, δ), λmin(δ))
    end
end

function λpol2(λ0, δ, τ)
    c1full(λ0, δ) > τ ? λmin(δ) : max(λ0, λmin(δ))
end

# ================================
# Seuils pour δ
# ================================
function δ0(τ)
    ((τ^(1 / ζ) - 1)^(-ζ))
end

function δmax(τ)
    ((τ^(1 / ζ) - 1)^(-ζ))
end

# ================================
# Fonctions étendues
# ================================
function cminD(δ)
    cmin(δ * κ)
end

function cminF(δ)
    κ * cmin(δ / κ)
end

function χminD(δ)
    λmin(δ * κ)
end

function χminF(δ)
    λmin(δ / κ)
end

function c1U(δ, τF)
    τF * cminF(δ) / cminD(δ)
end

function c1D(χ, δ)
    c1full(χ, δ * κ)
end

# ================================
# Résolution des équations
# ================================
function δFDNC(τD, τF)
    find_zero(δ -> c1U(δ, τF) - τD, 0.001)
end

function δFDC(χ, τD, τF)
    find_zero(δ -> c1U(δ, τF) - c1D(χ, δ), 0.001)
end

function δDNCDC(χ, τD)
    find_zero(δ -> c1D(χ, δ) - τD, 0.001)
end

function χDNC(δ0, τF0)
    find_zero(χ -> c1D(χ, δ0) - c1U(δ0, τF0), 0.99)
end

function χDND(δ0, τD0)
    find_zero(χ -> c1D(χ, δ0) - τD0, 0.99)
end

# ================================
# Tracé des graphiques
# ================================

# Fonction de ligne pointillée
function dashedline(λ0, τ0, color, opacity = 1.0)
    plot!([λ0, λ0], [0, τ0], linestyle=:dash, color=color, alpha=opacity, label="")
end

c1full7 = λ -> (λ <= 0.5 ? 1.0 : 1.15)
c1full9 = λ -> (λ <= 0.5 ? 1.0 : 1.05)
λpol9 = λ -> (λ <= 0.5 ? 0.5 : 1.0)
λ_range = 0:0.01:1

#Plot CostRatio
costratio = plot(
    λ_range, λ -> c1full(λ, 1.25),
    label = "δ = 1.25",
    xlabel = "Regional content requirement (χₐ)",
    ylabel = "Cost Penalty for ROO Compliance (C̃)",
    linewidth = 2,
    framestyle = :box
)
plot!(λ_range, λ -> c1full(λ, 1.0), label = "δ = 1")
plot!(λ_range, λ -> c1full(λ, 0.8), label = "δ = 0.8")
hline!([1.1], linestyle=:dash, color=:gray, label="τ = 1.1")  # Ligne horizontale
vline!([0.675], linestyle=:dash, color=:green, label = "")  # Ligne horizontale
vline!([0.885], linestyle=:dash, color=:red, label = "")  # Ligne horizontale
savefig("$output_figures/figure_1a.pdf")
# display(costratio)

#Plot PartShares

partshares = plot(
    λ_range, λ -> λpol2(λ, 1.25, τ),
    label = "δ = 1.25",
    xlabel = "Regional content requirement (χₐ)",
    ylabel = "Firm domestic part share (χ)",
    linewidth = 2,
    framestyle = :box
    )
    plot!(λ_range, λ -> λpol2(λ, 0.999, τ), label = "δ = 1")
    plot!(λ_range, λ -> λpol2(λ, 0.8, τ), label = "δ = 0.8")
    
    # display(partshares)
savefig("$output_figures/figure_1b.pdf")

# Plot PartShares_SinglePart
partshares1 = plot(
    λ -> 1.0,
    0, 1,
    label="δ ≥ 1",
    xlabel="Regional content requirement (χₐ)",
    ylabel="Firm domestic part share (χ)",
    legend=:topright,
    frame=:box,
    linewidth=2
    )
    plot!(λpol9, 0, 1, label="δ < 0.8")
    plot!(λ -> 0.5, 0, 1, label="0.8 ≤ δ < 1")
savefig("$output_figures/figure_5a.pdf")

#Plot CostPenalty_SinglePart
costratio1 = plot(
    λ_range, λ -> 1.0,  # Ligne constante C = 1
    label = "δ >= 1",
    xlabel = "Regional content requirement (χₐ)",
    ylabel = "Cost Penalty for ROO Compliance (C̃)",
    linewidth = 2,
    framestyle = :box
    )
    plot!(λ_range, λ -> c1full9(λ), label = "δ = 0.9", linewidth = 2)
    plot!(λ_range, λ -> c1full7(λ), label = "δ = 0.7", linewidth = 2)
    hline!([1.1], linestyle = :dash, color = :gray, label = "")
savefig("$output_figures/figure_5b.pdf")
    
    
    