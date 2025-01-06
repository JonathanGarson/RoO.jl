using Plots

# figures 1 ========================================

# Define Parameters
ζ = 0.25  # Adjust if necessary
τ = 1.1  # Tariff threshold

# Define functions (from the translated Mathematica code)
function c1full(λ, δ, ζ)
    λmin_δ = (1 + δ^(-1 / ζ))^(-1)
    if λ > λmin_δ
        c = λ^(ζ + 1) + δ * (1 - λ)^(ζ + 1)
        cmin_δ = (1 + δ^(-1 / ζ))^(-ζ)
        return c / cmin_δ
    else
        return 1.0
    end
end

function λmin(δ)
    1 / (1 + δ^(-1 / ζ))
end

# Generate data
λ_vals = 0.0:0.01:1.0  # Regional content requirement (χᵣ)
δ_values = [0.8, 1.0, 1.25]  # Different δ scenarios
colors = [:green, :orange, :blue]  # Colors for lines

# Plotting
p = plot(legend=:topright, xlabel="Regional content requirement (χᵣ)", ylabel="Cost Penalty for ROO Compliance (C̃)", frame=:box, size=(800, 600))
for (δ, color) in zip(δ_values, colors)
    c_vals = [c1full(λ, δ) for λ in λ_vals]
    plot!(λ_vals, c_vals, label="δ = $δ", color=color, lw=2)
end

# Add horizontal line for τ
hline!([τ], color=:black, linestyle=:solid, label="τ", lw=1)

# Add dashed vertical lines at λmin for each δ
for (δ, color) in zip(δ_values, colors)
    λ_star = λmin(δ)
    vline!([λ_star], color=color, linestyle=:dash, label=false)
end