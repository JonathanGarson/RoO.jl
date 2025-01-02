# We test our julia functions based on the results obtained from the original paper functions. 

using RoO
using Test
include("../src/AALA_calibration_functions.jl")


# We test for the functions in AALA_calibration_functions.jl ========================================
@testset "AALA_calibration_functions.jl" begin
    # Test lambda_RCR
    @test isapprox(lambda_RCR(0.5, [0.4]), [0.16666666666666663]; atol=1e-8)

    # Test chi_lambda
    @test chi_lambda([0.5], 1.0, 1.0) == [0.5]

    # Test chi_U
    @test isapprox(chi_U(1.0, 1.0), 0.5; atol=1e-8)

    # Test lambda_U
    @test isapprox(lambda_U(1.0, 1.0), 0.5; atol=1e-8)

    # Test pdf_U
    @test isapprox(pdf_U([0.5,0.6], 1.0, 1.0, 1.0), [2.013036e-09, 5.234425e-09]; atol=1e-8)

    # Test C_U
    @test isapprox(C_U(0.6, 1.0), 0.375; atol=1e-1)

    # Test C_comply
    @test isapprox(C_comply(0.5, 0.5, 0.5), 0.1732807, atol=1e-5)

    # Test C_tilde
    @test isapprox(C_tilde(1.0, 0.5, 0.5), 5.828427; atol=1e-5)

    # Test delta_max
    @test isapprox(delta_max(0.5, 0.5), 11.65685; atol=1e-5)

    # Test delta_star

    # Test delta_circ

    # Test beta_draws
    @test beta_draws(1, 0.5, 0.5) isa Union{Float64, AbstractVector{Float64}}

    # Test ubeta_draws
    @test ubeta_draws(1, 0.5, 0.5) isa Union{Float64, AbstractVector{Float64}}

    # MAIN FUNCTIONS
    # sim lambda

    # sim_lambda_alpha
    
    # sim_lambda_alpha_o

    # sim_lambda_alpha_DRF
    
    

end

# We test for the calibration of the model ==========================================================
@testset "RoO.jl" begin
    # Write your tests here.
end
