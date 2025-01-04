# We test our julia functions based on the results obtained from the original paper functions. 

include("../src/AALA_calibration_functions.jl")
using .AALA_calibration_functions
using DataFrames
using RoO
using Revise
using Test


# We test for the functions in AALA_calibration_functions.jl ========================================
@testset "AALA_calibration_functions.jl" begin
    # Test lambda_RCR
    @test isapprox(AALA_calibration_functions.lambda_RCR(0.5, [0.4]), [0.16666666666666663]; atol=1e-8)

    # Test chi_lambda
    @test AALA_calibration_functions.chi_lambda([0.5], 1.0, 1.0) == [0.5]

    # Test chi_U
    @test isapprox(AALA_calibration_functions.chi_U(1.0, 1.0), 0.5; atol=1e-8)

    # Test lambda_U
    @test isapprox(AALA_calibration_functions.lambda_U(1.0, 1.0), 0.5; atol=1e-8)

    # Test pdf_U
    @test isapprox(AALA_calibration_functions.pdf_U([0.5,0.6], 1.0, 1.0, 1.0), [2.013036e-09, 5.234425e-09]; atol=1e-8)

    # Test C_U
    @test isapprox(AALA_calibration_functions.C_U(0.6, 1.0), 0.375; atol=1e-1)

    # Test C_comply
    @test isapprox(AALA_calibration_functions.C_comply(0.5, 0.5, 0.5), 0.1732807, atol=1e-5)

    # Test C_tilde
    @test isapprox(AALA_calibration_functions.C_tilde(1.0, 0.5, 0.5), 5.828427; atol=1e-5)

    # Test delta_max
    @test isapprox(AALA_calibration_functions.delta_max(0.5, 0.5), 11.65685; atol=1e-5)

    # Test delta_star
    @test isapprox(AALA_calibration_functions.delta_star(0.6, 1.02, 4), 0.8849212; atol = 1e-4)

    # Test delta_circ
    @test isapprox(AALA_calibration_functions.delta_circ(0.6,4), 1.144714; atol = 1e-4)

    # Test beta_draws
    @test AALA_calibration_functions.beta_draws(1, 0.5, 0.5) isa Union{Float64, AbstractVector{Float64}}

    # Test ubeta_draws
    @test AALA_calibration_functions.ubeta_draws(1, 0.5, 0.5) isa Union{Float64, AbstractVector{Float64}}

    # Test clean density data
    @test AALA_calibration_functions.clean_density_data(
        DataFrame(:kernell_x => [1,2,3,4,5], :kernell_y => [1,2,3,4,5]), :den_data) isa DataFrame

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
