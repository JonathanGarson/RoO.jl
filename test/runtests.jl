using RoO
using Test
include("../src/AALA_calibration_functions.jl")

# We test for the functions in AALA_calibration_functions.jl ========================================
@testset "AALA_calibration_functions.jl" begin
    # Test lambda_RCR
    @test isapprox(lambda_RCR(0.5, 0.4), [0.16666666666666663]; atol=1e-8)

    # Test chi_lambda
    @test chi_lambda([0.5], 1, 1) == [0.5]

    # Test chi_U
    @test isapprox(chi_U(1, 1), 0.5; atol=1e-8)
end

# We test for the calibration of the model ==========================================================
@testset "RoO.jl" begin
    # Write your tests here.
end
