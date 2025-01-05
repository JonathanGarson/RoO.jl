include("../src/AALA_calibration_functions.jl")
include("../src/AALA_solving_alt.jl")
include("../src/AALA_clean.jl")

using .AALA_calibration_functions
using DataFrames
using RoO
using Revise
using Test

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
    @test isapprox(delta_star(0.6, 1.02, 4), 0.8849212; atol = 1e-4)

    # Test delta_circ
    @test isapprox(delta_circ(0.6,4), 1.144714; atol = 1e-4)

    # Test beta_draws
    @test beta_draws(1, 0.5, 0.5) isa Union{Float64, AbstractVector{Float64}}

    # Test ubeta_draws
    @test ubeta_draws(1, 0.5, 0.5) isa Union{Float64, AbstractVector{Float64}}

    # Test clean density data
    @test clean_density_data(
        DataFrame(:kernell_x => [1,2,3,4,5], :kernell_y => [1,2,3,4,5]), :den_data) isa DataFrame
    
end

@testset "AALA_solving_alt.jl" begin
    # Test lambda_RCR
    @test isapprox(λ_RCR(0.5, [0.4]), [0.16666666666666663]; atol=1e-8)

    # Test χ_λ
    @test χ_λ(1.0, 4.0) == [0.5]

    # Test χ_U
    @test isapprox(χ_U(1.0, 1.0), 0.5; atol=1e-8)

    # Test λ_U
    @test isapprox(λ_U(1.0, 1.0), 0.5; atol=1e-8)

    # Test pdf_U
    @test isapprox(pdf_U([0.5,0.6], 1.0, 1.0, 1.0), [2.013036e-09, 5.234425e-09]; atol=1e-8)

    # Test C_U
    @test isapprox(C_U(0.6, 1.0), 0.375; atol=1e-1)

    # Test C_comply
    @test isapprox(C_comply(0.5, 0.5, 0.5), 0.1732807; atol=1e-5)

    # Test compliance cost
    @test C_tilde(0.615, 1.1, 4.0) isa Union{Float64, AbstractVector{Float64}}

    # Test delta max
    @test δ_max(1.1, 4.0) .> 0.0
    
    # Test delta star
    @test δ_star(0.6, 1.02, 4.0) .> 0.0 && δ_star(0.6, 1.02, 4.0) .< δ_max(1.1, 4.0)
    
    # Test delta circ
    @test δ_circ(0.6, 4.0) .> 0.0 
end


# @testset "RoO.jl" begin

# end

@testset "AALA_clean.jl" begin
      
    # Unit Test: Check summary statistics for a dataset with missing and non-missing values

    #Test function summarize
        # Test 1: Data with some missing values
        data1 = [1.0, 2.0, missing, 4.0, 5.0, missing]
        result1 = summarize(data1)

        @test result1["mean"] == 3.0  # mean of [1.0, 2.0, 4.0, 5.0] is 3.0
        @test result1["min"] == 1.0   # min of [1.0, 2.0, 4.0, 5.0] is 1.0
        @test result1["max"] == 5.0   # max of [1.0, 2.0, 4.0, 5.0] is 5.0
        @test result1["missing_count"] == 2  # There are 2 missing values
        @test result1["nonmissing_count"] == 4  # There are 4 non-missing values

        # Test 2: Data with all non-missing values
        data2 = [10.0, 20.0, 30.0, 40.0]
        result2 = summarize(data2)

        @test result2["mean"] == 25.0  # mean of [10.0, 20.0, 30.0, 40.0] is 25.0
        @test result2["min"] == 10.0   # min of [10.0, 20.0, 30.0, 40.0] is 10.0
        @test result2["max"] == 40.0   # max of [10.0, 20.0, 30.0, 40.0] is 40.0
        @test result2["missing_count"] == 0  # No missing values
        @test result2["nonmissing_count"] == 4  # All values are non-missing

        # Test 4: Data with no missing values
        data4 = [1.0, 2.0, 3.0]
        result4 = summarize(data4)

        @test result4["mean"] == 2.0  # mean of [1.0, 2.0, 3.0] is 2.0
        @test result4["min"] == 1.0   # min of [1.0, 2.0, 3.0] is 1.0
        @test result4["max"] == 3.0   # max of [1.0, 2.0, 3.0] is 3.0
        @test result4["missing_count"] == 0  # No missing values
        @test result4["nonmissing_count"] == 3  # All values are non-missing

    # Test merge_stata

        # Example DataFrames for the test
        DTx = DataFrame(id = [1, 2, 3], value = [10, 20, 30])
        DTy = DataFrame(id = [2, 3, 4], value = [200, 300, 400])
        
        # Perform the merge
        result = merge_stata(DTx, DTy, :id)
        
        # Check if the result is a DataFrame
        @test typeof(result) == DataFrame

end