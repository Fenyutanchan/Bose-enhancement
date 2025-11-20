result_file = joinpath(@__DIR__, "integral_over_s_and_t.m")

result_string = join(readlines(result_file)[2:end])

result_string = replace(result_string, "MPl" => "NU.M_Pl", "Pi" => "π", ' ' => "")
result_string = "integral_over_s_and_t(k1, Eq, p1, smax) = " * result_string * "\n"

open(joinpath(@__DIR__, "tool_script-integral_over_s_and_t.jl"), "w+") do io
    write(io, result_string)
end
