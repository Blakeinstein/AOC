### A Pluto.jl notebook ###
# v0.20.21

using Markdown
using InteractiveUtils

# ╔═╡ a02868a2-6df2-4d50-8cd0-5ca77c6d8cea
YEAR = 2025

# ╔═╡ d843733f-a453-44c4-b759-6e84dcb535e5
DAY = 3

# ╔═╡ 74bb4e7b-c622-40bc-8ab7-b06068d5595c
begin
	problem = read(`aoc -y $YEAR -d $DAY read -m -P`, String)
	@eval @md_str $problem
end

# ╔═╡ 5367dbdf-ee9f-4360-a7d3-a8a5988d90fa
real_input_fp = `../input/day$DAY.txt`

# ╔═╡ 8a79b53a-c8fe-11f0-9297-97044163d935
run(`aoc -y $YEAR -d $DAY download -I -i $real_input_fp`)

# ╔═╡ ea4bd3be-5078-4e13-8373-6804189aae80
test_input = """987654321111111 811111111111119 234234234234278 818181911112111"""

# ╔═╡ 649d2ff5-f85a-43ed-8993-81a51dda4ad1
# ╠═╡ show_logs = false
real_input = read(`cat $real_input_fp`, String)

# ╔═╡ 25c1db54-f713-416e-bee4-efc299a9bac6
function parse_input(input_str::String)::Vector{Vector{UInt8}}
	return map(
		x -> map(y -> parse(UInt8, y), collect(x)),
		split(input_str)
	)
end

# ╔═╡ b24a080f-199a-4632-86a0-7af867567c2e
function solver(input_parser, part_solver, input)
	parsed_input = input_parser(input)
	return string(part_solver(parsed_input))
end

# ╔═╡ a9dd5194-b718-4030-b645-3df968d4730a
function calc_max_joltage(battery::Vector{UInt8})::Number
	size = length(battery)
	curr_max = battery[1] * 10 + battery[end]
	for i in 1:size, j in i+1:size
		curr_max = max(curr_max, battery[i] * 10 + battery[j])
	end
	return curr_max
end

# ╔═╡ 9efaf158-c88a-47a4-a0b6-86c5227d5daa
function solve_part1(batteries::Vector{Vector{UInt8}})::Number
	joltage = 0
	for battery in batteries
		joltage += calc_max_joltage(battery)
	end
	return joltage
end

# ╔═╡ c34928c8-a273-4d63-8fa5-e3de89fff8d7
solver(parse_input, solve_part1, test_input)

# ╔═╡ 6f275cf8-4367-4398-aa21-8f1d9cd8c36e
solver(parse_input, solve_part1, real_input)

# ╔═╡ 20a788b1-cba1-44c1-b3ed-27f4dd220195
function calc_max_12_joltage(battery::Vector{UInt8})::Number
	size = length(battery)
    to_remove = size - 12
    stack = UInt8[]
    
    for digit in battery
        while to_remove > 0 && !isempty(stack) && stack[end] < digit
            pop!(stack)
            to_remove -= 1
        end
        push!(stack, digit)
    end
    
    return parse(Int, join(stack[1:12]))
end

# ╔═╡ f7bb2230-22ba-4b40-8af2-dd7226e0a9b2
function solve_part2(batteries::Vector{Vector{UInt8}})::Number
	sum(map(calc_max_12_joltage, batteries))
end

# ╔═╡ 154fec09-f761-43aa-aaed-129d4d2f711e
solver(parse_input, solve_part2, test_input)

# ╔═╡ 4b783820-f72d-4ea8-bf8c-bbc2d556484c
solver(parse_input, solve_part2, real_input)

# ╔═╡ 00000000-0000-0000-0000-000000000001
PLUTO_PROJECT_TOML_CONTENTS = """
[deps]
"""

# ╔═╡ 00000000-0000-0000-0000-000000000002
PLUTO_MANIFEST_TOML_CONTENTS = """
# This file is machine-generated - editing it directly is not advised

julia_version = "1.12.2"
manifest_format = "2.0"
project_hash = "71853c6197a6a7f222db0f1978c7cb232b87c5ee"

[deps]
"""

# ╔═╡ Cell order:
# ╟─a02868a2-6df2-4d50-8cd0-5ca77c6d8cea
# ╟─d843733f-a453-44c4-b759-6e84dcb535e5
# ╟─74bb4e7b-c622-40bc-8ab7-b06068d5595c
# ╟─5367dbdf-ee9f-4360-a7d3-a8a5988d90fa
# ╟─8a79b53a-c8fe-11f0-9297-97044163d935
# ╟─ea4bd3be-5078-4e13-8373-6804189aae80
# ╟─649d2ff5-f85a-43ed-8993-81a51dda4ad1
# ╠═25c1db54-f713-416e-bee4-efc299a9bac6
# ╠═b24a080f-199a-4632-86a0-7af867567c2e
# ╠═a9dd5194-b718-4030-b645-3df968d4730a
# ╠═9efaf158-c88a-47a4-a0b6-86c5227d5daa
# ╠═c34928c8-a273-4d63-8fa5-e3de89fff8d7
# ╠═6f275cf8-4367-4398-aa21-8f1d9cd8c36e
# ╠═20a788b1-cba1-44c1-b3ed-27f4dd220195
# ╠═f7bb2230-22ba-4b40-8af2-dd7226e0a9b2
# ╠═154fec09-f761-43aa-aaed-129d4d2f711e
# ╠═4b783820-f72d-4ea8-bf8c-bbc2d556484c
# ╟─00000000-0000-0000-0000-000000000001
# ╟─00000000-0000-0000-0000-000000000002
