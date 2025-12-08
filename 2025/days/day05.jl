### A Pluto.jl notebook ###
# v0.20.21

using Markdown
using InteractiveUtils

# ╔═╡ a02868a2-6df2-4d50-8cd0-5ca77c6d8cea
YEAR = 2025

# ╔═╡ d843733f-a453-44c4-b759-6e84dcb535e5
DAY = 5

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
test_input = """3-5
10-14
16-20
12-18

1
5
8
11
17
32"""

# ╔═╡ 649d2ff5-f85a-43ed-8993-81a51dda4ad1
# ╠═╡ show_logs = false
real_input = read(`cat $real_input_fp`, String)

# ╔═╡ 92b5db11-99b0-4aa3-8627-5f5c24bf0ef3
struct Input
	ranges::Vector{UnitRange{Int64}}
	ingredients::Vector{Int64}
end

# ╔═╡ 25c1db54-f713-416e-bee4-efc299a9bac6
function parse_input(input_str::String)::Input
	range, ingredients = split(strip(input_str), "\n\n", limit=2)
	return Input(
		map(
			x -> begin
				s, e = split(x, "-", limit=2)
				parse(Int64, s):parse(Int64, e)
			end, 
			split(range, "\n")
		),
		map(
			x -> parse(Int64, x),
			split(ingredients, "\n")
		)
	)
end

# ╔═╡ ec2c77a2-9481-42e6-98a2-f55935eddb50
parse_input(test_input)

# ╔═╡ b24a080f-199a-4632-86a0-7af867567c2e
function solver(input_parser, part_solver, input)
	parsed_input = input_parser(input)
	return string(part_solver(parsed_input))
end

# ╔═╡ 9efaf158-c88a-47a4-a0b6-86c5227d5daa
function solve_part1(input::Input)::Number
	count(
		any(ing in range for range in input.ranges) for ing in input.ingredients
	)
end

# ╔═╡ c34928c8-a273-4d63-8fa5-e3de89fff8d7
solver(parse_input, solve_part1, test_input)

# ╔═╡ 6f275cf8-4367-4398-aa21-8f1d9cd8c36e
solver(parse_input, solve_part1, real_input)

# ╔═╡ bea5a2ef-51e3-4460-8c82-e1b06c6fdf3a
mutable struct Range 
	start::Int64
	stop::Int64
end

# ╔═╡ 1e684f5b-45d3-40f1-be3a-76294e461f62
function length(range::Range)::Number
	range.stop - range.start + 1
end

# ╔═╡ 35e7c381-bf29-449b-a728-d9092fe59fe8
function intersects(range::Range, source::Range)::Bool
	ac_range = range.start:range.stop
	return source.start in ac_range || source.stop in ac_range
end

# ╔═╡ 81d8f377-49d1-4847-9a9d-86606d90fc3b
function isless(a::Range, b::Range)::Bool
	a.start < b.start
end

# ╔═╡ f7bb2230-22ba-4b40-8af2-dd7226e0a9b2
function solve_part2(input::Input)::Number
	existing_ranges = collect(map(x -> Range(x.start, x.stop), input.ranges))
	sort!(existing_ranges, by=x->x.start)
	final = [existing_ranges[1]]
	for range in existing_ranges[2:end]
		if range.start <= final[end].stop
			final[end].stop = max(final[end].stop, range.stop)
		else
			push!(final, range)
		end
	end
	sum(map(length, final))
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
# ╠═ea4bd3be-5078-4e13-8373-6804189aae80
# ╠═649d2ff5-f85a-43ed-8993-81a51dda4ad1
# ╠═92b5db11-99b0-4aa3-8627-5f5c24bf0ef3
# ╠═25c1db54-f713-416e-bee4-efc299a9bac6
# ╠═ec2c77a2-9481-42e6-98a2-f55935eddb50
# ╠═b24a080f-199a-4632-86a0-7af867567c2e
# ╠═9efaf158-c88a-47a4-a0b6-86c5227d5daa
# ╠═c34928c8-a273-4d63-8fa5-e3de89fff8d7
# ╠═6f275cf8-4367-4398-aa21-8f1d9cd8c36e
# ╠═bea5a2ef-51e3-4460-8c82-e1b06c6fdf3a
# ╠═1e684f5b-45d3-40f1-be3a-76294e461f62
# ╠═35e7c381-bf29-449b-a728-d9092fe59fe8
# ╠═81d8f377-49d1-4847-9a9d-86606d90fc3b
# ╠═f7bb2230-22ba-4b40-8af2-dd7226e0a9b2
# ╠═154fec09-f761-43aa-aaed-129d4d2f711e
# ╠═4b783820-f72d-4ea8-bf8c-bbc2d556484c
# ╟─00000000-0000-0000-0000-000000000001
# ╟─00000000-0000-0000-0000-000000000002
