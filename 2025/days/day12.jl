### A Pluto.jl notebook ###
# v0.20.21

using Markdown
using InteractiveUtils

# ╔═╡ a02868a2-6df2-4d50-8cd0-5ca77c6d8cea
YEAR = 2025

# ╔═╡ d843733f-a453-44c4-b759-6e84dcb535e5
DAY = 12

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
test_input = """0:
###
##.
##.

1:
###
##.
.##

2:
.##
###
##.

3:
##.
###
##.

4:
###
#..
###

5:
###
.#.
###

4x4: 0 0 0 0 2 0
12x5: 1 0 1 0 2 2
12x5: 1 0 1 0 3 2"""

# ╔═╡ 649d2ff5-f85a-43ed-8993-81a51dda4ad1
# ╠═╡ show_logs = false
real_input = read(`cat $real_input_fp`, String)

# ╔═╡ 73892e9b-d257-4d2a-98e0-665aa2675cda
struct Tree
	width::Int64
	height::Int64
	shape_counts::Vector{Int64}
end

# ╔═╡ 25c1db54-f713-416e-bee4-efc299a9bac6
function parse_input(input_str::AbstractString)::Tuple{Vector{AbstractString}, Vector{Tree}}
	raw_shapes..., raw_trees = collect(split(strip(input_str), "\n\n"))

	trees = map(
		line -> begin
			width, height, shape_counts... = parse.(Int64, first.(eachmatch(r"(\d+)", line)))
			Tree(width, height, shape_counts)
		end,
		split(raw_trees, "\n")
	)

	return raw_shapes, trees
end

# ╔═╡ 4beafbfe-b1a0-48d1-9103-e635fe43930e
parse_input(test_input)

# ╔═╡ b24a080f-199a-4632-86a0-7af867567c2e
function solver(input_parser, part_solver, input)
	parsed_input = input_parser(input)
	return string(part_solver(parsed_input))
end

# ╔═╡ 9efaf158-c88a-47a4-a0b6-86c5227d5daa
function solve_part1(input::Tuple{Vector{AbstractString}, Vector{Tree}})::Number
	shapes, trees = input
	shape_sizes = map(shape -> count(isequal('#'), shape), shapes)

	count(
		tree -> begin
			area = tree.width * tree.height
			nec_space = sum(enumerate(tree.shape_counts)) do (idx, val) 
				shape_sizes[idx] * val
			end
			area > (1.2 * nec_space)
		end,
		trees
	)
end

# ╔═╡ c34928c8-a273-4d63-8fa5-e3de89fff8d7
solver(parse_input, solve_part1, test_input)

# ╔═╡ 6f275cf8-4367-4398-aa21-8f1d9cd8c36e
solver(parse_input, solve_part1, real_input)

# ╔═╡ f7bb2230-22ba-4b40-8af2-dd7226e0a9b2
function solve_part2(input::String)::Number
	return length(input)
end

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
# ╠═74bb4e7b-c622-40bc-8ab7-b06068d5595c
# ╠═5367dbdf-ee9f-4360-a7d3-a8a5988d90fa
# ╠═8a79b53a-c8fe-11f0-9297-97044163d935
# ╠═ea4bd3be-5078-4e13-8373-6804189aae80
# ╠═649d2ff5-f85a-43ed-8993-81a51dda4ad1
# ╠═73892e9b-d257-4d2a-98e0-665aa2675cda
# ╠═25c1db54-f713-416e-bee4-efc299a9bac6
# ╠═4beafbfe-b1a0-48d1-9103-e635fe43930e
# ╠═b24a080f-199a-4632-86a0-7af867567c2e
# ╠═9efaf158-c88a-47a4-a0b6-86c5227d5daa
# ╠═c34928c8-a273-4d63-8fa5-e3de89fff8d7
# ╠═6f275cf8-4367-4398-aa21-8f1d9cd8c36e
# ╠═f7bb2230-22ba-4b40-8af2-dd7226e0a9b2
# ╟─00000000-0000-0000-0000-000000000001
# ╟─00000000-0000-0000-0000-000000000002
