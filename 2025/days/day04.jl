### A Pluto.jl notebook ###
# v0.20.21

using Markdown
using InteractiveUtils

# ╔═╡ a02868a2-6df2-4d50-8cd0-5ca77c6d8cea
YEAR = 2025

# ╔═╡ d843733f-a453-44c4-b759-6e84dcb535e5
DAY = 4

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
test_input = """..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@."""

# ╔═╡ 649d2ff5-f85a-43ed-8993-81a51dda4ad1
# ╠═╡ show_logs = false
real_input = read(`cat $real_input_fp`, String)

# ╔═╡ 25c1db54-f713-416e-bee4-efc299a9bac6
function parse_input(input_str::String)::Matrix{Bool}
	reduce(hcat, 
		map(
			x -> map(y -> y== '@', collect(x)),
			split(strip(input_str), "\n")
		)
	)'
end

# ╔═╡ b24a080f-199a-4632-86a0-7af867567c2e
function solver(input_parser, part_solver, input)
	parsed_input = input_parser(input)
	return string(part_solver(parsed_input))
end

# ╔═╡ 58348b51-385b-4969-9802-9fce6b02b95c
function expand_matrix(mat::Matrix{T}, fill_value::T)::Matrix{T} where T
    rows, cols = size(mat)
    new_rows = rows + 2
    new_cols = cols + 2
    
    # Create a new matrix filled with the specified value
    expanded_matrix = fill(fill_value, new_rows, new_cols)
    
    # Place the original matrix into the center of the new matrix
    expanded_matrix[2:rows+1, 2:cols+1] = mat
    
    return expanded_matrix
end

# ╔═╡ fe5652a1-5ed1-46fe-b7cd-d21768ccde65
begin
	offsets = reduce(vcat, collect(d for d in CartesianIndices(ntuple(_->-1:1, 2))))
	
	function get_neighbor_idxs(idx::CartesianIndex)::Vector{CartesianIndex}
		collect((idx + d) for d in offsets)
	end
end

# ╔═╡ 094b8f66-56d1-4e5e-b6b8-9105a476b7b7
function is_pickable(map::Matrix{Bool}, idx::CartesianIndex)::Bool
	if !map[idx]
		return false
	end

	neighbor_idxs = get_neighbor_idxs(idx)
	
	neighbors = (map[i] for i in neighbor_idxs)
	
	return count(neighbors) <= 4
end

# ╔═╡ 9efaf158-c88a-47a4-a0b6-86c5227d5daa
function solve_part1(map::Matrix{Bool})::Number
	expanded_mat = expand_matrix(map, false)
	height, width = size(expanded_mat)
	count = 0
	for i in 2:height-1, j in 2:width-1
		idx = CartesianIndex(i, j)
		count += is_pickable(expanded_mat, idx)
	end
	return count
end

# ╔═╡ c34928c8-a273-4d63-8fa5-e3de89fff8d7
solver(parse_input, solve_part1, test_input)

# ╔═╡ 6f275cf8-4367-4398-aa21-8f1d9cd8c36e
solver(parse_input, solve_part1, real_input)

# ╔═╡ f7bb2230-22ba-4b40-8af2-dd7226e0a9b2
function solve_part2(map::Matrix{Bool})::Number
	expanded_mat = expand_matrix(map, false)
	height, width = size(expanded_mat)
	total_count = 0
	change = 99
	while change != 0
		idxs = []
		for i in 2:height-1, j in 2:width-1
			idx = CartesianIndex(i, j)
			if is_pickable(expanded_mat, idx)
				push!(idxs, idx)
			end
		end
		change = length(idxs)
		for idx in idxs
			expanded_mat[idx] = false
		end
		total_count += change
	end
	return total_count
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
# ╠═25c1db54-f713-416e-bee4-efc299a9bac6
# ╠═b24a080f-199a-4632-86a0-7af867567c2e
# ╠═58348b51-385b-4969-9802-9fce6b02b95c
# ╠═fe5652a1-5ed1-46fe-b7cd-d21768ccde65
# ╠═094b8f66-56d1-4e5e-b6b8-9105a476b7b7
# ╠═9efaf158-c88a-47a4-a0b6-86c5227d5daa
# ╠═c34928c8-a273-4d63-8fa5-e3de89fff8d7
# ╠═6f275cf8-4367-4398-aa21-8f1d9cd8c36e
# ╠═f7bb2230-22ba-4b40-8af2-dd7226e0a9b2
# ╠═154fec09-f761-43aa-aaed-129d4d2f711e
# ╠═4b783820-f72d-4ea8-bf8c-bbc2d556484c
# ╟─00000000-0000-0000-0000-000000000001
# ╟─00000000-0000-0000-0000-000000000002
