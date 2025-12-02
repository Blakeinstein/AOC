### A Pluto.jl notebook ###
# v0.20.21

using Markdown
using InteractiveUtils

# ╔═╡ a02868a2-6df2-4d50-8cd0-5ca77c6d8cea
YEAR = 2025

# ╔═╡ d843733f-a453-44c4-b759-6e84dcb535e5
DAY = 1

# ╔═╡ 74bb4e7b-c622-40bc-8ab7-b06068d5595c
run(`aoc -y $YEAR -d $DAY`)

# ╔═╡ 5367dbdf-ee9f-4360-a7d3-a8a5988d90fa
real_input_fp = `../input/day$DAY.txt`

# ╔═╡ 8a79b53a-c8fe-11f0-9297-97044163d935
run(`aoc -y $YEAR -d $DAY download -I -i $real_input_fp`)

# ╔═╡ ea4bd3be-5078-4e13-8373-6804189aae80
test_input = """L68
L30
R48
L5
R60
L55
L1
L99
R14
L82"""

# ╔═╡ 649d2ff5-f85a-43ed-8993-81a51dda4ad1
# ╠═╡ show_logs = false
real_input = read(`cat $real_input_fp`, String)

# ╔═╡ a5888f1d-d211-4c65-8003-a20d7314cac2
struct Move
	direction::Char
	steps::Number
end

# ╔═╡ 25c1db54-f713-416e-bee4-efc299a9bac6
function parse_input(input_str::String)::Vector{Move}
	return map((line) -> Move(line[1], parse(Int, line[2:end])), split(input_str))
end

# ╔═╡ b24a080f-199a-4632-86a0-7af867567c2e
function solver(input_parser, part_solver, input)
	parsed_input = input_parser(input)
	return string(part_solver(parsed_input))
end

# ╔═╡ 778d8406-c983-460a-8d03-8a60e2236d45
function rotate(curr::Int, move::Move)::Int
	mod(
		begin 
			if move.direction == 'R'
				curr + move.steps
			else
				curr - move.steps
			end
		end, 
		100
	)
end

# ╔═╡ 9efaf158-c88a-47a4-a0b6-86c5227d5daa
function solve_part1(moves::Vector{Move})::Number
	count = 0
	pos = 50
	for move in moves
		pos = rotate(pos, move)
		count += (pos == 0)
	end
	return count
end

# ╔═╡ c34928c8-a273-4d63-8fa5-e3de89fff8d7
solver(parse_input, solve_part1, test_input)

# ╔═╡ 6f275cf8-4367-4398-aa21-8f1d9cd8c36e
solver(parse_input, solve_part1, real_input)

# ╔═╡ f7bb2230-22ba-4b40-8af2-dd7226e0a9b2
function solve_part2(moves::Vector{Move})::Number
	count = 0
	pos = 50
	for move in moves
		if move.direction == 'R'
			count += floor((pos + move.steps) / 100) - floor(pos / 100)
		else
			count += floor((pos - 1) / 100) - floor((pos - 1 - move.steps) / 100)
		end
		pos = rotate(pos, move)
	end
	return count
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
# ╠═a02868a2-6df2-4d50-8cd0-5ca77c6d8cea
# ╠═d843733f-a453-44c4-b759-6e84dcb535e5
# ╠═74bb4e7b-c622-40bc-8ab7-b06068d5595c
# ╠═5367dbdf-ee9f-4360-a7d3-a8a5988d90fa
# ╠═8a79b53a-c8fe-11f0-9297-97044163d935
# ╠═ea4bd3be-5078-4e13-8373-6804189aae80
# ╠═649d2ff5-f85a-43ed-8993-81a51dda4ad1
# ╠═a5888f1d-d211-4c65-8003-a20d7314cac2
# ╠═25c1db54-f713-416e-bee4-efc299a9bac6
# ╠═b24a080f-199a-4632-86a0-7af867567c2e
# ╠═778d8406-c983-460a-8d03-8a60e2236d45
# ╠═9efaf158-c88a-47a4-a0b6-86c5227d5daa
# ╠═c34928c8-a273-4d63-8fa5-e3de89fff8d7
# ╠═6f275cf8-4367-4398-aa21-8f1d9cd8c36e
# ╠═f7bb2230-22ba-4b40-8af2-dd7226e0a9b2
# ╠═154fec09-f761-43aa-aaed-129d4d2f711e
# ╠═4b783820-f72d-4ea8-bf8c-bbc2d556484c
# ╟─00000000-0000-0000-0000-000000000001
# ╟─00000000-0000-0000-0000-000000000002
