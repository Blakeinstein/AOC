### A Pluto.jl notebook ###
# v0.20.21

using Markdown
using InteractiveUtils

# ╔═╡ a02868a2-6df2-4d50-8cd0-5ca77c6d8cea
YEAR = 2025

# ╔═╡ d843733f-a453-44c4-b759-6e84dcb535e5
DAY = 7

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
test_input = """.......S.......
...............
.......^.......
...............
......^.^......
...............
.....^.^.^.....
...............
....^.^...^....
...............
...^.^...^.^...
...............
..^...^.....^..
...............
.^.^.^.^.^...^.
..............."""

# ╔═╡ 649d2ff5-f85a-43ed-8993-81a51dda4ad1
# ╠═╡ show_logs = false
real_input = read(`cat $real_input_fp`, String)

# ╔═╡ 25c1db54-f713-416e-bee4-efc299a9bac6
function parse_input(input_str::String)::Matrix{Char}
	return stack(split(strip(input_str), "\n"); dims=1)
end

# ╔═╡ 11051175-358a-45b7-a4fd-ecab87bedc5e
t =  parse_input(test_input)

# ╔═╡ b24a080f-199a-4632-86a0-7af867567c2e
function solver(input_parser, part_solver, input)
	parsed_input = input_parser(input)
	return string(part_solver(parsed_input))
end

# ╔═╡ 9efaf158-c88a-47a4-a0b6-86c5227d5daa
function solve_part1(input::Matrix{Char})::Number
	count = 0
	height, width = size(input)

	beams = Set()
	push!(beams, findfirst(==('S'), input[1, :]))
	for i in 2:height
		new_set = Set()
		for x in beams
			if x <= 0 || x > width
				continue
			end
			if input[i, x] == '^'
				push!(new_set, x-1)
				push!(new_set, x+1)
				count += 1
			else
				push!(new_set, x)
			end
		end
		copy!(beams, new_set)
	end
	
	return count
end

# ╔═╡ c34928c8-a273-4d63-8fa5-e3de89fff8d7
solver(parse_input, solve_part1, test_input)

# ╔═╡ 6f275cf8-4367-4398-aa21-8f1d9cd8c36e
solver(parse_input, solve_part1, real_input)

# ╔═╡ 9c5abf00-09fd-448a-8510-1b23eb734e4d
function push_next(key::Int64, value::Int64, beams::Dict{Int64, Int64}, width::Int64)
	if key <= 0 || key > width
		return
	end
	if !haskey(beams, key)
		beams[key] = 0
	end
	beams[key] += value
end

# ╔═╡ d4ab8d8b-9e5a-458b-a925-41e22a02b6ba
function recursive(line_idx::Int64, beams::Dict{Int64, Int64}, map::Matrix{Char}, height::Int64, width::Int64)::Number
	if line_idx > height
		return sum(values(beams))
	end
	next_beams::Dict{Int64, Int64} = Dict()
	for (key, val) in beams
		if map[line_idx, key] == '^'
			push_next(key-1, val, next_beams, width)
			push_next(key+1, val, next_beams, width)
		else
			push_next(key, val, next_beams, width)
		end
	end
	

	return recursive(line_idx + 1, next_beams, map, height, width)
end

# ╔═╡ f7bb2230-22ba-4b40-8af2-dd7226e0a9b2
function solve_part2(input::Matrix{Char})::Number
	count = 0
	dups = 0
	height, width = size(input)

	beams::Dict{Int64, Int64} = Dict()
	beams[findfirst(==('S'), input[1, :])] = 1
	
	return recursive(2, beams, input, height, width)
end

# ╔═╡ 154fec09-f761-43aa-aaed-129d4d2f711e
solver(parse_input, solve_part2, test_input)

# ╔═╡ 4b783820-f72d-4ea8-bf8c-bbc2d556484c
solver(parse_input, solve_part2, real_input)

# ╔═╡ Cell order:
# ╟─a02868a2-6df2-4d50-8cd0-5ca77c6d8cea
# ╟─d843733f-a453-44c4-b759-6e84dcb535e5
# ╟─74bb4e7b-c622-40bc-8ab7-b06068d5595c
# ╟─5367dbdf-ee9f-4360-a7d3-a8a5988d90fa
# ╠═8a79b53a-c8fe-11f0-9297-97044163d935
# ╠═ea4bd3be-5078-4e13-8373-6804189aae80
# ╠═649d2ff5-f85a-43ed-8993-81a51dda4ad1
# ╠═25c1db54-f713-416e-bee4-efc299a9bac6
# ╠═11051175-358a-45b7-a4fd-ecab87bedc5e
# ╠═b24a080f-199a-4632-86a0-7af867567c2e
# ╠═9efaf158-c88a-47a4-a0b6-86c5227d5daa
# ╠═c34928c8-a273-4d63-8fa5-e3de89fff8d7
# ╠═6f275cf8-4367-4398-aa21-8f1d9cd8c36e
# ╠═9c5abf00-09fd-448a-8510-1b23eb734e4d
# ╠═d4ab8d8b-9e5a-458b-a925-41e22a02b6ba
# ╠═f7bb2230-22ba-4b40-8af2-dd7226e0a9b2
# ╠═154fec09-f761-43aa-aaed-129d4d2f711e
# ╠═4b783820-f72d-4ea8-bf8c-bbc2d556484c
