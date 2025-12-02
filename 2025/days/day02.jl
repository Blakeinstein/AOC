### A Pluto.jl notebook ###
# v0.20.21

using Markdown
using InteractiveUtils

# ╔═╡ a16fd76f-a452-44ef-bb81-dd2440cb85fb
using Memoize

# ╔═╡ a02868a2-6df2-4d50-8cd0-5ca77c6d8cea
YEAR = 2025

# ╔═╡ d843733f-a453-44c4-b759-6e84dcb535e5
DAY = 2

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
test_input = """11-22,95-115,998-1012,1188511880-1188511890,222220-222224,
1698522-1698528,446443-446449,38593856-38593862,565653-565659,
824824821-824824827,2121212118-2121212124"""

# ╔═╡ 649d2ff5-f85a-43ed-8993-81a51dda4ad1
# ╠═╡ show_logs = false
real_input = read(`cat $real_input_fp`, String)

# ╔═╡ 25c1db54-f713-416e-bee4-efc299a9bac6
function parse_input(input_str::String)Vector{UnitRange{Int64}}
	return map(
		(ranges) -> begin
			parts = split(ranges, "-", limit=2)
			range(parse(Int, parts[1]), parse(Int, parts[2]))
		end,
		split(input_str, ",")
	)
end

# ╔═╡ 18c6c9c5-508a-45bf-a67c-231b52ae47d6
@memoize function is_invalid(num::Int)::Bool
	num_as_str = string(num)
	size = length(num_as_str)
	if (size & 1) > 0
		return false
	end
	middle = div(size,2)
	return num_as_str[begin:middle] == num_as_str[middle+1:end]
end

# ╔═╡ b24a080f-199a-4632-86a0-7af867567c2e
function solver(input_parser, part_solver, input)
	parsed_input = input_parser(input)
	return string(part_solver(parsed_input))
end

# ╔═╡ 9efaf158-c88a-47a4-a0b6-86c5227d5daa
function solve_part1(domains::Vector{UnitRange{Int64}})::Number
	sum = 0
	for domain in domains
		for number in domain
			if is_invalid(number)
				sum += number
			end
		end
	end
	return sum
end

# ╔═╡ c34928c8-a273-4d63-8fa5-e3de89fff8d7
solver(parse_input, solve_part1, test_input)

# ╔═╡ 6f275cf8-4367-4398-aa21-8f1d9cd8c36e
solver(parse_input, solve_part1, real_input)

# ╔═╡ d726888a-71b7-4fc2-a42c-7ad346bd395d
@memoize function is_invalid_2(num::Int)::Bool
	num_as_str = string(num)
	size = length(num_as_str)
	for i in reverse(1:div(size, 2))
		if rem(size, i) > 0
			continue
		end

		if allequal(Iterators.partition(num_as_str, i))
			return true
		end
	end
	return false
end

# ╔═╡ f7bb2230-22ba-4b40-8af2-dd7226e0a9b2
function solve_part2(domains::Vector{UnitRange{Int64}})::Number
	sum = 0
	for domain in domains
		for number in domain
			if is_invalid_2(number)
				sum += number
			end
		end
	end
	return sum
end

# ╔═╡ 154fec09-f761-43aa-aaed-129d4d2f711e
solver(parse_input, solve_part2, test_input)

# ╔═╡ 4b783820-f72d-4ea8-bf8c-bbc2d556484c
solver(parse_input, solve_part2, real_input)

# ╔═╡ 00000000-0000-0000-0000-000000000001
PLUTO_PROJECT_TOML_CONTENTS = """
[deps]
Memoize = "c03570c3-d221-55d1-a50c-7939bbd78826"

[compat]
Memoize = "~0.4.4"
"""

# ╔═╡ 00000000-0000-0000-0000-000000000002
PLUTO_MANIFEST_TOML_CONTENTS = """
# This file is machine-generated - editing it directly is not advised

julia_version = "1.12.2"
manifest_format = "2.0"
project_hash = "5931d89eb294868bca5396fa27c70ed924814002"

[[deps.MacroTools]]
git-tree-sha1 = "1e0228a030642014fe5cfe68c2c0a818f9e3f522"
uuid = "1914dd2f-81c6-5fcd-8719-6d5c9610ff09"
version = "0.5.16"

[[deps.Memoize]]
deps = ["MacroTools"]
git-tree-sha1 = "2b1dfcba103de714d31c033b5dacc2e4a12c7caa"
uuid = "c03570c3-d221-55d1-a50c-7939bbd78826"
version = "0.4.4"
"""

# ╔═╡ Cell order:
# ╠═a02868a2-6df2-4d50-8cd0-5ca77c6d8cea
# ╠═d843733f-a453-44c4-b759-6e84dcb535e5
# ╟─74bb4e7b-c622-40bc-8ab7-b06068d5595c
# ╠═5367dbdf-ee9f-4360-a7d3-a8a5988d90fa
# ╠═8a79b53a-c8fe-11f0-9297-97044163d935
# ╠═ea4bd3be-5078-4e13-8373-6804189aae80
# ╠═649d2ff5-f85a-43ed-8993-81a51dda4ad1
# ╠═25c1db54-f713-416e-bee4-efc299a9bac6
# ╠═a16fd76f-a452-44ef-bb81-dd2440cb85fb
# ╠═18c6c9c5-508a-45bf-a67c-231b52ae47d6
# ╠═b24a080f-199a-4632-86a0-7af867567c2e
# ╠═9efaf158-c88a-47a4-a0b6-86c5227d5daa
# ╠═c34928c8-a273-4d63-8fa5-e3de89fff8d7
# ╠═6f275cf8-4367-4398-aa21-8f1d9cd8c36e
# ╠═d726888a-71b7-4fc2-a42c-7ad346bd395d
# ╠═f7bb2230-22ba-4b40-8af2-dd7226e0a9b2
# ╠═154fec09-f761-43aa-aaed-129d4d2f711e
# ╠═4b783820-f72d-4ea8-bf8c-bbc2d556484c
# ╟─00000000-0000-0000-0000-000000000001
# ╟─00000000-0000-0000-0000-000000000002
