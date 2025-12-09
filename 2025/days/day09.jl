### A Pluto.jl notebook ###
# v0.20.21

using Markdown
using InteractiveUtils

# ╔═╡ 4e48d665-9732-4d03-bad3-ed405c79955e
begin
	using StaticArrays
end

# ╔═╡ a02868a2-6df2-4d50-8cd0-5ca77c6d8cea
YEAR = 2025

# ╔═╡ d843733f-a453-44c4-b759-6e84dcb535e5
DAY = 9

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
test_input = """7,1
11,1
11,7
9,7
9,5
2,5
2,3
7,3"""

# ╔═╡ 649d2ff5-f85a-43ed-8993-81a51dda4ad1
# ╠═╡ show_logs = false
real_input = read(`cat $real_input_fp`, String)

# ╔═╡ 25c1db54-f713-416e-bee4-efc299a9bac6
function parse_input(input_str::String)::Vector{SVector{2, Int64}}
	return map(
		x -> SVector(
			parse.(Int64, split(x, ","))...
		),
		split(strip(input_str), "\n")
	)
end

# ╔═╡ f8d3c5eb-514c-4300-81ec-d09ec70c67a7
parse_input(test_input)

# ╔═╡ b24a080f-199a-4632-86a0-7af867567c2e
function solver(input_parser, part_solver, input)
	parsed_input = input_parser(input)
	return string(part_solver(parsed_input))
end

# ╔═╡ d95b117a-9322-44d2-a3a7-4232ac0ee115
function area(v1::SVector{2, Int64}, v2::SVector{2, Int64})::Number
	(
		(abs(v1[2] - v2[2]) + 1) * (abs(v1[1] - v2[1]) + 1)
	)
end

# ╔═╡ 9efaf158-c88a-47a4-a0b6-86c5227d5daa
function solve_part1(input::Vector{SVector{2, Int64}})::Number
	len = length(input)
	curr_max = 0
	for i in 1:len, j in (i+1):len
		curr_max = max(curr_max, area(input[i], input[j]))
	end
	return curr_max
end

# ╔═╡ c34928c8-a273-4d63-8fa5-e3de89fff8d7
solver(parse_input, solve_part1, test_input)

# ╔═╡ 6f275cf8-4367-4398-aa21-8f1d9cd8c36e
solver(parse_input, solve_part1, real_input)

# ╔═╡ 3f5d2c00-a442-49fa-8fea-0d59b88d69fa
function is_valid_rect(v1::SVector{2, Int64}, v2::SVector{2, Int64}, polygon::Vector{SVector{2, Int64}})::Bool
    x1, x2 = minmax(v1[1], v2[1])
    y1, y2 = minmax(v1[2], v2[2])

    n = length(polygon)

    for i in 1:n
        p_curr = polygon[i]
        p_next = polygon[mod1(i+1, n)]
        
        if p_curr[1] == p_next[1]
            edge_x = p_curr[1]
            if x1 < edge_x < x2
                min_y, max_y = minmax(p_curr[2], p_next[2])
                if max(y1, min_y) < min(y2, max_y)
                    return false
                end
            end
        else 
            edge_y = p_curr[2]
            if y1 < edge_y < y2
                min_x, max_x = minmax(p_curr[1], p_next[1])
                if max(x1, min_x) < min(x2, max_x)
                    return false
                end
            end
        end
    end

    return true
end

# ╔═╡ f7bb2230-22ba-4b40-8af2-dd7226e0a9b2
function solve_part2(input::Vector{SVector{2, Int64}})::Number
	len = length(input)
	curr_max = 0
	for i in 1:len, j in (i+1):len
		if is_valid_rect(input[i], input[j], input)
			curr_max = max(curr_max, area(input[i], input[j]))
		end
	end
	return curr_max
end

# ╔═╡ 154fec09-f761-43aa-aaed-129d4d2f711e
solver(parse_input, solve_part2, test_input)

# ╔═╡ 4b783820-f72d-4ea8-bf8c-bbc2d556484c
solver(parse_input, solve_part2, real_input)

# ╔═╡ 00000000-0000-0000-0000-000000000001
PLUTO_PROJECT_TOML_CONTENTS = """
[deps]
StaticArrays = "90137ffa-7385-5640-81b9-e52037218182"

[compat]
StaticArrays = "~1.9.15"
"""

# ╔═╡ 00000000-0000-0000-0000-000000000002
PLUTO_MANIFEST_TOML_CONTENTS = """
# This file is machine-generated - editing it directly is not advised

julia_version = "1.12.2"
manifest_format = "2.0"
project_hash = "068db752413f7e5b4135b9586a4ac6c5dfb95bb7"

[[deps.Artifacts]]
uuid = "56f22d72-fd6d-98f1-02f0-08ddc0907c33"
version = "1.11.0"

[[deps.CompilerSupportLibraries_jll]]
deps = ["Artifacts", "Libdl"]
uuid = "e66e0078-7015-5450-92f7-15fbd957f2ae"
version = "1.3.0+1"

[[deps.Dates]]
deps = ["Printf"]
uuid = "ade2ca70-3891-5945-98fb-dc099432e06a"
version = "1.11.0"

[[deps.Libdl]]
uuid = "8f399da3-3557-5675-b5ff-fb832c97cbdb"
version = "1.11.0"

[[deps.LinearAlgebra]]
deps = ["Libdl", "OpenBLAS_jll", "libblastrampoline_jll"]
uuid = "37e2e46d-f89d-539d-b4ee-838fcccc9c8e"
version = "1.12.0"

[[deps.OpenBLAS_jll]]
deps = ["Artifacts", "CompilerSupportLibraries_jll", "Libdl"]
uuid = "4536629a-c528-5b80-bd46-f80d51c5b363"
version = "0.3.29+0"

[[deps.PrecompileTools]]
deps = ["Preferences"]
git-tree-sha1 = "07a921781cab75691315adc645096ed5e370cb77"
uuid = "aea7be01-6a6a-4083-8856-8a6e6704d82a"
version = "1.3.3"

[[deps.Preferences]]
deps = ["TOML"]
git-tree-sha1 = "0f27480397253da18fe2c12a4ba4eb9eb208bf3d"
uuid = "21216c6a-2e73-6563-6e65-726566657250"
version = "1.5.0"

[[deps.Printf]]
deps = ["Unicode"]
uuid = "de0858da-6303-5e67-8744-51eddeeeb8d7"
version = "1.11.0"

[[deps.Random]]
deps = ["SHA"]
uuid = "9a3f8284-a2c9-5f02-9a11-845980a1fd5c"
version = "1.11.0"

[[deps.SHA]]
uuid = "ea8e919c-243c-51af-8825-aaa63cd721ce"
version = "0.7.0"

[[deps.StaticArrays]]
deps = ["LinearAlgebra", "PrecompileTools", "Random", "StaticArraysCore"]
git-tree-sha1 = "b8693004b385c842357406e3af647701fe783f98"
uuid = "90137ffa-7385-5640-81b9-e52037218182"
version = "1.9.15"

    [deps.StaticArrays.extensions]
    StaticArraysChainRulesCoreExt = "ChainRulesCore"
    StaticArraysStatisticsExt = "Statistics"

    [deps.StaticArrays.weakdeps]
    ChainRulesCore = "d360d2e6-b24c-11e9-a2a3-2a2ae2dbcce4"
    Statistics = "10745b16-79ce-11e8-11f9-7d13ad32a3b2"

[[deps.StaticArraysCore]]
git-tree-sha1 = "6ab403037779dae8c514bad259f32a447262455a"
uuid = "1e83bf80-4336-4d27-bf5d-d5a4f845583c"
version = "1.4.4"

[[deps.TOML]]
deps = ["Dates"]
uuid = "fa267f1f-6049-4f14-aa54-33bafae1ed76"
version = "1.0.3"

[[deps.Unicode]]
uuid = "4ec0a83e-493e-50e2-b9ac-8f72acf5a8f5"
version = "1.11.0"

[[deps.libblastrampoline_jll]]
deps = ["Artifacts", "Libdl"]
uuid = "8e850b90-86db-534c-a0d3-1478176c7d93"
version = "5.15.0+0"
"""

# ╔═╡ Cell order:
# ╟─a02868a2-6df2-4d50-8cd0-5ca77c6d8cea
# ╟─d843733f-a453-44c4-b759-6e84dcb535e5
# ╟─74bb4e7b-c622-40bc-8ab7-b06068d5595c
# ╟─5367dbdf-ee9f-4360-a7d3-a8a5988d90fa
# ╟─8a79b53a-c8fe-11f0-9297-97044163d935
# ╠═ea4bd3be-5078-4e13-8373-6804189aae80
# ╠═649d2ff5-f85a-43ed-8993-81a51dda4ad1
# ╠═4e48d665-9732-4d03-bad3-ed405c79955e
# ╠═25c1db54-f713-416e-bee4-efc299a9bac6
# ╠═f8d3c5eb-514c-4300-81ec-d09ec70c67a7
# ╠═b24a080f-199a-4632-86a0-7af867567c2e
# ╠═d95b117a-9322-44d2-a3a7-4232ac0ee115
# ╠═9efaf158-c88a-47a4-a0b6-86c5227d5daa
# ╠═c34928c8-a273-4d63-8fa5-e3de89fff8d7
# ╠═6f275cf8-4367-4398-aa21-8f1d9cd8c36e
# ╠═3f5d2c00-a442-49fa-8fea-0d59b88d69fa
# ╠═f7bb2230-22ba-4b40-8af2-dd7226e0a9b2
# ╠═154fec09-f761-43aa-aaed-129d4d2f711e
# ╠═4b783820-f72d-4ea8-bf8c-bbc2d556484c
# ╟─00000000-0000-0000-0000-000000000001
# ╟─00000000-0000-0000-0000-000000000002
