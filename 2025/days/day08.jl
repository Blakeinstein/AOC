### A Pluto.jl notebook ###
# v0.20.21

using Markdown
using InteractiveUtils

# ╔═╡ 96acbe1f-588d-4c0e-bd64-aaeebff18802
begin 
	using StaticArrays
	using Distances
	using Graphs
end

# ╔═╡ a02868a2-6df2-4d50-8cd0-5ca77c6d8cea
YEAR = 2025

# ╔═╡ d843733f-a453-44c4-b759-6e84dcb535e5
DAY = 8

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
test_input = """162,817,812
57,618,57
906,360,560
592,479,940
352,342,300
466,668,158
542,29,236
431,825,988
739,650,466
52,470,668
216,146,977
819,987,18
117,168,530
805,96,715
346,949,466
970,615,88
941,993,340
862,61,35
984,92,344
425,690,689"""

# ╔═╡ 649d2ff5-f85a-43ed-8993-81a51dda4ad1
# ╠═╡ show_logs = false
real_input = read(`cat $real_input_fp`, String)

# ╔═╡ 25c1db54-f713-416e-bee4-efc299a9bac6
function parse_input(input_str::String)::Vector{SVector{3, Int64}}
	map(
		line -> begin
			x,y,z = parse.(Int64, split(line, ",", limit=3))
			SVector(x,y,z)
		end,
		split(strip(input_str), "\n")
	)
end

# ╔═╡ 3da51459-53e5-445c-bf11-d203dff23d4f
parse_input(test_input)

# ╔═╡ b24a080f-199a-4632-86a0-7af867567c2e
function solver(input_parser, part_solver, input, is_test=false)
	parsed_input = input_parser(input)
	return string(part_solver(parsed_input, is_test))
end

# ╔═╡ fcd60571-b50b-492e-8ec4-51351a4031ae
function find_set!(parents::Dict, i)
    if parents[i] != i
        parents[i] = find_set!(parents, parents[i]) # Path compression
    end
    return parents[i]
end

# ╔═╡ 11c5d6dd-94ca-4701-91b1-22b4d3e38ce2
function union_sets!(parents::Dict, i, j)
    root_i = find_set!(parents, i)
    root_j = find_set!(parents, j)
    if root_i != root_j
        parents[root_i] = root_j # Merge sets
    end
end

# ╔═╡ 9efaf158-c88a-47a4-a0b6-86c5227d5daa
function solve_part1(input::Vector{SVector{3, Int64}}, is_test::Bool=false)::Number
	conn_count = is_test ? 10 : 1000
	count = length(input)
	distances = []
	for i in 1:count, j in (i+1):count
		push!(distances, (euclidean(input[i], input[j]), i, j))
	end
	sort!(distances)

	
	edges = []
    for (_, u, v) in distances[begin:conn_count]
        push!(edges, (u, v))
    end

	graph = SimpleDiGraph(Edge.(edges))

    return prod(
		first(
			sort(
				map(length, weakly_connected_components(graph))
			, rev=true),
			3
		)
	)
	
end

# ╔═╡ c34928c8-a273-4d63-8fa5-e3de89fff8d7
solver(parse_input, solve_part1, test_input, true)

# ╔═╡ 6f275cf8-4367-4398-aa21-8f1d9cd8c36e
solver(parse_input, solve_part1, real_input)

# ╔═╡ f7bb2230-22ba-4b40-8af2-dd7226e0a9b2
function solve_part2(input::Vector{SVector{3, Int64}}, is_test::Bool = false)::Number
	count = length(input)
	distances = []
	for i in 1:count, j in (i+1):count
		push!(distances, (euclidean(input[i], input[j]), i, j))
	end
	sort!(distances)

	
	graph = SimpleDiGraph(count)
    for (_, u, v) in distances
       add_edge!(graph, u, v)
	   if length(weakly_connected_components(graph)) == 1
		   return input[u][1] * input[v][1]
	   end
    end


    return 0
end

# ╔═╡ 154fec09-f761-43aa-aaed-129d4d2f711e
solver(parse_input, solve_part2, test_input, true)

# ╔═╡ 4b783820-f72d-4ea8-bf8c-bbc2d556484c
solver(parse_input, solve_part2, real_input)

# ╔═╡ 00000000-0000-0000-0000-000000000001
PLUTO_PROJECT_TOML_CONTENTS = """
[deps]
Distances = "b4f34e82-e78d-54a5-968a-f98e89d6e8f7"
Graphs = "86223c79-3864-5bf0-83f7-82e725a168b6"
StaticArrays = "90137ffa-7385-5640-81b9-e52037218182"

[compat]
Distances = "~0.10.12"
Graphs = "~1.13.2"
StaticArrays = "~1.9.15"
"""

# ╔═╡ 00000000-0000-0000-0000-000000000002
PLUTO_MANIFEST_TOML_CONTENTS = """
# This file is machine-generated - editing it directly is not advised

julia_version = "1.12.2"
manifest_format = "2.0"
project_hash = "2cf8e934797cf1dcd1543ddc863c512b8ebb3b97"

[[deps.ArnoldiMethod]]
deps = ["LinearAlgebra", "Random", "StaticArrays"]
git-tree-sha1 = "d57bd3762d308bded22c3b82d033bff85f6195c6"
uuid = "ec485272-7323-5ecc-a04f-4719b315124d"
version = "0.4.0"

[[deps.Artifacts]]
uuid = "56f22d72-fd6d-98f1-02f0-08ddc0907c33"
version = "1.11.0"

[[deps.Base64]]
uuid = "2a0f44e3-6c83-55bd-87e4-b1978d98bd5f"
version = "1.11.0"

[[deps.CompilerSupportLibraries_jll]]
deps = ["Artifacts", "Libdl"]
uuid = "e66e0078-7015-5450-92f7-15fbd957f2ae"
version = "1.3.0+1"

[[deps.DataStructures]]
deps = ["OrderedCollections"]
git-tree-sha1 = "e357641bb3e0638d353c4b29ea0e40ea644066a6"
uuid = "864edb3b-99cc-5e75-8d2d-829cb0a9cfe8"
version = "0.19.3"

[[deps.Dates]]
deps = ["Printf"]
uuid = "ade2ca70-3891-5945-98fb-dc099432e06a"
version = "1.11.0"

[[deps.Distances]]
deps = ["LinearAlgebra", "Statistics", "StatsAPI"]
git-tree-sha1 = "c7e3a542b999843086e2f29dac96a618c105be1d"
uuid = "b4f34e82-e78d-54a5-968a-f98e89d6e8f7"
version = "0.10.12"

    [deps.Distances.extensions]
    DistancesChainRulesCoreExt = "ChainRulesCore"
    DistancesSparseArraysExt = "SparseArrays"

    [deps.Distances.weakdeps]
    ChainRulesCore = "d360d2e6-b24c-11e9-a2a3-2a2ae2dbcce4"
    SparseArrays = "2f01184e-e22b-5df5-ae63-d93ebab69eaf"

[[deps.Graphs]]
deps = ["ArnoldiMethod", "DataStructures", "Inflate", "LinearAlgebra", "Random", "SimpleTraits", "SparseArrays", "Statistics"]
git-tree-sha1 = "d80e8b7220b884117938b2d219487eea06f9e42e"
uuid = "86223c79-3864-5bf0-83f7-82e725a168b6"
version = "1.13.2"

    [deps.Graphs.extensions]
    GraphsSharedArraysExt = "SharedArrays"

    [deps.Graphs.weakdeps]
    Distributed = "8ba89e20-285c-5b6f-9357-94700520ee1b"
    SharedArrays = "1a1011a3-84de-559e-8e89-a11a2f7dc383"

[[deps.Inflate]]
git-tree-sha1 = "d1b1b796e47d94588b3757fe84fbf65a5ec4a80d"
uuid = "d25df0c9-e2be-5dd7-82c8-3ad0b3e990b9"
version = "0.1.5"

[[deps.InteractiveUtils]]
deps = ["Markdown"]
uuid = "b77e0a4c-d291-57a0-90e8-8db25a27a240"
version = "1.11.0"

[[deps.JuliaSyntaxHighlighting]]
deps = ["StyledStrings"]
uuid = "ac6e5ff7-fb65-4e79-a425-ec3bc9c03011"
version = "1.12.0"

[[deps.Libdl]]
uuid = "8f399da3-3557-5675-b5ff-fb832c97cbdb"
version = "1.11.0"

[[deps.LinearAlgebra]]
deps = ["Libdl", "OpenBLAS_jll", "libblastrampoline_jll"]
uuid = "37e2e46d-f89d-539d-b4ee-838fcccc9c8e"
version = "1.12.0"

[[deps.MacroTools]]
git-tree-sha1 = "1e0228a030642014fe5cfe68c2c0a818f9e3f522"
uuid = "1914dd2f-81c6-5fcd-8719-6d5c9610ff09"
version = "0.5.16"

[[deps.Markdown]]
deps = ["Base64", "JuliaSyntaxHighlighting", "StyledStrings"]
uuid = "d6f4376e-aef5-505a-96c1-9c027394607a"
version = "1.11.0"

[[deps.OpenBLAS_jll]]
deps = ["Artifacts", "CompilerSupportLibraries_jll", "Libdl"]
uuid = "4536629a-c528-5b80-bd46-f80d51c5b363"
version = "0.3.29+0"

[[deps.OrderedCollections]]
git-tree-sha1 = "05868e21324cede2207c6f0f466b4bfef6d5e7ee"
uuid = "bac558e1-5e72-5ebc-8fee-abe8a469f55d"
version = "1.8.1"

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

[[deps.Serialization]]
uuid = "9e88b42a-f829-5b0c-bbe9-9e923198166b"
version = "1.11.0"

[[deps.SimpleTraits]]
deps = ["InteractiveUtils", "MacroTools"]
git-tree-sha1 = "be8eeac05ec97d379347584fa9fe2f5f76795bcb"
uuid = "699a6c99-e7fa-54fc-8d76-47d257e15c1d"
version = "0.9.5"

[[deps.SparseArrays]]
deps = ["Libdl", "LinearAlgebra", "Random", "Serialization", "SuiteSparse_jll"]
uuid = "2f01184e-e22b-5df5-ae63-d93ebab69eaf"
version = "1.12.0"

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

[[deps.Statistics]]
deps = ["LinearAlgebra"]
git-tree-sha1 = "ae3bb1eb3bba077cd276bc5cfc337cc65c3075c0"
uuid = "10745b16-79ce-11e8-11f9-7d13ad32a3b2"
version = "1.11.1"
weakdeps = ["SparseArrays"]

    [deps.Statistics.extensions]
    SparseArraysExt = ["SparseArrays"]

[[deps.StatsAPI]]
deps = ["LinearAlgebra"]
git-tree-sha1 = "178ed29fd5b2a2cfc3bd31c13375ae925623ff36"
uuid = "82ae8749-77ed-4fe6-ae5f-f523153014b0"
version = "1.8.0"

[[deps.StyledStrings]]
uuid = "f489334b-da3d-4c2e-b8f0-e476e12c162b"
version = "1.11.0"

[[deps.SuiteSparse_jll]]
deps = ["Artifacts", "Libdl", "libblastrampoline_jll"]
uuid = "bea87d4a-7f5b-5778-9afe-8cc45184846c"
version = "7.8.3+2"

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
# ╠═5367dbdf-ee9f-4360-a7d3-a8a5988d90fa
# ╠═8a79b53a-c8fe-11f0-9297-97044163d935
# ╠═ea4bd3be-5078-4e13-8373-6804189aae80
# ╠═649d2ff5-f85a-43ed-8993-81a51dda4ad1
# ╠═96acbe1f-588d-4c0e-bd64-aaeebff18802
# ╠═25c1db54-f713-416e-bee4-efc299a9bac6
# ╠═3da51459-53e5-445c-bf11-d203dff23d4f
# ╠═b24a080f-199a-4632-86a0-7af867567c2e
# ╠═fcd60571-b50b-492e-8ec4-51351a4031ae
# ╠═11c5d6dd-94ca-4701-91b1-22b4d3e38ce2
# ╠═9efaf158-c88a-47a4-a0b6-86c5227d5daa
# ╠═c34928c8-a273-4d63-8fa5-e3de89fff8d7
# ╠═6f275cf8-4367-4398-aa21-8f1d9cd8c36e
# ╠═f7bb2230-22ba-4b40-8af2-dd7226e0a9b2
# ╠═154fec09-f761-43aa-aaed-129d4d2f711e
# ╠═4b783820-f72d-4ea8-bf8c-bbc2d556484c
# ╟─00000000-0000-0000-0000-000000000001
# ╟─00000000-0000-0000-0000-000000000002
