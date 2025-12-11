### A Pluto.jl notebook ###
# v0.20.21

using Markdown
using InteractiveUtils

# ╔═╡ 3f07fcc3-f218-4ad7-b5e1-3d6113979ad7
using Graphs

# ╔═╡ 8915dea3-0553-429b-ae77-459234c2d906
using DataStructures

# ╔═╡ 3f5f003e-dfaa-435f-b588-a03e99258edc
using Memoization

# ╔═╡ a02868a2-6df2-4d50-8cd0-5ca77c6d8cea
YEAR = 2025

# ╔═╡ d843733f-a453-44c4-b759-6e84dcb535e5
DAY = 11

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
test_input = """aaa: you hhh
you: bbb ccc
bbb: ddd eee
ccc: ddd eee fff
ddd: ggg
eee: out
fff: out
ggg: out
hhh: ccc fff iii
iii: out"""

# ╔═╡ 649d2ff5-f85a-43ed-8993-81a51dda4ad1
# ╠═╡ show_logs = false
real_input = read(`cat $real_input_fp`, String)

# ╔═╡ b769d05b-be0d-4998-ad59-655bd8342424
struct Rack
	map::Dict{String, Int64}
	graph::SimpleDiGraph
end

# ╔═╡ 293165d3-a7eb-4a2c-b3fc-b91af0544c6b
function get_index_from_map(map::Dict{AbstractString, Int64}, key::AbstractString, or_else::Int64)::Tuple{Int64, Int64}
	if haskey(map, key)
		return (map[key], or_else)
	end
	map[key] = or_else
	return (map[key], or_else + 1)
end

# ╔═╡ 25c1db54-f713-416e-bee4-efc299a9bac6
function parse_input(input_str::String)::Rack
	lines = split(strip(input_str), "\n")
	graph = SimpleDiGraph(length(lines) + 1)
	last_index = 1
	map::Dict{AbstractString, Int64} = Dict()
	for line in lines
		source, nodes... = collect(eachmatch(r"([a-z]+)", line))
		source_idx, last_index = get_index_from_map(map, source[1], last_index)
		for node in nodes
			node_idx, last_index = get_index_from_map(map, node[1], last_index)
			add_edge!(graph, source_idx, node_idx)
		end
	end
	return Rack(
		map,
		graph
	)
end

# ╔═╡ 3d0c3498-2f80-4232-b323-d0eece688061
r = parse_input(test_input)

# ╔═╡ 21c393ff-a98f-45a4-adfb-b44ae29058f7
outdegree(r.graph)

# ╔═╡ b24a080f-199a-4632-86a0-7af867567c2e
function solver(input_parser, part_solver, input)
	parsed_input = input_parser(input)
	return string(part_solver(parsed_input))
end

# ╔═╡ 9efaf158-c88a-47a4-a0b6-86c5227d5daa
function solve_part1(rack::Rack)::Number
	source_idx = rack.map["you"]
	dest_idx = rack.map["out"]

    paths = collect(all_simple_paths(rack.graph, source_idx, dest_idx))

	return length(paths)
end

# ╔═╡ c34928c8-a273-4d63-8fa5-e3de89fff8d7
solver(parse_input, solve_part1, test_input)

# ╔═╡ 6f275cf8-4367-4398-aa21-8f1d9cd8c36e
solver(parse_input, solve_part1, real_input)

# ╔═╡ 8f6c702e-325e-45a1-8994-305f6a0a2e4b
test_input2 = """svr: aaa bbb
aaa: fft
fft: ccc
bbb: tty
tty: ccc
ccc: ddd eee
ddd: hub
hub: fff
eee: dac
dac: fff
fff: ggg hhh
ggg: out
hhh: out"""

# ╔═╡ 115acef9-6002-426c-8f51-3748e4a85148
neighbors(r.graph, 5)

# ╔═╡ ea266dd2-79e6-4f11-a379-d229ca4a3a5d
@memoize function calc_paths(graph::SimpleDiGraph, source::Int64, dest::Int64)::Number
	if source == dest
		return 1
	end
	adj = neighbors(graph, source)
	return sum(
		x -> calc_paths(graph, x, dest),
		adj,
		init = 0
	)
end

# ╔═╡ f7bb2230-22ba-4b40-8af2-dd7226e0a9b2
function solve_part2(rack::Rack)::Number
	source_idx = rack.map["svr"]
	dest_idx = rack.map["out"]
	fft_idx = rack.map["fft"]
	dac_idx = rack.map["dac"]

	return calc_paths(rack.graph, source_idx, fft_idx) * calc_paths(rack.graph, fft_idx, dac_idx) * calc_paths(rack.graph, dac_idx, dest_idx)
end

# ╔═╡ 154fec09-f761-43aa-aaed-129d4d2f711e
solver(parse_input, solve_part2, test_input2)

# ╔═╡ 4b783820-f72d-4ea8-bf8c-bbc2d556484c
solver(parse_input, solve_part2, real_input)

# ╔═╡ 00000000-0000-0000-0000-000000000001
PLUTO_PROJECT_TOML_CONTENTS = """
[deps]
DataStructures = "864edb3b-99cc-5e75-8d2d-829cb0a9cfe8"
Graphs = "86223c79-3864-5bf0-83f7-82e725a168b6"
Memoization = "6fafb56a-5788-4b4e-91ca-c0cea6611c73"

[compat]
DataStructures = "~0.19.3"
Graphs = "~1.13.2"
Memoization = "~0.2.2"
"""

# ╔═╡ 00000000-0000-0000-0000-000000000002
PLUTO_MANIFEST_TOML_CONTENTS = """
# This file is machine-generated - editing it directly is not advised

julia_version = "1.12.2"
manifest_format = "2.0"
project_hash = "85b411ec7b6263672d6fe838680cb06ef54b43bd"

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

[[deps.Memoization]]
deps = ["MacroTools"]
git-tree-sha1 = "7dbf904fa6c4447bd1f1d316886bfbe29feacf45"
uuid = "6fafb56a-5788-4b4e-91ca-c0cea6611c73"
version = "0.2.2"

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
# ╠═74bb4e7b-c622-40bc-8ab7-b06068d5595c
# ╠═5367dbdf-ee9f-4360-a7d3-a8a5988d90fa
# ╠═8a79b53a-c8fe-11f0-9297-97044163d935
# ╠═ea4bd3be-5078-4e13-8373-6804189aae80
# ╠═649d2ff5-f85a-43ed-8993-81a51dda4ad1
# ╠═3f07fcc3-f218-4ad7-b5e1-3d6113979ad7
# ╠═b769d05b-be0d-4998-ad59-655bd8342424
# ╠═293165d3-a7eb-4a2c-b3fc-b91af0544c6b
# ╠═25c1db54-f713-416e-bee4-efc299a9bac6
# ╠═3d0c3498-2f80-4232-b323-d0eece688061
# ╠═21c393ff-a98f-45a4-adfb-b44ae29058f7
# ╠═b24a080f-199a-4632-86a0-7af867567c2e
# ╠═8915dea3-0553-429b-ae77-459234c2d906
# ╠═9efaf158-c88a-47a4-a0b6-86c5227d5daa
# ╠═c34928c8-a273-4d63-8fa5-e3de89fff8d7
# ╠═6f275cf8-4367-4398-aa21-8f1d9cd8c36e
# ╠═8f6c702e-325e-45a1-8994-305f6a0a2e4b
# ╠═3f5f003e-dfaa-435f-b588-a03e99258edc
# ╠═115acef9-6002-426c-8f51-3748e4a85148
# ╠═ea266dd2-79e6-4f11-a379-d229ca4a3a5d
# ╠═f7bb2230-22ba-4b40-8af2-dd7226e0a9b2
# ╠═154fec09-f761-43aa-aaed-129d4d2f711e
# ╠═4b783820-f72d-4ea8-bf8c-bbc2d556484c
# ╟─00000000-0000-0000-0000-000000000001
# ╟─00000000-0000-0000-0000-000000000002
