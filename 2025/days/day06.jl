### A Pluto.jl notebook ###
# v0.20.21

using Markdown
using InteractiveUtils

# ╔═╡ a02868a2-6df2-4d50-8cd0-5ca77c6d8cea
YEAR = 2025

# ╔═╡ d843733f-a453-44c4-b759-6e84dcb535e5
DAY = 6

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
test_input = """123 328  51 64 
 45 64  387 23 
  6 98  215 314
*   +   *   +  """

# ╔═╡ 649d2ff5-f85a-43ed-8993-81a51dda4ad1
# ╠═╡ show_logs = false
real_input = read(`cat $real_input_fp`, String)

# ╔═╡ 25c1db54-f713-416e-bee4-efc299a9bac6
function parse_input(input_str::String)::Tuple{Matrix{Int64}, Vector{Char}}
	lines = collect(split(strip(input_str), "\n"))

	operators = map(x -> x[1], split(lines[end]))
	operands = stack(parse.(Int, split(line)) for line in lines[begin:end-1]; dims=1)
	return (operands, operators)
end

# ╔═╡ 183f8fdc-cf26-4c08-8558-0c0998e2df47
parse_input(test_input)

# ╔═╡ b24a080f-199a-4632-86a0-7af867567c2e
function solver(input_parser, part_solver, input)
	parsed_input = input_parser(input)
	return string(part_solver(parsed_input))
end

# ╔═╡ 2a431acd-d32c-4356-8848-d978dc860c00
function get_op(char::Char)
	eval(Meta.parse(string(char)))
end

# ╔═╡ 9efaf158-c88a-47a4-a0b6-86c5227d5daa
function solve_part1(input::Tuple{Matrix{Int64}, Vector{Char}})::Number
	ans = 0
	operands, operators = input
	for (idx, op) in enumerate(operators)
		ans += reduce(get_op(op), operands[:, idx])
	end
	return ans
end

# ╔═╡ c34928c8-a273-4d63-8fa5-e3de89fff8d7
solver(parse_input, solve_part1, test_input)

# ╔═╡ 6f275cf8-4367-4398-aa21-8f1d9cd8c36e
solver(parse_input, solve_part1, real_input)

# ╔═╡ 094b4c65-6175-4e1a-82d2-96dbefdb716a
function max_string_length(strings::Vector{String})::Number
	maximum(map(length, strings))
end

# ╔═╡ 75004f0e-5c5e-41a4-9f54-1628a85de8e3
function parse_input2(input_str::String)::Tuple{Vector{String}, String}
	lines = collect(split(strip(input_str, ['\n']), "\n"))

	operators = lines[end]
	operands = lines[begin:end-1]
	return (operands, operators)
end

# ╔═╡ c6bf8885-435d-4c3b-8c1f-ea6c32d607db
function calc_ans(op::Char, start::Number, size::Number, ops::Vector{String})
	numbers = fill("", size)
	for i in 1:size
		for line in ops
			ch = line[start + i - 1]
			if ch != ' '
				numbers[i] *= ch
			end
		end
	end
	reduce(get_op(op), parse.(Int64, numbers))
end

# ╔═╡ f7bb2230-22ba-4b40-8af2-dd7226e0a9b2
function solve_part2(input::Tuple{Vector{String}, String})::Number
	ans = 0
	operands, operators = input
	last_idx = 1
	last_operand = operators[1]
	for (i, val) in enumerate(operators[2:end])
		if val != ' '
			ans += calc_ans(last_operand, last_idx, i - last_idx, operands)
			last_idx = i + 1
			last_operand = val
		end			
	end
	final_idx = length(operators)
	ans += calc_ans(last_operand, last_idx, final_idx - last_idx + 1, operands)
	return ans
end

# ╔═╡ 154fec09-f761-43aa-aaed-129d4d2f711e
solver(parse_input2, solve_part2, test_input)

# ╔═╡ 4b783820-f72d-4ea8-bf8c-bbc2d556484c
solver(parse_input2, solve_part2, real_input)

# ╔═╡ Cell order:
# ╟─a02868a2-6df2-4d50-8cd0-5ca77c6d8cea
# ╟─d843733f-a453-44c4-b759-6e84dcb535e5
# ╟─74bb4e7b-c622-40bc-8ab7-b06068d5595c
# ╟─5367dbdf-ee9f-4360-a7d3-a8a5988d90fa
# ╟─8a79b53a-c8fe-11f0-9297-97044163d935
# ╠═ea4bd3be-5078-4e13-8373-6804189aae80
# ╟─649d2ff5-f85a-43ed-8993-81a51dda4ad1
# ╠═25c1db54-f713-416e-bee4-efc299a9bac6
# ╠═183f8fdc-cf26-4c08-8558-0c0998e2df47
# ╠═b24a080f-199a-4632-86a0-7af867567c2e
# ╠═2a431acd-d32c-4356-8848-d978dc860c00
# ╠═9efaf158-c88a-47a4-a0b6-86c5227d5daa
# ╠═c34928c8-a273-4d63-8fa5-e3de89fff8d7
# ╠═6f275cf8-4367-4398-aa21-8f1d9cd8c36e
# ╠═094b4c65-6175-4e1a-82d2-96dbefdb716a
# ╠═75004f0e-5c5e-41a4-9f54-1628a85de8e3
# ╠═c6bf8885-435d-4c3b-8c1f-ea6c32d607db
# ╠═f7bb2230-22ba-4b40-8af2-dd7226e0a9b2
# ╠═154fec09-f761-43aa-aaed-129d4d2f711e
# ╠═4b783820-f72d-4ea8-bf8c-bbc2d556484c
