from hashlib import md5

target = "000000"
def solve(key, length):
	num = 0
	while not md5((key + str(num)).encode('utf-8')).hexdigest()[:length] == target[:length] : 
		num += 1
	return num

def q1(key):
  return solve(key, 5)

def q2(key):
  return solve(key, 6)

print(q1("ckczppom"))
print(q2("ckczppom"))