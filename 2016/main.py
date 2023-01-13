from sys import argv
from importlib import import_module
from time import perf_counter

def new(day):
  with open("src/template.py", "r") as file :
    filedata = file.read()
    filedata = filedata.replace("INPUT_FILE = None", f"INPUT_FILE = \"input/2016/day{day}.txt\"")
    with open(f"src/day{day}.py", "w") as file:
      file.write(filedata)

def run(day):
  code = import_module(f"src.day{day}")
  t1 = perf_counter()
  code.solve()
  t2 = perf_counter()
  print(f"\n\nExecution time: {t2 - t1:0.4f} seconds")

if __name__ == "__main__":
  day = int(next(x for x in argv if x.isnumeric()))
  should_run = True
  if "-c" in argv:
    should_run = False
  
  if should_run:
    run(day)
  else:
    new(day)