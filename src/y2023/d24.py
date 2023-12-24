import re
import itertools
import z3
from typing import NamedTuple

## Switched to Python for today as I couldn't find any good clojure z3 wrappers
## and installing z3 for java seemed like too much work XD
## Run => pip install z3-solver to install z3

Hail = NamedTuple("Hail", px=int, py=int, pz=int, vx=int, vy=int, vz=int, slope=float, intercept=float)

with open("input/2023/24.txt") as input_file:
    input_lines = input_file.readlines()
    input_lines = [line.strip('\n') for line in input_lines]

test_area = (200000000000000, 400000000000000)

hailstones = []
for line in input_lines:
  px, py, pz, vx, vy, vz = [int(s) for s in re.findall(r'-?\d+', line)]
  slope = vy / vx
  intercept = py - slope * px
  hailstones.append(Hail(px, py, pz, vx, vy, vz, slope, intercept))

counter = 0
for h1, h2 in itertools.combinations(hailstones, 2):
    h1: Hail
    h2: Hail
    if h1.slope == h2.slope:
        continue
    ix = (h2.intercept - h1.intercept) / (h1.slope - h2.slope)
    iy = h1.slope * ix + h1.intercept
    t1 = (ix - h1.px) / h1.vx
    t2 = (ix - h2.px) / h2.vx
    if t1 < 0 or t2 < 0:
        continue
    if test_area[0] <= ix <= test_area[1] and test_area[0] <= iy <= test_area[1]:
        counter += 1
print(f"Part 1: {counter}")

pxr, pyr, pzr, vxr, vyr, vzr = z3.Reals("pxr pyr pzr vxr vyr vzr")
solver = z3.Solver()
for k, h in enumerate(hailstones[:3]):
  tK = z3.Real(f"t{k}")
  solver.add(tK > 0)
  solver.add(pxr + tK * vxr == h.px + tK * h.vx)
  solver.add(pyr + tK * vyr == h.py + tK * h.vy)
  solver.add(pzr + tK * vzr == h.pz + tK * h.vz)
solver.check()
total = sum(solver.model()[v].as_long() for v in [pxr, pyr, pzr])
print(f"Part 2: {total}")