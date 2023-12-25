import networkx as nx

NX = nx.Graph()

with open("input/2023/25.txt") as input_file:
    input_lines = input_file.read().splitlines()

nodes = []
for x in input_lines:
  node = x.split(":")[0]
  nodes.append(node)
  cons = x.split(": ")[1].split(" ")
  for con in cons:
    NX.add_edge(node, con, capacity=1.0)

for i in range(1,len(input_lines)):
  cut_value, partition = nx.minimum_cut(NX,nodes[0],nodes[i])
  if cut_value == 3:
    print("Part 1:", len(partition[0])*len(partition[1]))
    break