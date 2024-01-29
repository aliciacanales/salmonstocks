used_b = 0
i = 1
budget = 1000000 * .1

n = 10
bpassage = array(50000, n)

while (used_b < budget) {
  used_b = bpassage[i] + used_b

  i = i + 1
  print(i)
}



