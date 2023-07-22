
import fileinput
import statistics

DATA = []

for line in fileinput.input():
    DATA.append(float(line[2:7]))

print('\n\nSamples : %d\nMean    : %.4fs\nSt Dev  : %.4fs' % (len(DATA), statistics.mean(DATA), statistics.stdev(DATA)))