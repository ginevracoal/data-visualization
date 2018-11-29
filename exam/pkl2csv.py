import pickle
import pandas
import sys


if(len(sys.argv)<2):
    print("give me the filename")
    sys.exit(1)

name=sys.argv[1]
f=open(name,"rb")
p=pickle.load(f)
d=pandas.DataFrame(p)
d.to_csv(name+".csv")