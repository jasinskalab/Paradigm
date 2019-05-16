import csv
from time import sleep
import os
from os import listdir
from os.path import isfile, join
   
# Betty Zhang, BOLD Lab, Spring 2019

# a function that reads the input data until hitting the empty line
def read_until_emptyline(infile_name, outputfilepath, pos):
    infile = open(infile_name) #subject to change    
    start = infile.readlines()[pos:] #skip the first few lines
    if len(start)<=1:
        infile.close()
        return [False, pos]
    output = open(outputfilepath, 'w')
    #print(str(len(start))+' lines found.')
    infile.close()

    readCSV = csv.reader(start, delimiter=',')

    for i, l in enumerate(readCSV):
        if not l:
            #print('Break after '+str(i)+' lines.')
            break
        for element in l:
            output.write(element+',') #comma-separated elements within line l
        output.write('\n') #at the end of each line l, add a line break
        pos+=1 #increment the line number
    output.close()
    pos=pos+1 #to skip the empty line in between
    #sleep(0.5) #for debugging, prevent runaway file creation
    return [True, pos]

#the main: can use a loop to create names for the output file
def main():
    num=1
    moreFile = True
    pos=2
    choice = int(input("type 1 if you want to analyze the entire folder; 2 for a single file: "))

    if choice==2:
        infile_name = input("type in the repository for the file you want to analyze: ")
        while moreFile:
            outfile_name = os.getcwd() + '/' + os.path.basename(infile_name)[0:-4] + '-table' + str(num) + '.csv'
            moreFile = read_until_emptyline(infile_name, outfile_name, pos)
            num+=1
    else:
        mypath = input("type in the repository for the folder you want to analyze (or just hit Enter for current directory): ")
        if mypath == "":
                mypath = os.getcwd()
        print("Checking folder at " + mypath)
        for root, subdirs, files in os.walk(mypath):
            for subdir in subdirs:
                print('\t- subdirectory ' + subdir)
                filepath=os.path.join(root, subdir)
                onlyfiles = [f for f in listdir(filepath) if isfile(join(filepath, f))]   
                for file in onlyfiles:
                    infile_name = file
                    
                    if moreFile==False:
                        moreFile=True
                        pos=2
                        num=1
                    while moreFile:
                        outfile_name = os.getcwd() + '/' + os.path.basename(infile_name)[0:-4] + '-table' + str(num) + '.csv'
                        print(outfile_name)
                        [moreFile, pos] = read_until_emptyline(filepath+'/'+infile_name, outfile_name, pos)
                        num+=1

main()           
                
