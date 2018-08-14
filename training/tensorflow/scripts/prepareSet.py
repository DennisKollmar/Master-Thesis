import sys, getopt, os, csv, ntpath


def buildSet( baseFolder, listName):
    with open(baseFolder+"/lists/"+listName+".csv") as listFile:

        listFileReader = csv.reader(listFile, delimiter=',')
        setFolder = baseFolder+"/training-sets/"+listName
        if not os.path.exists(setFolder):
            os.makedirs(setFolder)

        for listFileRow in listFileReader:
            fileName = listFileRow[0]
            imageName = ntpath.basename(fileName)
            label = listFileRow[1]
            print 'Filename: '+fileName+' Imagename: '+imageName+' Label: '+label

            labelFolder = setFolder+'/'+label
            if not os.path.exists(labelFolder):
                os.makedirs(labelFolder)

            srcName = baseFolder+'/images/'+fileName
            if os.path.exists(srcName):
                os.symlink(srcName, labelFolder+'/'+imageName)
    return

def main(argv):
   listName = ''
   baseFolder = ''
   try:
      opts, args = getopt.getopt(argv,"hl:b:",["listName=","baseFolder="])
   except getopt.GetoptError:
      print 'prepareSet.py -l <listName> -b <baseFolder>'
      sys.exit(2)

   for opt, arg in opts:
       if opt == '-h':
           print 'prepareSet.py -l <listName> -b <baseFolder>'
           sys.exit()
       elif opt in ("-l", "--listName"):
           listName = arg
       elif opt in ("-b", "--baseFolder"):
           baseFolder = arg

   if listName == '' or baseFolder == '':
       print 'prepareSet.py -l <listName> -b <baseFolder>'
       sys.exit(2)

   print 'baseFolder ....: ', baseFolder
   print 'listName ......: ', listName

   buildSet(baseFolder, listName)

if __name__ == "__main__":
   main(sys.argv[1:])
