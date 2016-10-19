import ConfigParser, csv, re

config = ConfigParser.ConfigParser()
config.read('py.conf')

config_option = "DEFAULT"

csv_dir = config.get(config_option, 'csv_dir')
print "csv_dir: %s\n" % (csv_dir)

# PLoS Biol. 5 (3), E17 (2007)"|17355172|"Publication Status: Available-Online prior to print" 
# Submitted (02-MAR-2007) J. Craig Venter Institute, 9704 Medical	Center Drive, Rockville, MD 20850, USA
# Patent: US 6812339-A 13152 02-NOV-2004;	Applera Corporation; Norwalk, CT
def parse_year():
    in_csvfile = "%s/REFERENCE.csv" % csv_dir
    out_csvfile = "%s/REFERENCE_with_year.csv" % csv_dir
    p1 = re.compile(r'\(((?:19|20)\d\d)\)')
    p2 = re.compile(r'Submitted \([0-3]\d-[A-Z][A-Z][A-Z]-((?:19|20)\d\d)\)')
    p_patent = re.compile(r'^Patent:\s.*?\s[0-3]\d-[A-Z][A-Z][A-Z]-((?:19|20)\d\d)')
    out = []
    with open(out_csvfile, 'w') as fout:
        with open(in_csvfile) as fin:
            for row in csv.reader(fin, delimiter='|'):
                arr = row
                journal = arr[-3]
                if p_patent.match(journal): # we don't want to process patent references
                    continue
                m = p1.search(journal) or p2.search(journal)
                year = ""
                if m:
                    year = m.groups()[0]
                out.append( "%-7s  %s" % (year, journal) )
                arr.append(year)
                fout.write("|".join(arr))
                fout.write("\n")
    
    #open("/tmp/a", 'w').write("\n".join(out))

if __name__ == "__main__":
    parse_year()
