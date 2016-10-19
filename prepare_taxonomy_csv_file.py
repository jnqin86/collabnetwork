debug = True
debug = False

import ConfigParser, csv, re

config = ConfigParser.ConfigParser()
config.read('py.conf')

config_option = "DEFAULT"

csv_dir = config.get(config_option, 'csv_dir')
taxonomy_dir = config.get(config_option, 'taxonomy_dir')
print "csv_dir: %s\ntaxonomy_dir: %s\n" % (csv_dir, taxonomy_dir)

def gen_taxonomy_csv_files(task):

    id_name = {}
    id_uniquename = {}
    def read_names():
        file_dmp_names = "%s/names.dmp" % taxonomy_dir
        #file_csv_names = "%s/TAXNAME.csv" % csv_dir
        cnt = 0
        maxL = 0
        for line in open(file_dmp_names, 'r').readlines():
            tax_id, name, unique_name, name_class, foo = [x.strip() for x in line.split('|')]
            if not name_class == "scientific name":
                continue
            #out.append("%s|%s" % (tax_id, name))
            id_name[tax_id] = name
            id_uniquename[tax_id] = unique_name
            if len(name) > maxL: maxL = len(name)
            cnt += 1
            if debug and cnt > 100: break
        print 'max length: ', maxL 
        print len(id_name.keys())
        print len(id_uniquename.keys())
        #return id_name

    def write_csv_for_nodes():
        #id_name = read_names()
        read_names()
        file_dmp_nodes = "%s/nodes.dmp" % taxonomy_dir
        file_csv_nodes = "%s/TAXNODE.csv" % csv_dir
        cnt = 0 
        out = []
        for line in open(file_dmp_nodes, 'r').readlines():
            row = line.replace("\t", "")[:-2].split("|")
            id, pid = row[0], row[1]
            name, unique_name, pname = id_name[id], id_uniquename[id], id_name[pid]
            new_row = [id, name, unique_name, pid, pname]
            new_row.extend(row[2:])
            out.append("|".join(new_row))
            cnt += 1
            if debug and cnt > 100: break
        open(file_csv_nodes, 'w').write("\n".join(out))

    def write_csv_for_divisions():
        file_dmp = "%s/division.dmp" % taxonomy_dir
        file_csv = "%s/TAXDIVISION.csv" % csv_dir
        out = []
        for line in open(file_dmp, 'r').readlines():
            row = line.replace("\t", "")[:-2]
            out.append(row)
        open(file_csv, 'w').write("\n".join(out))

    if task == "create_TAXNODE_csv":
        write_csv_for_nodes()
    elif task == "create_TAXDIVISION_csv":
        write_csv_for_divisions()

def generate_new_organism_csv():
    NUM_TOP_BOTTOM_ANCIENTS = 3
    id_parentId, id_name, id_rank, name_id, name_shared_uniquenames = {}, {}, {}, {}, {}

    def read_taxonomy_csv():
        csvfile = open("%s/TAXNODE.csv" % csv_dir)
        for row in csv.reader(csvfile, delimiter='|'):
        #2|Bacteria|131567|cellular organisms|superkingdom||0|0|11|0|0|0|0|0|
            node_id, node_name, unique_name, parent_id, parent_name, rank = row[:6]
            id_name[node_id] = node_name
            name_id[node_name] = node_id
            if node_name not in name_shared_uniquenames: name_shared_uniquenames[node_name] = []
            name_shared_uniquenames[node_name].append(unique_name)
            id_parentId[node_id] = parent_id
            id_rank[node_id] = rank

    def gen_new_organism_csv():
        read_taxonomy_csv()
        csvfile = open("%s/ORGANISM.csv" % csv_dir)
        out = []
        out_org_tax = []
        already_processed = {}
        weird_species_name_cnt = {}
        for row in csv.reader(csvfile, delimiter='|', quotechar='"'):
        #139189705|"marine metagenome    unclassified sequences; metagenomes; ecological metagenomes."
            organism_id, content = row
            species, seq_order = content.split('\t',1)
            try:
                if species in name_shared_uniquenames and len(name_shared_uniquenames[species]) > 1:
                    print "shared weird name:", row, name_shared_uniquenames[species]
                    weird_species_name_cnt[species] = weird_species_name_cnt.get(species, 0) +1
                    continue
            except:
                print "tab key wrong"
                print content
                return
            #print "%-30s %s" % (species, seq_order)
            species_id = name_id[species] if species in name_id else ''
            species_rank = id_rank[species_id] if species_id in id_rank else ''
            items = [species_id, species, species_rank]
            seq_order = re.sub('\s+', ' ', seq_order[:-1])
            parents = seq_order.split('; ')
            top_parent_size, bottom_parent_size = 3, 3


            for direction in (1, -1):
                if direction == -1:
                    parents.reverse()
                tops = [''] * NUM_TOP_BOTTOM_ANCIENTS * top_parent_size
                for i in range(min(NUM_TOP_BOTTOM_ANCIENTS, len(parents))):
                    tops[i*top_parent_size+1] = parents[i]
                    if parents[i] in name_id:
                        pid = name_id[parents[i]]
                        tops[i*top_parent_size] = pid
                        tops[i*top_parent_size+2] = id_rank[pid]
                items += tops

            items.append(content)
            if species_id != '' and not species_id in already_processed:
                already_processed[species_id] = 1
                out.append("|".join(items))
            out_org_tax.append("%s|%s" % (organism_id, species_id))

        print weird_species_name_cnt

        out_csvfile = "%s/ORGANISM_NEW.csv" % csv_dir
        open(out_csvfile, 'w').write("\n".join(out))
        out_orgtax_csvfile = "%s/ORGANISM_TAX.csv" % csv_dir
        open(out_orgtax_csvfile, 'w').write("\n".join(out_org_tax))
    
    def list_hierarchy(id):
        read_taxonomy_csv()
        pid = id_parentId[id]
        level = 0
        while True:
            level += 1
            print "%3s %10s %40s    %20s" % (level, id, id_name[id], id_rank[id])
            if id == '1':
                break
            id = pid
            pid = id_parentId[id]

    #list_hierarchy('9913')
    #list_hierarchy('408172')
    gen_new_organism_csv()

def update_annotation_csv_with_tax_id_column():
#gi BIGINT, version INT, keywordsId INT, sourceId INT, organismId INT, commentId INT, dblinkId INT, locus VARCHAR(500),
        organismId_taxId = {}
        def read_organism_tax_map():
            orgtax_csvfile = "%s/ORGANISM_TAX.csv" % csv_dir
            for row in open(orgtax_csvfile).readlines():
                organismId, tax_id = row.strip().split("|")
                organismId_taxId[organismId] = tax_id

        read_organism_tax_map()

        csvfile = open("%s/ANNOTATION.csv" % csv_dir)
        fout = open("%s/ANNOTATION_tax_id.csv" % csv_dir, 'w')
        for line in csvfile:
            organismId = line.split('|')[4]
            tax_id = organismId_taxId[organismId]
            #print organismId, tax_id
            fout.write("%s|%s\n" % (line.strip(), organismId_taxId[organismId]))


if __name__ == "__main__":
    #update_annotation_csv_with_tax_id_column()
    #gen_taxonomy_csv_files(task='create_TAXDIVISION_csv')
    #gen_taxonomy_csv_files(task='create_TAXNODE_csv')
    generate_new_organism_csv()
