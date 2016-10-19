<?php
include './helper.php';
$mysqli = connect_database();

$task_create_all_tables = 1; 
$task_create_taxonomy_tables = 11; 
$task_load_csv_files_to_database = 2; 
$task_add_indexes_to_tables = 3; 
$task_add_locus_detailed_fields_to_Annotation = 5;
$task_split_patent_Reference = 14;
$task_add_taxid_to_Annotation = 21;
$task_update_taxid_to_Annotation = 22;

$task = $task_add_locus_detailed_fields_to_Annotation;
$task = $task_add_indexes_to_tables;
#$task = $task_add_taxid_to_Annotation;    // too slow. Instead, add ANNOTATION.csv an addition column "tax_id" in prepare_taxonomy_csv_file.py
#$task = $task_update_taxid_to_Annotation; // then reload ANNOTATION.csv to database

$task = $task_split_patent_Reference;

$task = $task_create_taxonomy_tables;

$task = $task_load_csv_files_to_database;  # note that REFERENCE_with_year
$task = $task_create_all_tables;


if ($task == $task_create_all_tables) 
{
    create_all_tables($mysqli);
}
elseif ($task == $task_load_csv_files_to_database)
{
    load_csv_files($mysqli);
}
elseif ($task == $task_add_indexes_to_tables)
{
    add_indexes_to_tables($mysqli);
}
elseif ($task == $task_create_taxonomy_tables)
{
    create_taxonomy_tables($mysqli);
}
elseif ($task == $task_add_locus_detailed_fields_to_Annotation)
{
    add_locus_detailed_fields_to_Annotation($mysqli);
}
elseif ($task == $task_add_taxid_to_Annotation)
{
    add_taxid_to_Annotation($mysqli);
}
elseif ($task == $task_update_taxid_to_Annotation)
{
    update_taxid_to_Annotation($mysqli);
}
elseif ($task == $task_split_patent_Reference) 
{
    split_patent_reference($mysqli);
}
else
    print "wrong task\n";


// -----------------------------------------------
function split_patent_reference($mysqli) 
{
    $sql = "CREATE TABLE Reference_backup LIKE Reference";
    run_sql($sql, $mysqli);

    $sql = "INSERT INTO Reference_backup SELECT * FROM Reference";
    run_sql($sql, $mysqli);

    $sql = "CREATE TABLE ReferencePatent LIKE Reference";
    run_sql($sql, $mysqli);

    $sql = "INSERT INTO ReferencePatent SELECT * FROM Reference WHERE journal LIKE 'Patent%'";
    run_sql($sql, $mysqli);

    $sql = "DELETE FROM Reference WHERE journal LIKE 'Patent%'";
    run_sql($sql, $mysqli);
}

function add_locus_detailed_fields_to_Annotation($mysqli)
{
    $field_datatype = array(
        'locus_name' => "CHAR(16)", 
        'locus_sequence_length' => "INT",
        'locus_sequence_strands' => "CHAR(3)",
        'locus_nucleic_acid_type' => "CHAR(6)", 
        'locus_linear_circular' => "CHAR(8)", 
        'locus_division_code' => "CHAR(3)", 
        'locus_date' => "DATE"
    ); 
       
    $is_drop_fields_only = true;
    $is_drop_fields_only = false;
    if ($is_drop_fields_only)
    {
        foreach ($field_datatype as $field => $datatype )
        {
            $sql = "ALTER TABLE Annotation DROP $field";
            print "$sql\n";
            if (! $mysqli->query($sql) ) {
                print "  - FAILED\n\n";
            }
        }
        return;
    }

    foreach ($field_datatype as $field => $datatype )
    {
        $sql = "ALTER TABLE Annotation ADD $field $datatype, ADD INDEX ($field)";
        print "$sql\n";
        if (! $mysqli->query($sql) ) {
            print "  - FAILED\n\n";
        }
    }
}

function add_taxid_to_Annotation($mysqli)
{
    $field_datatype = array(
        'tax_id' => "INT" 
    );
    foreach ($field_datatype as $field => $datatype )
    {
        $sql = "ALTER TABLE Annotation ADD $field $datatype, ADD INDEX ($field)";
        print "$sql\n";
        if (! $mysqli->query($sql) ) {
            print "  - FAILED\n\n";
        }
    }
}

function update_taxid_to_Annotation($mysqli) 
{
    $sql = "SELECT * FROM Organism_tax";
    $result = $mysqli->query($sql);
    $cnt = 0;
    $organism_tax = array();
    while ($row = $result->fetch_object()) {
        //print $row->organism_id . " -> " .  $row->tax_id . "\n";
        $organism_tax[ $row->organism_id ] = $row->tax_id;
        $cnt += 1;
        //if ($cnt>10) break;
    }

    $cnt = 0;
    $sql = "SELECT * FROM Annotation";
    $result = $mysqli->query($sql);
    while ($row = $result->fetch_object()) {
        $gi = $row->gi;
        $organismId = $row->organismId;
        if ($organism_tax[$organismId] != NULL) {
            $tax_id = $organism_tax[$organismId];
            $q = "UPDATE Annotation SET tax_id = " . $tax_id . " WHERE gi = " . $gi;
            if (! $mysqli->query($q) ) {
                print "FAILED TO UPDATE / SET tax_id\n" . $q . "\n";
            }
            $cnt += 1;
            //if ($cnt>10) break;
        }
    }
}

function load_csv_files($mysqli)
{
    $conf = read_config();
    $csv_dir = $conf["csv_dir"];

    $tables = array('taxNode');
    $tables = array('taxDivision');
    $tables = array('Reference', 'Annotation', 'AnnotationReference', 'Keywords', 'Source', 'Comment', 'Organism', 'Dblink');
    $tables = array('Organism_new', 'Organism_tax');
    $tables = array('Organism_new');
    $tables = array('Reference');
    foreach ($tables as $table)
    {
        $null_clause = "";
        if ($table == "Annotation")
        {
            $null_clause = "(gi, version, @keywordsId, sourceId, organismId, @commentId, @dblinkId, locus, accession, definition, @segment, locus_name, locus_sequence_length, @locus_sequence_strands, locus_nucleic_acid_type, locus_linear_circular, locus_division_code, @locus_date, @tax_id) SET keywordsId = nullif(@keywordsId,''), commentId = nullif(@commentId,''), dblinkId = nullif(@dblinkId,''), segment = nullif(@segment,''), locus_sequence_strands = nullif(@locus_sequence_strands,''), locus_date = STR_TO_DATE(@locus_date, '%d-%b-%Y'), tax_id = nullif(@tax_id,'')";
        }
        elseif ($table == "Reference")
        {
            $null_clause = "(id, reference, @authors, @consortium, title, journal, @pubmed, @remark, @year) SET authors = nullif(@authors,''), consortium = nullif(@consortium,''), pubmed = nullif(@pubmed,''), remark = nullif(@remark,''), year = nullif(@year,'')";
        }
        elseif ($table == "taxNode")
        {
            $null_clause = "(id, name, unique_name, parent_id, parent_name, @rank, @locus_prefix, division_id, inherited_div_flag, genetic_code_id, inherited_GC_flag, mitochondrial_genetic_code_id, inherited_MGC_flag, GenBank_hidden_flag, hidden_subtree_root_flag, @comments) SET rank = nullif(@rank,''), locus_prefix = nullif(@locus_prefix,''), comments = nullif(@comments,'')";
        }
        elseif ($table == "Organism_tax")
        {
            $null_clause = "(organism_id, @tax_id) SET tax_id = nullif(@tax_id,'')";
        }
        elseif ($table == "Organism_new")
        {
            $null_clause = "(tax_id, tax_name, tax_rank, @top1_tax_id, top1_tax_name, top1_tax_rank, @top2_tax_id, top2_tax_name, top2_tax_rank, @top3_tax_id, top3_tax_name, top3_tax_rank, @bottom1_tax_id, bottom1_tax_name, bottom1_tax_rank, @bottom2_tax_id, bottom2_tax_name, bottom2_tax_rank, @bottom3_tax_id, bottom3_tax_name, bottom3_tax_rank, content) SET top1_tax_id=nullif(@top1_tax_id,''),top2_tax_id=nullif(@top2_tax_id,''),top3_tax_id=nullif(@top3_tax_id,''), bottom1_tax_id=nullif(@bottom1_tax_id,''),bottom2_tax_id=nullif(@bottom2_tax_id,''),bottom3_tax_id=nullif(@bottom3_tax_id,'')";   
        }

        $filename = strtoupper($table); 
        if ($table == "AnnotationReference") {
            $filename = "ANNOTATION_REFERENCE";
        } else if ($table == "Annotation") {
            $filename = "ANNOTATION_tax_id";
        } elseif ($table == "Reference") { # for new Reference table (with year added)
            $filename = "REFERENCE_with_year";
        }

        $csv_fpath = $csv_dir . "/" . $filename . ".csv";
        $sql = "LOAD DATA LOCAL INFILE '$csv_fpath' INTO TABLE genbank.$table FIELDS TERMINATED BY '|' ENCLOSED BY '\"' " . $null_clause ; 

        print("$sql\n");
        if (! $mysqli->query($sql) ) 
        {
            print "     FAILED\n\n";
        }
    }
}


// -----------------------------------------------
function add_indexes_to_tables($mysqli)
{
    $table_indexes = array("table" => "Annotation", "indexes" => array("keywordsId", "sourceId", "organismId", "commentId", "dblinkId", "accession", "definition", "locus", "segment", "version"));
    $table_indexes = array("table" => "Reference", "indexes" => array("reference", "authors", "consortium", "title", "journal", "pubmed"));

    foreach ($table_indexes["indexes"] as $index)
    {
        $sql = "CREATE INDEX " . $index . " ON " . $table_indexes["table"] . " (" . $index . ")";
        print "$sql\n";
        $mysqli->query($sql);
    }
}


// -----------------------------------------------
function create_all_tables($mysqli)
{
    $tables = array('AnnotationReference');
    $tables = array('Reference', 'Annotation', 'AnnotationReference', 'Keywords', 'Source', 'Comment', 'Organism', 'Dblink');
    $tables = array('Annotation');
    $tables = array('Reference');
    foreach ($tables as $table) {
        create_table($mysqli, $table);
    }
}

function create_taxonomy_tables($mysqli)
{
    $tables = array('taxNode');
    $tables = array('taxDivision');
    $tables = array('Organism_new', 'Organism_tax');
    $tables = array('Organism_new');
    foreach ($tables as $table) {
        create_table($mysqli, $table);
    }
}

function create_table($mysqli, $table) 
{
    $sql = "SET foreign_key_checks = 0";
    $mysqli->query($sql);

    $sql = "DROP TABLE IF EXISTS " . $table;
    if (! $mysqli->query($sql)) 
    {
        print " - FALIED: $sql\n\n";
    }

    if ($table == "Reference") {
        $sql = "CREATE TABLE IF NOT EXISTS " . $table . " (
            id BIGINT,
            reference VARCHAR(200),
            authors VARCHAR(10000),
            consortium VARCHAR(1000),
            title VARCHAR(1000),
            journal VARCHAR(5000),
            pubmed VARCHAR(50),
            remark VARCHAR(1000),
            year INT,
            INDEX (reference),
            INDEX (authors),
            INDEX (consortium),
            INDEX (title),
            INDEX (journal),
            INDEX (pubmed),
            INDEX (year),
            PRIMARY KEY (id)
        )";
    } 
    else if ($table == "Annotation") {
        $sql = "CREATE TABLE IF NOT EXISTS " . $table . " (
            gi BIGINT, 
            version INT,
            keywordsId INT,
            sourceId INT,
            organismId INT,
            commentId INT,
            dblinkId INT,
            locus VARCHAR(500),
            accession VARCHAR(50),
            definition VARCHAR(500),
            segment VARCHAR(50),
            locus_name CHAR(16), 
            locus_sequence_length INT,
            locus_sequence_strands CHAR(3),
            locus_nucleic_acid_type CHAR(6), 
            locus_linear_circular CHAR(8), 
            locus_division_code CHAR(3), 
            locus_date DATE,
            tax_id INT,
            INDEX (keywordsId),
            INDEX (sourceId),
            INDEX (organismId),
            INDEX (commentId),
            INDEX (dblinkId),
            INDEX (accession),
            INDEX (definition),
            INDEX (locus),
            INDEX (segment),
            INDEX (version),
            INDEX (locus_name),
            INDEX (locus_sequence_length),
            INDEX (locus_sequence_strands),
            INDEX (locus_nucleic_acid_type),
            INDEX (locus_linear_circular),
            INDEX (locus_division_code),
            INDEX (locus_date),
            INDEX (tax_id),
            PRIMARY KEY (gi)
        )";
    }
    else if ($table == "AnnotationReference") {
        //keyId BIGINT AUTO_INCREMENT,
        $sql = "CREATE TABLE IF NOT EXISTS " . $table . " (
            keyId BIGINT,
            gi BIGINT,
            referenceId BIGINT,
            PRIMARY KEY (keyId),
            INDEX (gi),
            INDEX (referenceId),
            FOREIGN KEY (gi) REFERENCES Annotation(gi),
            FOREIGN KEY (referenceId) REFERENCES Reference(id)
        )";
    }
    else if ($table == "Keywords") {
        $sql = "CREATE TABLE IF NOT EXISTS " . $table . " (
            id INT,
            content VARCHAR(5000),
            INDEX (content),
            PRIMARY KEY(id)
        )";
    }
    else if ($table == "Source") {
        $sql = "CREATE TABLE IF NOT EXISTS " . $table . " (
            id INT,
            content VARCHAR(5000),
            INDEX (content),
            PRIMARY KEY(id)
        )";
    }
    else if ($table == "Organism") {
        $sql = "CREATE TABLE IF NOT EXISTS " . $table . " (
            id INT,
            content VARCHAR(5000),
            INDEX (content),
            PRIMARY KEY(id)
        )";
    }
    else if ($table == "Comment") {
        $sql = "CREATE TABLE IF NOT EXISTS " . $table . " (
            id INT,
            content VARCHAR(5000),
            PRIMARY KEY(id)
        )";
    }
    else if ($table == "Dblink") {
        $sql = "CREATE TABLE IF NOT EXISTS " . $table . " (
            id INT,
            content VARCHAR(5000),
            PRIMARY KEY(id)
        )";
    }

    // taxonomy
    /*else if ($table == "taxName") {
        $sql = "CREATE TABLE IF NOT EXISTS " . $table . " (
            tax_id INT,
            name VARCHAR(200),
            INDEX (name),
            PRIMARY KEY(tax_id)
        )";
    }
     */
    else if ($table == "taxNode") {
        $sql = "CREATE TABLE IF NOT EXISTS " . $table . " (
            id INT,
            name VARCHAR(200),
            unique_name VARCHAR(200),
            parent_id INT,
            parent_name VARCHAR(200),
            rank VARCHAR(100),
            locus_prefix CHAR(2),
            division_id INT,
            inherited_div_flag INT,
            genetic_code_id INT,
            inherited_GC_flag INT,
            mitochondrial_genetic_code_id INT,
            inherited_MGC_flag INT,
            GenBank_hidden_flag INT,
            hidden_subtree_root_flag INT,
            comments VARCHAR(1000),
            PRIMARY KEY(id),
            INDEX (name),
            INDEX (parent_id),
            INDEX (parent_name),
            INDEX (rank),
            INDEX (locus_prefix),
            INDEX (division_id),
            INDEX (genetic_code_id),
            INDEX (mitochondrial_genetic_code_id)
        )";
    }
    else if ($table == "taxDivision") {
        $sql = "CREATE TABLE IF NOT EXISTS " . $table . " (
            id INT,
            code CHAR(3),
            name VARCHAR(200),
            comments VARCHAR(1000),
            INDEX (code),
            INDEX (name),
            PRIMARY KEY(id)
        )";
    }
    else if ($table == "Organism_new") {
        $sql = "CREATE TABLE IF NOT EXISTS " . $table . " (
            tax_id INT, 
            tax_name VARCHAR(500),
            tax_rank VARCHAR(100), 
            top1_tax_id INT,
            top1_tax_name VARCHAR(500),
            top1_tax_rank VARCHAR(100), 
            top2_tax_id INT,
            top2_tax_name VARCHAR(500),
            top2_tax_rank VARCHAR(100), 
            top3_tax_id INT,
            top3_tax_name VARCHAR(500),
            top3_tax_rank VARCHAR(100),
            bottom1_tax_id INT,
            bottom1_tax_name VARCHAR(500),
            bottom1_tax_rank VARCHAR(100),
            bottom2_tax_id INT,
            bottom2_tax_name VARCHAR(500),
            bottom2_tax_rank VARCHAR(100),
            bottom3_tax_id INT,
            bottom3_tax_name VARCHAR(500),
            bottom3_tax_rank VARCHAR(100),
            content VARCHAR(5000),
            INDEX (tax_name),
            INDEX (tax_rank),
            INDEX (top1_tax_id),
            INDEX (top1_tax_name),
            INDEX (top1_tax_rank),
            INDEX (top2_tax_id),
            INDEX (top2_tax_name),
            INDEX (top2_tax_rank),
            INDEX (top3_tax_id),
            INDEX (top3_tax_name),
            INDEX (top3_tax_rank),
            INDEX (bottom1_tax_id),
            INDEX (bottom1_tax_name),
            INDEX (bottom1_tax_rank),
            INDEX (bottom2_tax_id),
            INDEX (bottom2_tax_name),
            INDEX (bottom2_tax_rank),
            INDEX (bottom3_tax_id),
            INDEX (bottom3_tax_name),
            INDEX (bottom3_tax_rank),
            PRIMARY KEY(tax_id)
        )";
    }
    else if ($table == "Organism_tax") {
        $sql = "CREATE TABLE IF NOT EXISTS " . $table . " (
            organism_id INT,
            tax_id INT,
            INDEX (tax_id),
            PRIMARY KEY(organism_id)
        )";
    }
    

    if (! $mysqli->query($sql))
    {
        echo "Failed to create Table \"" . $table . "\".\n";
    } else {
        echo "Table \"" . $table . "\" successfully created.\n";
    }

    $sql = "SET foreign_key_checks = 1";
    $mysqli->query($sql);
}

function run_sql($sql, $mysqli)
{
    print "$sql\n";
    if (! $mysqli->query($sql) ) {
        print "  - FAILED\n\n";
    }
}

?>
