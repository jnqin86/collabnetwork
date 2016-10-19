<?php
ini_set('memory_limit', '-1');

include './helper.php';

$debug = true;
$debug = false;

if ($debug)
{
    define("MAX_FILES_TO_PARSE", 1);
    define("MAX_RECORDS_IN_A_FILE_FOR_PARSING", 10);
}
else
{
    define("MAX_FILES_TO_PARSE", 99999);
    define("MAX_RECORDS_IN_A_FILE_FOR_PARSING", 999999999);
}

define("LINE_BREAK_WITHIN_A_FIELD", "\t");  // use "\t" instead of "\n" when concatenating lines for easy to read the content of multiple lines
define("CSV_FIELD_SEPARATOR", "|");


$Conf = read_config();

init_csv_files();
parse_all_files();


#------------------------------------------------------
function init_csv_files() 
{
    global $Conf, $Fhandles;
    $Fhandles = array();
    foreach (array(ANNOTATION, REFERENCE, ANNOTATION_REFERENCE, COMMENT, KEYWORDS, ORGANISM, SOURCE, DBLINK) as $hash_key) {
        $csv_file = $Conf['csv_dir'] . "/$hash_key.csv";
        $Fhandles[$hash_key] = fopen($csv_file, "w");
    }
}


function parse_all_files()
{
    global $Conf, $AR_keyId;
    $AR_keyId = 0;

    $fileCnt = 0;
    foreach (scandir($Conf["annotation_gz_dir"]) as $file) {
        if ($file == "." || $file == "..") continue;
        $fname_ann = substr($file, 0, -3);
        print "$fname_ann\n";

        parse($fname_ann);
        if (++$fileCnt >= MAX_FILES_TO_PARSE) break;
    }
}


function unzip_file($fname_ann)
{
    global $Conf;
    $fpath_gz =  $Conf["annotation_gz_dir"] . "/$fname_ann.gz";

    if (! file_exists($fpath_gz)) 
    {
        throw new Exception("Annotation file not existing: \"$fpath_gz\"");
    }
    else
    {
        $tmp_dir = $Conf["tmp_dir"];
        $cmd = "cp \"$fpath_gz\" $tmp_dir";
        exec($cmd);
        $cmd = "gunzip \"$tmp_dir/$fname_ann.gz\"";
        print "$cmd\n";
        exec($cmd);
    }
}


function parse($fname_ann)
{
    global $Conf;

    $keys_annotation = array(LOCUS, DEFINITION, ACCESSION, VERSION, GI, KEYWORDS, SEGMENT, DBLINK, SOURCE, ORGANISM, COMMENT, 'locus_name', 'locus_sequence_length', 'locus_sequence_strands', 'locus_nucleic_acid_type', 'locus_linear_circular', 'locus_division_code', 'locus_date');
    $keys_reference = array(REFERENCE, AUTHORS, CONSRTM, TITLE, JOURNAL, PUBMED, REMARK);

    $fpath_ann = $Conf["tmp_dir"] . "/$fname_ann";
    if (! file_exists($fpath_ann)) 
    {
        unzip_file($fname_ann);
    }

    print("parsing: $fpath_ann\n");

    $is_patent = strpos($fname_ann, "pat");

    $t0 = microtime(true); 

    init_global_hashes();

    $ann = init_array($keys_annotation);
    $ref = init_array($keys_reference);
    $prev_key = NULL;
    $records = 0;
    $refIds_in_one_record = array();
    foreach (file($fpath_ann) as $line)
    {
        $key = trim(substr($line, 0, KEYWORD_COLUMNS)); 
        $val = trim(substr($line, KEYWORD_COLUMNS)); 
        if (! $key)
            $key = $prev_key;
        else
            $prev_key = $key;

        if ($key == GENBANK_RECORD_SEP) 
        {
            save_reference($ref, $ann[GI], $refIds_in_one_record, $is_patent);
            save_annotation($ann, $refIds_in_one_record);


            $ann = init_array($keys_annotation);
            $ref = init_array($keys_reference);
            $refIds_in_one_record = array();

            $records ++;
            if ($records >= MAX_RECORDS_IN_A_FILE_FOR_PARSING) break;
            if ($records % 1000 == 0) print_time($t0, 'time used for processing '. $records);
        }

        if (in_array($key, $keys_annotation))
        {
            if ($key == VERSION)
            {
                //AF000122.1  GI:2098806
                list($ver, $gi) = split("  ", $val);
                list($foo, $ann[VERSION]) = split("\.", $ver);
                $ann[GI] = trim(substr($gi, 3));
            }
            else 
            {
                if ($ann[$key])
                    $ann[$key] .= LINE_BREAK_WITHIN_A_FIELD . $val;
                else
                    $ann[$key] = $val;
            }

            if ($key == LOCUS)
            {
/*
13-28      Locus name
30-40      Length of sequence
45-47      spaces, ss- (single-stranded)
48-53      NA, DNA, RNA, tRNA
56-63      'linear'
65-67      The division code
69-79      Date
 */
                $ann["locus_name"] = trim(substr($line, 13-1, 16));
                $ann["locus_sequence_length"] = trim(substr($line, 30-1, 11));
                $ann["locus_sequence_strands"] = trim(substr($line, 45-1, 3));
                $ann["locus_nucleic_acid_type"] = trim(substr($line, 48-1, 6));
                $ann["locus_linear_circular"] = trim(substr($line, 56-1, 8));
                $ann["locus_division_code"] = trim(substr($line, 65-1, 3));
                $ann["locus_date"] = trim(substr($line, 69-1, 11));
            }

        }

        if (in_array($key, $keys_reference))
        {
            if ($key == REFERENCE && $ref[REFERENCE])
            {
                save_reference($ref, $ann[GI], $refIds_in_one_record, $is_patent);
                $ref = init_array($keys_reference);
            }
            if ($ref[$key]) {
                $ref[$key] .= LINE_BREAK_WITHIN_A_FIELD . $val;
            } else {
                $ref[$key] = $val;
            }
        }

    }
}


function save_reference($ref, $gi, &$refIds_in_one_record, $is_patent)
{
    global $Hash_Reference;
    if ($ref[PUBMED])
    {
        $k = $ref[PUBMED];
    }
    else 
    {
        $au = $ref[AUTHORS] ? $ref[AUTHORS] : ($ref[CONSRTM] ? $ref[CONSRTM] : NULL);
        $ti = $ref[TITLE];
        $jo = $ref[JOURNAL];
        $k = gen_hash_key($au . $ti . $jo);
    }

    if ($is_patent)
    { // every patent reference (in Journal) is unique
        $rk = $gi*1000+1;
        save_reference_table($ref, $rk);
    } 
    else 
    { 
        if (! array_key_exists($k, $Hash_Reference)) {
            $Hash_Reference[$k] = ((int)$gi) * 1000 + sizeof($refIds_in_one_record) +1;
            save_reference_table($ref, $Hash_Reference[$k]);
        } 
        $rk = $Hash_Reference[$k];
    }
    save_annotation_reference_table($gi, $rk);
    array_push($refIds_in_one_record, $rk);
}


function save_annotation(&$ann, $refIds_in_one_record)
{
    global $Hash_Misc;

    foreach (array_keys($Hash_Misc) as $category)
    {
        $content = array_key_exists($category, $ann) ? trim($ann[$category]) : NULL;

        if (! $content || $content == ".") //  KEYWORDS  .
            $ann[$category] = NULL;
        else
        {
            $key = gen_hash_key($content);
            if (! array_key_exists($key, $Hash_Misc[$category]))
            {
                $Hash_Misc[$category][$key] = $ann[GI]; 
                save_misc_table($category, $ann[GI], $content);
            }
            $ann[$category] = $Hash_Misc[$category][$key];

        }
    }

    save_annotation_table($ann);
}


function save_annotation_table($ann) 
{
    global $Fhandles;
    fputcsv($Fhandles[ANNOTATION], array(
        $ann[GI], $ann[VERSION], $ann[KEYWORDS], $ann[SOURCE], $ann[ORGANISM], $ann[COMMENT], $ann[DBLINK],
        $ann[LOCUS], $ann[ACCESSION], $ann[DEFINITION], $ann[SEGMENT],
        $ann["locus_name"], $ann["locus_sequence_length"], $ann["locus_sequence_strands"], $ann["locus_nucleic_acid_type"],
        $ann["locus_linear_circular"], $ann["locus_division_code"], $ann["locus_date"]
    ), CSV_FIELD_SEPARATOR);
}

function save_reference_table($ref, $refId) 
{
    global $Fhandles;
    fputcsv($Fhandles[REFERENCE], array($refId, $ref[REFERENCE], $ref[AUTHORS], $ref[CONSRTM], $ref[TITLE], $ref[JOURNAL], $ref[PUBMED], $ref[REMARK]), CSV_FIELD_SEPARATOR);
}

function save_annotation_reference_table($gi, $refId) 
{
    global $Fhandles, $AR_keyId;
    ++$AR_keyId;
    fputcsv($Fhandles["ANNOTATION_REFERENCE"], array($AR_keyId, $gi, $refId), CSV_FIELD_SEPARATOR);
}

function save_misc_table($category, $id, $content) 
{
    global $Fhandles;
    fputcsv($Fhandles[$category], array($id, $content), CSV_FIELD_SEPARATOR);
}


function gen_hash_key($s) 
{
    return strtolower(preg_replace('/\W+/', '', $s));
}


function init_array($keys) {
    $array = array();
    foreach ($keys as $k) $array[$k] = NULL;
    return $array;
}


function init_global_hashes() {
    global $Hash_Reference, $Hash_Misc;
    $Hash_Reference = array();
    $Hash_Misc = array();
    foreach (array(COMMENT, KEYWORDS, ORGANISM, SOURCE, DBLINK) as $hash_key) {
        $Hash_Misc[$hash_key] = array();
    }
}

