<?php

define("GENBANK_RECORD_SEP", "//");
define("KEYWORD_COLUMNS", 12);

define("LOCUS", "LOCUS");
define("DEFINITION", "DEFINITION");
define("ACCESSION", "ACCESSION");
define("VERSION", "VERSION");
define("GI", "GI");
define("KEYWORDS", "KEYWORDS");
define("SEGMENT", "SEGMENT");
define("DBLINK", "DBLINK");
define("SOURCE", "SOURCE");
define("ORGANISM", "ORGANISM");
define("COMMENT", "COMMENT");
define("REFERENCE", "REFERENCE");
define("AUTHORS", "AUTHORS");
define("CONSRTM", "CONSRTM");
define("TITLE", "TITLE");
define("JOURNAL", "JOURNAL");
define("PUBMED", "PUBMED");
define("REMARK", "REMARK");

define("ANNOTATION", "ANNOTATION");
define("ANNOTATION_REFERENCE", "ANNOTATION_REFERENCE");


function connect_database() 
{
    $conf = read_config();
    print_r($conf);
    $mysqli = new mysqli($conf['host'], $conf['user'], $conf['password'], $conf['database'], '3306', $conf['mysql_sock']);
    #print_r($mysqli);
    if ($mysqli->connect_errno) {
        echo "Failed to connect to MySQL: (" . $mysqli->connect_errno . ") " . $mysqli->connect_error;
        exit();
    } else {
        echo "successfully connect to database: ". $conf['database'] . "\n";
    }
    return $mysqli;
}

function read_config()
{
    $fileConf = "genbank.conf";
    if (!($conf = parse_ini_file($fileConf))) {
        throw new Exception("\nConfiuration file not found: " . $fileConf);
    }
    return $conf;
}

function print_time($t0, $msg)
{
    $t1 = microtime(true);
    echo sprintf("$msg %.5fs\n", ($t1 - $t0));
}

function generateRandomString($length = 10) {
    $characters = '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ';
    $randomString = '';
    for ($i = 0; $i < $length; $i++) {
        $randomString .= $characters[rand(0, strlen($characters) - 1)];
    }
    return $randomString;
}
