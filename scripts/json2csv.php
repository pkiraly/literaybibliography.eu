<?php

$input = '/home/pkiraly/data/marc21/output/elb/translations-export.jsonld';
$output = 'data/translations-export.csv';
if (file_exists($output))
  unlink($output);
file_put_contents($output, "author,publicationPlace,publicationYear,sourceLanguage,targetLanguage\n");

$handle = fopen($input, "r");
if ($handle) {
  while (($line = fgets($handle)) !== false) {
    // process the line read.
    $record = json_decode($line);
    # echo $record->id, "\n";
    $csv = sprintf("%s,%s,%s,%s,%s\n",
      getFirst($record, 'author'),
      getFirst($record, 'publicationPlace'),
      getFirst($record, 'publicationYear'),
      getFirst($record, 'sourceLanguage'),
      getFirst($record, 'targetLanguage'),
    );
    file_put_contents($output, $csv, FILE_APPEND);
  }
  fclose($handle);
}

function getFirst($record, $key) {
  $array = $record->{$key};
  $first = empty($array) ? '' : $array[0];
  if ($key == 'author') {
    $first = preg_replace('/ ; .*$/', '', $first);
  }
  if (preg_match("/,/", $first)) {
    $first = str_replace('"', '""', $first);
    $first = sprintf('"%s"', $first);
  }
  return $first;
}