<?php

$config_file = 'configuration.cnf';
if (!file_exists($config_file))
  die("There should be a configuration file 'configuration.cnf' but it is missing!\n");

$configuration = @parse_ini_file($config_file, false, INI_SCANNER_TYPED);
if (!$configuration) {
  die("Failed to read config file!");
}

$short_options = "d:l:";
$long_options = ["directory:", "label:"];
$options = getopt($short_options, $long_options);

if (isset($options["d"]) || isset($options["directory"])) {
  $directory = isset($options["d"]) ? $options["d"] : $options["directory"];
} else {
  die("Missing directory parameter!\n");
}

if (isset($options["l"]) || isset($options["label"])) {
  $label = isset($options["l"]) ? $options["l"] : $options["label"];
} else {
  die("Missing label parameter!\n");
}

echo 'directory: ', $directory, "\n";
echo 'label: ', $label, "\n";

$input = $configuration['qa_output'] . '/' . $directory .'/translations-export.jsonld';
$output = 'data/' . $directory . '-translations.csv';
echo 'output: ', $output, "\n";
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

$directoryFile = 'data/directory.csv';
$index = readCsv($directoryFile, 'machine_name');
$record = (object)[
  'machine_name' => $directory,
  'label' => $label,
  'output' => $output,
];
$index[$directory] = $record;
saveDirectory($index, $directoryFile);

// Functions

function getFirst($record, $key) {
  if (!isset($record->{$key}))
    return '';

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

function saveDirectory($directory, $directoryFile) {
  if (file_exists($directoryFile))
    unlink($directoryFile);

  file_put_contents($directoryFile, "machine_name,label,output\n");

  foreach ($directory as $key => $record) {
    $csv = sprintf("%s,\"%s\",%s\n",
      $record->machine_name,
      $record->label,
      $record->output
    );

    file_put_contents($directoryFile, $csv, FILE_APPEND);
  }
}

function readCsv($csvFile, $id = '') {
  $records = [];
  if (file_exists($csvFile)) {
    $lineNumber = 0;
    $header = [];

    $handle = fopen($csvFile, "r");
    if ($handle) {
      while (($line = fgets($handle)) !== false) {
        $lineNumber++;
        $values = str_getcsv($line);
        if ($lineNumber == 1) {
          $header = $values;
        } else {
          if (count($header) != count($values))
            echo sprintf("error in %s line #%d: %d vs %d\n", $csvFile, $lineNumber, count($header), count($values));

          $record = (object)array_combine($header, $values);
          if ($id != '' && isset($record->{$id})) {
            $records[$record->{$id}] = $record;
          } else {
            $records[] = $record;
          }
        }
      }
    }
  } else {
    echo 'file does not exist! ' . $csvFile . "\n";
  }
  return $records;
}
