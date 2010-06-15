<?

$table = array_fill(0, 1000, array("a"=>1, "b"=>2, "c"=>3, "d"=>4, "e"=>5, "f"=>6, "g"=>7, "h"=>8, "i"=>9, "j"=>10));

function test_bigtable($table)
{
ob_start();
?>
<table>
<?
foreach($table as $rowNum => $row)
{
?>
<tr>
<?
foreach($row as $value)
{
?>
<td><? echo $value; ?></td>
<?
}
?>
</tr>
<?
}
?>
</table>
<?
return ob_get_clean();
}

$request_count = 1000;

$start = microtime(true);
for ($i = 0; $i < $request_count; $i++)
{
  test_bigtable($table);
}
$elapsed = microtime(true) - $start;
$time_per_request = ($elapsed / $request_count) * 1000;
echo "\"PHP\", $time_per_request\n";
?>
