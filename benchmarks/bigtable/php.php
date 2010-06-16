<?php
$table = array_fill(0, 1000, array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10));

function test_bigtable($table) {
	ob_start();
?>
<table>
<?php foreach($table as $row) { ?>
<tr>
<?php foreach($row as $value) { ?>
<td><?php echo $value; ?></td>
<?php } ?>
</tr>
<?php } ?>
</table>
<?php
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
