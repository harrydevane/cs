<?php
try {
    $dbhandle = new PDO('mysql:host=dragon.kent.ac.uk; dbname=co323', 'co323', 'pa33word');
} catch (PDOException $e) {
    die('Error connecting to database: ' . $e->getMessage());
}

$sql = "SELECT   cid, name, AVG(mark) AS avg_mark
        FROM     Grade g JOIN Assessment a ON g.aid = a.aid
        GROUP BY cid, name
        ORDER BY cid, name;";

$query = $dbhandle->prepare($sql);

if ($query->execute() === FALSE) {
    die('Error running query: ' . implode($query->errorInfo(), ' '));
}

$results = $query->fetchAll();
?>

<h2>Details of all assessment marks</h2>

<table>
    <tr>
        <th>Course ID</th>
        <th>Name</th>
        <th>Average Mark</th>
    </tr>
    <?php foreach ($results as $row) { ?>
        <tr>
            <td><?php echo $row['cid']; ?></td>
            <td><?php echo $row['name']; ?></td>
            <td><?php echo $row['avg_mark']; ?></td>
        </tr>
    <?php } ?>
</table>
