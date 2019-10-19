<?php
try {
    $dbhandle = new PDO('mysql:host=dragon.kent.ac.uk; dbname=co323', 'co323', 'pa33word');
} catch (PDOException $e) {
    die('Error connecting to database: ' . $e->getMessage());
}

$sql = "SELECT  c.cid, c.title, name, weighting, mark
        FROM    Grade g JOIN Assessment a ON g.aid = a.aid JOIN Course c on a.cid = c.cid
        WHERE   sid = :s;";

$param = array(':s' => $_GET['student_id']);
$query = $dbhandle->prepare($sql);

if ($query->execute($param) === FALSE) {
    die('Error running query: ' . implode($query->errorInfo(), ' '));
}

$results = $query->fetchAll();

$sql = "SELECT   cid,
                 SUM(mark*weighting)/100 AS Final
        FROM     Grade g JOIN Assessment a ON g.aid = a.aid
        WHERE    sid = :s
        GROUP BY cid;";

$param = array(':s' => $_GET['student_id']);
$query = $dbhandle->prepare($sql);

if ($query->execute($param) === FALSE) {
    die('Error running query: ' . implode($query->errorInfo(), ' '));
}

$final_results = $query->fetchAll();
$final_marks = array();

foreach ($final_results as $row) {
    $final_marks[$row['cid']] = $row['Final'];
}
?>

<h2>Details of all results</h2>

<table>
    <tr>
        <th>Course ID</th>
        <th>Course Title</th>
        <th>Name</th>
        <th>Weighting</th>
        <th>Mark</th>
        <th>Overall Mark</th>
    </tr>
    <?php foreach ($results as $row) { ?>
        <tr>
            <td><?php echo $row['cid']; ?></td>
            <td><?php echo $row['title']; ?></td>
            <td><?php echo $row['name']; ?></td>
            <td><?php echo $row['weighting']; ?></td>
            <td><?php echo $row['mark']; ?></td>
            <td><?php echo $final_marks[$row['cid']]; ?></td>
        </tr>
    <?php } ?>
</table>
