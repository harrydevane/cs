<?php
try {
    $dbhandle = new PDO('mysql:host=dragon.kent.ac.uk; dbname=co323', 'co323', 'pa33word');
} catch (PDOException $e) {
    die('Error connecting to database: ' . $e->getMessage());
}

$sql = "SELECT    name, weighting
        FROM      Assessment a JOIN Course c ON a.cid = c.cid
        WHERE     title = 'Web technologies'
        ORDER BY  name;";

$query = $dbhandle->prepare($sql);

if ($query->execute() === FALSE) {
    die('Error running query: ' . implode($query->errorInfo(), ' '));
}

$results = $query->fetchAll();
?>

<h2>Details of all assessments</h2>

<?php foreach ($results as $row) { ?>
    <p>
        <?php echo $row['name']; ?>: <?php echo $row['weighting']; ?>
    </p>
<?php } ?>
