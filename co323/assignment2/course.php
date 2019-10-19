<?php
try {
    $dbhandle = new PDO('mysql:host=dragon.kent.ac.uk; dbname=co323', 'co323', 'pa33word');
} catch (PDOException $e) {
    die('Error connecting to database: ' . $e->getMessage());
}

$sql = "SELECT * FROM Course";

$query = $dbhandle->prepare($sql);

if ($query->execute() === FALSE) {
    die('Error running query: ' . implode($query->errorInfo(), ' '));
}

$results = $query->fetchAll();
?>

<h2>Details of all courses</h2>

<?php foreach ($results as $row) { ?>
    <p>
        <?php echo $row['cid']; ?>: <?php echo $row['title']; ?>
    </p>
<?php } ?>
