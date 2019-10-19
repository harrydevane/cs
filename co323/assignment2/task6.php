<?php
try {
    $dbhandle = new PDO('mysql:host=dragon.kent.ac.uk; dbname=co323', 'co323', 'pa33word');
} catch (PDOException $e) {
    die('Error connecting to database: ' . $e->getMessage());
}

$sql = "SELECT sid, forename, surname, gender FROM Student";

$query = $dbhandle->prepare($sql);

if ($query->execute() === FALSE) {
    die('Error running query: ' . implode($query->errorInfo(), ' '));
}

$results = $query->fetchAll();
?>

<h2>Details of all students</h2>

<form method="GET" action="task7.php">
    <select name="student_id" onchange="if (this.value != 0) this.form.submit()">

        <option>Select a student</option>

        <?php foreach ($results as $row) { ?>
            <option value="<?php echo $row['sid']; ?>"><?php echo $row['sid']; ?><?php echo $row['forename']; ?><?php echo $row['surname']; ?><?php echo $row['gender']; ?></option>
        <?php } ?>

    </select>
</form>
