<!DOCTYPE html>
<html>

<head>
	<title>Messages</title>

	<link rel="stylesheet" type="text/css" href="<?php echo base_url(); ?>css/style.css" />
</head>

<body>
	<div class="container">
		<div class="navbar">
			<?php if ($logged_in) { ?>
				<a href="<?php echo base_url(); ?>user/view/<?php echo $username ?>">Messages</a>
				<a href="<?php echo base_url(); ?>message">New Post</a>
				<a href="<?php echo base_url(); ?>user/feed/<?php echo $username ?>">Followed</a>
			<?php } else { ?>
				<a href="<?php echo base_url(); ?>user/login">Login</a>
			<?php } ?>
			<a href="<?php echo base_url(); ?>search">Search</a>
			<?php if ($logged_in) { ?>
				<a href="<?php echo base_url(); ?>user/logout">Logout</a>
				<span>Logged in as: <?php echo $username; ?></span>
			<?php } else { ?>
				<span>Not logged in</span>
			<?php } ?>
		</div>
		<div class="content">
			<h2>Messages</h2>
			<?php if ($logged_in && !$following && isset($name) && $username !== $name) { ?>
				<div class="spacer">You are not currently following <?php echo $name; ?>. Click <a href="<?php echo base_url(); ?>user/follow/<?php echo $name; ?>">here</a> to follow them!</div>
			<?php } ?>

			<br />

			<?php foreach ($results as $row) { ?>
				<div class="message">
					<?php if (substr(uri_string(), 0, strlen('user/view/'.$row['user_username'])) === 'user/view/'.$row['user_username']) { ?>
						<div class="username"><?php echo $row['user_username']; ?></div>
					<?php } else { ?>
						<div class="username"><a href="<?php echo base_url(); ?>user/view/<?php echo $row['user_username']; ?>"><?php echo $row['user_username']; ?></a></div>
					<?php } ?>
					<div class="text"><?php echo $row['text']; ?></div>
					<div class="posted_at"><b><?php echo date('F jS Y \a\t h:i A', strtotime($row['posted_at'])); ?></b></div>
					<br />
				</div>
			<?php } ?>
		</div>
	</div>
</body>

</html>
