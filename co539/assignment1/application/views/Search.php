<!DOCTYPE html>
<html>

<head>
	<title>Search</title>

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
			<h2>Search</h2>
			<form method="get" action="<?php echo base_url(); ?>search/dosearch">
				<div class="spacer">
					<input type="text" name="string" placeholder="Search term..." />
				</div>
				<div class="spacer">
					<button type="submit">Search</button>
				</div>
			</form>
		</div>
	</div>
</body>

</html>
