<!DOCTYPE html>
<html>

<head>
	<title>New Post</title>

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
			<h2>Post a Message</h2>
			<form method="post" action="<?php echo base_url(); ?>message/dopost">
				<div class="spacer">
					<textarea name="message" cols="80" rows="20" placeholder="Type your message..."></textarea>
				</div>
				<div class="spacer">
					<button type="submit">Submit</button>
				</div>
			</form>
		</div>
	</div>
</body>

</html>
