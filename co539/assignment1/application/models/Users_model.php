<?php
defined('BASEPATH') OR exit('No direct script access allowed');

class Users_model extends CI_Model {

	/**
	 * Class constructor
	 *
	 * Loads the database.
	 *
	 * @return	void
	 */
	public function __construct()
	{
		$this->load->database();
	}

	/**
	 * Check Login
	 *
	 * Checks whether a provided username and password are valid.
	 *
	 * @param	string	$username	Username
	 * @param	string	$pass	Password
	 * @return	bool
	 */
	public function checkLogin($username, $pass)
	{
		$sql = 'SELECT * FROM Users WHERE username = ? AND password = ?';
		$query = $this->db->query($sql, array($username, sha1($pass)));
		return $query->num_rows() > 0;
	}

	/**
	 * Is Following
	 *
	 * Checks whether a user is following another user.
	 *
	 * @param	string	$follower	Follower username
	 * @param	string	$followed	Followed username
	 * @return	bool
	 */
	public function isFollowing($follower, $followed)
	{
		$sql = 'SELECT * FROM User_Follows WHERE follower_username = ? AND followed_username = ?';
		$query = $this->db->query($sql, array($follower, $followed));
		return $query->num_rows() > 0;
	}

	/**
	 * Follow
	 *
	 * Sets a user as following another user.
	 *
	 * @param	string	$follower	Follower username
	 * @param	string	$followed	Followed username
	 * @return	void
	 */
	public function follow($follower, $followed)
	{
		$sql = 'INSERT INTO User_Follows (follower_username, followed_username) VALUES (?, ?)';
		$this->db->query($sql, array($follower, $followed));
	}
}
