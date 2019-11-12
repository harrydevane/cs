<?php
defined('BASEPATH') OR exit('No direct script access allowed');

class Messages_model extends CI_Model {

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
	 * Get Messages By Poster
	 *
	 * Gets all messages posted by a specified username.
	 *
	 * @param	string	$name	Username
	 * @return	array
	 */
	public function getMessagesByPoster($name)
	{
		$sql = 'SELECT user_username, text, posted_at FROM Messages WHERE user_username = ? ORDER BY posted_at DESC';
		$query = $this->db->query($sql, array($name));
		return $query->result_array();
	}

	/**
	 * Search Messages
	 *
	 * Searches for all messages containing a specified string.
	 *
	 * @param	string	$string	String
	 * @return	array
	 */
	public function searchMessages($string)
	{
		$sql = 'SELECT user_username, text, posted_at FROM Messages WHERE text LIKE ? ORDER BY posted_at DESC';
		$query = $this->db->query($sql, array('%'.$string.'%'));
		return $query->result_array();
	}

	/**
	 * Insert Message
	 *
	 * Inserts a message into the database.
	 *
	 * @param	string	$poster	Poster username
	 * @param	string	$string	Message
	 * @return	void
	 */
	public function insertMessage($poster, $string)
	{
		$sql = 'INSERT INTO Messages (user_username, text, posted_at) VALUES (?, ?, NOW())';
		$this->db->query($sql, array($poster, $string));
	}

	/**
	 * Get Followed Messages
	 *
	 * Gets all messages posted by users which a provided username follows.
	 *
	 * @param	string	$name	Username
	 * @return	array
	 */
	public function getFollowedMessages($name)
	{
		$sql = 'SELECT user_username, text, posted_at FROM Messages JOIN User_Follows ON user_username = followed_username WHERE follower_username = ? GROUP BY id ORDER BY posted_at DESC';
		$query = $this->db->query($sql, array($name));
		return $query->result_array();
	}
}
