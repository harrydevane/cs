<?php
defined('BASEPATH') OR exit('No direct script access allowed');

class Message extends CI_Controller {

	/**
	 * Class constructor
	 *
	 * Loads the session library and url helper.
	 *
	 * @return	void
	 */
	public function __construct()
	{
		parent::__construct();

		$this->load->library('session');
		$this->load->helper('url');
	}

	/**
	 * Index Page for this controller
	 *
	 * Redirects the user to the login page if they are not logged in,
	 * otherwise displays the Post view.
	 */
	public function index()
	{
		if ( ! isset($_SESSION['username']))
		{
			redirect('/user/login');
			return;
		}

		$data['logged_in'] = true;
		$data['username'] = $_SESSION['username'];

		$this->load->view('Post', $data);
	}

	/**
	 * Do Post
	 *
	 * Redirects the user to the login page if they are not logged in,
	 * otherwise reads the provided message from the POST parameters and
	 * inserts it into the database, before redirecting the user to the
	 * view page.
	 */
	public function doPost()
	{
		if ( ! isset($_SESSION['username']))
		{
			redirect('/user/login');
			return;
		}

		$this->load->model('messages_model');

		$username = $_SESSION['username'];
		$message = $_POST['message'];

		$this->messages_model->insertMessage($username, $message);

		redirect('/user/view/'.$username);
	}
}
