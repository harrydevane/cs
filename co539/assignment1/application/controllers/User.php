<?php
defined('BASEPATH') OR exit('No direct script access allowed');

class User extends CI_Controller {

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
	 * Displays the Login view if the user is not currently logged in,
	 * otherwise calls the view function.
	 */
	public function index()
	{
		$logged_in = isset($_SESSION['username']);
		$data['logged_in'] = $logged_in;

		if ($logged_in)
		{
			$this->view($_SESSION['username']);
		}
		else
		{
			$this->load->view('Login', $data);
		}
	}

	/**
	 * View
	 *
	 * Loads the messages and users models, gets all messages posted
	 * by the provided username, checks to see if the user is following
	 * the username if they are logged in and displays the results
	 * in the ViewMessages view.
	 *
	 * @param	string	$name	Username
	 */
	public function view($name)
	{
		$this->load->model('messages_model');
		$this->load->model('users_model');

		$data['name'] = $name;

		$results = $this->messages_model->getMessagesByPoster($name);
		$data['results'] = $results;

		$logged_in = isset($_SESSION['username']);
		$data['logged_in'] = $logged_in;

		if ($logged_in)
		{
			$username = $_SESSION['username'];
			$data['username'] = $username;

			$data['following'] = $this->users_model->isFollowing($username, $name);
		}

		$this->load->view('ViewMessages', $data);
	}

	/**
	 * Login
	 *
	 * Displays the Login view.
	 */
	public function login()
	{
		$logged_in = isset($_SESSION['username']);
		$data['logged_in'] = $logged_in;

		if ($logged_in)
		{
			$data['username'] = $_SESSION['username'];
		}

		$this->load->view('Login', $data);
	}

	/**
	 * Do Login
	 *
	 * Loads the users mode, reads the provided username and password
	 * from the POST parameters and checks to see if they are valid in the database.
	 * If the username and password are valid, sets the user as logged in and
	 * redirects them to the user view page. Otherwise, displays the Login view
	 * with a message informing them that the provided username and password
	 * were invalid.
	 */
	public function doLogin()
	{
		$this->load->model('users_model');

		$username = $_POST['username'];
		$password = $_POST['password'];

		if ($this->users_model->checkLogin($username, $password))
		{
			$this->session->set_userdata('username', $username);

			redirect('/user/view/'.$username);
		}
		else
		{
			$data['logged_in'] = false;
			$data['failed_login'] = true;

			$this->load->view('Login', $data);
		}
	}

	/**
	 * Logout
	 *
	 * Sets the user as logged out and redirects them to the login page.
	 */
	public function logout()
	{
		unset($_SESSION['username']);

		redirect('/user/login');
	}

	/**
	 * Follow
	 *
	 * Redirects the user to the login page if they are not logged in,
	 * otherwise loads the messages model and sets the user as following
	 * the provided user in the database, before redirecting them to the
	 * user view page.
	 *
	 * @param	string	$followed	Followed username
	 */
	public function follow($followed)
	{
		if ( ! isset($_SESSION['username']))
 		{
			redirect('/user/login');
			return;
		}

		$this->load->model('users_model');

		$username = $_SESSION['username'];
		$this->users_model->follow($username, $followed);

		redirect('/user/view/'.$followed);
	}

	/**
	 * Feed
	 *
	 * Loads the messages model, gets all messages posted by users
	 * which the user follows and displays the results in the
	 * ViewMessages view.
	 *
	 * @param	string	$followed	Followed username
	 */
	public function feed($name)
	{
		$this->load->model('messages_model');

		$logged_in = isset($_SESSION['username']);
		$data['logged_in'] = $logged_in;

		if ($logged_in)
		{
			$data['username'] = $_SESSION['username'];
		}

		$results = $this->messages_model->getFollowedMessages($name);
		$data['results'] = $results;

		$data['following'] = true;

		$this->load->view('ViewMessages', $data);
	}
}
