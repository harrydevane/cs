<?php
defined('BASEPATH') OR exit('No direct script access allowed');

class Search extends CI_Controller {

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
	 * Displays the Search view.
	 */
	public function index()
	{
		$logged_in = isset($_SESSION['username']);
		$data['logged_in'] = $logged_in;

		if ($logged_in)
		{
			$data['username'] = $_SESSION['username'];
		}

		$this->load->view('Search', $data);
	}

	/**
	 * Do Search
	 *
	 * Loads the messages model, reads the provided string from the GET
	 * parameters and searches the database for messages containing it,
	 * before displaying the results in the ViewMessages view.
	 */
	public function dosearch()
	{
		$this->load->model('messages_model');

		$logged_in = isset($_SESSION['username']);
		$data['logged_in'] = $logged_in;

		if ($logged_in)
		{
			$data['username'] = $_SESSION['username'];
		}

		$string = $_GET['string'];

		$results = $this->messages_model->searchMessages($string);
		$data['results'] = $results;

		$data['following'] = true;

		$this->load->view('ViewMessages', $data);
	}
}
