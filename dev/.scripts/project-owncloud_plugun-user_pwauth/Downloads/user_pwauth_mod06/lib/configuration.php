<?php
namespace OCA\user_pwauth\lib;

class Configuration {
	public $params = array();
	//constant DEFAULTS = [
	const DEFAULTS = [
		'list_delim'   => ';',
		'group_prefix' => '@',
		'min_id'       => '100',
		'uid_list'     => '1000-1100',
		'usr_list'     => '@users',
		'adm_list'     => '@wheel',
		'pwauth_path'  => '/usr/sbin/pwauth',
		'app_name'     => 'user_pwauth',
	];

	public function __construct($params = array()) {
		$this->params = $params;
		foreach((array_keys(self::DEFAULTS)) as $param)
			if (!isset($this->params[$param]))
				$this->params[$param] = \OC_Appconfig::getValue(self::DEFAULTS['app_name'], $param, self::DEFAULTS[$param]);
	}

	public function test(){
		//return (string)'hello';
		//return (string)($defaults['group_prefix']);
		//return (string)($this->params['uid_list']);
		return self::DEFAULTS['uid_list'];
	}
	
	//public function __get(


	//public function updateList( $list = 'uid_list', 
}
