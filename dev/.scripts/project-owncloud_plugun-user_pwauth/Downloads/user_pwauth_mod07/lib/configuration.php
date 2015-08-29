<?php
namespace OCA\user_pwauth\lib;

class Configuration {
	const appNameIndex = 1; 
	const DEFAULTS = [
		'list_delim'   => ';',
		'group_prefix' => '@',
		'min_id'       => '100',
		'uid_list'     => '1000-1100',
		'usr_list'     => '@users',
		'adm_list'     => '@wheel',
		'pwauth_path'  => '/usr/sbin/pwauth',
	];
	public $params = array();

	public function __construct($params = array()) {
		$this->params = $params;
	}

	public function dbPushParam($params = NULL) {
		if     (is_string($params)) $params = [ $params ];
		elseif ($params === NULL)   $params = array_keys(self::DEFAULTS);
		
		foreach($params as $param)
			if (isset($this->params[$param])) \OC_Appconfig::setValue(self::getAppName(), $param, $this->params[$param]);
	}
	public function dbPullParam($params = NULL) {
		if     (is_string($params)) $params = [ $params ];
		elseif ($params === NULL)   $params = array_keys(self::DEFAULTS);

		foreach($params as $param)
			$this->params[$param] = \OC_Appconfig::getValue(self::getAppName(), $param, self::DEFAULTS[$param]);
	}

	public function getParam($param){
		if (isset($this->params[$param])) {
			return (string)($this->params[$param]);
		} else {
			$this->params[$param] = \OC_Appconfig::getValue(self::getAppName(), $param, self::DEFAULTS[$param]);
			return (string)($this->params[$param]);
		}
	}


	public function test(){
		//return (string)'hello';
		//return (string)($defaults['group_prefix']);
		//return (string)($this->params['uid_list']);
		//return self::DEFAULTS['uid_list'];
		return (string)(__NAMESPACE__);
	}

	public static function getAppName(){
		return (string)(explode("\\",__NAMESPACE__)[self::appNameIndex]);
	}
	
	//public function __get(


	//public function updateList( $list = 'uid_list', 
}
