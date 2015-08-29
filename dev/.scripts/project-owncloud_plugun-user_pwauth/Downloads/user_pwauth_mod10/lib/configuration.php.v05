<?php
namespace OCA\user_pwauth\lib;

class Configuration {
	const appNameIndex = 1; 
	const DEFAULTS = [
		'list_delim'   => ';',
		'group_prefix' => '@',
		'min_id'       => '100',
	];
	const LISTS = [
		'uid_list' => 'uid_uids',
		'usr_list' => 'usr_uids',
		'adm_list' => 'adm_uids',
	];
	const GUI = [
		'uid_list'                => '1000-1100',
		'usr_list'                => '@users',
		'adm_list'                => '@wheel',
		'pwauth_path'             => '/usr/sbin/pwauth',
		'domain_auth'             => 'winbindd',
		'pwauth_pam_include_path' => '/etc/pam.d/pwauth.owncloud',
	];
	const DOMAIN_AUTH_TYPES = [
		'winbindd' => [
			'name'            => 'Winbind',
			'log'             => '/usr/local/etc/owncloud/pwauth-owncloud_pam-modifier.log',
			'actions'         => [ 'clear', 'add', ],
			'add'             => 'uid',
		],
		'sssd'     => '',
	];
	private $config = array();
	private $params = array();
	private $lists = array();
	private $defaults = array();
	
	private $AppName = "";

	public function __construct($config = array()) {
		$this->config = $config;
		$this->lists = self::LISTS;
		$this->defaults = array_merge(self::GUI, self::DEFAULTS);
		$this->params = array_merge($this->defaults, array_flip(self::LISTS));
		$this->AppName = (string)(explode("\\",__NAMESPACE__)[self::appNameIndex]);
		foreach ($this->getProperties() as $prop)
			$this->config[$prop->getName()] = &$this->{$prop->getName()};
	}

	private function dbPushParam($params = NULL) {
		if     (is_string($params)) $params = [ $params ];
		elseif ($params === NULL)   $params = array_keys($this->defaults);
		
		foreach($params as $param)
			if (isset($this->config[$param])) \OC_Appconfig::setValue(
				$this->AppName,
				$param,
				$this->config[$param]
			);
	}

	private function dbPullParam($params = NULL) {
		if     (is_string($params)) $params = [ $params ];
		elseif ($params === NULL)   $params = array_keys($this->defaults);

		foreach($params as $param)
			$this->config[$param] = \OC_Appconfig::getValue(
				$this->AppName,
				$param,
				$this->defaults[$param]
			);
	}

	public function __set($param, $data){
		if (isset($this->defaults[$param])) {
			$this->config[$param] = $data;
			$this->dbPushParam($param);
			return true;
		} else {
			return false;
		}
	}
	public function __isset($param){
		return isset($this->config[$param]);
	}
	public function __get($param){
		if (!isset($this->config[$param]) && isset($this->params[$param]))
			$this->dbPullParam($param);
		return $this->config[$param];
	}
	public function test(){
		//return (string)'hello';
		//return (string)($defaults['group_prefix']);
		//return (string)($this->params['uid_list']);
		//return self::DEFAULTS['uid_list'];
		return (string)(__NAMESPACE__);
	}
	
	
	public function getUIDS($param = 'uid_list'){
		return false;
	}
	public function updateUIDS($param = 'uid_list'){
		// fail if not a valid list
		if (!isset($this->lists[$param])) return false;
				\OC_Log::write(
					'OC_USER_PWAUTH',
					'updateUIDS :: ' . $param . ' :: ' . $this->lists[$param],
					3);

		
		$list =	explode($this->list_delim, $this->{$param});
		$uids = array();
		$ranges = array();
		$users = array();
		$groups = array();
		foreach($list as $item) {
			$item = trim($item);
			// check for numberic ranges
			if (strpos($item, "-") !== FALSE) {
				false;
				$range = explode("-", $item);
				$range[0] = (int)(trim($range[0]));
				$range[1] = (int)(trim($range[1]));
				if (($range[0] >= (int)$this->min_id) &&
				    ($range[0] < $range[1]))
					$ranges[] = $range[0] . '-' . $range[1]; 
					//for($i = $range[0]; $i <= $range[1]; $i++) $users[] = $i;
			// check for group prefix
			} elseif ((string)$item{0} === (string)$this->group_prefix) {
				$groups[] = preg_replace('/\s+/',' ', $item);
				//$groups[] = preg_replace('/\s+/',' ', ltrim($item, $this->group_prefix));
			// otherwise is user
			} else {
				$users[] = $item;
			}
		}
		$list = array_merge($ranges, $groups, $users);
		$list = implode($this->list_delim, $list);

		foreach ($ranges as $range) {
			$range = explode("-", $range);
			for($i = $range[0]; $i <= $range[1]; $i++) $uids[] = $i;
		}
		foreach ($users as $user) {
			if (ctype_digit((string)$user))
				$posix = posix_getpwuid($user);
			else    $posix = posix_getpwnam($user);
			if ($posix !== false)
				$uids[] = $posix['uid'];
		}
		foreach ($groups as $group) {
			$group = ltrim($group, $this->group_prefix);
			if (ctype_digit((string)$group))
				$posix = posix_getgrgid($group);
			else    $posix = posix_getgrnam($group);
			if ($posix !== false)
				foreach ($posix['members'] as $index => $user)
					foreach ([ posix_getpwnam($user) ] as $posix)
						if ($posix !== false) $uids[] = $posix['uid'];
		}
		//$this->config[self::LISTS[$param]] = implode($this->list_delim, $users);
		$this->config[$param] = $list;
		$this->dbPushParam($param);
		$this->config[self::LISTS[$param]] = implode($this->list_delim, $uids);
		$this->dbPushParam(self::LISTS[$param]);
		return true;
	}



	private function getProperties($modifiers = \ReflectionProperty::IS_PRIVATE){
		// (int) $modifiers can be any combination of ::IS_STATIC, ::IS_PUBLIC, ::IS_PROTECTED, ::IS_PRIVATE
		// to combine delimit with pipe (|)
		return (new \ReflectionClass(__CLASS__))->getProperties($modifiers);
	}
}
