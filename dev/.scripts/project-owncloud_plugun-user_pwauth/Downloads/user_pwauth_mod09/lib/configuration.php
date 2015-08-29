<?php
namespace OCA\user_pwauth\lib;

class Configuration {
	const appNameIndex = 1; 
	const DEFAULTS = array(
		'list_delim'   => ';',
		'group_prefix' => '@',
		'min_id'       => '100',
	);
	const LISTS = array(
		'uid_list' => 'uid_uids',
		'usr_list' => 'usr_uids',
		'adm_list' => 'adm_uids',
	);
	const GUI = array(
		'uid_list'                => '1000-1100',
		'usr_list'                => '@users',
		'adm_list'                => '@wheel',
		'pwauth_path'             => '/usr/sbin/pwauth',
		'domain_auth'             => 'winbindd',
		'pwauth_pam_include_path' => '/etc/pam.d/pwauth.owncloud',
	);
	const DOMAIN_AUTH_TYPES = array(
		'winbindd' => array(
			'name'            => 'Winbind',
			'log'             => '/usr/local/etc/owncloud/pwauth-owncloud_pam-modifier.log',
			'actions'         => [ 'clear', 'add', ],
			'add'             => 'uid',
		),
		'sssd'     => '',
	);
	private $config = array();

	public function __construct($config = array()) {
		$this->config = $config;
		$this->config['lists'] = self::LISTS;
		$this->config['defaults'] = array_merge(self::GUI, self::DEFAULTS);
		$this->config['params'] = array_merge($this->config['defaults'], array_flip(self::LISTS));
		$this->config['AppName'] = (string)(explode("\\",__NAMESPACE__)[self::appNameIndex]);
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
	
	public function getUIDS($params = NULL){
		if (is_null($params))       $params = self::LISTS;
		elseif (is_string($params)) $params = [ $params => self::LISTS[$params], ];
		elseif (is_array($params)) {
			foreach ($params as $index => $param) {
				unset($params[$index]);
				if (array_key_exists($param, self::LISTS))     $params[$index] = self::LISTS[$index];
				elseif (array_key_exists($param, self::LISTS)) $params[$param] = self::LISTS[$param];
			}
		}
		else return false;
		$uids = array();
		foreach ($params as $param => $list)
			foreach (explode($this->list_delim, $this->{$list}) as $uid)
				$uids[] = $uid;
			
		if (empty($uids)) return false;
		else              return $uids;
	}
	public function updateGIDS($param = 'uid_list'){
		if (!array_key_exists($param, self::LISTS)) return false;
		$list =	explode($this->list_delim, $this->{$param});
		$groups = array();
		foreach($list as $item) {
			$item = trim($item);
			if ((string)$item{0} === (string)$this->group_prefix)
				$groups[] = preg_replace('/\s+/',' ', $item);
		}
		foreach ($groups as $group) {
			$group = ltrim($group, $this->group_prefix);
			if (ctype_digit((string)$group))
				$posix = posix_getgrgid($group);
			else    $posix = posix_getgrnam($group);
			if ($posix !== false) {
				$group = $posix['name'];
				if (!\OC_Group::groupExists($group))
					\OC_Group::createGroup($group);
				$group_members=\OC_Group::usersInGroup($group);
				$uids_to_add=array_diff($posix['members'], $group_members);
				$uids_to_del=array_diff($group_members, $posix['members']);
				foreach ($uids_to_del as $uid)
					\OC_Group::removeFromGroup($uid,$group);
				foreach ($uids_to_add as $uid)
					\OC_Group::addToGroup($uid,$group);
				if (empty(\OC_Group::usersInGroup($group)))
					\OC_Group::deleteGroup($group);
			}
		}
	}
	public function updateUIDS($param = 'uid_list'){
		// fail if not a valid list
		if (!array_key_exists($param, self::LISTS)) return false;
		
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
			// check for group prefix
			} elseif ((string)$item{0} === (string)$this->group_prefix) {
				$groups[] = preg_replace('/\s+/',' ', $item);
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
				foreach ($posix['members'] as $user)
					foreach ([ posix_getpwnam($user) ] as $posix)
						if ($posix !== false) $uids[] = $posix['uid'];
		}
		// update datebase
		$this->config[$param] = $list;
		$this->dbPushParam($param);
		$old_uids = explode($this->list_delim, $this->{self::LISTS[$param]});
		$this->config[self::LISTS[$param]] = implode($this->list_delim, $uids);
		$this->dbPushParam(self::LISTS[$param]);
		
		// update groups
		$uids_to_add = array_diff($uids, $old_uids);
		$uids_to_del = array_diff($old_uids, $uids);
		if (empty($uids_to_add) && empty($uids_to_del)) return true;

		$group = (($param == 'adm_list') ? 'admin' : 'Users');
		if (!\OC_Group::groupExists($group))
			\OC_Group::createGroup($group);
		foreach ($uids_to_del as $uid)
			foreach ([ posix_getpwuid($uid) ] as $posix)
				if (($posix !== false) && (\OC_Group::inGroup($posix['name'],$group)))
					\OC_Group::removeFromGroup($posix['name'],$group);
		foreach ($uids_to_add as $uid)
			foreach ([ posix_getpwuid($uid) ] as $posix)
				if (($posix !== false) && (!\OC_Group::inGroup($posix['name'],$group)))
					\OC_Group::addToGroup($posix['name'],$group);
		return true;
	}



	private function getProperties($modifiers = \ReflectionProperty::IS_PRIVATE){
		// (int) $modifiers can be any combination of ::IS_STATIC, ::IS_PUBLIC, ::IS_PROTECTED, ::IS_PRIVATE
		// to combine delimit with pipe (|)
		return (new \ReflectionClass(__CLASS__))->getProperties($modifiers);
	}
}
