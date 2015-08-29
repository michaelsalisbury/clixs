<?php
/*
            DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE
                    Version 2, December 2004

 Copyright (C) 2004 Sam Hocevar <sam@hocevar.net>

 Everyone is permitted to copy and distribute verbatim or modified
 copies of this license document, and changing it is allowed as long
 as the name is changed.

            DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE
   TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION

  0. You just DO WHAT THE FUCK YOU WANT TO.

*/

/**
 * ownCloud
 *
 * @author Clément Véret
 * @copyright 2012 Clément Véret veretcle+owncloud@mateu.be
 *
 */

//class USER_LDAP extends BackendUtility implements \OCP\IUserBackend, \OCP\UserInterface {
class OC_USER_PWAUTH extends OC_User_Backend implements \OCP\IUserBackend, \OCP\UserInterface {
//class OC_USER_PWAUTH extends OC_User_Backend implements OC_User_Interface {
	protected $pwauth_bin_path;
	protected $pwauth_uid_list;
	protected $pwauth_usr_list;
	protected $pwauth_adm_list;
	private $user_search;

	public function __construct() {
		$this->pwauth_bin_path = OC_Appconfig::getValue('user_pwauth', 'pwauth_path', OC_USER_BACKEND_PWAUTH_PATH);
		$this->pwauth_usr_list = OC_Appconfig::getValue('user_pwauth', 'usr_list', OC_USER_BACKEND_PWAUTH_USR_LIST);
		$this->pwauth_adm_list = OC_Appconfig::getValue('user_pwauth', 'adm_list', OC_USER_BACKEND_PWAUTH_ADM_LIST);
		$list = explode(";", OC_Appconfig::getValue('user_pwauth', 'uid_list', OC_USER_BACKEND_PWAUTH_UID_LIST));
		$r = array();
		foreach($list as $entry) {
			if(strpos($entry, "-") === FALSE) {
				$r[] = $entry;
			} else {
				$range = explode("-", $entry); 
				if($range[0] < 0) { $range[0] = 0; }
				if($range[1] < $range[0]) { $range[1] = $range[0]; }

				for($i = $range[0]; $i <= $range[1]; $i++) {
					$r[] = $i;
				}
			}
		}
		$this->pwauth_uid_list = $r;
	}

	public function getBackendName(){
		return 'PWAUTH';
	}
	
	public function implementsActions($actions) {
		$log_header = $this->getDebugHeader();
		$this->log($log_header . (string)($this->getSupportedActions()) . ' actions:' . (string)$actions);
		return (bool)((
			OC_User_Backend::CHECK_PASSWORD |
			OC_User_Backend::SET_PASSWORD |
			OC_User_Backend::GET_DISPLAYNAME |
			OC_User_Backend::SET_DISPLAYNAME
			) & $actions);
	}
	
	private function userMatchesFilter($user) {
                return (strripos($user, $this->user_search) !== false);
        }
	
	public function deleteUser($_uid) {
		// Can't delete user
		OC_Log::write('OC_USER_PWAUTH', 'Not possible to delete local users from web frontend using unix user backend',3);
		return false;
	}
	/*
	private loadUser($uid) {
		if (empty($this->cache[$uid])) {
			$query = OC_DB::prepare('SELECT `uid`, `displayname` FROM `*PREFIX*users` WHERE LOWER(`uid`) = LOWER(?)');
			$result = $query->execute(array($uid));

			if (OC_DB::isError($result)) {
				OC_Log::write('core', OC_DB::getErrorMessage($result), OC_Log::ERROR);
				return false;
			}

			while ($row = $result->fetchRow()) {
				$this->cache[$uid]['uid'] = $row['uid'];
				$this->cache[$uid]['displayname'] = $row['displayname'];
			}
		}
		return true;
	}
	*/
	public function getDisplayName($uid) {
		$log_header = $this->getDebugHeader();
		$config = \OC::$server->getConfig();
		$displayName = $config->getUserValue($uid, 'pwauth', 'displayName', $uid);
		$this->log($log_header . ' user:' . (string)$uid . ' displayName:' . $displayName);
		return $displayName;
	}
	public function setDisplayName($uid, $displayName) {
		$log_header = $this->getDebugHeader();
		$this->log($log_header . ' user:' . (string)$uid . ' displayName:' . $displayName);
		$config = \OC::$server->getConfig();
		$config->setUserValue($uid, 'pwauth', 'displayName', $displayName);
	}

	private function getDebugHeader($depth = 6){
		$debug = debug_backtrace();
		$str = ' :: ';
		$cnt = 1;
		do {
			if ( $debug[$depth]['function'] === $debug[$depth - 1]['function'] ) {
				$cnt++;
			} else {
				$str = ($depth > 1 ? ' < ' : NULL) .
					($cnt > 1 ? $cnt . '*' : NULL) .
					$debug[$depth]['function'] . '[' . $depth . ']' . $str;
				$cnt = 1;
			}
			
		} while ( $depth-- > 1 );
		return $str;
	}
	private function log($line){
		OC_Log::write('OC_USER_PWAUTH', $line, 3);
	}

	public function checkPassword( $uid, $password ) {
		$log_header = $this->getDebugHeader();

		$uid = strtolower($uid);
		
		$unix_user = posix_getpwnam($uid);
		
		// checks if the Unix UID number is allowed to connect
		if (empty($unix_user)) {
			$this->log($log_header . ' user:' . $uid . ' (posix failed)');
			return false; //user does not exist
		} else {
			$this->log($log_header . ' user:' . $uid . ' (posix:' . (string)$unix_user['uid'] . ')');
		}
		//if(!in_array($unix_user['uid'], $this->pwauth_uid_list)) return false;
		
		
		$handle = popen($this->pwauth_bin_path, 'w');
                if ($handle === false) {
			// Can't open pwauth executable
			OC_Log::write('OC_USER_PWAUTH', 'Cannot open pwauth executable, check that it is installed on server.',3);
                        return false;
                }
 
                if (fwrite($handle, "$uid\n$password\n") === false) {
			// Can't pipe uid and password
                        return false;
                }
 
                // Is the password valid?
	        $result = pclose( $handle );
                if ($result == 0){
			if (!in_array($unix_user['uid'], $this->pwauth_uid_list)) {
				$uid_list = OC_Appconfig::getValue('user_pwauth', 'uid_list', OC_USER_BACKEND_PWAUTH_UID_LIST);
				OC_Appconfig::setValue(
					'user_pwauth',
					'uid_list',
					(string)$uid_list . ';' . (string)$unix_user['uid']
				);
			}
			return $uid;
		} else {
			$this->log($log_header . $uid . ' (posix:' . (string)$password . ':denied)');
		}
                return false;
	}

	public static function canUserChangePassword($uid) {
		$log_header = $this->getDebugHeader();
		$this->log($log_header . ' user:' . (string)$uid);
		return false;
	}
	public function setPassword ( $uid, $password ) {
		$log_header = $this->getDebugHeader();
		$this->log($log_header . ' user:' . (string)$uid . ' passwd:' . (string)$password);
		//## Can't change the password whatever happened
		//## but this is the only way to enable mail address change 
		//## in the preferences
		return false;
	}
	
	public function userExists( $uid ){
		$log_header = $this->getDebugHeader();
		$user = posix_getpwnam( strtolower($uid) );
		//#WORKS#OC_Log::write('OC_USER_PWAUTH', 'userExists called, user:' . $uid . ' posix:' . (string)$user, 3);
		//#WORKS#OC_Log::write('OC_USER_PWAUTH', 'userExists called, posix:' . (string)(implode('|', $user )), 3);
		if (is_array($user)) {
			$this->log($log_header . ' user:' . (string)$uid . ' posix:' . (string)(implode('|', (array_values( $user )) )));
			//WORKS#OC_Log::write(
			//	'OC_USER_PWAUTH',
			//	$header . ' user:' . (string)$uid . ' posix:' . (string)(implode('|', (array_keys( $user )) )),
			//	3);
			//WORKS#OC_Log::write(
			//	'OC_USER_PWAUTH',
			//	$header . ' user:' . (string)$uid . ' posix:' . (string)$user['uid'],
			//	3);
			return true;
		} else {
			$this->log($log_header . ' user:' . (string)$uid . ' :posix failed');
			return false;
		}
	}
	
	/*
	* this is a tricky one : there is no way to list all users which UID > 1000 directly in PHP
	* so we just scan all UIDs from $pwauth_min_uid to $pwauth_max_uid
	*
	* for OC4.5.* this functions implements limitation and offset via array_slice and search via array_filter using internal function userMatchesFilter
	*/
	public function getUsers($search = '', $limit = 10, $offset = 10){
		$returnArray = array();
		foreach($this->pwauth_uid_list as $f) {
			if(is_array($array = posix_getpwuid($f))) {
				$returnArray[] = $array['name'];
			}
		}
		
		$this->user_search = $search;
		if(!empty($this->user_search)) {
			$returnArray = array_filter($returnArray, array($this, 'userMatchesFilter'));
		} 
	
		if($limit = -1)
			$limit = null;
		
		return array_slice($returnArray, $offset, $limit);
	}
}

?>
