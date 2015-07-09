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

#class USER_LDAP extends BackendUtility implements \OCP\IUserBackend, \OCP\UserInterface {
#\OCP\IUserBackend
#class OC_USER_PWAUTH extends OC_User_Backend implements \OCP\IUserBackend, \OCP\UserInterface {
class OC_USER_PWAUTH extends OC_User_Backend implements OC_User_Interface {
	protected $pwauth_bin_path;
	protected $pwauth_uid_list;
	private $user_search;

	public function __construct() {
		$this->pwauth_bin_path = OC_Appconfig::getValue('user_pwauth', 'pwauth_path', OC_USER_BACKEND_PWAUTH_PATH);
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
	
	// those functions are directly inspired by user_ldap
	
				#| OC_User_Backend::SET_PASSWORD
				#| SET_PASSWORD
	public function implementsAction($actions) {
		return (bool)((OC_USER_BACKEND_CHECK_PASSWORD
				| OC_USER_BACKEND_SET_PASSWORD
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

	public function checkPassword( $uid, $password ) {
		$uid = strtolower($uid);
		
		$unix_user = posix_getpwnam($uid);
		
		// checks if the Unix UID number is allowed to connect
		if(empty($unix_user)) return false; //user does not exist
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
 
                # Is the password valid?
	        $result = pclose( $handle );
                if ($result == 0){
			if (!in_array($unix_user['uid'], $this->pwauth_uid_list)) {
				$rng = new \OC\Security\SecureRandom();
				$msg = $rng->getMediumStrengthGenerator();
				$password = $msg->generate(30);
				OC_User::createUser($uid, $password);

				#$query = OC_DB::prepare('INSERT INTO `*PREFIX*users` ( `uid`, `password` ) VALUES( ?, ? )');
				#$result = $query->execute(array($uid, \OC::$server->getHasher()->hash($password)));

				$uid_list = OC_Appconfig::getValue('user_pwauth', 'uid_list', OC_USER_BACKEND_PWAUTH_UID_LIST);
				OC_Appconfig::setValue(
					'user_pwauth',
					'uid_list',
					(string)$uid_list . ';' . (string)$unix_user['uid']
				);
			}
			return $uid;
		}
                return false;
	}

	public function setPassword ( $uid, $password ) {
		$debug = debug_backtrace();
		$header = (string)('userExists' .
				' < ' .	$debug[1]['function'] .
				' < ' . $debug[2]['function'] .
				',');

		OC_Log::write(
			'OC_USER_PWAUTH',
			$header . ' user:' . (string)$uid . ' passwd:' . (string)$password,
			3);
		## Can't change the password whatever happened
		## but this is the only way to enable mail address change 
		## in the preferences
		return false;
	}
	
	public function userExists2( $uid ){
		$debug = debug_backtrace();
		$header = 'userExists' .
				' < ' .	$debug[1]['function'] .
				' < ' . $debug[2]['function'] .
				',';


		$user = posix_getpwnam( strtolower($uid) );
		#WORKS#OC_Log::write('OC_USER_PWAUTH', 'userExists called, user:' . $uid . ' posix:' . (string)$user, 3);
		#WORKS#OC_Log::write('OC_USER_PWAUTH', 'userExists called, posix:' . (string)(implode('|', $user )), 3);
		if (is_array($user)) {
			OC_Log::write(
				'OC_USER_PWAUTH',
				$header . ' user:' . (string)$uid . ' posix:' . (string)(implode('|', (array_values( $user )) )),
				3);
			#WORKS#OC_Log::write(
			#	'OC_USER_PWAUTH',
			#	$header . ' user:' . (string)$uid . ' posix:' . (string)(implode('|', (array_keys( $user )) )),
			#	3);
			#WORKS#OC_Log::write(
			#	'OC_USER_PWAUTH',
			#	$header . ' user:' . (string)$uid . ' posix:' . (string)$user['uid'],
			#	3);
			return true;
		} else {
			OC_Log::write(
				'OC_USER_PWAUTH',
				$header . ' user:' . $uid . ' :posix failed', 3);
			return false;
		}
		#return is_array($user);
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
