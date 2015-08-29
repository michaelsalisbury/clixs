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
 * ownCloud - user_pwauth
 *
 * @author Clément Véret
 * @copyright 2012 Clément Véret veretcle+owncloud@mateu.be
 *
 */

OC_Util::checkAdminUser();

$config = new \OCA\user_pwauth\lib\Configuration();
OC_Log::write('OC_USER_PWAUTH', 'LIB :: ' . $config->test(), 3);
OC_Log::write('OC_USER_PWAUTH', 'LIB :: ' . $config->params['adm_list'], 3);

$params = array();
$params['uid_list'] =      OC_Appconfig::getValue('user_pwauth', 'uid_list',      OC_USER_BACKEND_PWAUTH_UID_LIST);
$params['usr_list'] =      OC_Appconfig::getValue('user_pwauth', 'usr_list',      OC_USER_BACKEND_PWAUTH_USR_LIST);
$params['adm_list'] =      OC_Appconfig::getValue('user_pwauth', 'adm_list',      OC_USER_BACKEND_PWAUTH_ADM_LIST);
$params['pwauth_path'] =   OC_Appconfig::getValue('user_pwauth', 'pwauth_path',   OC_USER_BACKEND_PWAUTH_PATH);
$params['list_delim'] =    OC_Appconfig::getValue('user_pwauth', 'list_delim',    OC_USER_BACKEND_PWAUTH_LIST_DELIM);
$params['group_prefix'] =  OC_Appconfig::getValue('user_pwauth', 'group_prefix',  OC_USER_BACKEND_PWAUTH_GROUP_PREFIX);
$params['min_id'] =        OC_Appconfig::getValue('user_pwauth', 'min_id',        OC_USER_BACKEND_PWAUTH_MIN_ID);



#$params['usr_list'] = $config->test();

if ($_POST) {
	// CSRF check
	OCP\JSON::callCheck();
	
	//foreach($params as $param){
	foreach((array_keys($params)) as $param){
		if(isset($_POST[$param]) && $params[$param] != $_POST[$param] ){
			OC_Log::write('OC_USER_PWAUTH', 'POST :: ' . $param . '[' . $_POST[$param] . ']', 3);
			OC_Appconfig::setValue('user_pwauth', $param, $_POST[$param]);
			$params[$param] = $_POST[$param];

			continue;
			// parse lists into uids
			switch ($param) {
				case 'uid_list':
				case 'usr_list':
				case 'adm_list':
					$list =	explode($params['list_delim'], $params[$param]);
					$users = array();
					$groups = array();
					foreach($list as $item) {
						$item = trim($item);
						#$item = trim(preg_replace('/\s+/',' ', $str))
						if ($item{0} === $params['group_prefix']) {
							$group[] = preg_replace('/\s+/',' ', ltrim($item, $params['group_prefix']));
						} elseif (strpos($item, "-") === FALSE) {
							$users[] = $item;
						} else {
							$range = explode("-", $item);
							$range[0] = (int)(trim(range[0]));
							$range[1] = (int)(trim(range[1]));
							if ($range[0] >= (int)$params['min_id'] &&
							    $range[0] < $range[1])
								for($i = $range[0]; $i <= $range[1]; $i++) $users[] = $i;
						}
					}
		
					break;
			}

			// if 'adm_list' update admin group 
			switch ($param) {
				case 'adm_list':

				break;
			}
		}
	}
}

// fill template
$tmpl = new OC_Template( 'user_pwauth', 'settings');
foreach((array_keys($params)) as $param) $tmpl->assign( $param, $params[$param] );

return $tmpl->fetchPage();
?>
