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

$params = array();
$params['uid_list'] =    OC_Appconfig::getValue('user_pwauth', 'uid_list',    OC_USER_BACKEND_PWAUTH_UID_LIST);
$params['usr_list'] =    OC_Appconfig::getValue('user_pwauth', 'usr_list',    OC_USER_BACKEND_PWAUTH_USR_LIST);
$params['adm_list'] =    OC_Appconfig::getValue('user_pwauth', 'adm_list',    OC_USER_BACKEND_PWAUTH_ADM_LIST);
$params['pwauth_path'] = OC_Appconfig::getValue('user_pwauth', 'pwauth_path', OC_USER_BACKEND_PWAUTH_PATH);

if ($_POST) {
	// CSRF check
	OCP\JSON::callCheck();
	
	//foreach($params as $param){
	foreach((array_keys($params)) as $param){
		if(isset($_POST[$param])){
			//$value = OC_Appconfig::getValue('user_pwauth', 'uid_list', NULL);
			if ($params[$param] != $_POST[$param]) {
				OC_Appconfig::setValue('user_pwauth', $param, $_POST[$param]);
				$params[$param] = $_POST[$param];
				OC_Log::write('OC_USER_PWAUTH', 'POST :: ' . $param . '[' . $_POST[$param] . ']', 3);
			


			}
			
		}
	}
}

// fill template
$tmpl = new OC_Template( 'user_pwauth', 'settings');
foreach((array_keys($params)) as $param) $tmpl->assign( $param, $params[$param] );

return $tmpl->fetchPage();
?>
