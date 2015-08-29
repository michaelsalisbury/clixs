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

$params = array('uid_list', 'pwauth_path', 'test_code');

if ($_POST) {
	// CSRF check
	OCP\JSON::callCheck();
	
	foreach($params as $param){
		if(isset($_POST[$param])){
			OC_Appconfig::setValue('user_pwauth', $param, $_POST[$param]);
		}
	}
}

// fill template
$tmpl = new OC_Template( 'user_pwauth', 'settings');
$tmpl->assign( 'uid_list', OC_Appconfig::getValue('user_pwauth', 'uid_list', OC_USER_BACKEND_PWAUTH_UID_LIST));
$tmpl->assign( 'pwauth_path', OC_Appconfig::getValue('user_pwauth', 'pwauth_path', OC_USER_BACKEND_PWAUTH_PATH));
$tmpl->assign( 'test_code', OC_Appconfig::getValue('user_pwauth', 'test_code', OC_USER_BACKEND_TEST_CODE));

return $tmpl->fetchPage();
?>
