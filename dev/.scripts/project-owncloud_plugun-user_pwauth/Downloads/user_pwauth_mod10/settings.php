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
$config_lists = \OCA\user_pwauth\lib\Configuration::LISTS;

if ($_POST) {
	// CSRF check
	OCP\JSON::callCheck();

	foreach($config->defaults as $param => $data){
		if (isset($_POST[$param]) && $config->{$param} != $_POST[$param]) {
			$config->{$param} = $_POST[$param];
			if (isset($config_lists[$param])){
				/**
				foreach ($config_lists as $key => $item)
				OC_Log::write(
					'OC_USER_PWAUTH',
					'SETTINGS::POST::' . $param . '|'. $key . '=' . $item . ' :: ' . $config_lists[$param],
					3);
				*/
				$config->updateUIDS($param);
				$config->updateGIDS($param);
			}
		}
	}
}
// fill template
$tmpl = new OC_Template( $config->AppName, 'settings');
foreach($config->defaults as $param => $data)
	$tmpl->assign( $param, $config->{$param} );

return $tmpl->fetchPage();
?>
