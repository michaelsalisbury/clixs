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
?>
<div class="section">
<form id="pwauth" action="#" method="post">
	<fieldset class="personalblock">
		<input type="submit" style="visibility: hidden;line-height:0;display:none" name="requesttoken"
			value="<?php echo $_['requesttoken'] ?>" id="requesttoken" />
		<legend><h2>Unix Authentication</h2></legend>
		<table><tr>
			<td><label for="pwauth_path">
				<?php echo $l->t('"pwauth" Command Path'); ?>
				</label><td>
			<td align="left" style="width:100%;">
				<input type="text" id="pwauth_path" name="pwauth_path" style="width:auto"
				value="<?php echo $_['pwauth_path']; ?>" />
				</td></tr></table>
		<table><tr>
			<td><label for="uid_list">
				<?php echo $l->t('UID/@GID Access List');?>
				</label></td>
			<td style="width:100%;">
				<input type="text" id="uid_list" name="uid_list" style="width:100%"
				value="<?php echo $_['uid_list']; ?>"
				original-title="<?php echo $l->t('uid_list_original-title'); ?>"/>
				<td></tr></table>
		<table><tr>
			<td><label for="usr_list">
					<?php echo $l->t('User/@Group Name Access List');?>
				</label></td>
			<td style="width:100%;">
				<input type="text" id="usr_list" name="usr_list" style="width:100%"
				value="<?php echo $_['usr_list']; ?>"
				original-title="<?php echo $l->t('usr_list_original-title'); ?>"/>
			<td></tr></table>
		<table><tr>
			<td><label for="adm_list">
					<?php echo $l->t('User/@Group Name Admin List');?>
				</label></td>
			<td style="width:100%;">
				<input type="text" id="adm_list" name="adm_list" style="width:100%"
				value="<?php echo $_['adm_list']; ?>"
				original-title="<?php echo $l->t('adm_list_original-title'); ?>"/>
			<td></tr></table>
		<p><?php echo $_['debug_data']; ?></p>
	</fieldset>
</form>
</div>
