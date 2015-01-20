cript for http://www.webupd8.org

#sudo apt-get install compiz compizconfig-settings-manager compiz-fusion-plugins-extra xautomation
ep(){ echo '##########################################################################';}
eh(){ ep; echo '#### '$*;}
rc(){ eh $*; $*; echo;}

rc gconftool-2 --get  /apps/metacity/keybinding_commands/command_1

#rc gconftool-2 -a --all-dirs /apps/compiz
#rc gconftool-2 -a --all-dirs /apps/compiz/general
#rc gconftool-2 -a --all-dirs /apps/compiz/general/screen0
rc gconftool-2 -a --all-dirs /apps/compiz/general/screen0/options

rc gconftool-2 --set --type int /apps/compiz/general/screen0/options/number_of_desktops "1"
rc gconftool-2 --get            /apps/compiz/general/screen0/options/number_of_desktops

rc gconftool-2 --set --type int /apps/compiz/general/screen0/options/hsize "3"
rc gconftool-2 --get            /apps/compiz/general/screen0/options/hsize

rc gconftool-2 --set --type int /apps/compiz/general/screen0/options/vsize "1"
rc gconftool-2 --get            /apps/compiz/general/screen0/options/vsize

rc gconftool-2 -a --all-dirs /apps/compiz/plugins/rotate/screen0/options

rc gconftool-2 --set --type float /apps/compiz/plugins/rotate/screen0/options/speed "1.75"
rc gconftool-2 --get              /apps/compiz/plugins/rotate/screen0/options/speed

rc gconftool-2 --set --type float /apps/compiz/plugins/rotate/screen0/options/zoom "0.5"
rc gconftool-2 --get              /apps/compiz/plugins/rotate/screen0/options/zoom

rc gconftool-2 --set --type float /apps/compiz/plugins/rotate/screen0/options/timestep "10.0"
rc gconftool-2 --get              /apps/compiz/plugins/rotate/screen0/options/timestep

exit 0

pluginlist=$(gconftool-2 --get --list-type string /apps/compiz/general/allscreens/options/active_plugins)
checkgrid=$(echo "$pluginlist" | grep grid)

if [[ ! $checkgrid ]]; then
        pluginstoset=$(echo "$pluginlist" | sed -e "s/]/\,grid]/")
        gconftool-2 --set --type list --list-type string /apps/compiz/general/allscreens/options/active_plugins "$pluginstoset"
fi


pluginlist2=$(gconftool-2 --get --list-type string /apps/compiz/general/allscreens/options/active_plugins)
checkcommands=$(echo "$pluginlist2" | grep commands)
if [[ ! $checkcommands ]]; then
        pluginstoset=$(echo "$pluginlist2" | sed -e "s/]/\,commands]/")
        gconftool-2 --set --type list --list-type string /apps/compiz/general/allscreens/options/active_plugins "$pluginstoset"
fi

gconftool-2 --set --type string /apps/metacity/keybinding_commands/command_1 "xte 'keydown Control_L' 'keydown Alt_L' 'key KP_1' 'keyup Control_L' 'keyup Alt_L'"
gconftool-2 --set --type string /apps/metacity/keybinding_commands/command_2 "xte 'keydown Control_L' 'keydown Alt_L' 'key KP_2' 'keyup Control_L' 'keyup Alt_L'"
gconftool-2 --set --type string /apps/metacity/keybinding_commands/command_3 "xte 'keydown Control_L' 'keydown Alt_L' 'key KP_3' 'keyup Control_L' 'keyup Alt_L'"
gconftool-2 --set --type string /apps/metacity/keybinding_commands/command_4 "xte 'keydown Control_L' 'keydown Alt_L' 'key KP_4' 'keyup Control_L' 'keyup Alt_L'"

gconftool-2 --set --type string /apps/metacity/keybinding_commands/command_5 "xte 'keydown Control_L' 'keydown Alt_L' 'key KP_6' 'keyup Control_L' 'keyup Alt_L'"
gconftool-2 --set --type string /apps/metacity/keybinding_commands/command_6 "xte 'keydown Control_L' 'keydown Alt_L' 'key KP_7' 'keyup Control_L' 'keyup Alt_L'"
gconftool-2 --set --type string /apps/metacity/keybinding_commands/command_7 "xte 'keydown Control_L' 'keydown Alt_L' 'key KP_8' 'keyup Control_L' 'keyup Alt_L'"
gconftool-2 --set --type string /apps/metacity/keybinding_commands/command_8 "xte 'keydown Control_L' 'keydown Alt_L' 'key KP_9' 'keyup Control_L' 'keyup Alt_L'"

gconftool-2 --set --type string /apps/compiz/plugins/commands/allscreens/options/run_command0_button '<BottomLeftEdge>Button3'
gconftool-2 --set --type string /apps/compiz/plugins/commands/allscreens/options/run_command1_button '<BottomEdge>Button3'
gconftool-2 --set --type string /apps/compiz/plugins/commands/allscreens/options/run_command2_button '<BottomRightEdge>Button3'
gconftool-2 --set --type string /apps/compiz/plugins/commands/allscreens/options/run_command3_button '<LeftEdge>Button3'

gconftool-2 --set --type string /apps/compiz/plugins/commands/allscreens/options/run_command4_button '<RightEdge>Button3'
gconftool-2 --set --type string /apps/compiz/plugins/commands/allscreens/options/run_command5_button '<TopLeftEdge>Button3'
gconftool-2 --set --type string /apps/compiz/plugins/commands/allscreens/options/run_command6_button '<TopEdge>Button3'
gconftool-2 --set --type string /apps/compiz/plugins/commands/allscreens/options/run_command7_button '<TopRightEdge>Button3'


