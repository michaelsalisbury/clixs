#require 'vnc'
require 'net/vnc'

puts 'hello world'

Net::VNC.open '10.173.119.90:0', :shared => true, :password => '5678' do |vnc|
  vnc.pointer_move 10, 10
  vnc.type 'localcosadmin'
  vnc.key_press :tab
  vnc.type 'COSTech2010!'
  vnc.key_press :return
end

