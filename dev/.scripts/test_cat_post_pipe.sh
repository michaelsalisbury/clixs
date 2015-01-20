#!/bin/bash


cat <<-END |
	one
	two
	three
END
echo wow :: `cat`


exec 46<<-END
	hello
	goodby
	END

echo cat :: `cat <&46 46<&-`

