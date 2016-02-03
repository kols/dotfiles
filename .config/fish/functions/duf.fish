function duf
	command du -sk $argv | sort -n | while read size fname
        for unit in k M G T P E Z Y
            if [ $size -lt 1024 ]
                echo -e "$size$unit\t$fname"
                break
            end
            set -l size (math $size/1024)
        end
    end
end
