ps aux | grep sbcl | grep -v grep | awk '{print "lsof -p " $2 " | tail"}' | sh
