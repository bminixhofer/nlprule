rules=$(cat $1 | grep "<rule.*" | wc -l)
rulegroups=$(cat $1 | grep "<rulegroup.*" | wc -l)
echo $(($rules-$rulegroups))