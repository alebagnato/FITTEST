echo "disabling macos fix for: $1"
dst=${1%".orig"}
cp $1 $dst