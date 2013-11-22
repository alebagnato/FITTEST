for i in $(find . -name "*.java")
do
    if grep -q COPYRIGHT $i
    then
        echo $i
        sed '1,23d' $i >$i.new
        cat copyright.txt $i.new >$i
        rm $i.new
    fi
done
