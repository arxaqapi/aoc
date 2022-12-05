
day=$1
filename="day$day.ml"

echo "let filename = \"data/$day""_input\"" >> "lib/"$filename
touch "data/$day""_input"