for file in *; do
  gsed -i "s/$1/$2/g" "$file"
done
