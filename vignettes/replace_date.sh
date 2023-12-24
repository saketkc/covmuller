for file in *; do
  sed -i "s/$1/$2/g" "$file"
done
