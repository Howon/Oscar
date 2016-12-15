Error: for f in *; do
	sed -i old '1s/^/Error: /' "$f"
done
