#!/bin/bash

# File to store package URLs
PACKAGE_FILE="emacs_package_list.txt"

# Directory for packages
PACKAGE_DIR="packages"

# Create package directory if it doesn't exist
mkdir -p "$PACKAGE_DIR"

# Check if package file exists
if [ ! -f "$PACKAGE_FILE" ]; then
    echo "Package file $PACKAGE_FILE not found. Creating it..."
    touch "$PACKAGE_FILE"
    echo "Please add package URLs to $PACKAGE_FILE, one per line."
    exit 1
fi

# Read package URLs from file and add as submodules
while IFS= read -r url
do
    # Skip empty lines and comments
    [[ -z "$url" || "$url" =~ ^#.*$ ]] && continue
    
    # Extract package name from URL
    package_name=$(basename "$url" .git)
    
    echo "Adding $package_name..."
    git submodule add "$url" "$PACKAGE_DIR/$package_name"
done < "$PACKAGE_FILE"

echo "All packages added successfully!"
