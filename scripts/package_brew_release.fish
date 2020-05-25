# Create an archive to package a homebrew release. We need build the
# executable, shell completions, archive them, and then generate the SHA256
# checksum for the archive.

set release_name "helm-renderer-darwin"
set completion_dir "completions"
set exe_name "helm-renderer"
set release_dir "release"
set release_ext ".tar.gz"
set release_fname "$release_name$release_ext"

# Create the binary
mkdir -p $release_dir
stack install --local-bin-path $release_dir

# Generate completions
mkdir -p $release_dir/$completion_dir
./$release_dir/$exe_name --bash-completion-script $exe_name > $release_dir/$completion_dir/$exe_name.bash 
./$release_dir/$exe_name --zsh-completion-script $exe_name > $release_dir/$completion_dir/$exe_name.zsh 
./$release_dir/$exe_name --fish-completion-script $exe_name > $release_dir/$completion_dir/$exe_name.fish 

# Compress the package into an archive
tar -czf $release_fname -C $release_dir .
rm -rf $release_dir

# Generate the SHA256 checksum
echo "SHA256 hash:"
sha2 -256 -q $release_fname | tee $release_name.sha256
