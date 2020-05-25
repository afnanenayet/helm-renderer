#!/usr/bin/env sh

# A script to generate the completions for the executable
mkdir -p completions

completion_script_filename="helm-renderer-completions"
exe_name="helm-renderer"

stack run -- --bash-completion-script "${exe_name}" > completions/"${completion_script_filename}".bash 
stack run -- --zsh-completion-script "${exe_name}" > completions/"${completion_script_filename}".zsh 
stack run -- --fish-completion-script "${exe_name}" > completions/"${completion_script_filename}".fish 
