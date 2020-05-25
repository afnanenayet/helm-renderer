_helm-renderer()
{
    local CMDLINE
    local IFS=$'\n'
    CMDLINE=(--bash-completion-index $COMP_CWORD)

    for arg in ${COMP_WORDS[@]}; do
        CMDLINE=(${CMDLINE[@]} --bash-completion-word $arg)
    done

    COMPREPLY=( $(helm-renderer "${CMDLINE[@]}") )
}

complete -o filenames -F _helm-renderer helm-renderer
