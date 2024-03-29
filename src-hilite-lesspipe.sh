#! /bin/sh

for source in "$@"; do
    case $source in
	*ChangeLog|*changelog) 
        source-highlight --failsafe -f esc --lang-def=changelog.lang --style-file=esc256.style -i "$source" ;;
	*Makefile|*makefile) 
        source-highlight --failsafe -f esc --lang-def=makefile.lang --style-file=esc256.style -i "$source" ;;
	*.tar|*.tgz|*.gz|*.bz2|*.xz)
        lesspipe "$source" ;;
    .bash*)
        source-highlight --failsafe -f esc --lang-def=sh.lang --style-file=esc256.style -i "$source" ;;
    *) source-highlight --failsafe --infer-lang -f esc --style-file=esc256.style -i "$source" ;;
    esac
done
