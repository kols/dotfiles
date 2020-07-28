# zplug "zplug/zplug", hook-build:'zplug --self-manage'
# zplug supercrabtree/k
zplug "zsh-users/zsh-completions", use:src
zplug "zsh-users/zsh-history-substring-search"
zplug "zsh-users/zsh-syntax-highlighting"
zplug "zsh-users/zsh-autosuggestions"
zplug "rupa/z", use:z.sh
# zplug "k4rthik/git-cal", as:command
# zplug ezekg/xo, from:gh-r, as:command, use:*darwin_amd64*
zplug "peco/peco", from:gh-r, as:command, use:*darwin_amd64*
zplug "stedolan/jq", from:gh-r, as:command, rename-to:jq, use:*osx-amd64*
# zplug "b4b4r07/emoji-cli," on:stedolan/jq
# zplug "mrowa44/emojify," as:command, use:emojify
# zplug "motemen/ghq", from:gh-r, as:command, use:*darwin_amd64*
# zplug "openshift/source-to-image", from:gh-r, as:command, rename-to:s2i, use:*darwin-amd64*
zplug "mafredri/zsh-async", from:github
zplug "sindresorhus/pure", use:pure.zsh, from:github, as:theme
zplug "jeffkaufman/icdiff", use:"{git-icdiff,icdiff}", as:command

