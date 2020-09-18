#!/bin/sh -e

# NOTE: Heavily inspired by get-stack.hs script for installing stack.
# https://raw.githubusercontent.com/commercialhaskell/stack/stable/etc/scripts/get-stack.sh

HOME_LOCAL_BIN="$HOME/.local/bin"
WASP_TEMP_DIR=""

main() {
    trap cleanup_temp_dir EXIT
    install_based_on_os
}

install_based_on_os() {
    case "$(uname)" in
        "Linux")
            install_from_bin_package "wasp-linux-x86_64.tar.gz"
            ;;
        "Darwin")
            install_from_bin_package "wasp-osx-x86_64.tar.gz"
            ;;
        *)
            die "Sorry, this installer does not support your operating system: $(uname)."
    esac
}

# TODO: Add option to specify which release to download.


# Download a Wasp binary package and install it in $HOME_LOCAL_BIN.
install_from_bin_package() {
    PACKAGE_URL="https://github.com/wasp-lang/wasp/releases/latest/download/$1"
    check_dl_tools
    make_temp_dir

    info "Downloading binary package to temporary dir and unpacking it there..."
    dl_to_file "$PACKAGE_URL" "$WASP_TEMP_DIR/$1"
    mkdir -p "$WASP_TEMP_DIR/wasp"
    if ! tar xzf "$WASP_TEMP_DIR/$1" -C "$WASP_TEMP_DIR/wasp"; then
      die "Unpacking binary package failed."
    fi

    DEST_DIR="$HOME_LOCAL_BIN"
    if [ ! -d "$DEST_DIR" ]; then
        info "$DEST_DIR does not exit, creating it..."
        # First try to create directory as current user, then try with sudo if it fails.
        if ! mkdir -p "$DEST_DIR" 2>/dev/null; then
            die "Could not create directory: $DEST_DIR."
        fi
    fi

    info "Installing Wasp files to $DEST_DIR..."
    if ! mv "$WASP_TEMP_DIR/wasp" "$DEST_DIR/wasp"; then
        die "Install to $DEST_DIR failed."
    fi

    # TODO: I should make sure here that $DEST_DIR is abs path,
    #   which I am pretty sure is not at the moment.
    echo -e "#!/usr/bin/env bash\nwaspc_datadir=$DEST_DIR/wasp/data $DEST_DIR/wasp/wasp-bin" > "$DEST_DIR/wasp/wasp"
    if ! chmod +x "$DEST_DIR/wasp/wasp"; then
        die "Failed to make $DEST_DIR/wasp/wasp executable."
    fi

    info "Wasp has been successfully installed to: $DEST_DIR/wasp"
    info ""

    if ! on_path "$(dirname $DEST_DIR)"; then
        info "WARNING: It looks like '$(dirname $DEST_DIR)' is not on your PATH"
             ", add it if you want to be able to invoke `wasp` command directly from anywhere."
        info ""
    fi
}

# Creates a temporary directory, which will be cleaned up automatically
# when the script finishes
make_temp_dir() {
    WASP_TEMP_DIR="$(mktemp -d 2>/dev/null || mktemp -d -t wasp)"
}

# Cleanup the temporary directory if it's been created.  called automatically
# when the script exits.
cleanup_temp_dir() {
    if [ -n "$WASP_TEMP_DIR" ] ; then
        rm -rf "$WASP_TEMP_DIR"
        WASP_TEMP_DIR=""
    fi
}

# print a message to stderr and exit with error code
die() {
    echo "$@" >&2
    exit 1
}

info() {
    echo -e "\033[0;33m{= Wasp installer =}\033[0m" "$@"
}


# Download a URL to file using 'curl' or 'wget'.
dl_to_file() {
    if has_curl ; then
        echo "$1" "$2" "CURL"
        if ! curl ${QUIET:+-sS} --fail -L -o "$2" "$1"; then
            die "curl download failed: $1"
        fi
    elif has_wget ; then
        echo "$1" "$2" "WGET"
        if ! wget ${QUIET:+-q} "-O$2" "$1"; then
            die "wget download failed: $1"
        fi
    else
        # should already have checked for this, otherwise this message will probably
        # not be displayed, since dl_to_stdout will be part of a pipeline
        die "Neither wget nor curl is available, please install one to continue."
    fi
}

# Check for 'curl' or 'wget' and attempt to install 'curl' if neither found,
# or fail the script if that is not possible.
check_dl_tools() {
    if ! has_curl && ! has_wget ; then
        if ! try_install_pkgs curl ; then
            die "Neither wget nor curl is available, please install one to continue."
        fi
    fi
}

# Check whether 'wget' command exists
has_wget() {
    has_cmd wget
}

# Check whether 'curl' command exists
has_curl() {
    has_cmd curl
}

# Check whether the given command exists
has_cmd() {
    command -v "$1" > /dev/null 2>&1
}

# Check whether the given path is listed in the PATH environment variable
on_path() {
    # TODO: Contribute this back to stack?
    # NOTE: We normalize them before comparison by expanding ~ in both PATH and in argument.
    echo :"${PATH//:\~/:$HOME}": | grep -q :"${1/#\~/$HOME}":
}

main
