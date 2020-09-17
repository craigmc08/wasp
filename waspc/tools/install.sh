#!/bin/sh -e

# NOTE: Heavily inspired by get-stack.hs script for installing stack.
# https://raw.githubusercontent.com/commercialhaskell/stack/stable/etc/scripts/get-stack.sh

HOME_LOCAL_BIN="$HOME/.local/bin"
WASP_TEMP_DIR=""

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

do_osx_install() {
    info "Using generic bindist..."
    info ""
    install_64bit_osx_binary
    info "NOTE: You may need to run 'xcode-select --install' and/or"
    info "      'open /Library/Developer/CommandLineTools/Packages/macOS_SDK_headers_for_macOS_10.14.pkg'"
    info "      to set up the Xcode command-line tools, which Stack uses."
    info ""
}

# Determine operating system and attempt to install.
do_os() {
    case "$(uname)" in
        "Linux")
            set_default_dest
            install_from_bindist "linux-x86_64.tar.gz"
            ;;
        "Darwin")
            set_default_dest
            install_from_bindist "osx-x86_64.tar.gz"
            ;;
        *)
            die "Sorry, this installer does not support your operating system: $(uname)."
    esac
}

# Download a URL to file using 'curl' or 'wget'.
dl_to_file() {
    if has_curl ; then
        if ! curl ${QUIET:+-sS} -L -o "$2" "$1"; then
            die "curl download failed: $1"
        fi
    elif has_wget ; then
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

# Download a Wasp bindist and install it in /usr/local/bin/wasp.
install_from_bindist() {
    RELEASE_URL="https://github.com/wasp-lang/wasp/releases/latest/download"
    check_dl_tools
    make_temp_dir

    dl_to_file "$RELEASE_URL" "$WASP_TEMP_DIR/$1.bindist"
    mkdir -p "$WASP_TEMP_DIR/$1"
    if ! tar xzf "$WASP_TEMP_DIR/$1.bindist" -C "$WASP_TEMP_DIR/$1"; then
      die "Unpacking bindist failed"
    fi
    # TODO: Stopped here.
    WASP_TEMP_EXE="$WASP_TEMP_DIR/$(basename "$DEST")" # TODO: What is this DEST?
    mv "$WASP_TEMP_DIR/$1"/*/wasp "$WASP_TEMP_EXE"
    destdir="$(dirname "$DEST")"
    if [ ! -d "$destdir" ]; then
        info "$destdir directory does not exist; creating it..."
        # First try to create directory as current user, then try with sudo if it fails.
        if ! mkdir -p "$destdir" 2>/dev/null; then
            if ! sudocmd "create the destination directory" mkdir -p "$destdir"; then
                die "Could not create directory: $DEST"
            fi
        fi
    fi
    # First attempt to install 'stack' as current user, then try with sudo if it fails
    info "Installing Stack to: $DEST..."
    if ! install -c -m 0755 "$STACK_TEMP_EXE" "$destdir" 2>/dev/null; then
      if ! sudocmd "copy 'stack' to the destination directory" install -c -o 0 -g 0 -m 0755 "$STACK_TEMP_EXE" "$destdir"; then
        die "Install to $DEST failed"
      fi
    fi

    post_install_separator
    info "Stack has been installed to: $DEST"
    info ""

    check_dest_on_path
}

install_x86_64_linux_binary() {
}

install_64bit_osx_binary() {
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
    echo ":$PATH:" | grep -q :"$1":
}

trap cleanup_temp_dir EXIT

check_stack_installed
do_os
check_home_local_bin_on_path
