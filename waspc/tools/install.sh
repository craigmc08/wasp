#!/bin/sh -e

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

# # Attempts to install on macOS.
# # If 'brew' exists, installs using Homebrew.  Otherwise, installs
# # the generic bindist.
# do_osx_install() {
#     info "Using generic bindist..."
#     info ""
#     install_64bit_osx_binary
#     info "NOTE: You may need to run 'xcode-select --install' and/or"
#     info "      'open /Library/Developer/CommandLineTools/Packages/macOS_SDK_headers_for_macOS_10.14.pkg'"
#     info "      to set up the Xcode command-line tools, which Stack uses."
#     info ""
# }
