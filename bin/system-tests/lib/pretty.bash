# --------------------------------------------------------------
#
# This script provides utilities for pretty printing
# info during system tests.
#
# --------------------------------------------------------------

# DESC
#   Prints green text (if connected to a TTY)
# ARGS
# - 1 : The text to print green
green () {
    if [[ -t 1 ]]; then
        echo -ne "\x1b[01;32m"
        echo -n "${1}"
        echo -ne "\x1b[00m"
    else
        echo -n "${1}"
    fi
}

# DESC
#   Prints red text (if connected to a TTY)
# ARGS
# - 1 : The text to print red
red () {
    if [[ -t 1 ]]; then
        echo -ne "\x1b[01;31m"
        echo -n "${1}"
        echo -ne "\x1b[00m"
    else
        echo -n "${1}"
    fi
}

# DESC
#   Prints bold text (if connected to a TTY)
# ARGS
# - 1 : The text to print bold
bold () {
    if [[ -t 1 ]]; then
        echo -ne "\x1b[01;1m"
        echo -n "${1}"
        echo -ne "\x1b[00m"
    else
        echo -n "${1}"
    fi
}

# DESC
#   Prints a rule
rule () {
    echo -n "-----------------"
    echo -n "-----------------"
    echo -n "-----------------"
    echo    "-----------------"
}

# DESC
#   Prints a lighter rule
rule_light () {
    echo -n "................."
    echo -n "................."
    echo -n "................."
    echo "................."
}
