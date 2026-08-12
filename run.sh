#!/bin/bash
cd "$(dirname "$0")"
sbcl --script brain.lisp
