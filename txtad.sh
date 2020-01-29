#!/bin/bash
stack clean --full
TXTAD_FILE=$1 stack build