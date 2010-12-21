#!/bin/sh

INFILES="../examples/Frink.png ../examples/RunFormatting.R ../examples/*.odt"
OUTFILES="Frink.png RunFormatting.R *.odt"

# Copy all the ODT files from the examples directory.
# This includes "formattingOut.odt", which is overwritten
# when we source "RunFormatting.R".
for X in $INFILES; do
    # Overwrite any existing file
    cp -v $X .
done

# Process the ODT files with odfWeave
# Note that this needs to be kept in sync with INFILES
R --slave --vanilla <<'EOF'
library(odfWeave)
odts <- Sys.glob('*.odt')
control <- odfWeaveControl(verbose=FALSE)
for (odt in odts) {
  if (odt == 'formatting.odt') {
    cat(sprintf('Processing %s with RunFormatting.R...\n', odt))
    source('RunFormatting.R')
  } else if (odt == 'formattingOut.odt') {
    # skip this
  } else {
    cat(sprintf('Processing %s with odfWeave...\n', odt))
    out <- paste('n-', odt, sep='')
    odfWeave(odt, out, control=control)
  }
}
EOF

# Check all of the ODT files (both input and output)
python odf-validate *.odt

# Remove the files we copied from ../examples, and all
# of the files created by odfWeave.  But keep the XML
# and log files created by odf-validate, since those
# are the real output of this test script.
for X in $OUTFILES; do
  echo "Removing file $X"
  /bin/rm $X
done
