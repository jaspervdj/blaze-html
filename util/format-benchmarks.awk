#!/bin/awk -f
BEGIN {
    RS="\n\n"
    FS="\n"
}

{
    # Skip first record.
    if(NR == 1) next;

    # Take title.
    sub("benchmarking ", "", $1)
    # Take mean.
    sub(",.*", "", $4)
    sub("mean: ", "", $4)
    # Take std dev.
    sub(",.*", "", $5)

    # Print list item.
    printf("- `%s`: %s (%s)\n", $1, $4, $5)
}
