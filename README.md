# Big Book of Bees 🐝

Data management rules of thumb:

* New release = new edition and new names for all files
* Do not label files by date, label them by content
* Try to avoid spaces in file names
* Share data as .csv when possible (make tidydata!)
  * Each row is an observation
  * Enter "unknown" for unknown, use NA only for truly missing data
  * Take note of column contents and rules where possible in a data dictionary
  * Never delete data - just make a new column to annotate its QC removal