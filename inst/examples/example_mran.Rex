\dontrun{

list_mran_snapshots()

use_mran_snapshot("2020-01-01")

# validate=TRUE will detect an invalid snapshot date
use_mran_snapshot("1970-01-01", validate=TRUE)

}
