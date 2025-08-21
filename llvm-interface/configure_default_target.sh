#!/bin/bash

set -eu

default_target="$1"

cat << EOF > obj/tmp-options-target.ads
package Options.Target is
   Default_Target_Triple : constant String := "$default_target";
end Options.Target;
EOF

./move-if-change obj/tmp-options-target.ads obj/options-target.ads
