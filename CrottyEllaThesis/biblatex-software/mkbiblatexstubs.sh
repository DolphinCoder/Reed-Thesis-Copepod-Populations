#!/bin/bash
while read NAME
do
# bbx
cat <<EOF > $NAME+sw.bbx
\ProvidesFile{$NAME+sw.bbx}[2020/04/02 $NAME style with software, Roberto Di Cosmo]
\RequireBibliographyStyle{$NAME}
\blx@inputonce{software.bbx}{biblatex style for software}{}{}{}{}
\endinput
EOF
# dbx
cat <<EOF > $NAME+sw.dbx
\ProvidesFile{$NAME+sw.dbx}[2020/04/02 biblatex-software data model, Roberto Di Cosmo]
\blx@inputonce{software.dbx}{biblatex data model extension for software}{}{}{}{}
\endinput
EOF
# cbx
cat <<EOF > $NAME+sw.cbx
\ProvidesFile{$NAME+sw.bbx}[2020/04/02 $NAME citation style with software, Roberto Di Cosmo]
\RequireCitationStyle{$NAME}
\endinput
EOF
done< stublist
