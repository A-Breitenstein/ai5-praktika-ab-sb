#!/bin/sh
#
# Copyright (c) 2009-2013 Luca Garulli
#

echo "           .                                          "
echo "          .\`        \`                                 "
echo "          ,      \`:.                                  "
echo "         \`,\`    ,:\`                                   "
echo "         .,.   :,,                                    "
echo "         .,,  ,,,                                     "
echo "    .    .,.:::::  \`\`\`\`                                 :::::::::     :::::::::   "
echo "    ,\`   .::,,,,::.,,,,,,\`;;                      .:    ::::::::::    :::    :::  "
echo "    \`,.  ::,,,,,,,:.,,.\`  \`                       .:    :::      :::  :::     ::: "
echo "     ,,:,:,,,,,,,,::.   \`        \`         \`\`     .:    :::      :::  :::     ::: "
echo "      ,,:.,,,,,,,,,: \`::, ,,   ::,::\`   : :,::\`  ::::   :::      :::  :::    :::  "
echo "       ,:,,,,,,,,,,::,:   ,,  :.    :   ::    :   .:    :::      :::  :::::::     "
echo "        :,,,,,,,,,,:,::   ,,  :      :  :     :   .:    :::      :::  :::::::::   "
echo "  \`     :,,,,,,,,,,:,::,  ,, .::::::::  :     :   .:    :::      :::  :::     ::: "
echo "  \`,...,,:,,,,,,,,,: .:,. ,, ,,         :     :   .:    :::      :::  :::     ::: "
echo "    .,,,,::,,,,,,,:  \`: , ,,  :     \`   :     :   .:    :::      :::  :::     ::: "
echo "      ...,::,,,,::.. \`:  .,,  :,    :   :     :   .:    :::::::::::   :::     ::: "
echo "           ,::::,,,. \`:   ,,   :::::    :     :   .:    :::::::::     ::::::::::  "
echo "           ,,:\` \`,,.                                  "
echo "          ,,,    .,\`                                  "
echo "         ,,.     \`,                                          GRAPH DATABASE  "
echo "       \`\`        \`.                                          DISTRIBUTED   "
echo "                 \`\`                                         www.orientdb.org "
echo "                 \`                                    "

cd "$(dirname "$0")"

# resolve links - $0 may be a softlink
PRG="$0"

while [ -h "$PRG" ]; do
  ls=`ls -ld "$PRG"`
  link=`expr "$ls" : '.*-> \(.*\)$'`
  if expr "$link" : '/.*' > /dev/null; then
    PRG="$link"
  else
    PRG=`dirname "$PRG"`/"$link"
  fi
done

# Get standard environment variables
PRGDIR=`dirname "$PRG"`

# Only set ORIENTDB_HOME if not already set
[ -f "$ORIENTDB_HOME"/bin/orient.sh ] || ORIENTDB_HOME=`cd "$PRGDIR/.." ; pwd`
export ORIENTDB_HOME

if [ ! -f "${CONFIG_FILE}" ]
then
  CONFIG_FILE=$ORIENTDB_HOME/config/orientdb-dserver-config.xml
fi

# Set JavaHome if it exists
if [ -f "${JAVA_HOME}/bin/java" ]; then 
   JAVA=${JAVA_HOME}/bin/java
else
   JAVA=java
fi
export JAVA

LOG_FILE=$ORIENTDB_HOME/config/orientdb-server-log.properties
WWW_PATH=$ORIENTDB_HOME/www
ORIENTDB_SETTINGS="-Dprofiler.enabled=true -Dcache.level1.enabled=false -Dcache.level2.enabled=false"
JAVA_OPTS_SCRIPT="-XX:+HeapDumpOnOutOfMemoryError -Djava.awt.headless=true -Dfile.encoding=UTF8"

$JAVA -server $JAVA_OPTS $JAVA_OPTS_SCRIPT $ORIENTDB_SETTINGS -Djava.util.logging.config.file="$LOG_FILE" -Dorientdb.config.file="$CONFIG_FILE" -Dorientdb.www.path="$WWW_PATH" -Dorientdb.build.number="1057" -cp "$ORIENTDB_HOME/lib/orientdb-server-1.6.1.jar:$ORIENTDB_HOME/lib/*" com.orientechnologies.orient.server.OServerMain
