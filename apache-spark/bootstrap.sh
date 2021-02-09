#!/bin/bash
mkdir /home/work/logs && mkdir /opt/spark/work
cp /opt/spark/conf/spark-env.sh.template /opt/spark/conf/spark-env.sh
echo "export SPARK_LOG_DIR=/home/work/logs" >> /opt/spark/conf/spark-env.sh
su - work /opt/spark/sbin/start-master.sh