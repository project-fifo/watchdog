#!/usr/bin/bash

USER=watchdog
GROUP=$USER

case $2 in
    PRE-INSTALL)
        if grep "^$GROUP:" /etc/group > /dev/null 2>&1
        then
            echo "Group already exists, skipping creation."
        else
            echo Creating watchdog group ...
            groupadd $GROUP
        fi
        if id $USER > /dev/null 2>&1
        then
            echo "User already exists, skipping creation."
        else
            echo Creating watchdog user ...
            useradd -g $GROUP -d /var/db/watchdog -s /bin/false $USER
        fi
        echo Creating directories ...
        mkdir -p /var/db/watchdog
        chown -R watchdog:watchdog /var/db/watchdog
        mkdir -p /var/log/watchdog/sasl
        chown -R watchdog:watchdog /var/log/watchdog
        if [ -d /tmp/watchdog ]
        then
            chown -R watchdog:watchdog /tmp/watchdog/
        fi
        ;;
    POST-INSTALL)
        echo Importing service ...
        svccfg import /opt/local/fifo-watchdog/share/watchdog.xml
        echo Trying to guess configuration ...
        IP=`ifconfig net0 | grep inet | awk -e '{print $2}'`
        CONFFILE=/opt/local/fifo-watchdog/etc/watchdog.conf
        if [ ! -f "${CONFFILE}" ]
        then
            cp ${CONFFILE}.example ${CONFFILE}
            sed --in-place -e "s/127.0.0.1/${IP}/g" ${CONFFILE}
            md5sum ${CONFFILE} > ${CONFFILE}.md5
        elif [ -f ${CONFFILE}.md5 ] && md5sum --quiet --strict -c ${CONFFILE}.md5 2&> /dev/null
        then
            echo "The config was not adjusted we'll regenerate it."
            cp ${CONFFILE}.example ${CONFFILE}
            sed --in-place -e "s/127.0.0.1/${IP}/g" ${CONFFILE}
            md5sum ${CONFFILE} > ${CONFFILE}.md5
        else
            echo "The config file was changed updating old settings."
        fi
        ;;
esac
